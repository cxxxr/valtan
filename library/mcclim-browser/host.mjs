import {EventQueue} from './event-queue.mjs';
import {pointerGrabCommands} from './pointer.mjs';

// Called inside the worker before loading the Valtan application bundle.
// All methods are closures: Lisp may extract and FUNCALL them without a JS receiver.
export function createCanvasHost(buffer, send = (...args) => postMessage(...args)) {
  const events = new EventQueue(buffer);
  const windows = new Map();
  const measuring = new OffscreenCanvas(1, 1).getContext('2d');
  let serial = 0;
  function createWindow(title, width, height, parent = null, managed = true) {
    const canvas = new OffscreenCanvas(Math.max(1, Math.ceil(width)), Math.max(1, Math.ceil(height)));
    const window = {id: ++serial, canvas, context: canvas.getContext('2d'), title: String(title),
      parent:parent?.id ?? null, managed, x:0, y:0, dirty: true};
    windows.set(window.id, window);
    send({type: 'create-window', id: window.id, title: window.title, width: canvas.width, height: canvas.height,
      parent:window.parent, managed});
    return window;
  }
  function present(window) {
    // transferToImageBitmap clears its canvas. Copy through a presentation
    // surface so incremental Lisp drawing keeps the original backing store.
    const copy = new OffscreenCanvas(window.canvas.width, window.canvas.height);
    copy.getContext('2d').drawImage(window.canvas, 0, 0);
    const bitmap = copy.transferToImageBitmap();
    send({type: 'paint', id: window.id, bitmap}, [bitmap]);
    window.dirty = false;
  }
  function destroyWindow(window) {
    if (!windows.has(window.id)) return;
    for (const child of [...windows.values()]) if (child.parent === window.id) destroyWindow(child);
    windows.delete(window.id); send({type:'destroy-window', id:window.id});
  }
  const host = {
    ...pointerGrabCommands(send),
    resizeViewport: (width, height) => { host.width = width; host.height = height; },
    createWindow, present,
    pattern: makeCanvasPattern,
    flipArea: flipCanvasArea,
    paintDesign: paintCanvasDesign,
    designStencil: canvasDesignStencil,
    createSurface: (width, height) => {
      const canvas = new OffscreenCanvas(Math.max(1, Math.ceil(width)), Math.max(1, Math.ceil(height)));
      return {canvas, context: canvas.getContext('2d')};
    },
    copyArea: copyCanvasArea,
    releaseSurface: surface => {surface.canvas.width=0;surface.canvas.height=0;},
    flush: () => {for (const window of windows.values()) if (window.dirty) present(window);},
    nextEvent: timeout => events.take(timeout == null ? Infinity : Number(timeout)),
    getWindow: id => windows.get(Number(id)),
    destroyWindow,
    resizeWindow: (window, width, height) => {
      width = Math.max(1, Math.ceil(width)); height = Math.max(1, Math.ceil(height));
      if (width === window.canvas.width && height === window.canvas.height) return;
      const saved = new OffscreenCanvas(window.canvas.width, window.canvas.height);
      saved.getContext('2d').drawImage(window.canvas, 0, 0);
      window.canvas.width = width; window.canvas.height = height;
      window.context.drawImage(saved, 0, 0);
      window.dirty = true;
      send({type: 'resize-window', id: window.id, width, height});
    },
    title: (window, title) => send({type: 'window-title', id: window.id, title: String(title)}),
    moveWindow: (window, x, y) => {
      window.x = Number(x); window.y = Number(y);
      send({type:'move-window', id:window.id, x:window.x, y:window.y});
    },
    raiseWindow: window => send({type:'window-raise', id:window.id}),
    buryWindow: window => send({type:'window-bury', id:window.id}),
    minimizeWindow: window => send({type:'window-minimize', id:window.id}),
    visible: (window, visible) => send({type: 'window-visible', id: window.id, visible: Boolean(visible)}),
    cursor: (window, cursor) => send({type: 'cursor', id: window.id, cursor: String(cursor)}),
    canvasCall: (window, operation, args) => {
      window.dirty = true;
      return window.context[String(operation)](...args);
    },
    canvasSet: (window, property, value) => { window.context[String(property)] = value; },
    measureText: (font, text) => {
      measuring.font = String(font);
      const metrics = measuring.measureText(String(text));
      return {width: metrics.width, ascent: metrics.fontBoundingBoxAscent ?? metrics.actualBoundingBoxAscent,
        descent: metrics.fontBoundingBoxDescent ?? metrics.actualBoundingBoxDescent,
        left: metrics.actualBoundingBoxLeft, right: metrics.actualBoundingBoxRight,
        actualAscent: metrics.actualBoundingBoxAscent, actualDescent: metrics.actualBoundingBoxDescent};
    },
    ready: () => send({type: 'ready'}),
    status: text => send({type: 'status', text: String(text)}),
    error: error => send({type: 'error', message: String(error?.stack ?? error)}),
  };
  return host;
}

// Copy pixel values, including transparent pixels, rather than compositing ink.
// Snapshot the source first so overlapping copies behave like a temporary pixmap.
export function copyCanvasArea(source,sx,sy,width,height,destination,dx,dy,clip=null) {
  const left=Math.max(0,-sx,-dx),top=Math.max(0,-sy,-dy);
  const right=Math.min(width,source.canvas.width-sx,destination.canvas.width-dx);
  const bottom=Math.min(height,source.canvas.height-sy,destination.canvas.height-dy);
  width=right-left;height=bottom-top;
  if(width<=0 || height<=0)return;
  sx+=left;sy+=top;dx+=left;dy+=top;
  const input=source.context.getImageData(sx,sy,width,height);
  if(!clip){destination.context.putImageData(input,dx,dy);destination.dirty=true;return;}
  const output=destination.context.getImageData(dx,dy,width,height),data=output.data;
  const mask=clip.context.getImageData(dx,dy,width,height).data,pixels=input.data;
  for(let i=0;i<data.length;i+=4){
    const weight=mask[i+3]/255;
    if(!weight)continue;
    const alpha=pixels[i+3]*weight+data[i+3]*(1-weight);
    for(let c=0;c<3;c++)data[i+c]=alpha?(pixels[i+c]*pixels[i+3]*weight+data[i+c]*data[i+3]*(1-weight))/alpha:0;
    data[i+3]=alpha;
  }
  destination.context.putImageData(output,dx,dy);destination.dirty=true;
}

// Flipping inks exchange two solid designs and must be an involution. Canvas
// 'difference' with an antialiased source does not meet that requirement.
// Quantize shape coverage once and XOR opaque device pixels. Transparent
// destination pixels and nonsolid design samples are left unchanged; CLIM does
// not specify a color exchange for those samples.
export function flipCanvasArea(destination, coverage, first, second) {
  const {width,height}=destination.canvas;
  const image=destination.context.getImageData(0,0,width,height),pixels=image.data;
  const mask=coverage.context.getImageData(0,0,width,height).data;
  const a=first.context.getImageData(0,0,width,height).data;
  const b=second.context.getImageData(0,0,width,height).data;
  for(let i=0;i<pixels.length;i+=4){
    if(mask[i+3]<128 || pixels[i+3]!==255 || a[i+3]!==255 || b[i+3]!==255)continue;
    pixels[i]^=a[i]^b[i]; pixels[i+1]^=a[i+1]^b[i+1]; pixels[i+2]^=a[i+2]^b[i+2];
  }
  destination.context.putImageData(image,0,0);
  destination.dirty=true;
}

// A flipping design is an operation on existing pixels, not an RGBA image.
// Keep that operation through OVER, masks and indexed/tiled patterns. Ordinary
// subdesigns are still rasterized by Canvas and arrive as paint surfaces.
function compileCanvasDesign(design) {
  const sample = surface => {
    const {width,height}=surface.canvas;
    const data=surface.context.getImageData(0,0,width,height).data;
    return (x,y) => {
      x=Math.floor(x);y=Math.floor(y);
      return x<0 || y<0 || x>=width || y>=height ? null : data.subarray(4*(y*width+x),4*(y*width+x+1));
    };
  };
  const mix = (color,before,weight) => {
    const alpha=before[3]*(1-weight)+color[3]*weight;
    for(let i=0;i<3;i++)color[i]=alpha ? (before[i]*before[3]*(1-weight)+color[i]*color[3]*weight)/alpha : 0;
    color[3]=alpha;
  };
  if(design.operation==='paint'){
    const pixel=sample(design.surface);
    return {flipping:false,opacity:(x,y)=>pixel(x,y)?.[3]??0,paint(color,x,y){
      const source=pixel(x,y);if(!source || !source[3])return;
      const alpha=source[3]+color[3]*(1-source[3]/255);
      for(let i=0;i<3;i++)color[i]=(source[i]*source[3]+color[i]*color[3]*(1-source[3]/255))/alpha;
      color[3]=alpha;
    }};
  }
  if(design.operation==='flip'){
    const first=sample(design.first),second=sample(design.second);
    const opaque=(a,b)=>a?.[3]===255 && b?.[3]===255;
    return {flipping:true,opacity:(x,y)=>opaque(first(x,y),second(x,y))?255:0,paint(color,x,y){
      const a=first(x,y),b=second(x,y);
      if(Math.round(color[3])!==255 || !opaque(a,b))return;
      for(let i=0;i<3;i++)color[i]=Math.round(color[i])^a[i]^b[i];
    }};
  }
  if(design.operation==='over'){
    const foreground=compileCanvasDesign(design.foreground),background=compileCanvasDesign(design.background);
    return {flipping:foreground.flipping||background.flipping,
      opacity(x,y){const a=foreground.opacity(x,y);return a+background.opacity(x,y)*(1-a/255);},
      paint(color,x,y){background.paint(color,x,y);foreground.paint(color,x,y);}};
  }
  if(design.operation==='mask'){
    const ink=compileCanvasDesign(design.ink),mask=sample(design.mask);
    const weight=(x,y)=>{const a=(mask(x,y)?.[3]??0)/255;return design.outside?1-a:a;};
    return {flipping:ink.flipping,opacity:(x,y)=>ink.opacity(x,y)*weight(x,y),paint(color,x,y){
      let amount=weight(x,y);
      // Fractional flipping coverage is unspecified by CLIM. The same binary
      // convention as flipCanvasArea preserves exact twice-drawn restoration.
      if(ink.flipping)amount=amount>=0.5?1:0;
      if(!amount)return;
      const before=color.slice();ink.paint(color,x,y);
      if(amount!==1)mix(color,before,amount);
    }};
  }
  if(design.operation==='indexed' || design.operation==='tile'){
    const tile=design.operation==='tile',{width,height,matrix}=design;
    const children=tile?[compileCanvasDesign(design.ink)]:Array.from(design.palette,compileCanvasDesign);
    const select=(x,y)=>{
      const u=matrix[0]*x+matrix[2]*y+matrix[4],v=matrix[1]*x+matrix[3]*y+matrix[5];
      if(!(width>0 && height>0))return null;
      x=tile?((u%width)+width)%width:u;y=tile?((v%height)+height)%height:v;
      if(x<0 || y<0 || x>=width || y>=height)return null;
      const child=tile?children[0]:children[design.indices[Math.floor(y)*width+Math.floor(x)]];
      if(!child)throw Error('Invalid indexed design palette entry');
      return {child,x,y};
    };
    return {flipping:children.some(child=>child.flipping),
      opacity(x,y){const selected=select(x,y);return selected?selected.child.opacity(selected.x,selected.y):0;},
      paint(color,x,y){const selected=select(x,y);if(selected)selected.child.paint(color,selected.x,selected.y);}};
  }
  throw Error(`Unknown Canvas design operation: ${design.operation}`);
}

export function canvasDesignStencil(design,width,height) {
  const canvas=new OffscreenCanvas(width,height),context=canvas.getContext('2d');
  const image=context.createImageData(width,height),operation=compileCanvasDesign(design);
  for(let y=0;y<height;y++)for(let x=0;x<width;x++)image.data[4*(y*width+x)+3]=operation.opacity(x+0.5,y+0.5);
  context.putImageData(image,0,0);
  return {canvas,context};
}

export function paintCanvasDesign(destination,coverage,design) {
  const {width,height}=destination.canvas,operation=compileCanvasDesign(design);
  const image=destination.context.getImageData(0,0,width,height),pixels=image.data;
  const mask=coverage.context.getImageData(0,0,width,height).data;
  for(let y=0;y<height;y++)for(let x=0;x<width;x++){
    const i=4*(y*width+x);
    const amount=operation.flipping?(mask[i+3]>=128?1:0):mask[i+3]/255;
    if(!amount)continue;
    const color=Array.from(pixels.subarray(i,i+4));operation.paint(color,x+0.5,y+0.5);
    const alpha=pixels[i+3]*(1-amount)+color[3]*amount;
    for(let c=0;c<3;c++)pixels[i+c]=alpha?(pixels[i+c]*pixels[i+3]*(1-amount)+color[c]*color[3]*amount)/alpha:0;
    pixels[i+3]=alpha;
  }
  destination.context.putImageData(image,0,0);destination.dirty=true;
}

// CLIM patterns use packed ARGB pixels; Canvas ImageData uses RGBA bytes.
// A fresh pattern observes mutable source arrays and palette inks on each draw.
export function makeCanvasPattern(surface, pixels, width, height, repeat, transform) {
  width=Number(width);height=Number(height);
  if (!width || !height) return 'rgba(0,0,0,0)';
  const canvas=new OffscreenCanvas(width,height);
  const context=canvas.getContext('2d');
  const data=context.createImageData(width,height);
  for(let i=0;i<width*height;i++){
    const argb=Number(pixels[i])>>>0;
    data.data[i*4]=(argb>>>16)&255;data.data[i*4+1]=(argb>>>8)&255;
    data.data[i*4+2]=argb&255;data.data[i*4+3]=argb>>>24;
  }
  context.putImageData(data,0,0);
  const pattern=surface.context.createPattern(canvas,repeat?'repeat':'no-repeat');
  pattern.setTransform(new DOMMatrix(Array.from(transform,Number)));
  return pattern;
}
