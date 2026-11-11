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
    createSurface: (width, height) => {
      const canvas = new OffscreenCanvas(Math.max(1, Math.ceil(width)), Math.max(1, Math.ceil(height)));
      return {canvas, context: canvas.getContext('2d')};
    },
    copyArea: (source, sx, sy, width, height, destination, dx, dy) => {
      destination.context.save();
      destination.context.resetTransform();
      destination.context.drawImage(source.canvas, sx, sy, width, height, dx, dy, width, height);
      destination.context.restore(); destination.dirty = true;
    },
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
