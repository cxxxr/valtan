// Canvas pixel contracts. Full Lisp-to-Canvas integration is checked separately.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import http from 'node:http';
import {fileURLToPath} from 'node:url';
import {launchChrome} from './chrome.mjs';
const root=path.resolve(path.dirname(fileURLToPath(import.meta.url)),'../..');

test('Canvas preserves patterns and implements reversible solid-design flipping',async()=>{
  const server=http.createServer((request,response)=>{
    if(request.url==='/'){response.setHeader('Content-Type','text/html');response.end('<!doctype html><title>Canvas pattern test</title>');return;}
    const filename={'/host.mjs':'host.mjs','/event-queue.mjs':'event-queue.mjs','/pointer.mjs':'pointer.mjs'}[request.url];
    if(!filename){response.statusCode=404;response.end();return;}
    response.setHeader('Content-Type','text/javascript');
    fs.createReadStream(path.join(root,'library/mcclim-browser',filename)).pipe(response);
  });
  let chrome;
  try{
    await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
    chrome=await launchChrome();const page=await chrome.page(`http://127.0.0.1:${server.address().port}/`);
    const result=await page.evaluate(`(async()=>{
      const {makeCanvasPattern}=await import('/host.mjs');
      const canvas=new OffscreenCanvas(20,20),context=canvas.getContext('2d'),surface={canvas,context};
      const pixel=(x,y)=>Array.from(context.getImageData(x,y,1,1).data);
      const draw=(pixels,w,h,repeat,transform)=>{context.fillStyle=makeCanvasPattern(surface,pixels,w,h,repeat,transform);context.fillRect(0,0,20,20);};
      context.imageSmoothingEnabled=false;
      draw([0xffff0000,0xff00ff00,0xff0000ff,0],2,2,false,[1,0,0,1,3,4]);
      const finite=[pixel(3,4),pixel(4,4),pixel(3,5),pixel(4,5),pixel(5,4)];
      context.clearRect(0,0,20,20);
      draw([0xffff0000,0xff0000ff],2,1,true,[2,0,0,2,1,2]);
      const tiled=[pixel(1,2),pixel(2,2),pixel(3,2),pixel(5,2),pixel(1,5)];
      context.clearRect(0,0,20,20);
      draw([0xffff0000,0xff0000ff],2,1,false,[0,1,-1,0,10,2]);
      const rotated=[pixel(9,2),pixel(9,3),pixel(10,2)];
      context.fillStyle='white';context.fillRect(0,0,20,20);
      draw([0x80ff0000],1,1,false,[1,0,0,1,0,0]);const alpha=pixel(0,0);
      const mutable=[0xffff0000];draw(mutable,1,1,false,[1,0,0,1,0,0]);mutable[0]=0xff0000ff;
      draw(mutable,1,1,false,[1,0,0,1,0,0]);const mutation=pixel(0,0);
      draw([],0,0,false,[1,0,0,1,0,0]);const empty=pixel(0,0);
      return {finite,tiled,rotated,alpha,mutation,empty};
    })()`);
    const red=[255,0,0,255],green=[0,255,0,255],blue=[0,0,255,255],clear=[0,0,0,0];
    assert.deepEqual(result.finite,[red,green,blue,clear,clear]);
    assert.deepEqual(result.tiled,[red,red,blue,red,red]);
    assert.deepEqual(result.rotated,[red,blue,clear]);
    assert.deepEqual(result.alpha,[255,127,127,255]);
    assert.deepEqual(result.mutation,blue);assert.deepEqual(result.empty,blue);
    const flipping=await page.evaluate(`(async()=>{
      const {flipCanvasArea}=await import('/host.mjs');
      const surface=()=>{const canvas=new OffscreenCanvas(24,24);return {canvas,context:canvas.getContext('2d')};};
      const destination=surface(),mask=surface(),a=surface(),b=surface();
      const fill=(s,color,x=0,y=0,w=24,h=24)=>{s.context.fillStyle=color;s.context.fillRect(x,y,w,h);};
      const pixels=s=>Array.from(s.context.getImageData(0,0,24,24).data);
      const pixel=(x,y)=>Array.from(destination.context.getImageData(x,y,1,1).data);
      fill(destination,'red');fill(destination,'blue',3,2,1,1);fill(destination,'#123456',4,2,1,1);
      destination.context.clearRect(0,0,2,1);fill(destination,'rgba(32,64,96,.5)',1,0,1,1);
      fill(a,'red');fill(b,'blue');
      // The designs can vary spatially, as long as their samples are solid.
      fill(a,'cyan',16,0,8,24);fill(b,'magenta',16,0,8,24);fill(destination,'cyan',16,0,8,24);
      fill(mask,'white',0,0,8,4);fill(mask,'rgba(255,255,255,.25)',8,0,1,1);
      fill(mask,'rgba(255,255,255,.75)',9,0,1,1);
      mask.context.beginPath();mask.context.arc(12,12,9,0,Math.PI*2);mask.context.fillStyle='white';mask.context.fill();
      const before=pixels(destination);
      flipCanvasArea(destination,mask,a,b);
      const samples=[pixel(2,2),pixel(3,2),pixel(4,2),pixel(18,12),pixel(8,0),pixel(9,0)];
      const first=pixels(destination),changed=first.some((v,i)=>v!==before[i]);
      const partialAlphaPreserved=first.slice(0,8).every((v,i)=>v===before[i]);
      flipCanvasArea(destination,mask,a,b);
      const restored=pixels(destination).every((v,i)=>v===before[i]);
      // Equal designs must be a no-op, including covered edge pixels.
      flipCanvasArea(destination,mask,a,a);
      return {samples,changed,partialAlphaPreserved,restored,equalUnchanged:pixels(destination).every((v,i)=>v===before[i]),dirty:destination.dirty};
    })()`);
    assert.deepEqual(flipping.samples,[blue,red,[0xed,0x34,0xa9,255],[255,0,255,255],red,blue]);
    assert.equal(flipping.changed,true);assert.equal(flipping.partialAlphaPreserved,true);
    assert.equal(flipping.restored,true);assert.equal(flipping.equalUnchanged,true);assert.equal(flipping.dirty,true);
  }finally{
    await chrome?.close();server.closeAllConnections();await new Promise(resolve=>server.close(resolve));
  }
});
