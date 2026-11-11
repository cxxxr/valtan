// Canvas pixel contracts. Full Lisp-to-Canvas integration is checked separately.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import http from 'node:http';
import {fileURLToPath} from 'node:url';
import {launchChrome} from './chrome.mjs';
const root=path.resolve(path.dirname(fileURLToPath(import.meta.url)),'../..');

test('Canvas preserves finite and tiled ARGB patterns, transforms, alpha and mutations',async()=>{
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
  }finally{
    await chrome?.close();server.closeAllConnections();await new Promise(resolve=>server.close(resolve));
  }
});
