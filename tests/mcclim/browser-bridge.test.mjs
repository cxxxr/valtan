// This verifies browser transport, not McCLIM itself. The actual Lisp GUI
// must pass a separate end-to-end test with the upstream sources loaded.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import http from 'node:http';
import {fileURLToPath} from 'node:url';
import {launchChrome} from './chrome.mjs';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const app = `
  const host = globalThis.valtanMcclim;
  const window = host.createWindow('Canvas bridge verification', 160, 100);
  host.canvasSet(window, 'fillStyle', '#ff0000');
  host.canvasCall(window, 'fillRect', [0, 0, 160, 100]);
  host.present(window); host.ready();
  for (;;) {
    const event = host.nextEvent();
    if (event.type === 'shutdown') break;
    if (event.type === 'pointerdown') {
      host.canvasSet(window, 'fillStyle', '#00ff00');
      host.canvasCall(window, 'fillRect', [0, 0, 20, 20]); host.present(window);
    }
    if (event.type === 'keydown' && event.key === 'a') {
      host.canvasSet(window, 'fillStyle', '#0000ff');
      host.canvasCall(window, 'fillRect', [20, 0, 20, 20]); host.present(window);
    }
  }
`;
const html = `<!doctype html><meta charset="utf-8"><div id="windows"></div>
<script type="module">
  import {mountMcclim} from '/library/mcclim-browser/client.mjs';
  const container = document.querySelector('#windows');
  const state = globalThis.bridgeTest = {ready: false, paints: 0, errors: []};
  addEventListener('error', event => state.errors.push(event.message));
  container.addEventListener('mcclim-ready', () => state.ready = true);
  container.addEventListener('mcclim-paint', () => state.paints++);
  container.addEventListener('mcclim-error', event => state.errors.push(event.detail));
  state.application = mountMcclim(container, '/library/mcclim-browser/worker.mjs', '/test-app.mjs');
</script>`;
async function waitFor(page, expression, timeout = 10000) {
  const deadline = Date.now() + timeout;
  while (Date.now() < deadline) {
    if (await page.evaluate(expression)) return;
    await new Promise(resolve => setTimeout(resolve, 25));
  }
  throw new Error(`Browser condition timed out: ${expression}; ${JSON.stringify(await page.evaluate('({url: location.href, body: document.body?.innerText, errors: globalThis.bridgeTest?.errors, resources: performance.getEntriesByType("resource").map(x => x.name)})'))}`);
}
function bridgeServer(application) {
  const server = http.createServer((request, response) => {
    response.setHeader('Cross-Origin-Opener-Policy', 'same-origin');
    response.setHeader('Cross-Origin-Embedder-Policy', 'require-corp');
    if (request.url === '/') {response.setHeader('Content-Type', 'text/html'); response.end(html); return;}
    response.setHeader('Content-Type', 'text/javascript');
    if (request.url === '/test-app.mjs') {response.end(application); return;}
    const file = path.resolve(root, '.' + request.url);
    if (!file.startsWith(root + path.sep) || !fs.existsSync(file) || !fs.statSync(file).isFile()) {
      response.statusCode = 404; response.end(); return;
    }
    response.end(fs.readFileSync(file));
  });
  return server;
}
test('Chrome commits native IME and direct text once while preserving key gestures', {timeout:60000}, async()=>{
  const server=bridgeServer(`
    const host=globalThis.valtanMcclim;
    host.createWindow('IME verification',300,100);host.ready();
    for(;;){const event=host.nextEvent();if(event.type==='shutdown')break;
      if(['keydown','keyup','text-input'].includes(event.type))host.status(JSON.stringify(event));}
  `);
  await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
  let chrome;
  try {
    chrome=await launchChrome();const page=await chrome.page(`http://127.0.0.1:${server.address().port}/`);
    await waitFor(page,'globalThis.bridgeTest?.ready');
    await page.evaluate(`bridgeTest.events=[];
      document.querySelector('#windows').addEventListener('mcclim-status',e=>bridgeTest.events.push(JSON.parse(e.detail)));
      bridgeTest.application.windows.get(1).canvas.focus();true`);
    const compose=text=>page.send('Input.imeSetComposition',{text,selectionStart:0,selectionEnd:text.length});
    await compose('日本');
    await compose('日本語');
    assert.deepEqual(await page.evaluate('bridgeTest.events'),[], 'uncommitted composition must not reach CLIM');
    await page.send('Input.insertText',{text:'日本語'});
    await waitFor(page,"bridgeTest.events.filter(e=>e.type==='text-input').length===3");
    await compose('取り消し');await compose('');
    await page.send('Input.insertText',{text:'😀'});
    await page.send('Input.dispatchKeyEvent',{type:'keyDown',key:'Process',windowsVirtualKeyCode:229});
    await page.send('Input.dispatchKeyEvent',{type:'keyUp',key:'Process',windowsVirtualKeyCode:229});
    await page.send('Input.dispatchKeyEvent',{type:'keyDown',key:'a',code:'KeyA',text:'a',windowsVirtualKeyCode:65});
    await page.send('Input.dispatchKeyEvent',{type:'keyUp',key:'a',code:'KeyA',windowsVirtualKeyCode:65});
    // This keyup follows all preceding input in the shared event queue.
    await waitFor(page,"bridgeTest.events.some(e=>e.type==='keyup' && e.key==='a')");
    const events=await page.evaluate('bridgeTest.events');
    assert.deepEqual(events.filter(e=>e.type==='text-input').map(e=>e.text),['日','本','語','😀']);
    assert.deepEqual(events.filter(e=>e.type!=='text-input').map(e=>[e.type,e.key]),[['keydown','a'],['keyup','a']]);
    assert.equal(await page.evaluate('bridgeTest.application.windows.get(1).input.value'),'');
    assert.deepEqual(await page.evaluate('bridgeTest.errors'),[]);
    await page.evaluate("bridgeTest.application.input({type:'shutdown'});true");
  }finally{
    await chrome?.close();server.closeAllConnections();await new Promise(resolve=>server.close(resolve));
  }
});
test('Chrome paints and accepts pointer/keyboard input while its worker blocks', {timeout: 60000}, async () => {
  const server = bridgeServer(app);
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  let chrome;
  try {
    chrome = await launchChrome();
    const page = await chrome.page(`http://127.0.0.1:${server.address().port}/`);
    await waitFor(page, 'globalThis.bridgeTest?.ready && bridgeTest.paints === 1');
    const sample = () => page.evaluate(`(() => {
      const source = document.querySelector('canvas'), copy = document.createElement('canvas');
      copy.width = source.width; copy.height = source.height;
      const context = copy.getContext('2d'); context.drawImage(source, 0, 0);
      return [5, 25, 100].map(x => [...context.getImageData(x, 5, 1, 1).data]);
    })()`);
    assert.deepEqual(await sample(), [[255,0,0,255], [255,0,0,255], [255,0,0,255]]);
    const point = await page.evaluate(`(() => {const r = document.querySelector('canvas').getBoundingClientRect(); return {x: r.left + 5, y: r.top + 5};})()`);
    await page.send('Input.dispatchMouseEvent', {type: 'mousePressed', ...point, button: 'left', clickCount: 1});
    await page.send('Input.dispatchMouseEvent', {type: 'mouseReleased', ...point, button: 'left', clickCount: 1});
    await waitFor(page, 'bridgeTest.paints === 2');
    await page.send('Input.dispatchKeyEvent', {type: 'keyDown', key: 'a', code: 'KeyA', text: 'a', windowsVirtualKeyCode: 65});
    await page.send('Input.dispatchKeyEvent', {type: 'keyUp', key: 'a', code: 'KeyA', windowsVirtualKeyCode: 65});
    await waitFor(page, 'bridgeTest.paints === 3');
    assert.deepEqual(await sample(), [[0,255,0,255], [0,0,255,255], [255,0,0,255]], 'incremental painting must preserve earlier pixels');
    assert.deepEqual(await page.evaluate('bridgeTest.errors'), []);
    await page.screenshot('/tmp/valtan-mcclim-browser-bridge.png');
    await page.evaluate('bridgeTest.application.dispose(); true');
  } finally {
    await chrome?.close();
    await new Promise(resolve => server.close(resolve));
  }
});

test('Chrome manages positioned, nested and unmanaged mirrors while its worker blocks', {timeout: 60000}, async () => {
  const application = `
    const host=globalThis.valtanMcclim;
    function create(title,w,h,parent,color,x,y,managed=true){
      const window=host.createWindow(title,w,h,parent,managed);
      host.canvasSet(window,'fillStyle',color);host.canvasCall(window,'fillRect',[0,0,w,h]);
      host.moveWindow(window,x,y);return window;
    }
    const first=create('First',160,120,null,'red',20,30);
    const second=create('Second',140,100,null,'blue',240,30);
    const child=create('Child',100,80,first,'lime',120,70);
    const popup=create('Popup',80,40,null,'purple',100,200,false);
    host.flush();host.ready();
    for(;;){
      const event=host.nextEvent();
      if(event.type==='shutdown')break;
      if(event.type==='window-configuration'){
        host.moveWindow(host.getWindow(event.window),event.x,event.y);
        host.status(JSON.stringify(event));
      }
      if(event.type==='window-restore'){
        host.visible(host.getWindow(event.window),true);host.status('restored');
      }
      if(event.type==='test-window-operation'){
        host[event.operation](host.getWindow(event.window),...(event.args??[]));
        host.flush();host.status(event.operation);
      }
    }`;
  const server=bridgeServer(application);
  await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
  let chrome;
  try {
    chrome=await launchChrome();
    const page=await chrome.page(`http://127.0.0.1:${server.address().port}/`);
    await waitFor(page,'globalThis.bridgeTest?.ready && bridgeTest.paints === 4');
    await page.evaluate(`document.querySelector('#windows').addEventListener('mcclim-status',e=>bridgeTest.status=e.detail);true`);
    async function state(id){return page.evaluate(`(()=>{const s=bridgeTest.application.windows.get(${id});
      const r=s.canvas.getBoundingClientRect();return {x:r.x,y:r.y,width:r.width,height:r.height,
      title:s.title.textContent,decorated:!s.title.hidden,hidden:s.section.hidden};})()`);}
    async function operation(id,operation,...args){
      await page.evaluate(`bridgeTest.status='';bridgeTest.application.input(${JSON.stringify({type:'test-window-operation',window:id,operation,args})});true`);
      await waitFor(page,`bridgeTest.status === ${JSON.stringify(operation)}`);
    }
    async function at(x,y){return page.evaluate(`document.elementFromPoint(${x},${y})?.closest('.mcclim-window')?.dataset.windowId`);}
    const a=await state(1),b=await state(2),c=await state(3),p=await state(4);
    assert.equal(b.x-a.x,220);assert.equal(b.y-a.y,0);
    assert.equal(c.x-a.x,120);assert.equal(c.y-a.y,70);
    assert.equal(a.decorated,true);assert.equal(c.decorated,false);assert.equal(p.decorated,false);
    assert.equal(await at(a.x+130,a.y+80),'3');
    assert.notEqual(await at(a.x+180,a.y+80),'3','child mirror must be clipped to its parent');
    await operation(2,'moveWindow',60,50);
    assert.equal(await at(a.x+80,a.y+60),'2');
    await operation(1,'raiseWindow');assert.equal(await at(a.x+80,a.y+60),'1');
    await operation(1,'buryWindow');assert.equal(await at(a.x+80,a.y+60),'2');
    await operation(2,'title','日本語のウィンドウ');assert.equal((await state(2)).title,'日本語のウィンドウ');
    await operation(1,'moveWindow',30,40);
    assert.equal((await state(3)).x,c.x+10);assert.equal((await state(3)).y,c.y+10);
    await operation(1,'resizeWindow',200,170);
    assert.equal((await state(1)).width,200);assert.equal((await state(1)).height,170);
    const rgba=await page.evaluate(`(()=>{const source=bridgeTest.application.windows.get(1).canvas;
      const copy=document.createElement('canvas');copy.width=source.width;copy.height=source.height;
      const ctx=copy.getContext('2d');ctx.drawImage(source,0,0);return [...ctx.getImageData(5,5,1,1).data];})()`);
    assert.deepEqual(rgba,[255,0,0,255],'resize preserves the existing backing store');
    await operation(1,'visible',false);assert.equal((await state(1)).hidden,true);
    await operation(1,'visible',true);assert.equal((await state(1)).hidden,false);
    await operation(1,'minimizeWindow');assert.equal((await state(1)).hidden,true);
    await page.evaluate(`document.querySelector('.mcclim-taskbar button').click();true`);
    await waitFor(page,`bridgeTest.status==='restored'`);
    assert.equal((await state(1)).hidden,false);
    assert.equal(await page.evaluate(`document.querySelectorAll('.mcclim-taskbar button').length`),0);
    await page.evaluate(`bridgeTest.application.windows.get(1).canvas.focus();true`);
    await operation(1,'raiseWindow');
    assert.equal(await page.evaluate(`document.activeElement === bridgeTest.application.windows.get(1).input`),true,
      'raising a focused mirror must preserve keyboard focus');
    const titlePoint=await page.evaluate(`(()=>{const r=bridgeTest.application.windows.get(1).title.getBoundingClientRect();return {x:r.x+10,y:r.y+r.height/2};})()`);
    assert.equal(await page.evaluate(`document.elementFromPoint(${titlePoint.x},${titlePoint.y})?.tagName`),'H2',JSON.stringify(titlePoint));
    await page.evaluate(`bridgeTest.dragEvents=[];
      for(const type of ['gotpointercapture','lostpointercapture'])
        bridgeTest.application.windows.get(1).title.addEventListener(type,e=>bridgeTest.dragEvents.push(e.type));true`);
    await page.send('Input.dispatchMouseEvent',{type:'mousePressed',...titlePoint,button:'left',clickCount:1});
    // CDP requires the active button as well as its mask to preserve capture.
    await page.send('Input.dispatchMouseEvent',{type:'mouseMoved',x:titlePoint.x+30,y:titlePoint.y+20,button:'left',buttons:1});
    await page.send('Input.dispatchMouseEvent',{type:'mouseReleased',x:titlePoint.x+30,y:titlePoint.y+20,button:'left',clickCount:1});
    await waitFor(page,`bridgeTest.status?.startsWith('{')`);
    assert.deepEqual(await page.evaluate('bridgeTest.dragEvents'),['gotpointercapture','lostpointercapture']);
    const configured=JSON.parse(await page.evaluate('bridgeTest.status'));
    assert.equal(configured.type,'window-configuration');assert.equal(configured.window,1);
    assert.equal(configured.x,60);assert.equal(configured.y,60);
    assert.equal(configured.width,200);assert.equal(configured.height,170);
    await page.send('Input.dispatchMouseEvent',{type:'mouseMoved',x:titlePoint.x+70,y:titlePoint.y+60});
    assert.deepEqual(await page.evaluate(`(()=>{const s=bridgeTest.application.windows.get(1);return [s.x,s.y];})()`),[60,60]);
    await operation(1,'destroyWindow');
    assert.deepEqual(await page.evaluate(`[...bridgeTest.application.windows.keys()]`),[2,4]);
    assert.deepEqual(await page.evaluate('bridgeTest.errors'),[]);
    await page.screenshot('/tmp/valtan-mcclim-window-bridge.png');
    await page.evaluate('bridgeTest.application.dispose();true');
  } finally {
    await chrome?.close();
    await new Promise(resolve=>server.close(resolve));
  }
});

test('Chrome routes grabbed pointer input outside and between mirrors', {timeout:60000}, async()=>{
  const application=`const host=globalThis.valtanMcclim;
    const a=host.createWindow('Grab owner',160,100), b=host.createWindow('Other mirror',160,100);
    host.moveWindow(a,20,30);host.moveWindow(b,240,30);host.flush();host.ready();
    for(;;){const e=host.nextEvent();if(e.type==='shutdown')break;
      if(e.type==='test-grab'){host.grabPointer(host.getWindow(e.window),e.multiple);host.status('grabbed');}
      else if(e.type==='test-ungrab'){host.ungrabPointer();host.status('ungrabbed');}
      else if(e.type==='test-destroy'){host.destroyWindow(host.getWindow(e.window));host.status('destroyed');}
      else if(['pointerdown','pointerup','pointermove','wheel'].includes(e.type))host.status(JSON.stringify(e));
    }`;
  const server=bridgeServer(application);await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
  let chrome;
  try{
    chrome=await launchChrome();const page=await chrome.page(`http://127.0.0.1:${server.address().port}/`);
    await waitFor(page,'globalThis.bridgeTest?.ready && bridgeTest.paints===2');
    await page.evaluate(`bridgeTest.events=[];bridgeTest.outsideClicks=0;
      document.querySelector('#windows').addEventListener('mcclim-status',e=>{
        bridgeTest.status=e.detail;if(e.detail.startsWith('{'))bridgeTest.events.push(JSON.parse(e.detail));});
      const button=document.createElement('button');button.id='outside';button.textContent='Outside';
      Object.assign(button.style,{position:'absolute',left:'600px',top:'240px',width:'100px',height:'40px'});
      button.onclick=()=>bridgeTest.outsideClicks++;document.body.append(button);
      bridgeTest.application.windows.get(2).canvas.focus();true`);
    async function input(event,status){
      await page.evaluate(`bridgeTest.status='';bridgeTest.application.input(${JSON.stringify(event)});true`);
      await waitFor(page,`bridgeTest.status===${JSON.stringify(status)}`);
    }
    const rect=id=>page.evaluate(`(()=>{const r=bridgeTest.application.windows.get(${id}).canvas.getBoundingClientRect();return {x:r.x,y:r.y};})()`);
    const a=await rect(1),b=await rect(2);
    const mouse=params=>page.send('Input.dispatchMouseEvent',params);
    async function click(x,y){
      await mouse({type:'mouseMoved',x,y});
      await mouse({type:'mousePressed',x,y,button:'left',clickCount:1});
      await mouse({type:'mouseReleased',x,y,button:'left',clickCount:1});
    }
    await input({type:'test-grab',window:1,multiple:false},'grabbed');
    assert.equal(await page.evaluate('document.activeElement===bridgeTest.application.windows.get(1).input'),true);
    await click(620,260);
    await waitFor(page,`bridgeTest.events.filter(e=>e.type==='pointerup').length===1`);
    const outside=await page.evaluate(`bridgeTest.events.filter(e=>e.type==='pointerup')[0]`);
    assert.equal(outside.window,1);assert.equal(outside.x,620-a.x);assert.equal(outside.y,260-a.y);
    assert.equal(await page.evaluate('bridgeTest.outsideClicks'),0,'grab prevents unrelated page controls from activating');
    await mouse({type:'mouseWheel',x:620,y:260,deltaX:3,deltaY:17});
    await waitFor(page,`bridgeTest.events.some(e=>e.type==='wheel')`);
    const wheel=await page.evaluate(`bridgeTest.events.find(e=>e.type==='wheel')`);
    assert.equal(wheel.window,1);assert.equal(wheel.dx,3);assert.equal(wheel.dy,17);
    await input({type:'test-ungrab'},'ungrabbed');
    assert.equal(await page.evaluate('document.activeElement===bridgeTest.application.windows.get(2).input'),true);
    await click(620,260);assert.equal(await page.evaluate('bridgeTest.outsideClicks'),1);
    // Begin a normal captured press on A, then use an owner-events grab. Input
    // over B must reach B even though the DOM pointer is still captured by A.
    await mouse({type:'mouseMoved',x:a.x+10,y:a.y+10});
    await mouse({type:'mousePressed',x:a.x+10,y:a.y+10,button:'left',clickCount:1});
    await input({type:'test-grab',window:1,multiple:true},'grabbed');
    await page.evaluate('bridgeTest.events=[];true');
    await mouse({type:'mouseMoved',x:b.x+15,y:b.y+20,button:'left',buttons:1});
    await mouse({type:'mouseReleased',x:b.x+15,y:b.y+20,button:'left',clickCount:1});
    await waitFor(page,`bridgeTest.events.some(e=>e.type==='pointerup')`);
    const owner=await page.evaluate(`bridgeTest.events.filter(e=>e.type==='pointermove'||e.type==='pointerup')`);
    assert.deepEqual(owner.map(e=>[e.type,e.window,e.x,e.y]),[['pointermove',2,15,20],['pointerup',2,15,20]]);
    await input({type:'test-ungrab'},'ungrabbed');
    await input({type:'test-grab',window:1,multiple:false},'grabbed');
    await input({type:'test-destroy',window:1},'destroyed');
    await click(620,260);assert.equal(await page.evaluate('bridgeTest.outsideClicks'),2,'destroyed grab owner releases input');
    assert.deepEqual(await page.evaluate('bridgeTest.errors'),[]);
    await page.evaluate('bridgeTest.application.dispose();true');
    await click(620,260);assert.equal(await page.evaluate('bridgeTest.outsideClicks'),3,'dispose removes document listeners');
  }finally{await chrome?.close();await new Promise(resolve=>server.close(resolve));}
});
