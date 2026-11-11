// Real CLIM frame lifecycle and browser interaction, with a fresh worker.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import readline from 'node:readline';
import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {hash} from './bundle.mjs';
const here=path.dirname(fileURLToPath(import.meta.url));
const args=process.argv.slice(2),threaded=args.includes('--threaded');
if(threaded)args.splice(args.indexOf('--threaded'),1);
if(args.length>1 || args.some(arg=>arg.startsWith('--')))
  throw Error('Usage: windows.mjs [--threaded] [report.json]');
const output=path.resolve(args[0]??`/tmp/valtan-mcclim-windows${threaded?'-threaded':''}.json`);
const fixture=fs.readFileSync(path.join(here,'window-checks.lisp'),'utf8')+
  (threaded?'\n'+fs.readFileSync(path.join(here,'threaded-window-checks.lisp'),'utf8'):'');
const startupTimeout=(threaded?3:1)*60*60*1000;
const evaluationTimeout=(threaded?30:2)*60*1000;
const snapshot=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-window-checks-'));
const fixtureFile=path.join(snapshot,'checks.lisp');fs.writeFileSync(fixtureFile,fixture);
const report={status:'running',complete:false,startedAt:new Date().toISOString(),
  scope:threaded?'Real McCLIM concurrent frame event loops, lifecycle and popup menu selection/cancellation in Chrome':
    'Real McCLIM frame lifecycle and popup menu selection/cancellation in Chrome; second frame has no concurrent event loop',
  threaded,startupTimeout,evaluationTimeout,
  checksSha256:hash(fixture),driverSha256:hash(fs.readFileSync(fileURLToPath(import.meta.url))),checks:[]};
const save=()=>fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');save();
const workbenchArgs=[path.join(here,'workbench.mjs'),'--startup-timeout',String(startupTimeout)];
if(threaded){
  const original=path.resolve(here,'../../example/try-using-mcclim');
  for(const name of fs.readdirSync(original))
    if(/\.(asd|system|lisp|html)$/.test(name))fs.copyFileSync(path.join(original,name),path.join(snapshot,name));
  fs.writeFileSync(path.join(snapshot,'app.system'),`(defsystem "app"
    :depends-on ("mcclim-load-trace" "mcclim-browser")
    :components ((:file "main")) :target :browser
    :target-features (:clim-without-swank :valtan-resumable :clim-mp)
    :compile-mode :runtime)`);
  workbenchArgs.push('--source-directory',snapshot);
}
const child=spawn(process.execPath,workbenchArgs,{stdio:['pipe','pipe','inherit']});
let serial=0, readyResolve, readyReject;
const ready=new Promise((resolve,reject)=>{readyResolve=resolve;readyReject=reject;});
const pending=new Map();
const exit=new Promise(resolve=>child.on('close',(code,signal)=>{
  const error=Error(`Workbench exited: ${JSON.stringify({code,signal})}`);
  readyReject(error);for(const {reject} of pending.values())reject(error);
  pending.clear();resolve({code,signal});
}));
const timer=setTimeout(()=>child.kill('SIGKILL'),startupTimeout+60*60*1000);
readline.createInterface({input:child.stdout}).on('line',line=>{
  const message=JSON.parse(line);
  if(message.work){report.work=message.work;save();}
  if(message.loading)process.stdout.write(message.loading+'\n');
  if(message.ready)readyResolve();
  const request=pending.get(message.id);
  if(request){pending.delete(message.id);message.error?request.reject(Error(message.error)):request.resolve(message.result);}
  else if(message.error)readyReject(Error(message.error));
});
const command=(op,fields={})=>new Promise((resolve,reject)=>{
  const id=++serial;pending.set(id,{resolve,reject});child.stdin.write(JSON.stringify({id,op,...fields})+'\n');
});
const evaluate=async source=>{
  const result=await command('eval',{source,timeout:evaluationTimeout});
  assert.equal(result?.ok,true,result?.error);return result.value;
};
const page=expression=>command('page',{expression});
const mouse=params=>command('cdp',{method:'Input.dispatchMouseEvent',params});
async function waitFor(expression){
  const deadline=Date.now()+(threaded?120000:30000);
  while(!await page(expression)){
    const errors=await page('mcclimTest.errors');assert.deepEqual(errors,[]);
    if(Date.now()>deadline)throw Error('Browser condition timed out: '+expression);
    await new Promise(resolve=>setTimeout(resolve,50));
  }
}
async function waitForLisp(source,expected){
  const deadline=Date.now()+(threaded?120000:30000);
  for(;;){
    const actual=await evaluate(source);
    if(actual===expected)return;
    if(Date.now()>deadline)throw Error(`Lisp condition timed out: ${source}; expected ${expected}, got ${actual}`);
    await new Promise(resolve=>setTimeout(resolve,100));
  }
}
function checked(name){report.checks.push(name);save();console.log('Passed: '+name);}
try {
  await ready;
  await page(`globalThis.workbenchPointerGrab=null;
    mcclimTest.application.worker.addEventListener('message',({data})=>{
      if(data.type==='pointer-grab')workbenchPointerGrab=data;
      else if(data.type==='pointer-ungrab')workbenchPointerGrab=null;
    });true`);
  const loaded=await command('eval',{file:fixtureFile,timeout:evaluationTimeout});assert.equal(loaded?.ok,true,loaded?.error);
  await evaluate(threaded?`(mcclim-window-checks::setup 'mcclim-threaded-window-checks::threaded-window-frame)
    (mcclim-threaded-window-checks::start)`:'(mcclim-window-checks::setup)');
  await waitFor('mcclimTest.application.windows.size===2');
  const ids=await page('[...mcclimTest.application.windows.keys()]');
  const [primary,secondary]=ids;
  const state=id=>`mcclimTest.application.windows.get(${id})`;
  async function geometry(id){return page(`(()=>{const s=${state(id)};return [s.x,s.y,s.canvas.width,s.canvas.height];})()`);}
  assert.deepEqual(await geometry(primary),[20,40,600,280]);
  assert.deepEqual(await geometry(secondary),[320,80,220,160]);checked('two real frames with requested geometry');
  async function secondaryKey(expected){
    await page(`${state(secondary)}.input.focus();true`);
    const before=await page(`${state(secondary)}.canvas.toDataURL()`);
    await command('cdp',{method:'Input.dispatchKeyEvent',params:{type:'keyDown',key:'+',text:'+',code:'Equal'}});
    await command('cdp',{method:'Input.dispatchKeyEvent',params:{type:'keyUp',key:'+',code:'Equal'}});
    await waitFor(`mcclimTest.status==='Secondary: ${expected}'`);
    assert.notEqual(await page(`${state(secondary)}.canvas.toDataURL()`),before);
  }
  if(threaded){
    await secondaryKey(1);
    assert.equal(await evaluate('(mcclim-threaded-window-checks::counts)'),'(1 0)');
    checked('second frame handles keyboard input in its own thread and dynamic frame context');
  }
  await evaluate('(setf (clim:frame-pretty-name mcclim-window-checks::*primary*) "日本語のフレーム")');
  await waitFor(`${state(primary)}.title.textContent==='日本語のフレーム'`);checked('frame pretty name reaches browser');
  const overlap=await page(`(()=>{const r=${state(secondary)}.canvas.getBoundingClientRect();return {x:r.x+20,y:r.y+60};})()`);
  const front=()=>page(`document.elementFromPoint(${overlap.x},${overlap.y})?.closest('.mcclim-window')?.dataset.windowId`);
  await evaluate('(clim:raise-frame mcclim-window-checks::*primary*)');
  assert.equal(await front(),String(primary));
  await evaluate('(clim:bury-frame mcclim-window-checks::*primary*)');
  assert.equal(await front(),String(secondary));checked('raise and bury match browser hit testing');
  await evaluate(`(clim:move-and-resize-sheet (mcclim-window-checks::top-sheet mcclim-window-checks::*secondary*) 180 90 260 180)
    (mcclim-window-checks::check-geometry mcclim-window-checks::*secondary* 180 90 260 180)
    (mcclim-window-checks::flush)`);
  await waitFor(`${state(secondary)}.canvas.width===260`);
  assert.deepEqual(await geometry(secondary),[180,90,260,180]);checked('Lisp move and resize update mirror and browser');
  await evaluate('(clim:disable-frame mcclim-window-checks::*secondary*)');
  await waitFor(`${state(secondary)}.section.hidden`);
  await evaluate('(clim:enable-frame mcclim-window-checks::*secondary*)');
  await waitFor(`!${state(secondary)}.section.hidden`);checked('disable and enable');
  await evaluate('(clim:raise-frame mcclim-window-checks::*primary*)');
  const title=await page(`(()=>{const r=${state(primary)}.title.getBoundingClientRect();return {x:r.x+30,y:r.y+r.height/2};})()`);
  await mouse({type:'mouseMoved',...title});
  await mouse({type:'mousePressed',...title,button:'left',clickCount:1});
  await mouse({type:'mouseMoved',x:title.x+30,y:title.y+20,button:'left',buttons:1});
  await mouse({type:'mouseReleased',x:title.x+30,y:title.y+20,button:'left',clickCount:1});
  await waitFor(`${state(primary)}.x===50 && ${state(primary)}.y===60`);
  // DOM dragging precedes queue delivery. In threaded mode, a test interrupt
  // can run before the application's configuration-event handler.
  await waitForLisp(`(multiple-value-list (clim:transform-position
    (clim:sheet-transformation (mcclim-window-checks::top-sheet mcclim-window-checks::*primary*)) 0 0))`,
    '((50 60))');
  await evaluate('(mcclim-window-checks::check-geometry mcclim-window-checks::*primary* 50 60 600 280)');
  checked('title drag reaches CLIM sheet transformation');
  await evaluate('(mcclim-window-checks::shrink-primary)');
  await waitFor(`${state(primary)}.section.hidden && document.querySelector('.mcclim-taskbar button')`);
  const restore=await page(`(()=>{const r=document.querySelector('.mcclim-taskbar button').getBoundingClientRect();return {x:r.x+r.width/2,y:r.y+r.height/2};})()`);
  await mouse({type:'mouseMoved',...restore});
  await mouse({type:'mousePressed',...restore,button:'left',clickCount:1});
  await mouse({type:'mouseReleased',...restore,button:'left',clickCount:1});
  await waitFor(`!${state(primary)}.section.hidden && !document.querySelector('.mcclim-taskbar button')`);
  await evaluate('(mcclim-window-checks::check-restored)');checked('minimize and user restore update frame state');
  const button=await page(`(()=>{const r=${state(primary)}.canvas.getBoundingClientRect();return {x:r.x+r.width/2,y:r.y+r.height-20};})()`);
  await mouse({type:'mouseMoved',...button});
  await mouse({type:'mousePressed',...button,button:'left',clickCount:1});
  await mouse({type:'mouseReleased',...button,button:'left',clickCount:1});
  await waitFor(`mcclimTest.status==='Counter: 1'`);checked('gadget input survives window movement and restore');
  if(threaded){
    await secondaryKey(2);
    assert.equal(await evaluate('(mcclim-threaded-window-checks::counts)'),'(2 1)');
    checked('both frame event loops keep handling independent input');
    assert.equal(await evaluate('(mcclim-threaded-window-checks::stop)'), '(:STOPPED)');
    checked('second frame exits and its thread joins before mirror destruction');
  }
  await evaluate('(mcclim-window-checks::destroy-secondary)');
  await waitFor('mcclimTest.application.windows.size===1');
  assert.deepEqual(await page('[...mcclimTest.application.windows.keys()]'),[primary]);
  assert.deepEqual(await page('mcclimTest.errors'),[]);checked('destroy removes Lisp and browser mirrors');
  // MENU-CHOOSE runs a nested event loop. Start it without blocking this driver
  // so actual mouse/key events can complete it; retain its eventual result.
  async function openMenu(){
    const request=await command('eval',{wait:false,source:`
      (clim:menu-choose '(("First item" :value :first) ("Second item" :value :second))
        :associated-window (clim:find-pane-named mcclim-window-checks::*primary* 'valtan-clim-demo::canvas))`});
    await waitFor('mcclimTest.application.windows.size===2');
    // A menu's mirror can exist before layout and pointer acquisition finish.
    // Observe the real browser command first: repeatedly interrupting the Lisp
    // task here can starve the menu whose progress we are trying to observe.
    await waitFor(`workbenchPointerGrab && workbenchPointerGrab.id!==${primary}`);
    await waitForLisp(`(not (null (clim-internals::port-grabbed-sheet
      (clim:port (mcclim-window-checks::top-sheet mcclim-window-checks::*primary*)))))`,'(T)');
    await evaluate(`(assert (clim-internals::port-grabbed-sheet
      (clim:port (mcclim-window-checks::top-sheet mcclim-window-checks::*primary*))))`);
    const popup=await page(`(()=>{const s=[...mcclimTest.application.windows.values()].find(s=>s.id!==${primary});
      const r=s.canvas.getBoundingClientRect();return {id:s.id,x:r.x,y:r.y,right:r.right,bottom:r.bottom,
        width:r.width,height:r.height,hidden:s.section.hidden,decorated:!s.title.hidden};})()`);
    assert.equal(popup.hidden,false);assert.equal(popup.decorated,false);
    assert.ok(popup.x>=0 && popup.y>=0 && popup.right<=await page('innerWidth') && popup.bottom<=await page('innerHeight'),JSON.stringify(popup));
    return {request,popup};
  }
  async function finishMenu(request,expected){
    await waitFor(`globalThis.workbenchResults[${request.evaluationId}]`);
    const result=await command('evaluation-result',{evaluationId:request.evaluationId});
    assert.equal(result.ok,true,result.error);assert.equal(result.value,expected);
    await waitFor('mcclimTest.application.windows.size===1');
    assert.equal(await page(`document.activeElement===${state(primary)}.input`),true);
  }
  // Open near the bottom-right of the visible frame to exercise menu clamping
  // against the desktop's available viewport, excluding the HTML page header.
  const edge=await page(`(()=>{const r=${state(primary)}.canvas.getBoundingClientRect();return {x:r.right-12,y:r.bottom-12};})()`);
  await mouse({type:'mouseMoved',...edge});
  await page(`${state(primary)}.canvas.focus();true`);
  let menu=await openMenu();
  report.menuScreenshot=path.join(report.work,'menu.png');await command('screenshot',{file:report.menuScreenshot});
  const choice={x:menu.popup.x+20,y:menu.popup.y+24};
  await mouse({type:'mouseMoved',...choice});
  await mouse({type:'mousePressed',...choice,button:'left',clickCount:1});
  await mouse({type:'mouseReleased',...choice,button:'left',clickCount:1});
  // MENU-CHOOSE returns the value, original item and selecting event. Check the
  // complete value separately instead of requiring a printed event identity.
  await waitFor(`globalThis.workbenchResults[${menu.request.evaluationId}]`);
  const selected=await command('evaluation-result',{evaluationId:menu.request.evaluationId});
  assert.equal(selected.ok,true,selected.error);assert.match(selected.value,/^\(:SECOND \("Second item" :VALUE :SECOND\)/);
  await waitFor('mcclimTest.application.windows.size===1');checked('popup placement, captured input and second-item selection');
  menu=await openMenu();
  const outside={x:await page('innerWidth-8'),y:80};
  await mouse({type:'mouseMoved',...outside});
  await mouse({type:'mousePressed',...outside,button:'left',clickCount:1});
  await mouse({type:'mouseReleased',...outside,button:'left',clickCount:1});
  await finishMenu(menu.request,'(NIL)');checked('outside click cancels menu and restores focus');
  menu=await openMenu();
  await command('cdp',{method:'Input.dispatchKeyEvent',params:{type:'keyDown',key:'Escape',code:'Escape',windowsVirtualKeyCode:27}});
  await command('cdp',{method:'Input.dispatchKeyEvent',params:{type:'keyUp',key:'Escape',code:'Escape',windowsVirtualKeyCode:27}});
  await finishMenu(menu.request,'(NIL)');checked('Escape cancels menu and releases the grab');
  await command('cdp',{method:'Emulation.setDeviceMetricsOverride',params:{width:900,height:700,deviceScaleFactor:1,mobile:false}});
  // Let the ResizeObserver enqueue its event before the Lisp assertions.
  await page('new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve)))');
  const available=await page(`(()=>{const r=document.querySelector('.mcclim-desktop').getBoundingClientRect();
    return {width:Math.floor(r.width),height:Math.floor(innerHeight-r.top)};})()`);
  await waitForLisp(`(let ((g (clim:find-graft :port
    (clim:port (mcclim-window-checks::top-sheet mcclim-window-checks::*primary*)))))
    (list (clim:graft-width g) (clim:graft-height g)))`,
    `((${available.width} ${available.height}))`);
  await evaluate(`(let ((g (clim:find-graft :port (clim:port (mcclim-window-checks::top-sheet mcclim-window-checks::*primary*)))))
    (assert (= (clim:graft-width g) ${available.width})) (assert (= (clim:graft-height g) ${available.height}))
    (assert (= (clim:bounding-rectangle-width (clim:sheet-region g)) ${available.width}))
    (assert (= (clim:bounding-rectangle-height (clim:sheet-region g)) ${available.height}))
    (assert (clim:identity-transformation-p (clim:sheet-transformation g))))
    (mcclim-window-checks::check-geometry mcclim-window-checks::*primary* 50 60 600 280)`);
  checked('viewport resize updates the real graft dimensions');
  await command('cdp',{method:'Input.dispatchKeyEvent',params:{type:'keyDown',key:'+',text:'+',code:'Equal'}});
  await command('cdp',{method:'Input.dispatchKeyEvent',params:{type:'keyUp',key:'+',code:'Equal'}});
  await waitFor(`mcclimTest.status==='Counter: 2'`);
  menu=await openMenu();
  await command('cdp',{method:'Input.dispatchKeyEvent',params:{type:'keyDown',key:'Escape',code:'Escape',windowsVirtualKeyCode:27}});
  await command('cdp',{method:'Input.dispatchKeyEvent',params:{type:'keyUp',key:'Escape',code:'Escape',windowsVirtualKeyCode:27}});
  await finishMenu(menu.request,'(NIL)');
  assert.deepEqual(await page('mcclimTest.errors'),[]);
  checked('keyboard input and popup menus survive viewport resize');
  report.screenshot=path.join(report.work,'windows.png');
  await command('screenshot',{file:report.screenshot});
  report.complete=true;report.status='passed';
} catch(error){report.status='failed';report.error=String(error);process.exitCode=1;}
finally {
  child.stdin.end('{"op":"close"}\n');
  const terminal=await exit;clearTimeout(timer);
  if(terminal.code!==0){report.status='failed';report.complete=false;report.terminal=terminal;process.exitCode=1;}
  if(report.work)report.workbench=JSON.parse(fs.readFileSync(path.join(report.work,'report.json'),'utf8'));
  report.finishedAt=new Date().toISOString();save();
}
console.log(`McCLIM windows: ${report.status}; report: ${output}`);
