// Exercise the unchanged upstream calculator through real browser pointer input.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import assert from 'node:assert/strict';
import {fileURLToPath} from 'node:url';
import {spawn} from 'node:child_process';
import {hash} from './bundle.mjs';
import {launchChrome} from './chrome.mjs';
import {createDemoServer} from './server.mjs';
const here=path.dirname(fileURLToPath(import.meta.url));
const [stageArg,outputArg]=process.argv.slice(2);
assert.ok(stageArg,'Usage: calculator.mjs completed-stage-work [output-directory]');
const stage=path.resolve(stageArg);
const work=outputArg?path.resolve(outputArg):fs.mkdtempSync(path.join(os.tmpdir(),'valtan-mcclim-calculator-'));
fs.mkdirSync(work,{recursive:true});
const manifest=JSON.parse(fs.readFileSync(path.join(stage,'manifest.json')));
const packages=manifest.records.find(r=>r.source.endsWith('/Core/system/packages.lisp'));
assert.ok(packages,'McCLIM source root missing from producer manifest');
const source=path.resolve(path.dirname(packages.source),'../../Examples/calculator.lisp');
const upstream=fs.readFileSync(source,'utf8');
const entry=fs.readFileSync(path.join(here,'calculator-entry.lisp'),'utf8');
const application=path.join(work,'application.lisp');
fs.writeFileSync(application,upstream+'\n'+entry);
const report={status:'running',complete:false,startedAt:new Date().toISOString(),work,
  scope:'Unchanged upstream calculator, compiled McCLIM libraries, production worker and actual browser pointer input',
  source:{file:source,sha256:hash(upstream)},entrySha256:hash(entry),driverSha256:hash(fs.readFileSync(fileURLToPath(import.meta.url))),
  producerManifest:{file:path.join(stage,'manifest.json'),sha256:hash(fs.readFileSync(path.join(stage,'manifest.json')))},steps:[]};
const output=path.join(work,'result.json');
const save=()=>fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');
let chrome,server;
save();
try{
  await new Promise((resolve,reject)=>{
    const child=spawn(process.execPath,[path.join(here,'staged-build.mjs'),stage,'gui','--application',application,'--output-directory',work],{stdio:'inherit'});
    child.on('error',reject);child.on('close',code=>code===0?resolve():reject(Error(`Build exited ${code}`)));
  });
  report.build=JSON.parse(fs.readFileSync(path.join(work,'gui-build.json')));save();
  server=createDemoServer(path.join(work,'gui'));
  await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
  chrome=await launchChrome();
  const page=await chrome.page(`http://127.0.0.1:${server.address().port}/`);
  async function waitFor(predicate,timeout=120000){
    const until=Date.now()+timeout;
    let state;
    do{
      state=await page.evaluate('globalThis.mcclimTest && ({ready:mcclimTest.ready,paints:mcclimTest.paints,status:mcclimTest.status,lastLog:mcclimTest.lastLog,errors:mcclimTest.errors})');
      if(state?.errors.length)throw Error(state.errors.join('\n'));
      if(state && predicate(state))return state;
      await new Promise(resolve=>setTimeout(resolve,500));
    }while(Date.now()<until);
    throw Error(`Calculator condition timed out: ${JSON.stringify(state)}`);
  }
  const initial=await waitFor(s=>s.ready && s.paints>0 && s.status.startsWith('{'),30*60*1000);
  const buttons=JSON.parse(initial.status);
  assert.equal(Object.keys(buttons).length,17);
  report.readyAt=new Date().toISOString();report.buttons=buttons;save();
  const canvas=await page.evaluate("(() => {const c=document.querySelector('canvas'),r=c.getBoundingClientRect();return {x:r.left,y:r.top,width:c.width,height:c.height};})()");
  report.canvas=canvas;
  for(const [name,[x,y]] of Object.entries(buttons))assert.ok(x>0 && y>0 && x<canvas.width && y<canvas.height,`Button outside canvas: ${name}`);
  await page.screenshot(path.join(work,'initial.png'));
  const pixels=()=>page.evaluate("document.querySelector('canvas').toDataURL()");
  const before=await pixels();
  async function click(name){
    const [dx,dy]=buttons[name],x=canvas.x+dx,y=canvas.y+dy;
    await page.send('Input.dispatchMouseEvent',{type:'mouseMoved',x,y});
    await page.send('Input.dispatchMouseEvent',{type:'mousePressed',x,y,button:'left',clickCount:1});
    await page.send('Input.dispatchMouseEvent',{type:'mouseReleased',x,y,button:'left',clickCount:1});
  }
  for(const [button,expected] of [['one','1'],['two','12'],['plus',null],['three','3'],['result','15'],['multiply',null],['four','4'],['result','60'],['ac','0'],['nine','9'],['ce','0'],['seven','7']]){
    const paints=await page.evaluate('mcclimTest.paints');
    await click(button);
    if(expected!==null)await waitFor(s=>s.status===`Calculator: ${expected}` && s.paints>paints);
    report.steps.push({button,expected});save();
  }
  const deadline=Date.now()+120000;
  while(await pixels()===before){assert.ok(Date.now()<deadline,'Calculator pixels did not change');await new Promise(resolve=>setTimeout(resolve,500));}
  await page.screenshot(path.join(work,'after-input.png'));
  assert.equal(hash(fs.readFileSync(source)),report.source.sha256);
  report.status='passed';report.complete=true;
}catch(error){report.status='failed';report.error=String(error);process.exitCode=1;}
finally{
  await chrome?.close();
  if(server?.listening){server.closeAllConnections();await new Promise(resolve=>server.close(resolve));}
  report.finishedAt=new Date().toISOString();save();
}
console.log(JSON.stringify({status:report.status,error:report.error,output}));
