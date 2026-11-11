// Real CLIM ACCEPT and Drei, driven through Chrome input and native IME events.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import readline from 'node:readline';
import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {hash} from './bundle.mjs';
const here=path.dirname(fileURLToPath(import.meta.url));
const output=path.resolve(process.argv[2]??'/tmp/valtan-mcclim-drei.json');
const fixture=fs.readFileSync(path.join(here,'drei-checks.lisp'),'utf8');
const snapshot=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-drei-checks-'));
const fixtureFile=path.join(snapshot,'checks.lisp');fs.writeFileSync(fixtureFile,fixture);
const report={status:'running',complete:false,startedAt:new Date().toISOString(),
  scope:'Drei-backed ACCEPT for edited Unicode text, an integer and member completion in a browser frame; not full Drei coverage or concurrent frame loops',
  checksSha256:hash(fixture),driverSha256:hash(fs.readFileSync(fileURLToPath(import.meta.url))),checks:[]};
const save=()=>fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');save();
const child=spawn(process.execPath,[path.join(here,'workbench.mjs')],{stdio:['pipe','pipe','inherit']});
let serial=0,readyResolve,readyReject,activeEvaluation;
const ready=new Promise((resolve,reject)=>{readyResolve=resolve;readyReject=reject;});
const pending=new Map();
const exit=new Promise(resolve=>child.on('close',(code,signal)=>{
  const error=Error(`Workbench exited: ${JSON.stringify({code,signal})}`);
  readyReject(error);for(const {reject} of pending.values())reject(error);
  pending.clear();resolve({code,signal});
}));
const timer=setTimeout(()=>child.kill('SIGKILL'),70*60*1000);
readline.createInterface({input:child.stdout}).on('line',line=>{
  const message=JSON.parse(line);
  if(message.work){report.work=message.work;save();}
  if(message.loading)console.log(message.loading);
  if(message.ready)readyResolve();
  const request=pending.get(message.id);
  if(request){pending.delete(message.id);message.error?request.reject(Error(message.error)):request.resolve(message.result);}
  else if(message.error)readyReject(Error(message.error));
});
const command=(op,fields={})=>new Promise((resolve,reject)=>{
  const id=++serial;pending.set(id,{resolve,reject});child.stdin.write(JSON.stringify({id,op,...fields})+'\n');
});
const evaluate=async source=>{
  const result=await command('eval',{source,timeout:120000});
  assert.equal(result?.ok,true,result?.error);return result.value;
};
const page=expression=>command('page',{expression});
const cdp=(method,params)=>command('cdp',{method,params});
async function waitFor(expression){
  const until=Date.now()+60000;
  while(!await page(expression)){
    assert.deepEqual(await page('mcclimTest.errors'),[]);
    if(activeEvaluation){
      const result=await page(`workbenchResults[${activeEvaluation}]`);
      if(result && !result.ok){report.result=result;throw Error(result.error);}
    }
    if(Date.now()>until)throw Error('Browser condition timed out: '+expression);
    await new Promise(resolve=>setTimeout(resolve,100));
  }
}
async function key(key){
  for(const type of ['keyDown','keyUp'])await cdp('Input.dispatchKeyEvent',{type,key,code:key});
}
async function click(point){
  await cdp('Input.dispatchMouseEvent',{type:'mouseMoved',...point});
  await cdp('Input.dispatchMouseEvent',{type:'mousePressed',...point,button:'left',clickCount:1});
  await cdp('Input.dispatchMouseEvent',{type:'mouseReleased',...point,button:'left',clickCount:1});
}
function checked(name){report.checks.push(name);save();console.log('Passed: '+name);}
try {
  await ready;
  const original=await page('[...mcclimTest.application.windows.keys()][0]');
  const baseline=await evaluate("(slot-value clim:*application-frame* 'valtan-clim-demo::count)");
  assert.match(baseline,/^\(\d+\)$/);const count=Number(baseline.slice(1,-1));
  const loaded=await command('eval',{file:fixtureFile});assert.equal(loaded?.ok,true,loaded?.error);
  const running=await command('eval',{source:'(mcclim-drei-checks::run)',wait:false});
  activeEvaluation=running.evaluationId;
  await waitFor("mcclimTest.status==='Drei string ready' && mcclimTest.application.windows.size===2");
  const editor=await page(`[...mcclimTest.application.windows.keys()].find(id=>id!==${original})`);
  await click(await page(`(()=>{const w=mcclimTest.application.windows.get(${editor}),r=w.canvas.getBoundingClientRect();return {x:r.left+30,y:r.top+20};})()`));
  await waitFor(`document.activeElement===mcclimTest.application.windows.get(${editor}).input`);
  await cdp('Input.insertText',{text:'abc'});
  await cdp('Input.imeSetComposition',{text:'日本語',selectionStart:0,selectionEnd:3});
  await cdp('Input.insertText',{text:'日本語'});
  await cdp('Input.insertText',{text:'😀'});
  await key('ArrowLeft');await key('Backspace');await key('Enter');
  await waitFor("mcclimTest.status==='Drei integer ready'");
  await evaluate('(assert (equal mcclim-drei-checks::*answers* \'("abc日本😀")))');
  checked('Drei edits native IME text and supplementary characters, then accepts the string');
  await cdp('Input.insertText',{text:'4x'});await key('Backspace');
  await cdp('Input.insertText',{text:'2'});await key('Enter');
  await waitFor("mcclimTest.status==='Drei completion ready'");
  await evaluate('(assert (equal mcclim-drei-checks::*answers* \'(42 "abc日本😀")))');
  checked('integer acceptance follows editing and multiline redisplay');
  await cdp('Input.insertText',{text:'al'});await key('Tab');
  // A queue round trip waits for editor redraw before capturing the evidence.
  await evaluate(':completion-events-consumed');
  report.screenshot=output.replace(/\.json$/, '')+'.png';
  await command('screenshot',{file:report.screenshot});
  await key('Enter');
  await waitFor(`!!workbenchResults[${running.evaluationId}] && mcclimTest.application.windows.size===1`);
  const result=await command('evaluation-result',{evaluationId:running.evaluationId});
  assert.equal(result?.ok,true,result?.error);
  assert.equal(result.value,'(("abc日本😀" 42 "alpha"))');report.result=result;
  checked('Tab completes the member and closing the input frame returns all accepted values');
  await click(await page(`(()=>{const c=mcclimTest.application.windows.get(${original}).canvas,r=c.getBoundingClientRect();return {x:r.left+r.width/2,y:r.bottom-20*r.height/c.height};})()`));
  await evaluate(`(assert (= (slot-value clim:*application-frame* 'valtan-clim-demo::count) ${count+1}))`);
  assert.deepEqual(await page('mcclimTest.errors'),[]);
  checked('the original frame still processes native pointer input');
  child.stdin.end('{"op":"close"}\n');const terminal=await exit;
  assert.equal(terminal.code,0,JSON.stringify(terminal));
  report.status='passed';report.complete=true;
} catch(error){report.status='failed';report.error=String(error);process.exitCode=1;child.kill('SIGTERM');}
finally {
  clearTimeout(timer);
  if(report.work && fs.existsSync(path.join(report.work,'report.json')))
    report.workbench=JSON.parse(fs.readFileSync(path.join(report.work,'report.json')));
  report.finishedAt=new Date().toISOString();save();
}
console.log(`McCLIM Drei: ${report.status}; report: ${output}`);
