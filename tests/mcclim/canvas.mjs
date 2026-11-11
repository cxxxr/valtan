// Cold-start integration test: real upstream CLIM draws into browser Canvas.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import readline from 'node:readline';
import {spawn} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {hash} from './bundle.mjs';
const here=path.dirname(fileURLToPath(import.meta.url));
const output=path.resolve(process.argv[2]??'/tmp/valtan-mcclim-canvas.json');
const fixture=fs.readFileSync(path.join(here,'canvas-checks.lisp'),'utf8');
const snapshot=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-canvas-checks-'));
const fixtureFile=path.join(snapshot,'checks.lisp');fs.writeFileSync(fixtureFile,fixture);
const report={status:'running',complete:false,startedAt:new Date().toISOString(),
  scope:'Real upstream CLIM pattern, clipping and composite-design pixel assertions in Chrome; not full McCLIM compatibility',
  checksSha256:hash(fixture)};
const save=()=>fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');save();
const child=spawn(process.execPath,[path.join(here,'workbench.mjs')],{stdio:['pipe','pipe','inherit']});
const exit=new Promise(resolve=>child.on('close',(code,signal)=>resolve({code,signal})));
const timer=setTimeout(()=>child.kill('SIGKILL'),70*60*1000);
let result;
try {
  for await(const line of readline.createInterface({input:child.stdout})){
    const message=JSON.parse(line);
    if(message.work){report.work=message.work;save();}
    if(message.loading)process.stdout.write(message.loading+'\n');
    if(message.ready)child.stdin.write(JSON.stringify({id:'canvas',op:'eval',file:fixtureFile,timeout:120000})+'\n');
    if(message.id==='canvas'){
      result=message;
      child.stdin.end('{"op":"close"}\n');
    }
    if(message.error && message.id!=='canvas')throw Error(message.error);
  }
  const terminal=await exit;
  if(terminal.code!==0)throw Error(`Workbench exited: ${JSON.stringify(terminal)}`);
  if(!result?.result?.ok)throw Error(result?.error??result?.result?.error??'Missing canvas result');
  if(result.result.value!=='(:PASSED 69)')throw Error(`Unexpected assertion count: ${result.result.value}`);
  const workbench=JSON.parse(fs.readFileSync(path.join(report.work,'report.json'),'utf8'));
  report.workbench=workbench;report.result=result.result;
  report.complete=true;report.status='passed';
} catch(error){
  report.status='failed';report.error=String(error);process.exitCode=1;
  child.kill('SIGTERM');
} finally {clearTimeout(timer);report.finishedAt=new Date().toISOString();save();}
console.log(`McCLIM Canvas: ${report.status}; report: ${output}`);
