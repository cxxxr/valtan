// Fresh real-McCLIM PNG stream output, then independent image validation.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import readline from 'node:readline';
import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {hash} from './bundle.mjs';
import {verifyPngOutput} from './png-write-verify.mjs';
const here=path.dirname(fileURLToPath(import.meta.url));
const args=process.argv.slice(2),node=args.includes('--node');
if(node)args.splice(args.indexOf('--node'),1);
if(args.length>1 || args.some(arg=>arg.startsWith('--')))
  throw Error('Usage: png-write.mjs [--node] [report.json]');
const output=path.resolve(args[0]??`/tmp/valtan-mcclim-png-write${node?'-node':''}.json`);
if(node){
  const directory=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-png-write-files-'));
  const fixture=path.join(directory,'checks.lisp'),destination=path.join(directory,'saved.png');
  fs.writeFileSync(fixture,fs.readFileSync(path.join(here,'png-write-checks.lisp'),'utf8')+`
(mcclim-png-write-checks::run)
(mcclim-png-write-checks::run-files #p${JSON.stringify(destination)})
(assert (= mcclim-png-write-checks::*checks* 51))
`);
  const child=spawn(process.execPath,[path.join(here,'probe.mjs'),'mcclim-browser',output,fixture,
    'clim-without-swank','use-salza2-zlib'],{stdio:'inherit'});
  const code=await new Promise((resolve,reject)=>{child.on('error',reject);child.on('close',resolve);});
  if(code!==0)process.exitCode=1;
} else {
const source=fs.readFileSync(path.join(here,'png-write-checks.lisp'),'utf8')+
  '\n(mcclim-png-write-checks::run)\n'+fs.readFileSync(path.join(here,'png-write-export.lisp'),'utf8');
const directory=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-png-write-checks-'));
const fixture=path.join(directory,'checks.lisp');fs.writeFileSync(fixture,source);
const report={status:'running',complete:false,startedAt:new Date().toISOString(),checksSha256:hash(source),
  scope:'Real McCLIM PNG output to binary streams in a fresh browser, including upstream ZPNG and independent decoding; filesystem output is tested separately on Node'};
const save=()=>fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');save();
const child=spawn(process.execPath,[path.join(here,'workbench.mjs')],{stdio:['pipe','pipe','inherit']});
const terminal=new Promise(resolve=>child.on('close',(code,signal)=>resolve({code,signal})));
const timer=setTimeout(()=>child.kill('SIGKILL'),70*60*1000);let result;
try {
  for await(const line of readline.createInterface({input:child.stdout})){
    const message=JSON.parse(line);
    if(message.work){report.work=message.work;save();}
    if(message.loading)console.log(message.loading);
    if(message.ready)child.stdin.write(JSON.stringify({op:'eval',id:'png-write',file:fixture,timeout:180000})+'\n');
    if(message.id==='png-write'){result=message;child.stdin.end('{"op":"close"}\n');}
    if(message.error && message.id!=='png-write')throw Error(message.error);
  }
  const ended=await terminal;assert.equal(ended.code,0,JSON.stringify(ended));
  assert.equal(result?.result?.ok,true,result?.error??result?.result?.error);
  report.validation=await verifyPngOutput(result.result.value);
  report.status='passed';report.complete=true;
} catch(error){report.status='failed';report.error=String(error);process.exitCode=1;child.kill('SIGTERM');}
finally {
  clearTimeout(timer);
  if(report.work && fs.existsSync(path.join(report.work,'report.json')))
    report.workbench=JSON.parse(fs.readFileSync(path.join(report.work,'report.json'),'utf8'));
  report.finishedAt=new Date().toISOString();save();
}
console.log(`McCLIM PNG output: ${report.status}; report: ${output}`);
}
