// Exercise the observer and actual JSON report on a recursively invoked suite.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import assert from 'node:assert/strict';
import {fileURLToPath} from 'node:url';
import {execFile} from 'node:child_process';
import {promisify} from 'node:util';
import {hash} from './bundle.mjs';
const here=path.dirname(fileURLToPath(import.meta.url)),root=path.resolve(here,'../..');
const output=path.resolve(process.argv[2]??'/tmp/valtan-fiveam-executions.json');
const work=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-fiveam-executions-'));
const names=['fiveam-executions.lisp','fiveam-execution-checks.lisp','upstream-checks.lisp','fiveam-execution-report-checks.lisp'];
const sources=Object.fromEntries(names.map(name=>[name,fs.readFileSync(path.join(here,name),'utf8')]));
const quote=text=>'"'+text.replaceAll('\\','\\\\').replaceAll('"','\\"')+'"';
const checks=path.join(work,'checks.lisp');
// The reporting function binds these two frame variables. No CLIM operations
// are used by these synthetic FiveAM cases; real library tests remain separate.
const frameBindings=`(defpackage :clim (:export :*application-frame*))
(defpackage :climi)
(defvar clim:*application-frame* nil)
(defvar climi::*default-application-frame* nil)
(pushnew :mcclim *features*)\n`;
fs.writeFileSync(checks,sources[names[0]]+'\n'+frameBindings+sources[names[2]]+
  `\n(mcclim-upstream-checks::load-test-source ${quote(path.join(here,names[1]))} ${quote(sources[names[1]])})\n`+sources[names[3]]);
const host=path.join(work,'host.lisp');
fs.writeFileSync(host,`(load ${quote(process.env.QUICKLISP_SETUP??path.join(os.homedir(),'quicklisp/setup.lisp'))})\n(ql:quickload :fiveam :silent t)\n`+
  sources[names[0]]+'\n'+sources[names[1]]);
const scheduled=path.join(work,'scheduled.mjs');
fs.writeFileSync(scheduled,`const lisp=globalThis.valtanFiveamExecutionKernel;
const scheduler=new lisp.CooperativeScheduler({checkpointBudget:100});
for(const name of ['RUN-CHECKS','RUN-REPORT-CHECKS']){
 const result=await scheduler.spawn(lisp.intern(name,'VALTAN-FIVEAM-EXECUTION-CHECKS'),[]).completion;
 if(result[0]!==lisp.S_t || lisp.allScheduledTasks().length!==0)throw Error('Scheduled checks failed: '+name);
}
console.log('FIVEAM-NESTED-SCHEDULED-PASS');\n`);
const report={status:'running',complete:false,startedAt:new Date().toISOString(),work,
  scope:'Nested FiveAM invocations, hidden failure/skip detection and JSON reporting; synthetic cases, not McCLIM or Drei coverage',
  sources:Object.fromEntries(names.map(name=>[name,hash(sources[name])])),runs:[]};
const save=()=>fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');save();
async function run(name,command,args){
  const row={name,command,args};report.runs.push(row);save();
  try{
    const {stdout,stderr}=await promisify(execFile)(command,args,{cwd:root,timeout:300000,maxBuffer:4*1024*1024});
    fs.writeFileSync(path.join(work,name+'.log'),stdout+stderr);row.status='passed';
  }catch(error){fs.writeFileSync(path.join(work,name+'.log'),(error.stdout??'')+(error.stderr??''));row.status='failed';throw error;}
  save();
}
try{
  await run('native','sbcl',['--script',host]);
  assert.match(fs.readFileSync(path.join(work,'native.log'),'utf8'),/FIVEAM-NESTED-EXECUTIONS-PASS/);
  for(const [name,options] of [['node',[]],['scheduled',['valtan-resumable','--browser','--after-load='+scheduled]]]){
    const probe=path.join(work,name+'.json');
    await run(name,process.execPath,[path.join(here,'probe.mjs'),'fiveam',probe,checks,'--runtime-timeout=120000',...options]);
    const result=JSON.parse(fs.readFileSync(probe));assert.equal(result.status,'passed');assert.equal(result.complete,true);
    report.runs.at(-1).probe=result;
    const log=fs.readFileSync(probe.replace(/\.json$/,'')+'-logs/run.log','utf8');
    assert.match(log,/FIVEAM-NESTED-REPORT-PASS/);
    if(name==='scheduled')assert.match(log,/FIVEAM-NESTED-SCHEDULED-PASS/);
  }
  report.status='passed';report.complete=true;
}catch(error){report.status='failed';report.error=String(error);process.exitCode=1;}
finally{report.finishedAt=new Date().toISOString();save();}
console.log(JSON.stringify({status:report.status,error:report.error,output}));
