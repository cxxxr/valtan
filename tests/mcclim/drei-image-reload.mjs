// Focused source reload and backend API checks. Full Drei self-compilation is a
// separate gate: it uses Drei's parser and re-evaluates all original 34 sources.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import assert from 'node:assert/strict';
import {fileURLToPath} from 'node:url';
import {execFile} from 'node:child_process';
import {promisify} from 'node:util';
import {hash} from './bundle.mjs';
const here=path.dirname(fileURLToPath(import.meta.url)),root=path.resolve(here,'../..');
const output=path.resolve(process.argv[2]??'/tmp/valtan-drei-image-reload.json');
const work=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-drei-image-reload-'));
const report={status:'running',complete:false,startedAt:new Date().toISOString(),work,
  scope:'Reload unchanged upstream lisp-syntax.lisp, parse/evaluate lisp-syntax-swank.lisp and restore the backend twice; check syntax image selection and 22 image APIs; not full Drei self-compilation',runs:[]};
const save=()=>fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');save();
const quote=text=>'"'+text.replaceAll('\\','\\\\').replaceAll('"','\\"')+'"';
async function run(name,command,args,timeout){
  try {
    const result=await promisify(execFile)(command,args,{cwd:root,timeout,maxBuffer:8*1024*1024});
    fs.writeFileSync(path.join(work,name+'.log'),result.stdout+result.stderr);
  } catch(error){fs.writeFileSync(path.join(work,name+'.log'),(error.stdout??'')+(error.stderr??''));throw error;}
}
try {
  const planFile=path.join(work,'plan.json');
  await run('plan','sbcl',['--script',path.join(here,'upstream-plan.lisp'),planFile,'drei-mcclim/test'],120000);
  const plan=JSON.parse(fs.readFileSync(planFile));
  const file=plan.sourceResources.find(file=>path.basename(file)==='lisp-syntax.lisp');
  assert.ok(file);
  const upstream=fs.readFileSync(file,'utf8');
  const swankFile=plan.sourceResources.find(file=>path.basename(file)==='lisp-syntax-swank.lisp');
  assert.ok(swankFile);
  const swank=fs.readFileSync(swankFile,'utf8');
  const names=['drei-image-reload-checks.lisp','drei-image-checks.lisp'];
  const sources=names.map(name=>fs.readFileSync(path.join(here,name),'utf8'));
  report.upstreamSource={file,sha256:hash(upstream)};
  report.upstreamSwankSource={file:swankFile,sha256:hash(swank)};
  const backend=path.join(root,'library/mcclim-portability/drei-image.lisp');
  report.backendSource={file:backend,sha256:hash(fs.readFileSync(backend))};
  report.checks=Object.fromEntries(names.map((name,i)=>[name,hash(sources[i])]));
  const checks=path.join(work,'checks.lisp');
  fs.writeFileSync(checks,`(defpackage :valtan-drei-image-reload-tests (:use :cl))
(defparameter valtan-drei-image-reload-tests::*upstream-lisp-syntax-source* ${quote(upstream)})
(defparameter valtan-drei-image-reload-tests::*upstream-swank-source* ${quote(swank)})\n`+
    sources.join('\n')+'\n(write-line "DREI-IMAGE-RELOAD-APIS-PASS")\n');
  for(const target of ['node','browser']){
    const row={target,status:'running'};report.runs.push(row);save();
    const probe=path.join(work,target+'.json');
    try {
      await run(target,process.execPath,[path.join(here,'probe.mjs'),'mcclim-portability/drei',probe,checks,
        'clim-without-swank',...(target==='browser'?['--browser']:[])],45*60*1000);
    } finally {
      if(fs.existsSync(probe))row.probe=JSON.parse(fs.readFileSync(probe));
      row.status=row.probe?.status??'failed';save();
    }
    assert.equal(row.probe.status,'passed');assert.equal(row.probe.complete,true);
    const log=fs.readFileSync(probe.replace(/\.json$/,'')+'-logs/run.log','utf8');
    assert.match(log,/DREI-IMAGE-RELOAD-PASS 35 components/);
    assert.match(log,/DREI-IMAGE-RELOAD-APIS-PASS/);
    assert.equal(hash(fs.readFileSync(backend)),report.backendSource.sha256);
    row.status='passed';save();
  }
  report.status='passed';report.complete=true;
}catch(error){report.status='failed';report.error=String(error);process.exitCode=1;}
finally{report.finishedAt=new Date().toISOString();save();}
console.log(JSON.stringify({status:report.status,error:report.error,output}));
