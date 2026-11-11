// Attempt every source in the upstream test-system plan, retaining load errors
// and failed/skipped checks. This driver never turns a subset into a full pass.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import readline from 'node:readline';
import assert from 'node:assert/strict';
import {spawn,spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {hash} from './bundle.mjs';
const here=path.dirname(fileURLToPath(import.meta.url)),root=path.resolve(here,'../..');
const output=path.resolve(process.argv[2]??'/tmp/valtan-mcclim-upstream.json');
const dir=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-mcclim-upstream-'));
const report={status:'running',complete:false,startedAt:new Date().toISOString(),
  scope:'All sources in the installed mcclim/test and mcclim/test-util ASDF plan, read with target features in real browser McCLIM; not GUI integration or complete extension coverage',
  targetSystems:['mcclim-browser/bezier','fiveam'],
  targetFeatures:['clim-without-swank','use-salza2-zlib'],
  workbenchSourceHashes:Object.fromEntries(['upstream-plan.lisp','upstream-checks.lisp','upstream.mjs']
    .map(name=>[name,hash(fs.readFileSync(path.join(here,name)))]))};
const save=()=>fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');save();
let child;
try {
  const manifest=path.join(dir,'source-plan.json');
  const planned=spawnSync('sbcl',['--script',path.join(here,'upstream-plan.lisp'),manifest],
    {encoding:'utf8',timeout:120000,maxBuffer:16*1024*1024});
  fs.writeFileSync(path.join(dir,'plan.log'),(planned.stdout??'')+(planned.stderr??''));
  assert.equal(planned.status,0,planned.stderr??String(planned.error));
  const plan=JSON.parse(fs.readFileSync(manifest));
  report.asdfVersion=plan.asdfVersion;
  report.upstreamDefinition={file:plan.definition,sha256:hash(fs.readFileSync(plan.definition))};
  const quote=value=>'"'+value.replaceAll('\\','\\\\').replaceAll('"','\\"')+'"';
  const snapshots=plan.files.map(original=>{
    const relative=path.relative(plan.root,original);assert.ok(!relative.startsWith('..') && !path.isAbsolute(relative));
    const file=path.join(dir,'test-sources',relative),source=fs.readFileSync(original,'utf8');
    fs.mkdirSync(path.dirname(file),{recursive:true});fs.writeFileSync(file,source);
    return {original,file,source,sha256:hash(source)};
  });
  report.sources=snapshots.map(({source,...metadata})=>metadata);save();
  // Some upstream pathname tests require their own source file to exist. Supply
  // the unchanged snapshots as explicit static resources, just as application
  // assets are bundled; the browser has no host filesystem access.
  fs.writeFileSync(path.join(dir,'mcclim-upstream-data.asd'),`(asdf:defsystem "mcclim-upstream-data"
    :pathname ${quote(path.join(dir,'test-sources')+'/')}
    :components (${snapshots.map(({file},i)=>`(:static-file "source-${i}" :pathname ${quote(path.relative(path.join(dir,'test-sources'),file))})`).join(' ')}))`);
  for(const file of fs.readdirSync(path.join(root,'example/try-using-mcclim')))
    if(/\.(system|lisp|html)$/.test(file))fs.copyFileSync(path.join(root,'example/try-using-mcclim',file),path.join(dir,file));
  fs.writeFileSync(path.join(dir,'app.system'),`(defsystem "app"
    :depends-on ("mcclim-load-trace" "mcclim-browser/bezier" "fiveam" "mcclim-upstream-data")
    :components ((:file "main")) :target :browser
    :target-features (:clim-without-swank :use-salza2-zlib) :compile-mode :runtime)`);
  const fixture=fs.readFileSync(path.join(here,'upstream-checks.lisp'),'utf8')+
    '\n(setf mcclim-upstream-checks::*files* nil)\n'+
    snapshots.map(({file,source})=>`(mcclim-upstream-checks::load-test-source ${quote(file)} ${quote(source)})`).join('\n')+
    '\n(mcclim-upstream-checks::run)\n';
  const checkFile=path.join(dir,'checks.lisp');fs.writeFileSync(checkFile,fixture);report.checksSha256=hash(fixture);save();
  child=spawn(process.execPath,[path.join(here,'workbench.mjs'),'--source-directory',dir],{stdio:['pipe','pipe','inherit']});
  const terminal=new Promise(resolve=>child.on('close',(code,signal)=>resolve({code,signal})));
  const timer=setTimeout(()=>child.kill('SIGKILL'),130*60*1000);let result;
  try {
    for await(const line of readline.createInterface({input:child.stdout})){
      const message=JSON.parse(line);
      if(message.work){report.work=message.work;save();}
      if(message.loading)console.log(message.loading);
      if(message.evaluating){report.lastProgress=message.evaluating;save();console.log(message.evaluating);}
      if(message.ready)child.stdin.write(JSON.stringify({op:'eval',id:'upstream',file:checkFile,timeout:60*60*1000})+'\n');
      if(message.id==='upstream'){result=message;child.stdin.end('{"op":"close"}\n');}
      if(message.error && message.id!=='upstream')throw Error(message.error);
    }
    const ended=await terminal;assert.equal(ended.code,0,JSON.stringify(ended));
    assert.equal(result?.result?.ok,true,result?.error??result?.result?.error);
    const value=result.result.value;assert.ok(value.startsWith('("') && value.endsWith('")'));
    const checks=JSON.parse(JSON.parse(value.slice(1,-1)));report.checks=checks;
    assert.deepEqual(checks.files.map(row=>row.file),snapshots.map(row=>row.file));
    assert.ok(checks.registered>0 && checks.results>0,'No upstream checks executed');
    report.complete=true;report.status=checks.status;
    if(checks.status!=='passed')process.exitCode=1;
  } finally {clearTimeout(timer);}
} catch(error){report.status='failed';report.error=String(error);process.exitCode=1;child?.kill('SIGTERM');}
finally {
  if(report.work && fs.existsSync(path.join(report.work,'report.json')))
    report.workbench=JSON.parse(fs.readFileSync(path.join(report.work,'report.json')));
  report.finishedAt=new Date().toISOString();save();
}
console.log(`McCLIM upstream: ${report.status}; report: ${output}`);
