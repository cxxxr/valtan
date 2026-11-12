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
const args=process.argv.slice(2);
const threaded=args.includes('--threaded');
if(threaded)args.splice(args.indexOf('--threaded'),1);
const modes=['--drei','--drei-performance','--drei-self-compilation','--drei-all'].filter(flag=>args.includes(flag));
if(modes.length>1)throw Error('Choose one Drei suite');
const mode=modes[0];
if(mode)args.splice(args.indexOf(mode),1);
if(args.length>1 || args.some(arg=>arg.startsWith('--')))
  throw Error('Usage: upstream.mjs [--threaded] [--drei | --drei-performance | --drei-self-compilation | --drei-all] [report.json]');
const drei=!!mode,allDrei=mode==='--drei-all',selfCompilation=allDrei || mode==='--drei-self-compilation';
const suite=allDrei?'DREI-ALL':mode==='--drei-performance'?'BUFFER-PERFORMANCE-TESTS':selfCompilation?'SELF-COMPILATION-TEST':drei?'DREI-TESTS':'MCCLIM';
const output=path.resolve(args[0]??`/tmp/valtan-${mode?.slice(2)??'mcclim'}-upstream${threaded?'-threaded':''}.json`);
const testSystem=drei?'drei-mcclim/test':'mcclim/test';
const targetSystems=drei?['mcclim-browser','fiveam','automaton']:['mcclim-browser/bezier','fiveam'];
const targetFeatures=drei?['clim-without-swank']:['clim-without-swank','use-salza2-zlib'];
if(threaded)targetFeatures.push('valtan-resumable','clim-mp');
const startupTimeout=(threaded?3:1)*60*60*1000;
// The unchanged performance and self-compilation suites each took hours in
// earlier standalone runs. Allow both to finish in this combined invocation.
const evaluationTimeout=(allDrei?(threaded?24:12):threaded?6:mode && mode!=='--drei'?4:1)*60*60*1000;
const dir=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-mcclim-upstream-'));
const report={status:'running',complete:false,startedAt:new Date().toISOString(),
  scope:`All sources in the installed ${drei?'drei-mcclim/test':'mcclim/test and mcclim/test-util'} ASDF plan, read with target features in real browser McCLIM; not GUI integration or complete extension coverage`,
  testSystem,suite,selfCompilation,threaded,startupTimeout,evaluationTimeout,targetSystems,targetFeatures,
  workbenchSourceHashes:Object.fromEntries(['upstream-plan.lisp','fiveam-executions.lisp','upstream-checks.lisp','upstream.mjs']
    .map(name=>[name,hash(fs.readFileSync(path.join(here,name)))]))};
const save=()=>fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');save();
let child;
try {
  const manifest=path.join(dir,'source-plan.json');
  const planned=spawnSync('sbcl',['--script',path.join(here,'upstream-plan.lisp'),manifest,testSystem],
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
  report.sources=snapshots.map(({source,...metadata})=>metadata);
  const sourceResources=selfCompilation?(plan.sourceResources??[]):[];
  if(selfCompilation)assert.ok(sourceResources.length>0,'No Drei source resources in the component tree');
  report.sourceResources=sourceResources.map(file=>({file,sha256:hash(fs.readFileSync(file))}));save();
  // Some upstream pathname tests require their own source file to exist. Supply
  // the unchanged snapshots as explicit static resources, just as application
  // assets are bundled; the browser has no host filesystem access.
  fs.writeFileSync(path.join(dir,'mcclim-upstream-data.asd'),`(asdf:defsystem "mcclim-upstream-data"
    :depends-on (${selfCompilation?'"mcclim-upstream-data/drei-sources"':''})
    :pathname ${quote(path.join(dir,'test-sources')+'/')}
    :components (${snapshots.map(({file},i)=>`(:static-file "source-${i}" :pathname ${quote(path.relative(path.join(dir,'test-sources'),file))})`).join(' ')}))
    ${selfCompilation?`(asdf:defsystem "mcclim-upstream-data/drei-sources"
      :pathname ${quote(plan.root)}
      :components (${sourceResources.map((file,i)=>`(:static-file "drei-source-${i}" :pathname ${quote(path.relative(plan.root,file))})`).join(' ')}))`:''}`);
  for(const file of fs.readdirSync(path.join(root,'example/try-using-mcclim')))
    if(/\.(system|lisp|html)$/.test(file))fs.copyFileSync(path.join(root,'example/try-using-mcclim',file),path.join(dir,file));
  fs.writeFileSync(path.join(dir,'app.system'),`(defsystem "app"
    :depends-on ("mcclim-load-trace" ${targetSystems.map(quote).join(' ')} "mcclim-upstream-data")
    :components ((:file "main")) :target :browser
    :target-features (${targetFeatures.map(name=>':'+name).join(' ')}) :compile-mode :runtime)`);
  const fixture=(threaded?`(assert (member :clim-mp *features*))
    (assert (eq ((ffi:ref "lisp" "hasExecutionContext")) (ffi:ref "true")))
    `:'')+fs.readFileSync(path.join(here,'fiveam-executions.lisp'),'utf8')+'\n'+
    fs.readFileSync(path.join(here,'upstream-checks.lisp'),'utf8')+
    '\n(setf mcclim-upstream-checks::*files* nil)\n'+
    snapshots.map(({file,source})=>`(mcclim-upstream-checks::load-test-source ${quote(file)} ${quote(source)})`).join('\n')+
    (selfCompilation?'\n(setf (symbol-value (find-symbol "*RUN-SELF-COMPILATION-TEST*" "DREI-TESTS")) t)\n':'')+
    `\n(mcclim-upstream-checks::run ${allDrei?'(list (find-symbol "DREI-TESTS" "DREI-TESTS") (find-symbol "BUFFER-PERFORMANCE-TESTS" "DREI-TESTS"))':drei?`(find-symbol "${suite}" "DREI-TESTS")`:':mcclim'})\n`;
  const checkFile=path.join(dir,'checks.lisp');fs.writeFileSync(checkFile,fixture);report.checksSha256=hash(fixture);save();
  child=spawn(process.execPath,[path.join(here,'workbench.mjs'),'--source-directory',dir,
    '--startup-timeout',String(startupTimeout)],{stdio:['pipe','pipe','inherit']});
  const terminal=new Promise(resolve=>child.on('close',(code,signal)=>resolve({code,signal})));
  const timer=setTimeout(()=>child.kill('SIGKILL'),startupTimeout+evaluationTimeout+10*60*1000);let result;
  try {
    for await(const line of readline.createInterface({input:child.stdout})){
      const message=JSON.parse(line);
      if(message.work){report.work=message.work;save();}
      if(message.loading)console.log(message.loading);
      if(message.evaluating){report.lastProgress=message.evaluating;save();console.log(message.evaluating);}
      if(message.ready){
        for(const resource of report.sourceResources)
          assert.equal(hash(fs.readFileSync(resource.file)),resource.sha256,`Source changed during build: ${resource.file}`);
        child.stdin.write(JSON.stringify({op:'eval',id:'upstream',file:checkFile,timeout:evaluationTimeout})+'\n');
      }
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
