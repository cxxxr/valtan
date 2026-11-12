import fs from 'node:fs';
import path from 'node:path';
import http from 'node:http';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {launchChrome} from './chrome.mjs';
const [workArg]=process.argv.slice(2);
assert.ok(workArg, 'Usage: staged-api.mjs work');
const work=path.resolve(workArg),bundle=fs.readFileSync(path.join(work,'direct.mjs'));
const manifest=JSON.parse(fs.readFileSync(path.join(work,'manifest.json')));
const built=JSON.parse(fs.readFileSync(path.join(work,'direct-build.json')));
const sha=bytes=>createHash('sha256').update(bytes).digest('hex');
assert.equal(manifest.status,'produced');
assert.equal(manifest.complete,true);
assert.equal(built.bundleSha256,sha(bundle));
assert.equal(built.producerManifestSha256,sha(fs.readFileSync(path.join(work,'manifest.json'))));
const assertions=`(progn
 (assert (not (member :node *features*)))
 (assert (member :clim-mp *features*))
 (assert (clim:region-equal clim:+nowhere+ (clim:region-intersection clim:+everywhere+ clim:+nowhere+)))
 (assert (equal (multiple-value-list (clim:bounding-rectangle* (clim:make-bounding-rectangle 1 2 3 4))) '(1 2 3 4)))
 (assert (macro-function 'clim:define-application-frame))
 (assert (clim:presentation-typep 42.5 'real))
 (assert (clim:presentation-typep 42 'integer))
 (assert (not (clim:presentation-typep 42.5 'integer)))
 (assert (clim:presentation-typep 3 '(integer 0 3)))
 (assert (not (clim:presentation-typep 4 '(integer 0 3))))
 (let* ((system (asdf:find-system "drei-mcclim"))
        (components (asdf:module-components system))
        (backend (asdf:find-component system "valtan-image")))
  (assert (typep (drei-lisp-syntax::default-image) 'drei-lisp-syntax::valtan-local-image))
  (assert (= (length components) 35))
  (assert (eq (car (last components)) backend))
  (assert (eq (asdf:component-parent backend) system))
  (assert (probe-file (asdf:component-pathname backend)))
  (with-open-file (source (asdf:component-pathname backend))
   (assert (string= (read-line source) "(in-package #:drei-lisp-syntax)"))))
 (let* ((text "A日本語Z")
        (bytes (babel:string-to-octets text :encoding :iso-2022-jp)))
  (assert (equalp bytes #(65 27 36 66 70 124 75 92 56 108 27 40 66 90)))
  (assert (string= text (babel:octets-to-string bytes :encoding :iso-2022-jp))))
 (let ((thread (bt:make-thread (lambda () 42))))
  (assert (= 42 (bt:join-thread thread))))
 :restored)`;
const worker=`const lines=[],errors=[];console.log=(...args)=>{const message=args.join(' ');lines.push(message);postMessage({loading:true,message});};console.error=(...args)=>errors.push(args.join(' '));
try {
 const started=performance.now();await import('/app.mjs');const loadMs=performance.now()-started;
 const lisp=globalThis.stageReplayKernel,s=n=>lisp.intern(n,'COMMON-LISP'),str=x=>s('SIMPLE-MAKE-STRING').getFunction()(x);
 const form=s('READ-FROM-STRING').getFunction()(str(${JSON.stringify(assertions)}));
 const scheduler=new lisp.CooperativeScheduler({checkpointBudget:10000});
 const result=await scheduler.spawn(s('EVAL'),[form]).completion;
 if(result[0].name!=='RESTORED')throw new Error('Expected restored result');
 postMessage({done:true,lines,errors,loadMs,apiAndThreadAssertions:true,babelIso2022Jp:true,dreiBackendAndSource:true,sourceLoaderDisabled:true,liveTasks:lisp.allScheduledTasks().length});
}catch(error){postMessage({done:true,lines,errors:[...errors,String(error)]});}`;
const requests=[], server=http.createServer((req,res)=>{
 requests.push(req.url);
 if(req.url==='/app.mjs'){res.setHeader('Content-Type','text/javascript');res.end(bundle);}
 else if(req.url==='/worker.mjs'){res.setHeader('Content-Type','text/javascript');res.end(worker);}
 else if(req.url==='/'){res.setHeader('Content-Type','text/html');res.end(`<script>globalThis.stageResult={done:false};const w=new Worker('/worker.mjs',{type:'module'});w.onmessage=e=>{if(e.data.loading)stageResult.last=e.data.message;else stageResult=e.data};w.onerror=e=>stageResult={done:true,errors:[e.message]};</script>`);}
 else{res.statusCode=404;res.end();}
});
await new Promise(r=>server.listen(0,'127.0.0.1',r));let chrome;
const report={status:'running',complete:false,startedAt:new Date().toISOString(),scope:'Fresh browser replay of target-compiled McCLIM components with embedded source loading disabled; geometry, restored REAL/INTEGER presentation types, macro presence, Drei image backend/component/source resource, Babel ISO-2022-JP bytes/round-trip and scheduled BT join assertions. The backend resource is read, not loaded or evaluated. GUI validation is separate.',producerManifest:path.join(work,'manifest.json'),bundleSha256:createHash('sha256').update(bundle).digest('hex')};
report.driverSha256=sha(fs.readFileSync(new URL(import.meta.url)));
report.assertionsSha256=sha(assertions);
const output=path.join(work,'browser-result.json'),save=()=>fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');save();
try{
 chrome=await launchChrome();const page=await chrome.page(`http://127.0.0.1:${server.address().port}/`);
 const deadline=Date.now()+20*60*1000;let last='';
 while(!await page.evaluate('stageResult.done')){
  assert.ok(Date.now()<deadline,'Replay observation timed out');
  const current=await page.evaluate('stageResult.last');if(current&&current!==last){last=current;console.log(last);}
  await new Promise(r=>setTimeout(r,500));
 }
 const state=await page.evaluate('stageResult');Object.assign(report,state);
 assert.deepEqual(state.errors,[]);assert.ok(state.lines.join('\n').includes('TARGET-STAGE-MCCLIM-PASS'));
 assert.deepEqual(state.lines.filter(line=>line.startsWith('TARGET-REPLAY ')).map(line=>line.slice('TARGET-REPLAY '.length)),manifest.records.map(record=>record.source));
 assert.equal(state.apiAndThreadAssertions,true);assert.equal(state.liveTasks,0);
 assert.equal(state.babelIso2022Jp,true);
 assert.equal(state.dreiBackendAndSource,true);
 assert.ok(!requests.some(x=>x.endsWith('.lisp')));
 report.status='passed';report.complete=true;
}catch(error){report.status='failed';report.error=String(error);process.exitCode=1;}
finally{report.requests=requests;report.finishedAt=new Date().toISOString();save();await chrome?.close();await new Promise(r=>server.close(r));}
console.log(JSON.stringify({status:report.status,error:report.error,errors:report.errors,loadMs:report.loadMs,output}));
