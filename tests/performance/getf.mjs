// Measure ordinary GETF calls on a dual-compiled runtime. Startup is excluded.
// Usage: node tests/performance/getf.mjs /absolute/bundle.mjs /tmp/report.json
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {pathToFileURL} from 'node:url';
import {createHash} from 'node:crypto';
import {performance} from 'node:perf_hooks';
const [bundle,output]=process.argv.slice(2);
assert.ok(bundle && output, 'Provide a bundle path and report path');
await import(pathToFileURL(bundle));
const lisp=globalThis.valtanSchedulerKernel;
const getf=lisp.intern('GETF','COMMON-LISP').getFunction();
const keys=Array.from({length:64},(_,i)=>lisp.intern(`GETF-BENCH-${i}`,'KEYWORD'));
const plist=lisp.jsArrayToList(keys.flatMap((key,i)=>[key,i]));
const missing=lisp.intern('GETF-BENCH-MISSING','KEYWORD');
const workloads=[['hit-last',keys[63],63],['missing',missing,-1]];
const results=[];
for(const [name,key,expected] of workloads){
 const run=()=>assert.equal(getf(plist,key,-1),expected);
 let start=performance.now(),iterations=0;
 do{run();iterations++}while(performance.now()-start<500||iterations<100);
 const batch=Math.max(1,Math.ceil(iterations*100/(performance.now()-start)));
 const samples=[];
 for(let sample=0;sample<7;sample++){start=performance.now();for(let i=0;i<batch;i++)run();samples.push((performance.now()-start)/batch)}
 results.push({name,batch,samples,medianMs:[...samples].sort((a,b)=>a-b)[3]});
}
const report={date:new Date().toISOString(),node:process.version,bundleSha256:createHash('sha256').update(fs.readFileSync(bundle)).digest('hex'),results};
fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');console.log(JSON.stringify(report));
