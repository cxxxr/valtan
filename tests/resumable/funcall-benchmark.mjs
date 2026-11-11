import fs from 'node:fs';
import {createHash} from 'node:crypto';
const [bundle,output]=process.argv.slice(2);
await import(bundle);
const lisp=globalThis.valtanSchedulerKernel;
const f=name=>lisp.intern(name,'COMMON-LISP').getFunction();
const source=`(progn
 (defclass funcall-bench-cell () ((value :initform 0 :accessor funcall-bench-value)))
 (defmethod funcall-bench-step ((cell funcall-bench-cell)) (incf (funcall-bench-value cell)))
 (ffi:set (ffi:ref "globalThis" "valtanFuncallBench")
  (ffi:object
   "closure" (let ((next (lambda (i) (1+ i))))
               (lambda (n) (let ((total 0)) (dotimes (i n total) (incf total (funcall next i))))))
   "generic" (lambda (n) (let ((cell (make-instance 'funcall-bench-cell)))
                          (dotimes (i n) (funcall #'funcall-bench-step cell))
                          (funcall-bench-value cell))))))`;
f('EVAL')(f('READ-FROM-STRING')(f('SIMPLE-MAKE-STRING')(source)));
const sha=x=>createHash('sha256').update(x).digest('hex');
const report={startedAt:new Date().toISOString(),status:'running',bundleSha256:sha(fs.readFileSync(bundle)),sourceSha256:sha(source),source,results:[]};
const save=()=>fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');save();
const scheduler=new lisp.CooperativeScheduler({checkpointBudget:10000});
for(const [name,iterations,expected] of [['closure',20000,200010000],['generic',2000,2000]]){
 const samples=[];
 for(let trial=0;trial<4;trial++){
  const start=performance.now();const values=await scheduler.spawn(globalThis.valtanFuncallBench[name],[iterations]).completion;
  const ms=performance.now()-start;
  if(values.length!==1||values[0]!==expected)throw new Error(name+': incorrect result '+values);
  if(trial)samples.push(ms);console.log(name,trial,ms);
 }
 report.results.push({name,iterations,expected,samples,medianMs:[...samples].sort((a,b)=>a-b)[1]});save();
}
report.status='passed';report.finishedAt=new Date().toISOString();save();
