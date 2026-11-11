import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
await import('../dist-resumable/resumable.js');
const lisp=globalThis.valtanSchedulerKernel;
const symbol=(name,pkg='COMMON-LISP')=>lisp.intern(name,pkg);
const string=value=>symbol('SIMPLE-MAKE-STRING').getFunction()(value);
const call=(name,...args)=>symbol(name).getFunction()(...args);
symbol('*PACKAGE*').value=call('FIND-PACKAGE',string('COMMON-LISP-USER'));
const read=source=>call('READ-FROM-STRING',string(source));
const scheduler=new lisp.CooperativeScheduler({checkpointBudget:10000});
let checks=0;
const equal=(actual,expected)=>{assert.equal(actual,expected);checks++;};
const directory=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-defconstant-'));
try {
 for(const mode of ['ordinary','scheduled']){
  const run=async source=>mode==='ordinary'?call('EVAL',read(source)):
   (await scheduler.spawn(symbol('EVAL'),[read(source)]).completion)[0];
  const prefix=`${mode}-constant`,effects=`*${prefix}-effects*`,top=`+${prefix}-top+`,nested=`+${prefix}-nested+`;
  await run(`(defparameter ${effects} 0)`);
  await run(`(defconstant ${top} (progn (incf ${effects}) 19))`);
  equal(await run(effects),1);
  equal(await run(top),19);
  equal(await run(`(constantp '${top})`),lisp.S_t);
  const fn=await run(`(compile nil '(lambda (value) (defconstant ${nested} (progn (incf ${effects}) value))))`);
  equal(await run(effects),1);
  equal(await run(`(boundp '${nested})`),lisp.S_nil);
  equal(await run(`(constantp '${nested})`),lisp.S_nil);
  if(mode==='ordinary')call('FUNCALL',fn,37);else await scheduler.spawn(fn,[37]).completion;
  equal(await run(effects),2);
  equal(await run(nested),37);
  equal(await run(`(constantp '${nested})`),lisp.S_t);
  const fileName=`+${prefix}-file+`,accessor=`${prefix}-file-value`;
  const source=path.join(directory,mode+'.lisp'),artifact=source.replace('.lisp','.fasl');
  fs.writeFileSync(source,`(in-package :cl-user)
   (defconstant ${fileName} (progn (incf ${effects}) 71))
   (defun ${accessor} () #.${fileName})`);
  call('COMPILE-FILE',string(source));
  equal(await run(effects),3);
  equal(await run(`(fboundp '${accessor})`),lisp.S_nil);
  fs.unlinkSync(source);
  if(mode==='ordinary')call('LOAD',string(artifact));else await scheduler.spawn(symbol('LOAD'),[string(artifact)]).completion;
  equal(await run(effects),4);
  equal(await run(`(${accessor})`),71);
  equal(await run(`(constantp '${fileName})`),lisp.S_t);
 }
 equal(scheduler.tasks.size,0);
} finally {fs.rmSync(directory,{recursive:true,force:true});}
console.log('DEFCONSTANT-CHECKS-PASS '+JSON.stringify({checks,evalInitializerOnce:true,lexicalInitializer:true,noNestedCompileTimeEffects:true,topLevelFileRecognition:true,sourceDeletedBeforeLoad:true}));
