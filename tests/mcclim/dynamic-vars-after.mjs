const lisp=globalThis.valtanDynamicKernel;
if(!lisp || !globalThis.valtanDynamicVars) throw new Error('Dynamic variable tests did not load');
const scheduler=new lisp.CooperativeScheduler({checkpointBudget:10000});
const [results]=await scheduler.spawn(globalThis.valtanDynamicVars).completion;
console.log('DYNAMIC-VARS-RESULT '+JSON.stringify(results));
if(results.registered!==13 || !results.results || results.failed || results.skipped || results.unexecuted.length)
  throw new Error('McCLIM dynamic variable tests did not all pass');
