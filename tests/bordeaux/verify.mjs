const errors = [], originalError = console.error;
console.error = (...args) => { errors.push(args); originalError(...args); };
try { await import('../dist-bordeaux/bordeaux.js'); }
finally { console.error = originalError; }
if (errors.length || !globalThis.valtanBordeauxTests) throw new Error('Bordeaux load failed');
const { verifyBordeaux } = await import('./checks.mjs');
console.log('BORDEAUX-CHECKS-PASS ' + JSON.stringify(await verifyBordeaux(globalThis.valtanBordeauxKernel, globalThis.valtanBordeauxTests)));
