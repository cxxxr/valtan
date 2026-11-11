import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {buildDemo} from './bundle.mjs';
import {createDemoServer} from './server.mjs';
const work=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-mcclim-demo-'));
console.log(`Building McCLIM; build log: ${path.join(work,'build.log')}`);
await buildDemo(work);
const server=createDemoServer(work);
const port=Number(process.env.PORT ?? 8080);
await new Promise((resolve,reject)=>{server.once('error',reject);server.listen(port,'127.0.0.1',resolve);});
console.log(`McCLIM demo: http://127.0.0.1:${server.address().port}/`);
console.log('The page compiles the embedded Lisp sources during startup.');
for(const signal of ['SIGINT','SIGTERM']) process.once(signal,()=>{
  server.closeAllConnections();server.close(()=>process.exit(0));
});
