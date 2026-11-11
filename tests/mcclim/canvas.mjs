// Cold-start integration test: real upstream CLIM draws into browser Canvas.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import readline from 'node:readline';
import {spawn} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {hash} from './bundle.mjs';
const here=path.dirname(fileURLToPath(import.meta.url));
const args=process.argv.slice(2),flipping=args.includes('--flipping'),bezier=args.includes('--bezier'),threaded=args.includes('--threaded'),resumable=args.includes('--resumable')||threaded;
if(flipping)args.splice(args.indexOf('--flipping'),1);
if(bezier)args.splice(args.indexOf('--bezier'),1);
if(args.includes('--resumable'))args.splice(args.indexOf('--resumable'),1);
if(threaded)args.splice(args.indexOf('--threaded'),1);
if(args.length>1 || args.some(arg=>arg.startsWith('--')))
  throw Error('Usage: canvas.mjs [--flipping] [--bezier] [--resumable|--threaded] [report.json]');
const output=path.resolve(args[0]??`/tmp/valtan-mcclim-${bezier&&flipping?'bezier-flipping':flipping?'flipping':bezier?'bezier-canvas':'canvas'}${threaded?'-threaded':resumable?'-resumable':''}.json`);
const threadedChecks=`
(assert (member :clim-mp *features*))
(assert (eq ((ffi:ref "lisp" "hasExecutionContext")) (ffi:ref "true")))
(assert (typep clim:*application-frame* 'valtan-clim-demo::counter-frame))
(assert (bt:thread-alive-p
         (climi::port-event-process (clim:find-port :server-path '(:browser)))))
(let ((thread (bt:make-thread (lambda () (sleep .01) (values :joined 42)))))
  (assert (equal (multiple-value-list (bt:join-thread thread)) '(:joined 42))))
`;
const fixture=(threaded?threadedChecks:'')+fs.readFileSync(path.join(here,'canvas-checks.lisp'),'utf8')+
  '\n'+fs.readFileSync(path.join(here,'pixmap-checks.lisp'),'utf8')+
  '\n'+fs.readFileSync(path.join(here,'text-checks.lisp'),'utf8')+
  (bezier?'\n'+fs.readFileSync(path.join(here,'bezier-canvas-checks.lisp'),'utf8')+
    '\n'+fs.readFileSync(path.join(here,'bezier-region-checks.lisp'),'utf8'):'')+
  (flipping?'\n'+fs.readFileSync(path.join(here,'flipping-checks.lisp'),'utf8')+
    '\n'+fs.readFileSync(path.join(here,'composite-flipping-checks.lisp'),'utf8'):'')+
  `\n(values :passed (+ valtan-canvas-checks::*checks* mcclim-pixmap-checks::*checks* mcclim-text-checks::*checks*${bezier?' valtan-bezier-canvas-checks::*checks* valtan-bezier-canvas-checks::*region-checks*':''}${flipping?' mcclim-flipping-checks::*checks* mcclim-composite-flipping-checks::*checks*':''}))\n`;
const expectedChecks=86+53+60+(flipping?12+54:0)+(bezier?85:0);
const startupTimeout=resumable?3*60*60*1000:60*60*1000;
const evaluationTimeout=threaded?30*60*1000:120000;
const snapshot=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-canvas-checks-'));
const fixtureFile=path.join(snapshot,'checks.lisp');fs.writeFileSync(fixtureFile,fixture);
const report={status:'running',complete:false,startedAt:new Date().toISOString(),
  scope:`Real upstream CLIM pattern, clipping, composite-design, pixmap-copy and multiline-text assertions${bezier?', plus Bezier curves, areas, unions, differences and output replay':''}${flipping?', plus solid flipping designs':''} in Chrome; not full McCLIM compatibility`,
  expectedChecks,startupTimeout,evaluationTimeout,
  threadedEntryChecks:threaded?5:0,
  compilationMode: threaded ? 'dual functions; real application and I/O threads, checks interrupt the scheduled application' : resumable ? 'dual ordinary/resumable functions; drawing checks use ordinary calls' : 'ordinary functions',
  checksSha256:hash(fixture)};
const save=()=>fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');save();
const workbenchArgs=[path.join(here,'workbench.mjs'),'--startup-timeout',String(startupTimeout)];
if(bezier || resumable){
  const source=path.resolve(here,'../../example/try-using-mcclim');
  for(const file of fs.readdirSync(source))
    if(/\.(system|lisp|html)$/.test(file))fs.copyFileSync(path.join(source,file),path.join(snapshot,file));
  fs.writeFileSync(path.join(snapshot,'app.system'),`(defsystem "app"
    :depends-on ("mcclim-load-trace" "${bezier ? 'mcclim-browser/bezier' : 'mcclim-browser'}")
    :components ((:file "main")) :target :browser
    :target-features (:clim-without-swank :use-salza2-zlib${resumable ? ' :valtan-resumable' : ''}${threaded ? ' :clim-mp' : ''}) :compile-mode :runtime)`);
  workbenchArgs.push('--source-directory',snapshot);
}
const child=spawn(process.execPath,workbenchArgs,{stdio:['pipe','pipe','inherit']});
const exit=new Promise(resolve=>child.on('close',(code,signal)=>resolve({code,signal})));
const timer=setTimeout(()=>child.kill('SIGKILL'),startupTimeout+evaluationTimeout+5*60*1000);
let result;
try {
  for await(const line of readline.createInterface({input:child.stdout})){
    const message=JSON.parse(line);
    if(message.work){report.work=message.work;save();}
    if(message.loading)process.stdout.write(message.loading+'\n');
    if(message.ready)child.stdin.write(JSON.stringify({id:'canvas',op:'eval',file:fixtureFile,timeout:evaluationTimeout})+'\n');
    if(message.id==='canvas'){
      result=message;
      child.stdin.end('{"op":"close"}\n');
    }
    if(message.error && message.id!=='canvas')throw Error(message.error);
  }
  const terminal=await exit;
  if(terminal.code!==0)throw Error(`Workbench exited: ${JSON.stringify(terminal)}`);
  if(!result?.result?.ok)throw Error(result?.error??result?.result?.error??'Missing canvas result');
  if(result.result.value!==`(:PASSED ${expectedChecks})`)throw Error(`Unexpected assertion count: ${result.result.value}`);
  const workbench=JSON.parse(fs.readFileSync(path.join(report.work,'report.json'),'utf8'));
  report.workbench=workbench;report.result=result.result;
  report.complete=true;report.status='passed';
} catch(error){
  report.status='failed';report.error=String(error);process.exitCode=1;
  child.kill('SIGTERM');
} finally {
  clearTimeout(timer);
  if(result?.result)report.result=result.result;
  if(report.work && fs.existsSync(path.join(report.work,'report.json')))
    report.workbench=JSON.parse(fs.readFileSync(path.join(report.work,'report.json'),'utf8'));
  report.finishedAt=new Date().toISOString();save();
}
console.log(`McCLIM Canvas: ${report.status}; report: ${output}`);
