// Test-only worker: the production worker has no source-evaluation transport.
import {createCanvasHost} from './host.mjs';
let initialized = false;
self.addEventListener('message', async ({data}) => {
  if (data.type !== 'initialize' || initialized) return;
  initialized = true;
  const host = createCanvasHost(data.buffer);
  const ready = host.ready;
  let applicationTask;
  host.ready = () => {
    applicationTask = globalThis.valtanMcclimKernel?.currentScheduledTask();
    ready();
  };
  const next = host.nextEvent;
  const sources = new Map();
  host.nextEvent = timeout => {
    for (;;) {
      const event = next(timeout);
      if (event?.type === 'test-source') {
        sources.set(event.id, (sources.get(event.id) ?? '') + event.text);
      } else if (event?.type === 'test-evaluate') {
        const source = sources.get(event.id); sources.delete(event.id);
        const sendResult = result => {
          host.flush();
          postMessage({type:'test-result', id:event.id, ...result});
        };
        const failure = error => ({ok:false, error:String(error?.stack ?? error)});
        const lisp = globalThis.valtanMcclimKernel;
        const task = applicationTask ?? lisp?.currentScheduledTask();
        if (task) {
          // Preserve the application's dynamic context, including its frame,
          // while allowing test EVAL, JOIN and cleanup to suspend.
          task.scheduler.interrupt(task, lisp.makeResumable(function*() {
            try {sendResult(yield* lisp.callResumable(globalThis.valtanMcclimEvaluate, source ?? ''));}
            catch (error) {sendResult(failure(error));}
          }));
        } else {
          try {sendResult(globalThis.valtanMcclimEvaluate(source ?? ''));}
          catch (error) {sendResult(failure(error));}
        }
      } else return event;
    }
  };
  host.width = data.width; host.height = data.height;
  globalThis.valtanMcclim = host;
  const log = console.log.bind(console);
  console.log = (...args) => {log(...args); postMessage({type:'log', message:args.map(String).join(' ')});};
  try {await import(data.applicationURL);}
  catch (error) {host.error(error);}
});
