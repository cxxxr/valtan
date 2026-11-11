// Test-only worker: the production worker has no source-evaluation transport.
import {createCanvasHost} from './host.mjs';
let initialized = false;
self.addEventListener('message', async ({data}) => {
  if (data.type !== 'initialize' || initialized) return;
  initialized = true;
  const host = createCanvasHost(data.buffer);
  const next = host.nextEvent;
  const sources = new Map();
  host.nextEvent = timeout => {
    for (;;) {
      const event = next(timeout);
      if (event?.type === 'test-source') {
        sources.set(event.id, (sources.get(event.id) ?? '') + event.text);
      } else if (event?.type === 'test-evaluate') {
        const source = sources.get(event.id); sources.delete(event.id);
        let result;
        try {result = globalThis.valtanMcclimEvaluate(source ?? '');}
        catch (error) {result = {ok:false, error:String(error?.stack ?? error)};}
        host.flush();
        postMessage({type:'test-result', id:event.id, ...result});
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
