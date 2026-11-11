import {createCanvasHost} from './host.mjs';
let initialized = false;
self.addEventListener('message', async ({data}) => {
  if (data.type !== 'initialize' || initialized) return;
  initialized = true;
  const host = createCanvasHost(data.buffer);
  host.width = data.width; host.height = data.height;
  globalThis.valtanMcclim = host;
  const log = console.log.bind(console);
  console.log = (...args) => {
    log(...args);
    const message = args.map(String).join(' ');
    postMessage({type: 'log', message});
  };
  try { await import(data.applicationURL); }
  catch (error) { host.error(error); }
});
