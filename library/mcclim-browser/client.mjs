import {EventQueue} from './event-queue.mjs';
import {BrowserWindows} from './windows.mjs';
import {BrowserPointerGrab} from './pointer.mjs';
import {attachTextInput} from './keyboard.mjs';

export function mountMcclim(container, workerURL, applicationURL) {
  if (!globalThis.crossOriginIsolated || typeof SharedArrayBuffer === 'undefined')
    throw new Error('McCLIM needs COOP: same-origin and COEP: require-corp response headers');
  const queue = new EventQueue(EventQueue.allocate());
  const worker = new Worker(workerURL, {type: 'module'});
  const manager = new BrowserWindows(container, input);
  const windows = manager.windows;
  const pointerGrab = new BrowserPointerGrab(windows, input);
  const pending = [];
  let flushTimer;
  let disposed = false;
  function flush() {
    flushTimer = undefined;
    while (pending.length && queue.push(pending[0])) pending.shift();
    if (pending.length && !disposed) flushTimer = setTimeout(flush, 8);
  }
  function input(event) {
    if (disposed) return;
    if (pending.length || !queue.push(event)) {
      pending.push(event);
      if (flushTimer === undefined) flushTimer = setTimeout(flush, 0);
    }
  }
  const modifiers = event => (event.shiftKey ? 1 : 0) | (event.ctrlKey ? 2 : 0) |
    (event.altKey ? 4 : 0) | (event.metaKey ? 8 : 0);
  const announce = (name, detail) => container.dispatchEvent(new CustomEvent(name, {detail}));
  function createWindow(message) {
    const state = manager.create(message), {canvas} = state;
    attachTextInput(state, input);
    for (const type of ['pointerdown', 'pointerup', 'pointermove', 'pointerenter', 'pointerleave']) {
      canvas.addEventListener(type, event => {
        if (pointerGrab.active) return;
        if (type === 'pointerdown') { manager.raise(state); canvas.focus(); canvas.setPointerCapture(event.pointerId); }
        const rect = canvas.getBoundingClientRect();
        input({type, window: message.id, x: (event.clientX - rect.left) * canvas.width / rect.width,
          y: (event.clientY - rect.top) * canvas.height / rect.height,
          button: event.button, buttons: event.buttons, modifiers: modifiers(event), time: event.timeStamp});
        event.preventDefault();
      });
    }
    canvas.addEventListener('wheel', event => {
      const rect = canvas.getBoundingClientRect();
      input({type: 'wheel', window: message.id, x: (event.clientX - rect.left) * canvas.width / rect.width,
        y: (event.clientY - rect.top) * canvas.height / rect.height,
        dx: event.deltaX, dy: event.deltaY, modifiers: modifiers(event)});
      event.preventDefault();
    }, {passive: false});
    canvas.addEventListener('contextmenu', event => event.preventDefault());
    announce('mcclim-window', {id: message.id, canvas});
  }
  worker.addEventListener('message', ({data: message}) => {
    const state = windows.get(message.id);
    switch (message.type) {
      case 'create-window': createWindow(message); break;
      case 'paint':
        if (state) {
          state.context.transferFromImageBitmap(message.bitmap);
          announce('mcclim-paint', {id: message.id});
        } else message.bitmap.close();
        break;
      case 'resize-window': if (state) manager.resize(state, message.width, message.height); break;
      case 'move-window': if (state) manager.move(state, message.x, message.y); break;
      case 'destroy-window': if (state) manager.destroy(state); break;
      case 'window-title': if (state) manager.rename(state, message.title); break;
      case 'window-visible': if (state) manager.visible(state, message.visible); break;
      case 'window-raise': if (state) manager.raise(state); break;
      case 'window-bury': if (state) manager.bury(state); break;
      case 'window-minimize': if (state) manager.minimize(state); break;
      case 'pointer-grab': pointerGrab.grab(message.id, message.multipleWindow); break;
      case 'pointer-ungrab': pointerGrab.release(); break;
      case 'cursor': if (state) state.canvas.style.cursor = message.cursor; break;
      case 'ready': announce('mcclim-ready', {}); break;
      case 'status': announce('mcclim-status', message.text); break;
      case 'log': announce('mcclim-log', message.message); break;
      case 'error': announce('mcclim-error', message.message); break;
    }
  });
  worker.addEventListener('error', event => announce('mcclim-error', event.message));
  let viewport = manager.viewportSize();
  const updateViewport = () => {
    const next = manager.viewportSize();
    if (next.width === viewport.width && next.height === viewport.height) return;
    viewport = next; input({type:'viewport-resize', ...viewport});
  };
  const observer = new ResizeObserver(updateViewport);
  observer.observe(container);
  addEventListener('resize', updateViewport);
  addEventListener('scroll', updateViewport, {passive:true});
  worker.postMessage({type: 'initialize', buffer: queue.buffer, applicationURL, ...viewport});
  return {worker, windows, input, dispose() {
    disposed = true; clearTimeout(flushTimer); queue.close(); worker.terminate();
    pointerGrab.dispose();
    observer.disconnect(); removeEventListener('resize', updateViewport); removeEventListener('scroll', updateViewport);
    manager.dispose();
  }};
}
