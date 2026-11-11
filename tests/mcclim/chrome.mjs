import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {spawn} from 'node:child_process';

// Small CDP driver shared by the bridge test and the actual McCLIM GUI test.
export async function launchChrome() {
  const profile = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-chrome-'));
  const child = spawn(process.env.CHROME_BINARY ?? '/usr/bin/google-chrome',
    ['--headless=new', '--no-sandbox', '--disable-dev-shm-usage', '--remote-debugging-port=0',
     `--user-data-dir=${profile}`, '--no-first-run', '--no-default-browser-check', 'about:blank'],
    {stdio: ['ignore', 'ignore', 'pipe']});
  let diagnostic = '';
  const exited = new Promise(resolve => child.once('exit', resolve));
  const endpoint = await new Promise((resolve, reject) => {
    const timer = setTimeout(() => {child.kill(); reject(new Error(`Chrome startup timeout: ${diagnostic}`));}, 15000);
    child.on('error', error => {clearTimeout(timer); reject(error);});
    child.stderr.on('data', bytes => {
      diagnostic += bytes;
      const match = /DevTools listening on (ws:\/\/[^\s]+)/.exec(diagnostic);
      if (match) { clearTimeout(timer); resolve(match[1]); }
    });
    child.on('exit', code => {clearTimeout(timer); reject(new Error(`Chrome exited ${code}: ${diagnostic}`));});
  });
  const socket = new WebSocket(endpoint);
  await new Promise((resolve, reject) => {socket.onopen = resolve; socket.onerror = reject;});
  const pending = new Map(); let serial = 0;
  socket.onmessage = ({data}) => {
    const message = JSON.parse(data);
    if (!message.id) return;
    const call = pending.get(message.id); if (!call) return;
    pending.delete(message.id); clearTimeout(call.timer);
    if (message.error) call.reject(new Error(JSON.stringify(message.error)));
    else call.resolve(message.result);
  };
  socket.onclose = () => {
    for (const call of pending.values()) {clearTimeout(call.timer); call.reject(new Error('Chrome connection closed'));}
    pending.clear();
  };
  const send = (method, params = {}, sessionId) => new Promise((resolve, reject) => {
    if (socket.readyState !== WebSocket.OPEN) {reject(new Error('Chrome connection closed')); return;}
    const id = ++serial;
    const timer = setTimeout(() => {pending.delete(id); reject(new Error(`CDP timeout: ${method}`));}, 30000);
    pending.set(id, {resolve, reject, timer});
    socket.send(JSON.stringify({id, method, params, sessionId}));
  });
  return {
    async page(url) {
      const {targetId} = await send('Target.createTarget', {url: 'about:blank'});
      const {sessionId} = await send('Target.attachToTarget', {targetId, flatten: true});
      await send('Page.enable', {}, sessionId);
      const navigation = await send('Page.navigate', {url}, sessionId);
      if (navigation.errorText) throw new Error(`Navigation failed: ${navigation.errorText}`);
      return {
        send: (method, params) => send(method, params, sessionId),
        async evaluate(expression) {
          const result = await send('Runtime.evaluate', {expression, awaitPromise: true, returnByValue: true}, sessionId);
          if (result.exceptionDetails) throw new Error(JSON.stringify(result.exceptionDetails));
          return result.result.value;
        },
        async screenshot(filename) {
          const {data} = await send('Page.captureScreenshot', {format: 'png'}, sessionId);
          fs.writeFileSync(filename, Buffer.from(data, 'base64'));
        },
      };
    },
    async close() {
      try { await send('Browser.close'); } catch {}
      socket.close(); child.kill();
      await exited;
      for (const call of pending.values()) {clearTimeout(call.timer); call.reject(new Error('Chrome closed'));}
      fs.rmSync(profile, {recursive: true, force: true});
    },
  };
}
