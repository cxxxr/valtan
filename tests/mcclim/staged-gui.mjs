// Build and exercise the real McCLIM application in a browser worker.
import fs from 'node:fs';
import path from 'node:path';
import assert from 'node:assert/strict';
import {launchChrome} from './chrome.mjs';
import {hash} from './bundle.mjs';
import {createDemoServer} from './server.mjs';
const [workArg]=process.argv.slice(2);
assert.ok(workArg,'Usage: staged-gui.mjs stage-work');
const stageWork=path.resolve(workArg);
const work=path.join(stageWork,'gui'),threaded=true;
const output=path.join(stageWork,'gui-result.json');
const buildReport=JSON.parse(fs.readFileSync(path.join(stageWork,'gui-build.json')));
assert.equal(buildReport.mode,'gui');assert.equal(buildReport.librariesUseCompiledArtifacts,true);
assert.equal(buildReport.producerManifestSha256,hash(fs.readFileSync(path.join(stageWork,'manifest.json'))));
for(const [file,expected] of Object.entries(buildReport.assets))
  assert.equal(hash(fs.readFileSync(path.join(work,file))),expected,`Changed browser asset: ${file}`);
const report = {status: 'running', complete: false, startedAt: new Date().toISOString(), work,
  scope: 'Fresh Chrome with target-compiled McCLIM libraries, source fallback disabled for libraries, runtime-loaded application entry; real frame, drawing, gadget and keyboard input',
  compilationMode: threaded ? 'dual functions; application and I/O threads; production worker' : 'ordinary functions',
  producerManifest: path.join(stageWork,'manifest.json'),build:buildReport};
const save = () => fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
const server = createDemoServer(work);
let chrome;
save();
try {
  assert.equal(buildReport.bundleSha256,hash(fs.readFileSync(path.join(work,'app.mjs'))));
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  chrome = await launchChrome();
  const page = await chrome.page(`http://127.0.0.1:${server.address().port}/`);
  report.stage = 'load-and-render'; save();
  let lastLog = '';
  async function waitFor(predicate, milliseconds) {
    const deadline = Date.now() + milliseconds;
    do {
      const state = await page.evaluate(`globalThis.mcclimTest && ({ready: mcclimTest.ready, paints: mcclimTest.paints, status: mcclimTest.status, lastLog: mcclimTest.lastLog, errors: mcclimTest.errors})`);
      if (state?.lastLog && state.lastLog !== lastLog) {
        lastLog = state.lastLog; console.log(lastLog);
        fs.appendFileSync(path.join(work, 'browser.log'), lastLog + '\n');
      }
      if (state?.errors.length) throw new Error(state.errors.join('\n'));
      if (state && predicate(state)) return state;
      await new Promise(resolve => setTimeout(resolve, 1000));
    } while (Date.now() < deadline);
    throw new Error(`Browser condition timed out; last source: ${lastLog}`);
  }
  await waitFor(state => state.ready && state.paints > 0, (threaded ? 3 : 1) * 60 * 60 * 1000);
  report.readyAt = new Date().toISOString(); save();
  const canvas = await page.evaluate(`(() => {const c = document.querySelector('canvas'), r = c.getBoundingClientRect(); return {x: r.left, y: r.top, width: c.width, height: c.height};})()`);
  assert.ok(canvas.width >= 500 && canvas.height >= 200, JSON.stringify(canvas));
  async function pixels() {
    return page.evaluate(`(() => {const source = document.querySelector('canvas'), copy = document.createElement('canvas'); copy.width=source.width; copy.height=source.height; const context=copy.getContext('2d'); context.drawImage(source,0,0); const pixels=context.getImageData(0,0,copy.width,copy.height).data; let opaque=0, nonWhite=0; for(let i=0;i<pixels.length;i+=4) {if(pixels[i+3]) opaque++; if(pixels[i+3] && (pixels[i]<240 || pixels[i+1]<240 || pixels[i+2]<240)) nonWhite++;} return {opaque,nonWhite,data:source.toDataURL()};})()`);
  }
  const first = await pixels();
  assert.ok(first.opaque > 50000 && first.nonWhite > 2000, 'Expected real drawing and text on the frame');
  await page.screenshot(path.join(work, 'initial.png'));
  report.stage = 'input'; save();
  async function click(x,y) {
    await page.send('Input.dispatchMouseEvent', {type:'mouseMoved',x,y});
    await page.send('Input.dispatchMouseEvent', {type:'mousePressed',x,y,button:'left',clickCount:1});
    await page.send('Input.dispatchMouseEvent', {type:'mouseReleased',x,y,button:'left',clickCount:1});
  }
  await click(canvas.x + canvas.width / 2, canvas.y + canvas.height - 20);
  await waitFor(state => state.status === 'Counter: 1', threaded ? 120000 : 30000);
  await click(canvas.x + 200, canvas.y + 100);
  await page.send('Input.dispatchKeyEvent', {type:'keyDown',key:'+',code:'Equal',text:'+',modifiers:8,windowsVirtualKeyCode:187});
  await page.send('Input.dispatchKeyEvent', {type:'keyUp',key:'+',code:'Equal',modifiers:8,windowsVirtualKeyCode:187});
  await waitFor(state => state.status === 'Counter: 2', threaded ? 120000 : 30000);
  const after = await pixels(); assert.notEqual(first.data, after.data, 'Input must change rendered pixels');
  await page.screenshot(path.join(work, 'after-input.png'));
  report.complete = true; report.status = 'passed'; report.canvas = canvas;
  report.initialPixels = {opaque:first.opaque,nonWhite:first.nonWhite};
  report.input = ['push-button -> Counter: 1', 'keyboard + -> Counter: 2'];
} catch (error) {report.status = 'failed'; report.error = String(error); process.exitCode = 1;}
finally {
  await chrome?.close();
  if (server.listening) {server.closeAllConnections(); await new Promise(resolve => server.close(resolve));}
  report.finishedAt = new Date().toISOString(); save();
}
console.log(`McCLIM GUI: ${report.status}; report: ${output}`);
