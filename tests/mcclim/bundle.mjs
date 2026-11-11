import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawn} from 'node:child_process';
import {createHash} from 'node:crypto';
import {build} from 'esbuild';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const example = path.join(root, 'example/try-using-mcclim');
export const hash = bytes => createHash('sha256').update(bytes).digest('hex');
export const compilerHash = () => hash(fs.readFileSync(path.join(root, 'valtan')));
export async function buildDemo(work, {sourceDirectory = example, workerFile} = {}) {
  fs.mkdirSync(work, {recursive:true});
  // Keep both compilation products and served browser assets isolated, so a
  // later edit or another build cannot silently change an in-flight test.
  for (const file of fs.readdirSync(sourceDirectory))
    if (/\.(system|lisp|html)$/.test(file)) fs.copyFileSync(path.join(sourceDirectory,file),path.join(work,file));
  const assets = path.join(work, 'browser'); fs.mkdirSync(assets, {recursive:true});
  for (const file of ['client.mjs','host.mjs','worker.mjs','event-queue.mjs','windows.mjs','pointer.mjs','keyboard.mjs'])
    fs.copyFileSync(path.join(root,'library/mcclim-browser',file),path.join(assets,file));
  if (workerFile) fs.copyFileSync(workerFile, path.join(assets, 'worker.mjs'));
  await new Promise((resolve,reject) => {
    const log=fs.createWriteStream(path.join(work,'build.log'));
    const child=spawn(path.join(root,'valtan'),['build','app.system'],{cwd:work,stdio:['ignore','pipe','pipe']});
    let diagnostic=''; const timer=setTimeout(()=>child.kill('SIGKILL'),180000);
    for(const stream of [child.stdout,child.stderr]) stream.on('data',bytes=>{log.write(bytes);diagnostic+=bytes;});
    child.on('error',error=>{clearTimeout(timer);log.end();reject(error);});
    child.on('close',code=>{
      clearTimeout(timer);log.end();
      if(code || /debugger invoked|caught ERROR|Valtan error:/.test(diagnostic)) reject(new Error(diagnostic.slice(-12000)));
      else resolve();
    });
  });
  await build({entryPoints:[path.join(work,'.valtan-cache/app.js')],outfile:path.join(work,'app.mjs'),
    bundle:true,platform:'browser',format:'esm',logLevel:'silent',alias:{lisp:path.join(root,'kernel/lisp.js')}});
  return {bundleSha256:hash(fs.readFileSync(path.join(work,'app.mjs'))),
    browserAssetsSha256:hash(['index.html',...fs.readdirSync(assets).sort().map(file=>'browser/'+file)]
      .map(file=>file+'\0'+fs.readFileSync(path.join(work,file),'utf8')).join('\0'))};
}
