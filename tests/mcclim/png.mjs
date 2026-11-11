// Real McCLIM file APIs, embedded ASDF resources and Canvas pixel comparison.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import http from 'node:http';
import readline from 'node:readline';
import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {pngFixtures} from './png-fixtures.mjs';
import {launchChrome} from './chrome.mjs';
import {hash} from './bundle.mjs';
const here=path.dirname(fileURLToPath(import.meta.url)),root=path.resolve(here,'../..');
const output=path.resolve(process.argv[2]??'/tmp/valtan-mcclim-png.json');
const dir=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-png-checks-')),assets=path.join(dir,'images');
fs.mkdirSync(assets);
const fixtures=pngFixtures();
const checksSource=fs.readFileSync(path.join(here,'png-checks.lisp'),'utf8');
for(const fixture of fixtures)fs.writeFileSync(path.join(assets,fixture.name),fixture.bytes);
const bad=Buffer.from(fixtures[0].bytes);bad[32]^=1;
fs.writeFileSync(path.join(assets,'bad-crc.png'),bad);
fs.writeFileSync(path.join(assets,'truncated.png'),fixtures[0].bytes.subarray(0,12));
const report={status:'running',complete:false,startedAt:new Date().toISOString(),
  scope:'30 PNG variants decoded through real McCLIM file APIs and drawn in Chrome; not other bitmap formats or full PNG conformance',
  fixtures:fixtures.map(({name,bytes,width,height})=>({name,width,height,sha256:hash(bytes)})),
  sourceHashes:Object.fromEntries(['png-fixtures.mjs','png-checks.lisp','png.mjs'].map(f=>[f,hash(fs.readFileSync(path.join(here,f)))]))};
const save=()=>fs.writeFileSync(output,JSON.stringify(report,null,2)+'\n');save();
// Cross-check the independent encoder and expected colors using Chrome's native
// image decoder before using these bytes as the Lisp decoder's test oracle.
let chrome,server,child;
try {
  server=http.createServer((request,response)=>{response.setHeader('Content-Type','text/html');response.end('<title>PNG fixture reference</title>');});
  await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
  chrome=await launchChrome();const page=await chrome.page(`http://127.0.0.1:${server.address().port}/`);
  for(const fixture of fixtures){
    const pixels=await page.evaluate(`(async()=>{
      const bytes=Uint8Array.from(atob(${JSON.stringify(fixture.bytes.toString('base64'))}),c=>c.charCodeAt(0));
      const bitmap=await createImageBitmap(new Blob([bytes],{type:'image/png'}));
      const canvas=new OffscreenCanvas(bitmap.width,bitmap.height),ctx=canvas.getContext('2d');
      ctx.fillStyle='white';ctx.fillRect(0,0,canvas.width,canvas.height);ctx.drawImage(bitmap,0,0);
      const result={width:canvas.width,height:canvas.height,pixels:Array.from(ctx.getImageData(0,0,canvas.width,canvas.height).data)};
      bitmap.close();return result;})()`);
    assert.equal(pixels.width,fixture.width);assert.equal(pixels.height,fixture.height);
    fixture.rgba.forEach(([r,g,b,a],i)=>{
      [r,g,b].forEach((value,c)=>assert.ok(Math.abs(pixels.pixels[4*i+c]-Math.round((value*a+255*(255-a))/255))<=1,`${fixture.name} pixel ${i} channel ${c}`));
      assert.equal(pixels.pixels[4*i+3],255);
    });
  }
  report.nativeReference={status:'passed',images:fixtures.length};save();
  await chrome.close();chrome=null;await new Promise(resolve=>server.close(resolve));server=null;
  for(const file of fs.readdirSync(path.join(root,'example/try-using-mcclim')))
    if(/\.(system|lisp|html)$/.test(file))fs.copyFileSync(path.join(root,'example/try-using-mcclim',file),path.join(dir,file));
  const quoted=value=>'"'+value.replaceAll('\\','\\\\').replaceAll('"','\\"')+'"';
  fs.writeFileSync(path.join(dir,'mcclim-png-test-data.asd'),`(asdf:defsystem "mcclim-png-test-data" :pathname ${quoted(assets+'/')}
    :components (${fs.readdirSync(assets).map(name=>`(:static-file ${quoted(name)})`).join(' ')}))`);
  fs.writeFileSync(path.join(dir,'app.system'),`(defsystem "app"
    :depends-on ("mcclim-load-trace" "mcclim-browser" "mcclim-png-test-data")
    :components ((:file "main")) :target :browser :target-features (:clim-without-swank) :compile-mode :runtime)`);
  const entries=fixtures.map(f=>`(${quoted(path.join(assets,f.name))} ${f.width} ${f.height} (${f.rgba.map(([r,g,b,a])=>(a*2**24+r*2**16+g*256+b)).join(' ')}))`);
  const checks=checksSource+
    `\n(mcclim-png-checks::run '(${entries.join('\n')}) ${quoted(path.join(assets,'bad-crc.png'))} ${quoted(path.join(assets,'truncated.png'))})\n`;
  const checkFile=path.join(dir,'checks.lisp');fs.writeFileSync(checkFile,checks);report.checksSha256=hash(checks);save();
  child=spawn(process.execPath,[path.join(here,'workbench.mjs'),'--source-directory',dir],{stdio:['pipe','pipe','inherit']});
  const terminal=new Promise(resolve=>child.on('close',(code,signal)=>resolve({code,signal})));
  const timer=setTimeout(()=>child.kill('SIGKILL'),70*60*1000);let result;
  try {
    for await(const line of readline.createInterface({input:child.stdout})){
      const message=JSON.parse(line);
      if(message.work){report.work=message.work;save();}
      if(message.loading)console.log(message.loading);
      if(message.ready){
        // The browser never serves image files. Remove the originals too so
        // only ASDF-embedded bytes can satisfy ordinary CL file reads.
        fs.rmSync(assets,{recursive:true});report.originalAssetsRemoved=true;save();
        child.stdin.write(JSON.stringify({op:'eval',id:'png',file:checkFile,timeout:180000})+'\n');
      }
      if(message.id==='png'){result=message;child.stdin.end('{"op":"close"}\n');}
      if(message.error && message.id!=='png')throw Error(message.error);
    }
    const ended=await terminal;assert.equal(ended.code,0,JSON.stringify(ended));
    assert.equal(result?.result?.ok,true,result?.error??result?.result?.error);
    assert.equal(result.result.value,'(:PASSED 92)');report.result=result.result;
    report.complete=true;report.status='passed';
  } finally {clearTimeout(timer);}
} catch(error){report.status='failed';report.error=String(error);process.exitCode=1;child?.kill('SIGTERM');}
finally {
  await chrome?.close();if(server)await new Promise(resolve=>server.close(resolve));
  if(report.work && fs.existsSync(path.join(report.work,'report.json')))
    report.workbench=JSON.parse(fs.readFileSync(path.join(report.work,'report.json')));
  report.finishedAt=new Date().toISOString();save();
}
console.log(`McCLIM PNG: ${report.status}; report: ${output}`);
