import fs from 'node:fs';
import {fileURLToPath} from 'node:url';
import path from 'node:path';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {build} from 'esbuild';
import {readForms} from './staged-artifact.mjs';
const args=process.argv.slice(2);
function option(name){
 const index=args.indexOf(name);
 if(index<0)return null;
 assert.ok(args[index+1] && !args[index+1].startsWith('--'),`Missing value for ${name}`);
 const value=args[index+1];args.splice(index,2);return path.resolve(value);
}
const applicationArg=option('--application'),destinationArg=option('--output-directory');
const [workArg,mode='api']=args;
assert.ok(workArg && args.length<=2, 'Usage: staged-build.mjs work [api|gui] [--application source.lisp] [--output-directory directory]');
const work=path.resolve(workArg), cache=path.join(work,'.valtan-cache');
assert.ok(['api','gui'].includes(mode));
assert.ok(!applicationArg || mode==='gui','A custom application requires GUI mode');
const destination=destinationArg??work;
fs.mkdirSync(destination,{recursive:true});
const root=fileURLToPath(new URL('../..', import.meta.url));
const sha=p=>createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const manifest=JSON.parse(fs.readFileSync(path.join(work,'manifest.json')));
assert.equal(manifest.status,'produced'); assert.equal(manifest.complete,true);
assert.equal(new Set(manifest.records.map(r=>r.source)).size,manifest.records.length);
for(const record of manifest.records){
 assert.equal(sha(record.artifact),record.artifactSha256,`Artifact changed: ${record.artifact}`);
 assert.equal(sha(record.source),record.sourceSha256,`Source changed since compilation: ${record.source}`);
}
const walk=d=>fs.readdirSync(d,{withFileTypes:true}).flatMap(e=>e.isDirectory()?walk(path.join(d,e.name)):[path.join(d,e.name)]);
const files=walk(cache);
const embeddedSources=files.filter(file=>file.endsWith('.js')).flatMap(file=>{
 const code=fs.readFileSync(file,'utf8');
 return code.includes("lisp.intern('LOAD-EMBEDDED-SOURCE', 'SYSTEM')") &&
  /TMP_2=G_1\.func\(lisp\.codeArrayToString\(\[/.test(code)
  ? [file.slice(cache.length,-3).replaceAll('\\','')] : [];
});
assert.deepEqual(manifest.records.map(record=>record.source).sort(),
 embeddedSources.filter(source=>source!==path.join(work,'stage.lisp')).sort(),
 'The producer must compile every embedded component');
if(manifest.kernelSha256) assert.equal(sha(path.join(root,'kernel/lisp.js')),manifest.kernelSha256,
 'Kernel changed since production');
const moduleFor=source=>{const matches=files.filter(f=>f.replaceAll('\\','')===cache+source+'.js');assert.equal(matches.length,1,source);return matches[0]};
const helper=`(progn (defun system::load-embedded-compiled (function filename features)
 (dolist (feature features) (pushnew feature *features*))
 (let ((*package* (find-package :cl-user)) (*readtable* *readtable*)
       (*load-pathname* (pathname filename)) (*load-truename* (pathname filename))
       (system::*compiled-file-literals* (system:make-raw-array 0)))
  (progv system::*file-local-variables* (mapcar #'symbol-value system::*file-local-variables*)
   (funcall function))))
 (defun system::load-embedded-source (source filename features)
  (declare (ignore source features))
  (error "Unexpected source component ~A" filename)))`;
const replacements=new Map();
replacements.set(moduleFor(path.join(work,'stage.lisp')),`import * as lisp from 'lisp';
globalThis.stageReplayKernel=lisp;
globalThis.stageApplicationLoader=lisp.intern('LOAD-EMBEDDED-SOURCE','SYSTEM').getFunction();
lisp.intern('LOAD-EMBEDDED-APPLICATION','SYSTEM').func=globalThis.stageApplicationLoader;
const s=n=>lisp.intern(n,'COMMON-LISP');
s('EVAL').getFunction()(s('READ-FROM-STRING').getFunction()(s('SIMPLE-MAKE-STRING').getFunction()(${JSON.stringify(helper)})));`);
let formCount=0;
const applicationFile=mode==='gui'?(applicationArg??path.join(root,'example/try-using-mcclim/main.lisp')):null;
const applicationSource=applicationFile?fs.readFileSync(applicationFile,'utf8'):null;
let replacedApplication=false;
for(const record of manifest.records){
 if(mode==='gui' && record.source===path.join(work,'main.lisp')){
  assert.equal(replacedApplication,false);replacedApplication=true;
  const file=moduleFor(record.source),original=fs.readFileSync(file,'utf8');
  const sourceExpression=/TMP_2=G_1\.func\(lisp\.codeArrayToString\(\[([0-9, ]*)\]\)\);/;
  assert.ok(sourceExpression.test(original));
  replacements.set(file,original.replace("lisp.intern('LOAD-EMBEDDED-SOURCE', 'SYSTEM')", "lisp.intern('LOAD-EMBEDDED-APPLICATION','SYSTEM')")
   .replace(sourceExpression,()=>`TMP_2=G_1.func(lisp.codeArrayToString(${JSON.stringify(Array.from(applicationSource,c=>c.codePointAt(0)))}));`));
  continue;
 }
 const forms=readForms(fs.readFileSync(record.artifact,'utf8')); formCount+=forms.length;
 const body=forms.map(({code,call})=>`{const value=(()=>{\n${code}\n})();${call?"lisp.intern('FUNCALL','COMMON-LISP').getFunction()(value);":''}}`).join('\n');
 const file=moduleFor(record.source), original=fs.readFileSync(file,'utf8');
 const sourceExpression=/TMP_2=G_1\.func\(lisp\.codeArrayToString\(\[([0-9, ]*)\]\)\);/;
 assert.ok(sourceExpression.test(original),record.source);
 assert.ok(original.includes("lisp.intern('LOAD-EMBEDDED-SOURCE', 'SYSTEM')"),record.source);
 replacements.set(file,original.replace("lisp.intern('LOAD-EMBEDDED-SOURCE', 'SYSTEM')","lisp.intern('LOAD-EMBEDDED-COMPILED', 'SYSTEM')")
  .replace(sourceExpression,()=>`TMP_2=function(){\nconsole.log('TARGET-REPLAY '+${JSON.stringify(record.source)});\n${body}\nreturn lisp.S_t;};`));
}
assert.equal(replacedApplication,mode==='gui');
const output=path.join(destination,mode==='gui'?'gui/app.mjs':'direct.mjs');
await build({entryPoints:[path.join(cache,'app.js')],outfile:output,bundle:true,platform:'browser',format:'esm',logLevel:'silent',alias:{lisp:path.join(root,'kernel/lisp.js')},plugins:[{name:'compiled-component-replay',setup(build){build.onLoad({filter:/\.js$/},args=>replacements.has(args.path)?{contents:replacements.get(args.path),loader:'js',resolveDir:path.dirname(args.path)}:undefined)}}]});
const report={status:'built',mode,applicationSourceSha256:applicationSource?createHash('sha256').update(applicationSource).digest('hex'):null,kernelSha256:sha(path.join(root,'kernel/lisp.js')),producerManifestSha256:sha(path.join(work,'manifest.json')),librariesUseCompiledArtifacts:true,applicationUsesRuntimeSource:mode==='gui',components:manifest.records.length,forms:formCount,bytes:fs.statSync(output).size,bundleSha256:createHash('sha256').update(fs.readFileSync(output)).digest('hex'),producerManifest:path.join(work,'manifest.json'),cacheUnmodified:true};
report.applicationSource=applicationFile;
fs.writeFileSync(path.join(destination,mode==='gui'?'gui-build.json':'direct-build.json'),JSON.stringify(report,null,2)+'\n');console.log(JSON.stringify(report));

if(mode==='gui'){
 const gui=path.join(destination,'gui'),assets=path.join(gui,'browser');fs.mkdirSync(assets,{recursive:true});
 fs.copyFileSync(path.join(root,'example/try-using-mcclim/index.html'),path.join(gui,'index.html'));
 for(const file of ['client.mjs','host.mjs','worker.mjs','event-queue.mjs','windows.mjs','pointer.mjs','keyboard.mjs'])
  fs.copyFileSync(path.join(root,'library/mcclim-browser',file),path.join(assets,file));
 const files=['index.html',...fs.readdirSync(assets).sort().map(f=>'browser/'+f)];
 report.assets=Object.fromEntries(files.map(f=>[f,sha(path.join(gui,f))]));
 fs.writeFileSync(path.join(destination,'gui-build.json'),JSON.stringify(report,null,2)+'\n');
}
