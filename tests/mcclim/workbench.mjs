// Keep real McCLIM loaded while running additional drawing and protocol tests.
// Commands and results are JSON lines on stdin/stdout; the server is local only.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import readline from 'node:readline';
import {fileURLToPath} from 'node:url';
import {buildDemo,compilerHash,hash} from './bundle.mjs';
import {createDemoServer} from './server.mjs';
import {launchChrome} from './chrome.mjs';
const here=path.dirname(fileURLToPath(import.meta.url));
const root=path.resolve(here,'../..');
const work=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-mcclim-workbench-'));
const sourceDirectory=path.join(work,'sources'); fs.mkdirSync(sourceDirectory);
for(const name of fs.readdirSync(path.join(root,'example/try-using-mcclim')))
  if(/\.(system|lisp|html)$/.test(name)) fs.copyFileSync(path.join(root,'example/try-using-mcclim',name),path.join(sourceDirectory,name));
fs.writeFileSync(path.join(sourceDirectory,'main.lisp'),fs.readFileSync(path.join(here,'workbench-evaluator.lisp'),'utf8')+'\n'+fs.readFileSync(path.join(sourceDirectory,'main.lisp'),'utf8'));
const report={work,status:'building',complete:false,startedAt:new Date().toISOString(),compilerSha256:compilerHash()};
const save=()=>fs.writeFileSync(path.join(work,'report.json'),JSON.stringify(report,null,2)+'\n');
const say=value=>process.stdout.write(JSON.stringify(value)+'\n');
say({work}); save();
let chrome, server;
try {
  Object.assign(report,await buildDemo(work,{sourceDirectory,workerFile:path.join(here,'workbench-worker.mjs')}));
  server=createDemoServer(work);
  await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
  chrome=await launchChrome();
  const page=await chrome.page(`http://127.0.0.1:${server.address().port}/`);
  report.status='loading'; save();
  let lastLog='';
  async function state(){return page.evaluate('globalThis.mcclimTest && ({ready:mcclimTest.ready,paints:mcclimTest.paints,status:mcclimTest.status,lastLog:mcclimTest.lastLog,errors:mcclimTest.errors})');}
  const deadline=Date.now()+60*60*1000;
  for(;;){
    const current=await state();
    if(current?.lastLog && current.lastLog!==lastLog){lastLog=current.lastLog;say({loading:lastLog});}
    if(current?.errors.length) throw Error(current.errors.join('\n'));
    if(current?.ready && current.paints) break;
    if(Date.now()>deadline) throw Error('McCLIM startup timed out');
    await new Promise(resolve=>setTimeout(resolve,1000));
  }
  await page.evaluate(`globalThis.workbenchResults={};mcclimTest.application.worker.addEventListener('message',({data})=>{if(data.type==='test-result')workbenchResults[data.id]=data;});`);
  report.status='ready';report.readyAt=new Date().toISOString();save();say({ready:true,work});
  let serial=0;
  for await(const line of readline.createInterface({input:process.stdin,crlfDelay:Infinity})){
    let command, record;
    try{
      command=JSON.parse(line);let result;
      if(command.op==='close') break;
      if(command.op==='status') result=await state();
      else if(command.op==='eval'){
        const id=++serial;
        const source=command.file?fs.readFileSync(command.file,'utf8'):command.source;
        if(typeof source!=='string')throw Error('eval needs source or file');
        record={id,commandId:command.id,startedAt:new Date().toISOString(),
          sourceSha256:hash(source),file:command.file};
        fs.writeFileSync(path.join(work,`evaluation-${id}.lisp`),source);
        (report.evaluations??=[]).push(record);save();
        // At most 128 Unicode code points per queue entry, below its UTF-8 cap.
        const characters=Array.from(source);const events=[];
        for(let i=0;i<characters.length;i+=128) events.push({type:'test-source',id,text:characters.slice(i,i+128).join('')});
        events.push({type:'test-evaluate',id});
        await page.evaluate(`for(const event of ${JSON.stringify(events)})mcclimTest.application.input(event);`);
        if(command.wait===false){
          say({id:command.id,result:{evaluationId:id}});
          continue;
        }
        const until=Date.now()+(command.timeout??120000);
        do{
          result=await page.evaluate(`workbenchResults[${id}]`);
          if(result)break;
          if(Date.now()>until)throw Error('Evaluation timed out (the worker is retained for diagnosis)');
          await new Promise(resolve=>setTimeout(resolve,100));
        }while(true);
        record.result=result;record.finishedAt=new Date().toISOString();save();
      }else if(command.op==='evaluation-result'){
        result=await page.evaluate(`workbenchResults[${Number(command.evaluationId)}]`);
        if(result){
          record=report.evaluations.find(r=>r.id===command.evaluationId);
          if(record){record.result=result;record.finishedAt=new Date().toISOString();save();}
        }
      }else if(command.op==='cdp')result=await page.send(command.method,command.params??{});
      else if(command.op==='page')result=await page.evaluate(command.expression);
      else if(command.op==='screenshot'){await page.screenshot(command.file??path.join(work,'screen.png'));result={file:command.file??path.join(work,'screen.png')};}
      else throw Error('Unknown operation');
      say({id:command.id,result});
    }catch(error){
      if(record){record.error=String(error);record.finishedAt=new Date().toISOString();save();}
      say({id:command?.id,error:String(error)});
    }
  }
  report.status='closed';
}catch(error){report.status='failed';report.error=String(error);say({error:String(error)});process.exitCode=1;}
finally{
  await chrome?.close();
  if(server?.listening){server.closeAllConnections();await new Promise(resolve=>server.close(resolve));}
  report.finishedAt=new Date().toISOString();save();
}
