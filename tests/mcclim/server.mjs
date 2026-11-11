import fs from 'node:fs';
import path from 'node:path';
import http from 'node:http';
export function createDemoServer(work) {
  return http.createServer((request,response)=>{
    response.setHeader('Cross-Origin-Opener-Policy','same-origin');
    response.setHeader('Cross-Origin-Embedder-Policy','require-corp');
    const url=new URL(request.url,'http://localhost').pathname;
    const name=url==='/'?'index.html':url==='/app.mjs'?'app.mjs':
      /^\/library\/mcclim-browser\/[a-z-]+\.mjs$/.test(url)?'browser/'+path.basename(url):null;
    const file=name && path.join(work,name);
    if(!file || !fs.existsSync(file)){response.statusCode=404;response.end();return;}
    response.setHeader('Content-Type',name.endsWith('.html')?'text/html; charset=utf-8':'text/javascript; charset=utf-8');
    fs.createReadStream(file).pipe(response);
  });
}
