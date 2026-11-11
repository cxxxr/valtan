// Exercise the upstream Bezier extension and its real target dependencies.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawn} from 'node:child_process';
const here=path.dirname(fileURLToPath(import.meta.url));
const args=process.argv.slice(2),browser=args.includes('--browser');
if(browser)args.splice(args.indexOf('--browser'),1);
if(args.length>1)throw Error('Usage: bezier.mjs [--browser] [report.json]');
const directory=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-bezier-checks-'));
const checks=path.join(directory,'checks.lisp');
fs.writeFileSync(checks,['environment-checks.lisp','bezier-checks.lisp']
  .map(file=>fs.readFileSync(path.join(here,file),'utf8')).join('\n'));
const output=path.resolve(args[0]??`/tmp/valtan-mcclim-bezier${browser?'-browser':''}.json`);
const child=spawn(process.execPath,[path.join(here,'probe.mjs'),'mcclim-bezier/core',output,checks,
  'clim-without-swank','use-salza2-zlib','--depends-on=mcclim-portability','--depends-on=uiop',
  ...(browser?['--browser']:[])],{stdio:'inherit'});
child.on('error',error=>{console.error(error);process.exitCode=1;});
child.on('exit',code=>{process.exitCode=code??1;});
