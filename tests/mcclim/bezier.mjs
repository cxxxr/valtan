// Exercise the upstream Bezier extension and its real target dependencies.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawn} from 'node:child_process';
const here=path.dirname(fileURLToPath(import.meta.url));
const directory=fs.mkdtempSync(path.join(os.tmpdir(),'valtan-bezier-checks-'));
const checks=path.join(directory,'checks.lisp');
fs.writeFileSync(checks,['environment-checks.lisp','bezier-checks.lisp']
  .map(file=>fs.readFileSync(path.join(here,file),'utf8')).join('\n'));
const output=path.resolve(process.argv[2]??'/tmp/valtan-mcclim-bezier.json');
const child=spawn(process.execPath,[path.join(here,'probe.mjs'),'mcclim-bezier/core',output,checks,
  'clim-without-swank','use-salza2-zlib','--depends-on=mcclim-portability','--depends-on=uiop'],{stdio:'inherit'});
child.on('error',error=>{console.error(error);process.exitCode=1;});
child.on('exit',code=>{process.exitCode=code??1;});
