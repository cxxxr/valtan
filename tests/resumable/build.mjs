import { execFileSync } from 'node:child_process';
import { build } from 'esbuild';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
execFileSync(path.join(root, 'valtan'), ['build', 'resumable.system'], {
  cwd: path.join(root, 'tests'), stdio: 'inherit',
});
await build({
  entryPoints: [path.join(root, 'tests/.valtan-cache/resumable.js')],
  outfile: path.join(root, 'tests/dist-resumable/resumable.js'),
  bundle: true, platform: 'node', format: 'esm', target: 'node18',
  alias: { lisp: path.join(root, 'kernel/lisp.js') },
});
