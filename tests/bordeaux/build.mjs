import { execFileSync } from 'node:child_process';
import { build } from 'esbuild';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const system = process.argv[2] ?? 'bordeaux';
if (!['bordeaux', 'bordeaux-upstream'].includes(system)) throw new Error('Unknown Bordeaux test system');
execFileSync(path.join(root, 'valtan'), ['build', `${system}.system`], {
  cwd: path.join(root, 'tests'), stdio: 'inherit',
});
await build({ entryPoints: [path.join(root, `tests/.valtan-cache/${system}.js`)],
  outfile: path.join(root, `tests/dist-bordeaux/${system}.js`),
  bundle: true, platform: 'node', format: 'esm', target: 'node18',
  alias: { lisp: path.join(root, 'kernel/lisp.js') },
});
