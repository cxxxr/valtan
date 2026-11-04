import { defineConfig } from 'vite';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import fs from 'node:fs';
import { createHash } from 'node:crypto';
const root = path.dirname(fileURLToPath(import.meta.url));
const sourceHash = name => createHash('sha256')
  .update(fs.readFileSync(path.join(root, 'performance', name))).digest('hex');
const sources = {
  workloadsSha256: sourceHash('workloads.lisp'),
  semanticsSha256: sourceHash('semantics.lisp'),
};
export default defineConfig({
  resolve: { alias: { lisp: path.resolve(root, '../kernel/lisp.js') } },
  build: {
    outDir: 'dist-performance',
    lib: { entry: path.join(root, '.valtan-cache/performance.js'), formats: ['es'] },
    target: 'node18',
    ssr: true,
    rollupOptions: { output: {
      banner: `globalThis.valtanBenchmarkSources = ${JSON.stringify(sources)};`,
    } },
  },
});
