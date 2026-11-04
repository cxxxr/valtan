import { defineConfig } from 'vite';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
const root = path.dirname(fileURLToPath(import.meta.url));
export default defineConfig({
  resolve: { alias: { lisp: path.resolve(root, '../kernel/lisp.js') } },
  build: {
    outDir: 'dist-ansi',
    lib: { entry: path.join(root, '.valtan-cache/ansi.js'), formats: ['es'] },
    target: 'node18',
    ssr: true,
  },
});
