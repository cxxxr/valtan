import { defineConfig } from 'vite';
import fs from 'fs';
import path from 'path';

function getValtanPath() {
  try {
    return fs.readFileSync('.valtan-path', 'utf-8').trim();
  } catch (e) {
    return path.resolve(__dirname, '../kernel');
  }
}

export default defineConfig({
  resolve: {
    alias: {
      'lisp': path.resolve(getValtanPath(), 'lisp.js'),
    },
  },
  build: {
    outDir: 'dist',
    lib: {
      entry: './.valtan-cache/tests.js',
      formats: ['es'],
      fileName: 'main',
    },
    target: 'node18',
    ssr: true,
  },
});
