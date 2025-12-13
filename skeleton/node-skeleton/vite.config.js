import { defineConfig } from 'vite';
import fs from 'fs';
import path from 'path';

// Read the valtan kernel path from .valtan-path file
function getValtanPath() {
  try {
    return fs.readFileSync('.valtan-path', 'utf-8').trim();
  } catch (e) {
    // Fallback: assume kernel is in standard location
    return path.resolve(__dirname, '../../kernel');
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
      entry: './.valtan-cache/<% @var name %>.js',
      formats: ['es'],
      fileName: 'main',
    },
    rollupOptions: {
      external: [],
    },
    target: 'node18',
    ssr: true,
  },
});
