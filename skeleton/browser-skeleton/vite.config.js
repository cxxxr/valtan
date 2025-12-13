import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
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
  plugins: [react()],
  resolve: {
    alias: {
      'lisp': path.resolve(getValtanPath(), 'lisp.js'),
    },
  },
  build: {
    outDir: 'dist',
    rollupOptions: {
      input: './.valtan-cache/<% @var name %>.js',
      output: {
        entryFileNames: 'main.js',
      },
    },
  },
});
