import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import fs from 'fs';
import path from 'path';

function getValtanPath() {
  try {
    return fs.readFileSync('.valtan-path', 'utf-8').trim();
  } catch (e) {
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
    sourcemap: true,
  },
});
