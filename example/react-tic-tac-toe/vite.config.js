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
  plugins: [
    react({
      // Exclude valtan-generated files from React transform
      exclude: /\.valtan-cache\//,
    }),
  ],
  resolve: {
    alias: {
      'lisp': path.resolve(getValtanPath(), 'lisp.js'),
    },
  },
  optimizeDeps: {
    // Don't pre-bundle valtan files
    exclude: ['.valtan-cache'],
  },
  build: {
    outDir: 'dist',
    sourcemap: true,
  },
});
