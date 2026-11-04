import { build } from 'vite';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawnSync } from 'node:child_process';
const directory = await mkdtemp(join(tmpdir(), 'valtan-kernel-tests-'));
try {
  const output = join(directory, 'kernel.test.mjs');
  await build({ configFile: false, logLevel: 'silent', build: {
    ssr: new URL('./kernel.test.mjs', import.meta.url).pathname,
    target: 'node18', outDir: directory, emptyOutDir: false,
    rollupOptions: { output: { format: 'es', entryFileNames: 'kernel.test.mjs' } },
  } });
  const result = spawnSync(process.execPath, ['--test', output], { stdio: 'inherit' });
  process.exitCode = result.status ?? 1;
} finally {
  await rm(directory, { recursive: true, force: true });
}
