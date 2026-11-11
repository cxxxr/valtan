import test from 'node:test';
import assert from 'node:assert/strict';
import {Worker} from 'node:worker_threads';
import {EventQueue} from '../../library/mcclim-browser/event-queue.mjs';

test('input ordering, Unicode, overflow, wraparound and shutdown', () => {
  const queue = new EventQueue(EventQueue.allocate(2));
  // Exercise wraparound of the atomic signed counters as well as slot reuse.
  queue.header[0] = queue.header[1] = -1;
  assert.equal(queue.take(0), null);
  assert.equal(queue.push({key: '😀'}), true);
  assert.equal(queue.push({key: 'あ'}), true);
  assert.equal(queue.push({key: 'overflow'}), false);
  assert.deepEqual(queue.take(0), {key: '😀'});
  assert.equal(queue.push({key: 'third'}), true);
  assert.deepEqual(queue.take(0), {key: 'あ'});
  queue.close();
  assert.deepEqual(queue.take(0), {key: 'third'});
  assert.deepEqual(queue.take(0), {type: 'shutdown'});
  assert.equal(queue.push({key: 'closed'}), false);
});

test('a blocked worker wakes for input and for shutdown', {timeout: 10000}, async () => {
  const buffer = EventQueue.allocate();
  const moduleURL = new URL('../../library/mcclim-browser/event-queue.mjs', import.meta.url).href;
  const worker = new Worker(`
    const {parentPort, workerData} = require('node:worker_threads');
    import(workerData.moduleURL).then(({EventQueue}) => {
      const queue = new EventQueue(workerData.buffer);
      parentPort.postMessage('waiting');
      parentPort.postMessage(queue.take());
      parentPort.postMessage(queue.take());
    });
  `, {eval: true, workerData: {moduleURL, buffer}});
  const messages = [];
  try {
    await new Promise((resolve, reject) => {
      worker.on('error', reject);
      worker.on('message', message => {
        messages.push(message);
        if (message === 'waiting') setTimeout(() => new EventQueue(buffer).push({type: 'keydown', key: 'Enter'}), 20);
        else if (message.type === 'keydown') setTimeout(() => new EventQueue(buffer).close(), 20);
        else if (message.type === 'shutdown') resolve();
      });
    });
    assert.deepEqual(messages, ['waiting', {type: 'keydown', key: 'Enter'}, {type: 'shutdown'}]);
  } finally { await worker.terminate(); }
});
