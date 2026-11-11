// One DOM producer and one Lisp worker consumer. Only the worker blocks.
const HEADER_WORDS = 4;
const SLOT_BYTES = 1024;
const encoder = new TextEncoder();
const decoder = new TextDecoder();

export class EventQueue {
  static allocate(capacity = 256) {
    if (!Number.isInteger(capacity) || capacity < 2 || (capacity & (capacity - 1)))
      throw new Error('Event queue capacity must be a power of two');
    return new SharedArrayBuffer(HEADER_WORDS * 4 + capacity * SLOT_BYTES);
  }
  constructor(buffer) {
    this.buffer = buffer;
    this.header = new Int32Array(buffer, 0, HEADER_WORDS);
    this.bytes = new Uint8Array(buffer, HEADER_WORDS * 4);
    this.capacity = this.bytes.length / SLOT_BYTES;
    if (!Number.isInteger(this.capacity) || this.capacity < 2 || (this.capacity & (this.capacity - 1)))
      throw new Error('Invalid shared event buffer');
  }
  push(event) {
    if (Atomics.load(this.header, 2)) return false;
    const encoded = encoder.encode(JSON.stringify(event));
    if (encoded.length > SLOT_BYTES - 4) throw new Error('Input event exceeds queue slot size');
    const write = Atomics.load(this.header, 0) >>> 0;
    const read = Atomics.load(this.header, 1) >>> 0;
    if (((write - read) >>> 0) >= this.capacity) return false;
    const offset = (write & (this.capacity - 1)) * SLOT_BYTES;
    new DataView(this.buffer, HEADER_WORDS * 4 + offset, 4).setUint32(0, encoded.length, true);
    this.bytes.set(encoded, offset + 4);
    Atomics.store(this.header, 0, (write + 1) | 0);
    Atomics.add(this.header, 3, 1);
    Atomics.notify(this.header, 3);
    return true;
  }
  take(timeout = Infinity) {
    const deadline = performance.now() + Math.max(0, Number(timeout));
    for (;;) {
      const epoch = Atomics.load(this.header, 3);
      const read = Atomics.load(this.header, 1) >>> 0;
      const write = Atomics.load(this.header, 0) >>> 0;
      if (read !== write) {
        const offset = (read & (this.capacity - 1)) * SLOT_BYTES;
        const length = new DataView(this.buffer, HEADER_WORDS * 4 + offset, 4).getUint32(0, true);
        // Browser TextDecoder rejects views over SharedArrayBuffer. Copy while
        // this slot still belongs to the consumer, before publishing READ.
        const text = decoder.decode(this.bytes.slice(offset + 4, offset + 4 + length));
        Atomics.store(this.header, 1, (read + 1) | 0);
        return JSON.parse(text);
      }
      if (Atomics.load(this.header, 2)) return {type: 'shutdown'};
      const remaining = deadline - performance.now();
      if (remaining <= 0) return null;
      Atomics.wait(this.header, 3, epoch, remaining);
    }
  }
  close() {
    Atomics.store(this.header, 2, 1);
    // Change the waited-on value as well as waking existing waiters.
    Atomics.add(this.header, 3, 1);
    Atomics.notify(this.header, 3);
  }
}
