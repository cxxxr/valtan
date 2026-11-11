import { ResumableTask } from './execution';
import { resumableCheckpoint } from './resumable';

let currentTask = null;
export function currentScheduledTask() { return currentTask; }

// Same-heap cooperative scheduling: each slice restores its execution context,
// and each host turn has a finite slice/time budget so browser events can run.
export class CooperativeScheduler {
    constructor({ checkpointBudget = 1000, slicesPerTurn = 64, turnMilliseconds = 8 } = {}) {
        for (const value of [checkpointBudget, slicesPerTurn]) {
            if (!Number.isSafeInteger(value) || value < 1) throw new RangeError('Scheduler budgets must be positive integers');
        }
        if (!Number.isFinite(turnMilliseconds) || turnMilliseconds <= 0)
            throw new RangeError('Scheduler turn duration must be positive');
        this.checkpointBudget = checkpointBudget;
        this.slicesPerTurn = slicesPerTurn;
        this.turnMilliseconds = turnMilliseconds;
        this.queue = [];
        this.tasks = new Set();
        this.timer = null;
        this.pumping = false;
    }
    spawn(function_, args = []) {
        const task = new ResumableTask(function_, args);
        let resolve, reject;
        const completion = new Promise((yes, no) => { resolve = yes; reject = no; });
        // Unjoined failed tasks retain their rejection for JOIN without causing
        // an unhandled host rejection before the application has time to join.
        completion.catch(() => {});
        const handle = { scheduler: this, task, completion, resolve, reject,
            state: 'runnable', queued: false, pending: null, waitVersion: 0,
            values: null, error: null };
        this.tasks.add(handle);
        this.enqueue(handle, 'next', undefined);
        return handle;
    }
    enqueue(handle, operation, value) {
        handle.pending = { operation, value };
        handle.state = 'runnable';
        if (!handle.queued) { handle.queued = true; this.queue.push(handle); }
        this.schedule();
    }
    cancel(handle, error = new Error('Task cancelled')) {
        if (handle.scheduler !== this) throw new TypeError('Task belongs to another scheduler');
        if (!this.tasks.has(handle)) return false;
        // Ignore completion of an older wait after cancellation, even if cleanup
        // has already started another asynchronous wait in the same task.
        handle.waitVersion++;
        this.enqueue(handle, 'throw', error);
        return true;
    }
    schedule() {
        if (!this.pumping && this.timer === null) {
            this.timer = setTimeout(() => { this.timer = null; this.pump(); }, 0);
        }
    }
    pump() {
        this.pumping = true;
        const started = performance.now();
        try {
            for (let slices = 0; this.queue.length && slices < this.slicesPerTurn; slices++) {
                const handle = this.queue.shift();
                handle.queued = false;
                if (this.tasks.has(handle)) this.step(handle);
                if (performance.now() - started >= this.turnMilliseconds) break;
            }
        } finally {
            this.pumping = false;
            if (this.queue.length) this.schedule();
        }
    }
    step(handle) {
        const pending = handle.pending;
        handle.pending = null;
        handle.state = 'running';
        const previous = currentTask;
        currentTask = handle;
        try {
            const step = pending.operation === 'throw'
                ? handle.task.interrupt(pending.value, this.checkpointBudget)
                : handle.task.resume(pending.value, this.checkpointBudget);
            if (step.done) {
                handle.values = handle.task.values.slice();
                handle.state = 'completed';
                this.tasks.delete(handle);
                handle.resolve(handle.values);
            } else if (handle.pending?.operation === 'throw') {
                // The running function requested its own cancellation. Preserve
                // that request instead of replacing it with its yielded wait.
            } else if (step.value === resumableCheckpoint || step.value?.kind === 'yield') {
                this.enqueue(handle, 'next', undefined);
            } else if (step.value?.kind === 'await') {
                this.wait(handle, step.value.promise);
            } else {
                this.enqueue(handle, 'throw', new TypeError('Unknown scheduler suspension request'));
            }
        } catch (error) {
            if (!handle.task.done) {
                // A malformed host request can throw while being inspected.
                // Deliver that error through the suspended iterator as well.
                this.enqueue(handle, 'throw', error);
            } else {
                handle.error = error;
                handle.state = 'failed';
                this.tasks.delete(handle);
                handle.reject(error);
            }
        } finally { currentTask = previous; }
    }
    wait(handle, promise) {
        handle.state = 'waiting';
        const version = ++handle.waitVersion;
        const finish = (operation, value) => {
            if (handle.state === 'waiting' && handle.waitVersion === version)
                this.enqueue(handle, operation, value);
        };
        Promise.resolve(promise).then(value => finish('next', value), error => finish('throw', error));
    }
}
