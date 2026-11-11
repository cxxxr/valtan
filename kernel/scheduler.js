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
            state: 'runnable', queued: false, frames: [this.frame(task)], interruptions: [],
            values: null, error: null };
        this.tasks.add(handle);
        this.enqueue(handle, 'next', undefined);
        return handle;
    }
    frame(task, savedValues = null) {
        return { task, savedValues, pending: null, state: 'runnable', waitVersion: 0 };
    }
    activeFrame(handle) { return handle.frames[handle.frames.length - 1]; }
    wake(handle) {
        handle.state = 'runnable';
        if (!handle.queued) { handle.queued = true; this.queue.push(handle); }
        this.schedule();
    }
    enqueue(handle, operation, value, frame = this.activeFrame(handle)) {
        frame.pending = { operation, value };
        frame.state = 'runnable';
        if (frame === this.activeFrame(handle)) this.wake(handle);
    }
    interrupt(handle, function_, args = []) {
        if (handle.scheduler !== this) throw new TypeError('Task belongs to another scheduler');
        if (!this.tasks.has(handle)) return false;
        handle.interruptions.push({ function_, args: args.slice() });
        this.wake(handle);
        return true;
    }
    cancel(handle, error = new Error('Task cancelled')) {
        if (handle.scheduler !== this) throw new TypeError('Task belongs to another scheduler');
        if (!this.tasks.has(handle)) return false;
        // Ignore completion of an older wait after cancellation, even if cleanup
        // has already started another asynchronous wait in the same task.
        handle.interruptions.length = 0;
        // Unwind active interrupt callbacks before the original computation.
        // A callback catching cancellation must not abandon the parent task.
        for (const frame of handle.frames) {
            frame.waitVersion++;
            frame.pending = { operation: 'throw', value: error };
            frame.state = 'runnable';
        }
        this.wake(handle);
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
        let frame = this.activeFrame(handle);
        if (handle.interruptions.length && frame.pending?.operation !== 'throw') {
            const { function_, args } = handle.interruptions.shift();
            // The callback runs atop the target's bindings and call stack.
            // Its return values are discarded when the interrupted frame resumes.
            const context = handle.task.context;
            frame = this.frame(new ResumableTask(function_, args, context), context.values);
            frame.pending = { operation: 'next', value: undefined };
            handle.frames.push(frame);
        }
        const pending = frame.pending;
        if (!pending) { handle.state = frame.state; return; }
        frame.pending = null;
        frame.state = 'running';
        handle.state = 'running';
        const previous = currentTask;
        currentTask = handle;
        try {
            const step = pending.operation === 'throw'
                ? frame.task.interrupt(pending.value, this.checkpointBudget)
                : frame.task.resume(pending.value, this.checkpointBudget);
            if (step.done) {
                this.finishFrame(handle, frame, false);
            } else if (frame.pending?.operation === 'throw') {
                // The running function requested its own cancellation. Preserve
                // that request instead of replacing it with its yielded wait.
            } else if (step.value === resumableCheckpoint || step.value?.kind === 'yield') {
                this.enqueue(handle, 'next', undefined);
            } else if (step.value?.kind === 'await') {
                this.wait(handle, frame, step.value.promise);
            } else {
                this.enqueue(handle, 'throw', new TypeError('Unknown scheduler suspension request'));
            }
        } catch (error) {
            if (!frame.task.done) {
                // A malformed host request can throw while being inspected.
                // Deliver that error through the suspended iterator as well.
                this.enqueue(handle, 'throw', error);
            } else {
                this.finishFrame(handle, frame, true, error);
            }
        } finally {
            currentTask = previous;
            if (this.tasks.has(handle) && handle.interruptions.length) this.wake(handle);
        }
    }
    finishFrame(handle, frame, failed, error) {
        frame.waitVersion++;
        handle.frames.pop();
        const parent = this.activeFrame(handle);
        if (parent) {
            // A normal callback discards its values. An exceptional exit may
            // carry RETURN-FROM/THROW values in the context; retain those while
            // delivering the transfer through the interrupted stack's cleanup.
            if (!failed) handle.task.context.values = frame.savedValues;
            if (failed) {
                parent.waitVersion++;
                this.enqueue(handle, 'throw', error, parent);
            } else if (parent.pending) this.wake(handle);
            else handle.state = parent.state;
        } else {
            handle.interruptions.length = 0;
            this.tasks.delete(handle);
            if (failed) {
                handle.error = error; handle.state = 'failed'; handle.reject(error);
            } else {
                handle.values = frame.task.values.slice();
                handle.state = 'completed'; handle.resolve(handle.values);
            }
        }
    }
    wait(handle, frame, promise) {
        handle.state = frame.state = 'waiting';
        const version = ++frame.waitVersion;
        const finish = (operation, value) => {
            // An interrupted frame may settle while its callback is suspended.
            // Save its result, but only resume it after all callbacks have left.
            if (this.tasks.has(handle) && frame.state === 'waiting' && frame.waitVersion === version)
                this.enqueue(handle, operation, value, frame);
        };
        Promise.resolve(promise).then(value => finish('next', value), error => finish('throw', error));
    }
}
