import { currentScheduledTask } from './scheduler';
import { hasExecutionContext } from './execution';
import { makeResumable, setResumableImplementation } from './resumable';
import { S_nil, S_t } from './header';
import { values1 } from './values';

// Ordinary startup can hold uncontended locks. Resumable execution must use a
// scheduler: unrelated manually stepped tasks must not share this owner token.
const synchronousOwner = Object.freeze({ name: 'synchronous execution' });
function currentOwner() {
    const task = currentScheduledTask();
    if (task) return task;
    if (hasExecutionContext()) throw new Error('Locks require a scheduled task');
    return synchronousOwner;
}
function checkTimeout(timeout) {
    if (timeout !== null && (!Number.isFinite(timeout) || timeout < 0))
        throw new RangeError('Timeout must be null or non-negative finite milliseconds');
}

// Set preserves FIFO order and allows cancelled/timed-out waiters to be removed
// immediately. Timers never retain a settled waiter, including long timeouts.
class WaitQueue {
    constructor() { this.entries = new Set(); }
    add(timeout) {
        let resolve;
        const entry = { promise: new Promise(yes => { resolve = yes; }),
            timer: null, granted: false };
        entry.finish = value => {
            if (!this.entries.delete(entry)) return false;
            if (entry.timer !== null) clearTimeout(entry.timer);
            entry.timer = null;
            resolve(value);
            return true;
        };
        this.entries.add(entry);
        if (timeout !== null) {
            const deadline = performance.now() + timeout;
            const arm = () => {
                const remaining = deadline - performance.now();
                if (remaining <= 0) entry.finish(false);
                else entry.timer = setTimeout(arm, Math.min(remaining, 2147483647));
            };
            arm();
        }
        return entry;
    }
    first() { return this.entries.values().next().value; }
}

export class CooperativeLock {
    constructor(name = null, recursive = false) {
        this.name = name;
        this.recursive = recursive;
        this.owner = null;
        this.depth = 0;
        this.waiters = new WaitQueue();
    }
    *acquire(wait = true, timeout = null) {
        checkTimeout(timeout);
        const owner = currentOwner();
        if (this.owner === null) {
            this.owner = owner; this.depth = 1;
            return true;
        }
        if (this.owner === owner && this.recursive) { this.depth++; return true; }
        if (!wait || timeout === 0) return false;
        if (this.owner === owner) throw new Error('Cannot recursively acquire a non-recursive lock');
        if (owner === synchronousOwner) throw new Error('Contended locks require a scheduled task');
        const entry = this.waiters.add(timeout);
        entry.owner = owner;
        let claimed = false;
        try {
            const acquired = yield { kind: 'await', promise: entry.promise };
            claimed = acquired;
            return acquired;
        } finally {
            entry.finish(false);
            // Release can grant ownership before the promise continuation runs.
            // Cancellation in that window must pass the lock to the next waiter.
            if (entry.granted && !claimed) this.releaseOwner(owner);
        }
    }
    releaseOwner(owner) {
        if (this.owner !== owner) throw new Error('Only the lock owner may release it');
        if (--this.depth > 0) return;
        const entry = this.waiters.first();
        if (entry) {
            this.owner = entry.owner; this.depth = 1;
            entry.granted = true;
            entry.finish(true);
        } else { this.owner = null; this.depth = 0; }
    }
    release() { this.releaseOwner(currentOwner()); return this; }
}

export class CooperativeConditionVariable {
    constructor(name = null) { this.name = name; this.waiters = new WaitQueue(); }
    *wait(lock, timeout = null) {
        checkTimeout(timeout);
        if (!(lock instanceof CooperativeLock)) throw new TypeError('Expected a cooperative lock');
        const owner = currentOwner();
        if (lock.owner !== owner) throw new Error('Condition wait requires ownership of the lock');
        if (owner === synchronousOwner) throw new Error('Condition waits require a scheduled task');
        const depth = lock.depth;
        // Enqueue and release without a suspension between them: a notifier
        // cannot acquire the lock before the waiter has become visible.
        const entry = this.waiters.add(timeout);
        lock.depth = 1;
        lock.releaseOwner(owner);
        let notified, interrupted = false, interruption;
        try { notified = yield { kind: 'await', promise: entry.promise }; }
        catch (error) { interrupted = true; interruption = error; }
        finally { entry.finish(false); }
        // Reacquire before returning OR unwinding the caller's protected region.
        // Repeated cancellation during reacquisition must not expose an unlocked
        // region to its cleanup. Preserve the first interruption for propagation.
        for (;;) {
            try { yield* lock.acquire(); break; }
            catch (error) {
                if (!interrupted) { interrupted = true; interruption = error; }
            }
        }
        lock.depth = depth;
        if (interrupted) throw interruption;
        return notified;
    }
    notify(broadcast = false) {
        do {
            const entry = this.waiters.first();
            if (!entry) break;
            entry.finish(true);
        } while (broadcast);
    }
}

function requireLock(lock) {
    if (!(lock instanceof CooperativeLock)) throw new TypeError('Expected a cooperative lock');
    return lock;
}
function requireCondition(condition) {
    if (!(condition instanceof CooperativeConditionVariable)) throw new TypeError('Expected a condition variable');
    return condition;
}
const milliseconds = timeout => timeout === S_nil ? null : timeout;
export function CL_makeCooperativeLock(name, recursive) {
    return values1(new CooperativeLock(name, recursive !== S_nil));
}
export const CL_acquireCooperativeLock = makeResumable(function*(lock, wait, timeout) {
    return values1((yield* requireLock(lock).acquire(wait !== S_nil, milliseconds(timeout))) ? S_t : S_nil);
});
export function CL_releaseCooperativeLock(lock) { return values1(requireLock(lock).release()); }
export function CL_makeCooperativeCondition(name) { return values1(new CooperativeConditionVariable(name)); }
export function CL_cooperativeConditionWait() {
    // Refuse an ordinary foreign call before releasing its lock. In particular,
    // synchronous unwinding cannot wait to reacquire a contended lock.
    throw new Error('Condition waits require a resumable Lisp call');
}
setResumableImplementation(CL_cooperativeConditionWait, function*(condition, lock, timeout) {
    return values1((yield* requireCondition(condition).wait(lock, milliseconds(timeout))) ? S_t : S_nil);
});
export function CL_cooperativeConditionNotify(condition, broadcast) {
    requireCondition(condition).notify(broadcast !== S_nil);
    return values1(S_nil);
}
