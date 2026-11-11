import { CooperativeScheduler, currentScheduledTask, isScheduledTask, allScheduledTasks } from './scheduler';
import { callResumable, CL_funcall } from './function';
import { setResumableImplementation } from './resumable';
import { S_nil, toLispBoolean } from './header';
import { values1 } from './values';
import { normalizeInteger } from './number';

const defaultScheduler = new CooperativeScheduler();
const mainThread = Object.freeze({ name: 'Main thread' });
export function currentThread() { return currentScheduledTask() ?? mainThread; }
function requireThread(thread) {
    if (thread !== mainThread && !isScheduledTask(thread)) throw new TypeError('Expected a cooperative thread');
    return thread;
}
function alive(thread) {
    return thread === mainThread || (!thread.terminationRequested && thread.scheduler.tasks.has(thread));
}
function checkJoin(thread) {
    requireThread(thread);
    if (thread === currentThread()) throw new Error('Cannot join the current thread');
    if (thread === mainThread) throw new Error('Cannot join synchronous startup execution');
}
export class ThreadTermination extends Error {
    constructor() { super('Thread was terminated'); this.name = 'ThreadTermination'; }
}

// The Lisp adapters own public types and condition contracts. JOIN waits for
// cleanup and returns the handle; FAILED/ERROR/RESULT distinguish its outcome.
export function CL_cooperativeThreadOperation(operation, ...args) {
    const [thread] = args;
    switch (operation.name) {
    case 'MAKE': {
        const scheduler = currentScheduledTask()?.scheduler ?? defaultScheduler;
        const handle = scheduler.spawn(args[0]);
        handle.name = args[1];
        return values1(handle);
    }
    case 'CURRENT': return values1(currentThread());
    case 'P': return values1(toLispBoolean(thread === mainThread || isScheduledTask(thread)));
    case 'ALL': return values1([mainThread, ...allScheduledTasks().filter(alive)]);
    case 'NAME': return values1(requireThread(thread).name ?? 'Scheduled task');
    case 'ALIVE': return values1(toLispBoolean(alive(requireThread(thread))));
    case 'FAILED': return values1(toLispBoolean(requireThread(thread).state === 'failed'));
    case 'ERROR': return values1(requireThread(thread).error ?? S_nil);
    case 'RESULT': return values1(requireThread(thread).values ?? []);
    case 'JOIN':
        checkJoin(thread);
        if (thread.scheduler.tasks.has(thread)) throw new Error('Joining a live thread requires a resumable Lisp call');
        return values1(thread);
    case 'DESTROY':
        requireThread(thread);
        if (thread === currentThread()) throw new Error('Cannot destroy the current thread');
        if (thread === mainThread) throw new Error('Cannot destroy synchronous startup execution');
        if (!alive(thread)) return values1(S_nil);
        thread.terminationRequested = true;
        return values1(toLispBoolean(thread.scheduler.cancel(thread, new ThreadTermination())));
    case 'INTERRUPT':
        requireThread(thread);
        if (thread === currentThread()) { CL_funcall(args[1]); return values1(thread); }
        if (thread === mainThread) throw new Error('Synchronous startup execution has no resumable activation');
        return values1(thread.scheduler.interrupt(thread, args[1]) ? thread : S_nil);
    default: throw new Error(`Unknown cooperative thread operation: ${operation.name}`);
    }
}
setResumableImplementation(CL_cooperativeThreadOperation, function*(operation, ...args) {
    const [thread] = args;
    if (operation.name === 'JOIN') {
        checkJoin(thread);
        if (thread.scheduler.tasks.has(thread)) {
            // Target failure is inspected by the Lisp adapter. Cancellation of
            // the joining task itself still throws through this suspended call.
            yield { kind: 'await', promise: thread.completion.then(() => undefined, () => undefined) };
        }
        return values1(thread);
    }
    if (operation.name === 'INTERRUPT' && thread === currentThread()) {
        yield* callResumable(args[1]);
        return values1(thread);
    }
    return CL_cooperativeThreadOperation(operation, ...args);
});

class AtomicInteger {
    constructor(value) { this.value = value; }
}
export function CL_cooperativeAtomicOperation(operation, cell, value, replacement) {
    if (operation.name === 'MAKE') return values1(new AtomicInteger(cell));
    if (!(cell instanceof AtomicInteger)) throw new TypeError('Expected an atomic integer');
    switch (operation.name) {
    case 'VALUE': return values1(cell.value);
    case 'SET': cell.value = value; return values1(value);
    case 'CAS': {
        const equal = BigInt(cell.value) === BigInt(value);
        if (equal) cell.value = replacement;
        return values1(toLispBoolean(equal));
    }
    case 'INC': cell.value = normalizeInteger(BigInt.asUintN(64, BigInt(cell.value) + BigInt(value))); break;
    case 'DEC': cell.value = normalizeInteger(BigInt.asUintN(64, BigInt(cell.value) - BigInt(value))); break;
    default: throw new Error(`Unknown atomic integer operation: ${operation.name}`);
    }
    // One native call contains no suspension point: all updates are atomic with
    // respect to the cooperative tasks sharing this JavaScript heap.
    return values1(cell.value);
}
