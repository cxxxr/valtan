import { createBindingContext, withBindingContext } from './bindings';
import { currentValues, restoreValues } from './values';
import { swapCallStack } from './callstack';
import { callResumable } from './function';
import { resumableCheckpoint } from './resumable';

class ExecutionContext {
    constructor() {
        this.bindings = createBindingContext();
        this.values = [];
        this.callStack = [];
    }
}
let currentContext = null;

export function hasExecutionContext() { return currentContext !== null; }
export function createExecutionContext() { return new ExecutionContext(); }
export function withExecutionContext(context, function_) {
    if (!(context instanceof ExecutionContext)) throw new TypeError('Expected an execution context');
    if (context === currentContext) return function_();
    const previous = currentContext, values = currentValues();
    const stack = swapCallStack(context.callStack);
    restoreValues(context.values);
    currentContext = context;
    try { return withBindingContext(context.bindings, function_); }
    finally {
        context.values = currentValues();
        restoreValues(values);
        swapCallStack(stack);
        currentContext = previous;
    }
}

// A scheduler can advance tasks without owning Lisp values or binding stacks.
// Tasks run in one JS heap, so closures and ordinary mutable objects are shared.
export class ResumableTask {
    constructor(function_, args = []) {
        this.context = createExecutionContext();
        this.iterator = callResumable(function_, ...args);
        this.done = false;
        this.values = null;
        this.error = null;
        this.running = false;
    }
    resume(value, budget = 100) { return this.advance('next', value, budget); }
    interrupt(error, budget = 100) { return this.advance('throw', error, budget); }
    advance(operation, value, budget) {
        if (!Number.isSafeInteger(budget) || budget < 1) throw new RangeError('Task budget must be positive');
        if (this.running) throw new Error('Cannot resume a running task');
        if (this.done) throw new Error('Cannot resume a completed task');
        this.running = true;
        try {
            return withExecutionContext(this.context, () => {
                let step = this.iterator[operation](value);
                while (!step.done && step.value === resumableCheckpoint && --budget > 0)
                    step = this.iterator.next();
                if (step.done) {
                    this.done = true;
                    this.values = currentValues();
                }
                return step;
            });
        } catch (error) {
            this.done = true;
            this.error = error;
            throw error;
        } finally { this.running = false; }
    }
}
