import { createBindingContext, withBindingContext } from './bindings';
import { currentValues, restoreValues } from './values';
import { swapCallStack } from './callstack';
import { callResumable } from './function';
import { resumableCheckpoint, synchronousResumptionActive, withTaskCheckpoints } from './resumable';

class ExecutionContext {
    constructor() {
        this.bindings = createBindingContext();
        this.values = [];
        this.callStack = [];
        this.checkpointBudget = null;
    }
}
let currentContext = null;

export function hasExecutionContext() { return currentContext !== null; }
// Count compiler checkpoints where they occur instead of propagating every
// one through the entire generator stack. Direct iterator consumers retain the
// original checkpoint behavior when no task advance owns a budget.
export function resumableCheckpointDue() {
    if (currentContext !== null && currentContext.checkpointBudget !== null &&
        --currentContext.checkpointBudget > 0) return false;
    // Synchronous facades cannot hand control to a scheduler. They still count
    // any enclosing task's budget, which is observed after the callback returns.
    return !synchronousResumptionActive();
}
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
    // Interrupt activations share their parent's context and execute as a stack;
    // unrelated tasks use the default, independent execution context.
    constructor(function_, args = [], context = createExecutionContext()) {
        this.context = context;
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
            // Explicitly advancing another task inside a synchronous callback
            // starts its own resumable boundary and must honor that task's budget.
            return withTaskCheckpoints(() => withExecutionContext(this.context, () => {
                const previousBudget = this.context.checkpointBudget;
                this.context.checkpointBudget = budget;
                try {
                    let step = this.iterator[operation](value);
                    while (!step.done && step.value === resumableCheckpoint) {
                        // Counted checkpoints already exhausted the budget.
                        // Native/older generators still yield an uncounted
                        // checkpoint, consuming one unit here as before.
                        if (this.context.checkpointBudget <= 0 || --this.context.checkpointBudget <= 0) break;
                        step = this.iterator.next();
                    }
                    if (step.done) {
                        this.done = true;
                        this.values = currentValues();
                    }
                    return step;
                } finally {
                    this.context.checkpointBudget = previousBudget;
                }
            }));
        } catch (error) {
            this.done = true;
            this.error = error;
            throw error;
        } finally { this.running = false; }
    }
}
