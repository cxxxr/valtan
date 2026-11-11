// Optional resumable implementations preserve ordinary JS function identity.
// Synchronous foreign callbacks may execute them only without suspension.
const implementations = new WeakMap();
const nativeImplementations = new WeakMap();
export const resumableCheckpoint = Object.freeze({ kind: 'checkpoint' });

export function setResumableImplementation(function_, implementation) {
    implementations.set(function_, implementation);
    return function_;
}
export function getResumableImplementation(function_) {
    return implementations.get(function_);
}

// These implementations never enter Lisp to signal a condition. They throw a
// NativeLispError instead, caught within the calling resumable activation.
// Nested native helpers must also use raw implementations, while callbacks
// must keep using the public wrappers so their own handlers remain in scope.
export function setResumableNativeImplementation(function_, implementation) {
    nativeImplementations.set(function_, implementation);
    return function_;
}
export function getResumableNativeImplementation(function_) {
    return nativeImplementations.get(function_);
}

function runSynchronously(iterator) {
    let step = iterator.next();
    while (!step.done) {
        if (step.value === resumableCheckpoint) {
            step = iterator.next();
        } else {
            // Throw through the suspended stack so Lisp UNWIND-PROTECT and
            // dynamic bindings get their normal exceptional cleanup.
            const error = new Error('Cannot suspend across a synchronous JavaScript call boundary');
            step = iterator.throw(error);
        }
    }
    return step.value;
}

export function makeResumable(implementation) {
    const function_ = function(...args) {
        return runSynchronously(implementation.apply(this, args));
    };
    return setResumableImplementation(function_, implementation);
}
