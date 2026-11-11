import { setResumableNativeImplementation } from './resumable';

// Raw native operations report a Lisp condition without entering Lisp. Their
// public wrappers deliver it synchronously; CALL-RESUMABLE delivers it before
// returning to the caller, whose handlers and restarts are still active.
export class NativeLispError extends Error {
    constructor(function_, args) {
        super('Native operation requested a Lisp condition');
        this.function_ = function_;
        this.args = args;
    }
}

export function nativeLispError(function_, ...args) {
    throw new NativeLispError(function_, args);
}

export function makeNativeFunction(implementation) {
    const function_ = function(...args) {
        try { return implementation(...args); }
        catch (error) {
            if (!(error instanceof NativeLispError)) throw error;
            return error.function_.getFunction()(...error.args);
        }
    };
    return setResumableNativeImplementation(function_, implementation);
}
