import {
    values1
} from './values';

import {
    toLispBoolean
} from './header';

import {
    jsArrayToList
} from './cons';

import {
    pushCallStack,
    popCallStack
} from './callstack';

export function callFunction(symbol, ...args) {
    const func = symbol.getFunction();
    return func(...args);
}

export function callFunctionWithCallStack(symbol, ...args) {
    pushCallStack(jsArrayToList([symbol, ...args]));
    try {
        return callFunction(symbol, ...args);
    } finally {
        popCallStack();
    }
}

export function CL_apply(fn, args) {
    pushCallStack(jsArrayToList([fn, ...args]));
    try {
        return fn(...args);
    } finally {
        popCallStack();
    }
}

export function CL_functionp(x) {
    return values1(toLispBoolean(typeof x === 'function'));
}