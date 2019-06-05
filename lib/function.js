import {
    values1
} from './values';

import {
    toLispBoolean
} from './header';

export function callFunction(symbol, ...args) {
    const func = symbol.getFunction();
    return func(...args);
}

export function CL_apply(fn, args) {
    return fn(...args);
}

export function CL_functionp(x) {
    return values1(toLispBoolean(typeof x === 'function'));
}