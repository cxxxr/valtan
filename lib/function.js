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

const profileTable = new Map();

let enableProfiling = false;

export function startProfile() {
    enableProfiling = true;
}

export function finishProfile() {
    enableProfiling = false;
    for (let [symbol, value] of profileTable) {
        console.log(`${symbol.packageName}:${symbol.name},${value.count},${value.sumTime}`);
    }
}

export function callFunctionWithCallStack(symbol, ...args) {
    pushCallStack(jsArrayToList([symbol, ...args]));
    try {
        const start = Date.now();
        const result = callFunction(symbol, ...args);
        const end = Date.now();

        if (enableProfiling) {
            if (!profileTable.has(symbol)) {
                profileTable.set(symbol, {
                    count: 0,
                    sumTime: 0
                });
            }
            const info = profileTable.get(symbol);
            info.count++;
            info.sumTime += (end - start);
        }
        return result;
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