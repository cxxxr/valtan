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
    const list = [];
    for (let [symbol, value] of profileTable.entries()) {
        list.push({
            symbol,
            count: value.count,
            totalTime: value.totalTime
        });
    }
    list.sort((x, y) => {
        if (x.totalTime < y.totalTime) {
            return -1;
        }
        if (x.totalTime > y.totalTime) {
            return 1;
        }
        return 0;
    });
    console.log('Function name'.padStart(60), 'Count'.padStart(10), 'Total Time'.padStart(5));
    for (let {
            symbol,
            count,
            totalTime
        } of list) {
        console.log(`${symbol.packageName}:${symbol.name}`.padStart(60), String(count).padStart(10), String(totalTime).padStart(5));
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
                    totalTime: 0
                });
            }
            const info = profileTable.get(symbol);
            info.count++;
            info.totalTime += (end - start);
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