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

import {
    isSymbol
} from './symbol';

import {
    typeError
} from './error';

export function callFunction(symbol, ...args) {
    const func = symbol.getFunction();
    return func(...args);
}

const profileTable = new Map();
const profileCallStack = [];

let enableProfiling = false;

class profileStackEntry {
    constructor(fnName, lastTime) {
        this.fnName = fnName;
        this.lastTime = lastTime;
    }
}

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

function profileEnter(symbol) {
    if (symbol.__call_count === undefined) {
        symbol.__call_count = 1;
    } else {
        symbol.__call_count++;
    }
    if (symbol.__enter_time === undefined) {
        //symbol.__enter_time = Date.now();
        symbol.__hrtime = process.hrtime();
        if (symbol.__total_time === undefined) {
            symbol.__total_time = 0;
        }
    }
    if (symbol.__call_depth === undefined) {
        symbol.__call_depth = 1;
    } else {
        symbol.__call_depth++;
    }
}

function profileExit(symbol) {
    symbol.__call_depth--;
    if (symbol.__call_depth === 0) {
        //symbol.__total_time += (Date.now() - symbol.__enter_time);
        symbol.__total_time += process.hrtime(symbol.__hrtime)[1];
        symbol.__enter_time = undefined;
        profileTable.set(symbol, {
            count: symbol.__call_count,
            totalTime: symbol.__total_time
        });
    }
}

//function profileEnter(symbol) {
//    if (!profileTable.has(symbol)) {
//        profileTable.set(symbol, {
//            count: 1,
//            totalTime: 0,
//        });
//    } else {
//        const info = profileTable.get(symbol);
//        info.count++;
//    }
//    if (profileCallStack.length > 0) {
//        incProfileTime(profileCallStack[0], profileCallStack[0].fnName);
//    }
//    profileCallStack.push(new profileStackEntry(symbol, Date.now()));
//}

//function profileExit(symbol) {
//    incProfileTime(profileCallStack.pop(), symbol);
//    if (profileCallStack.length > 0) {
//        profileCallStack[0].lastTime = Date.now();
//    }
//}

function incProfileTime(entry, symbol) {
    const info = profileTable.get(symbol);
    info.totalTime += Date.now() - entry.lastTime;
}

//function withProfiling(symbol, fn, ...args) {
//    const start = Date.now();
//    const result = fn(...args);
//    const end = Date.now();
//    if (!profileTable.has(symbol)) {
//        profileTable.set(symbol, {
//            count: 0,
//            totalTime: 0
//        });
//    }
//    const info = profileTable.get(symbol);
//    info.count++;
//    info.totalTime += (end - start);
//    return result;
//}

export function callFunctionWithCallStack(symbol, ...args) {
    pushCallStack(jsArrayToList([symbol, ...args]));
    try {
        if (enableProfiling) {
            profileEnter(symbol);
            const result = callFunction(symbol, ...args);
            profileExit(symbol);
            return result;
        } else {
            return callFunction(symbol, ...args);
        }
    } finally {
        popCallStack();
    }
}

export function CL_apply(fn, args) {
    pushCallStack(jsArrayToList([fn, ...args]));
    try {
        if (enableProfiling && fn.lisp_symbol) {
            profileEnter(fn.lisp_symbol);
            const result = fn(...args);
            profileExit(fn.lisp_symbol);
            return result;
        } else {
            return fn(...args);
        }
    } finally {
        popCallStack();
    }
}

export function CL_funcall(fn, ...args) {
    if (isSymbol(fn)) {
        fn = fn.getFunction();
    } else if (typeof fn !== 'function') {
        typeError(fn, S_function);
    }

    if (enableProfiling && fn.lisp_symbol) {
        profileEnter(fn.lisp_symbol);
        const result = fn(...args);
        profileExit(fn.lisp_symbol);
        return result;
    } else {
        return fn(...args);
    }
}

export function CL_functionp(x) {
    return values1(toLispBoolean(typeof x === 'function'));
}