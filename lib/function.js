import {
    values1
} from './values';

import {
    toLispBoolean
} from './header';

const callStack = [];

export function callFunctionWithCallStack(symbol, ...args) {
  callStack.push([symbol.name, args]);
  const value = callFunction(symbol, ...args);
  callStack.pop();
  return value;
}

export function printCallStack() {
  console.log(callStack);
}

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