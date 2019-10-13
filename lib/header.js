export let tValue;
export let nilValue;

import {
  raise
} from './callstack';

export function initSymbols({
    t,
    nil
}) {
    tValue = t;
    tValue.value = t;
    tValue.plist = nil;
    nilValue = nil;
    nilValue.value = nil;
    nilValue.plist = nil;
}

export function toLispBoolean(x) {
    return x ? tValue : nilValue;
}

export function isString(x) {
    return typeof x === 'string' || x instanceof String;
}

export function argumentsError(name, args) {
  raise('invalid number of arguments for ~A: ~A', name, name, args.length);
}

export function typeError(value, typeName) {
  raise('The value ~S is not of the expected type ~A', value, typeName);
}

export function checkString(value) {
    if (!isString(value)) {
        typeError(value, "String");
    }
}
