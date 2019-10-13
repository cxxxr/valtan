export let tValue;
export let nilValue;
export let S_package;
export let S_symbol;
export let S_string;
export let S_type_error;

import {
    raise
} from './callstack';

export function initSymbols({
    t,
    nil,
    ...symbols
}) {
    tValue = t;
    tValue.value = t;
    tValue.plist = nil;
    nilValue = nil;
    nilValue.value = nil;
    nilValue.plist = nil;
    S_package = symbols["PACKAGE"];
    S_symbol = symbols["SYMBOL"];
    S_string = symbols["STRING"];
    S_type_error = symbols["TYPE-ERROR"];
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

export function typeError(datum, expectedType) {
    S_type_error.func(datum, expectedType);
}

export function checkString(value) {
    if (!isString(value)) {
        typeError(value, S_string);
    }
}