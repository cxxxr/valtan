export let tValue;
export let nilValue;

export function initSymbols({
    t,
    nil
}) {
    tValue = t;
    tValue.value = t;
    nilValue = nil;
    nilValue.value = nil;
}

export function toLispBoolean(x) {
    return x ? tValue : nilValue;
}

export function isString(x) {
    return typeof x === 'string' || x instanceof String;
}

export function argumentsError(name, args) {
    throw new Error(`${name} is invalid number of arguments for ${name}: ${args.length}`);
}

export function typeError(value, typeName) {
    throw new Error(`The value ${value} is not of the expected type ${typeName}`);
}

export function checkType(value, assert, name) {
    if (!assert(value)) {
        typeError(value, name);
    }
}

export function checkString(value) {
    if (!isString(value)) {
        typeError(value, "String");
    }
}