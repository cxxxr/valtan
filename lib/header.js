export let tValue;
export let nilValue;

export function initBoolean(t, nil) {
  tValue = t;
  tValue.value = t;
  nilValue = nil;
  nilValue.value = nil;
}

export function toLispBoolean(x) {
  return x ? tValue : nilValue;
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

export function typeChecker(type) {
  return (function(value) {
    return (value instanceof type);
  });
}

export function isString(x) {
  return typeof x === 'string' || x instanceof String;
}
