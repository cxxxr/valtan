import {
    S_nil
} from './header';

let current_values = [];
let primary_value;

export function values1(x) {
    // Most calls return one value. Materialize a stable snapshot only when a
    // multiple-value consumer actually asks for it.
    current_values = null;
    primary_value = x;
    return x;
}

export function values(...args) {
    current_values = args;
    return args.length === 0 ? S_nil : args[0];
}

export function CL_values(...args) {
    return values(...args);
}

export function currentValues() {
  return current_values === null ? [primary_value] : current_values;
}

export function restoreValues(values) {
  current_values = values;
}

export function CL_multipleValueCall(fn, ...args) {
    const vector = [];
    for (let i = 0; i < args.length - 1; i++) {
        vector.push(args[i]);
    }
    return fn(...vector.concat(currentValues()));
}
