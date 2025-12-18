import {
    toLispBoolean
} from './header';
import {
    values1,
    values
} from './values';

// Float class to distinguish floats from integers in JavaScript
// JavaScript treats 3 and 3.0 as identical, but Common Lisp distinguishes them
export class Float {
    constructor(value) {
        this.value = value;
    }
}

// Get the raw numeric value from a number (whether Float or primitive)
export function numberValue(x) {
    return (x instanceof Float) ? x.value : x;
}

// Check if x is any kind of number (primitive or Float)
function isNumber(x) {
    return (typeof(x) === 'number') || (x instanceof Float);
}

// Create a Float if needed (for preserving floatness in operations)
export function toFloat(value) {
    return new Float(value);
}

export function CL_numberp(x) {
    return values1(toLispBoolean(isNumber(x)));
}

export function CL_integerp(x) {
    // Float objects are never integers, even if the value is an integer
    if (x instanceof Float) {
        return values1(toLispBoolean(false));
    }
    return values1(toLispBoolean(typeof(x) === 'number' && Number.isInteger(x)));
}

export function CL_floatp(x) {
    // Float objects are always floats
    // Primitive numbers that aren't integers are also floats
    if (x instanceof Float) {
        return values1(toLispBoolean(true));
    }
    return values1(toLispBoolean(typeof(x) === 'number' && !Number.isInteger(x)));
}

// Check if either operand is a Float (result should be Float)
function eitherIsFloat(x, y) {
    return (x instanceof Float) || (y instanceof Float);
}

// Return result as Float if either input was Float, otherwise as primitive
function maybeFloat(result, wasFloat) {
    return wasFloat ? new Float(result) : result;
}

export function CL_add(x, y) {
    const wasFloat = eitherIsFloat(x, y);
    const result = numberValue(x) + numberValue(y);
    return values1(maybeFloat(result, wasFloat));
}

export function CL_sub(x, y) {
    const wasFloat = eitherIsFloat(x, y);
    const result = numberValue(x) - numberValue(y);
    return values1(maybeFloat(result, wasFloat));
}

export function CL_negate(x) {
    const wasFloat = x instanceof Float;
    const result = -numberValue(x);
    return values1(maybeFloat(result, wasFloat));
}

export function CL_mul(x, y) {
    const wasFloat = eitherIsFloat(x, y);
    const result = numberValue(x) * numberValue(y);
    return values1(maybeFloat(result, wasFloat));
}

export function CL_div(x, y) {
    // Division always produces a float
    const result = numberValue(x) / numberValue(y);
    return values1(new Float(result));
}

export function CL_floor(number, divisor) {
    const n = numberValue(number);
    const d = numberValue(divisor);
    const wasFloat = eitherIsFloat(number, divisor);
    const quotient = Math.floor(n / d);
    const remainder = n - quotient * d;
    return values(quotient, maybeFloat(remainder, wasFloat));
}

export function CL_rem(number, divisor) {
    const wasFloat = eitherIsFloat(number, divisor);
    const result = numberValue(number) % numberValue(divisor);
    return values1(maybeFloat(result, wasFloat));
}

export function CL_numberEqual(x, y) {
    return values1(toLispBoolean(numberValue(x) === numberValue(y)));
}

export function CL_numberNotEqual(x, y) {
    return values1(toLispBoolean(numberValue(x) !== numberValue(y)));
}

export function CL_greaterThan(x, y) {
    return values1(toLispBoolean(numberValue(x) > numberValue(y)));
}

export function CL_lessThan(x, y) {
    return values1(toLispBoolean(numberValue(x) < numberValue(y)));
}

export function CL_greaterEqual(x, y) {
    return values1(toLispBoolean(numberValue(x) >= numberValue(y)));
}

export function CL_lessEqual(x, y) {
    return values1(toLispBoolean(numberValue(x) <= numberValue(y)));
}

export function CL_round(number, divisor) {
    const n = numberValue(number);
    const d = numberValue(divisor);
    const wasFloat = eitherIsFloat(number, divisor);
    const quotient = Math.round(n / d);
    const remainder = n - quotient * d;
    return values(quotient, maybeFloat(remainder, wasFloat));
}

export function CL_truncate(number, divisor) {
    const n = numberValue(number);
    const d = numberValue(divisor);
    const wasFloat = eitherIsFloat(number, divisor);
    const quotient = Math.trunc(n / d);
    const remainder = n - quotient * d;
    return values(quotient, maybeFloat(remainder, wasFloat));
}

export function CL_ceiling(number, divisor) {
    const n = numberValue(number);
    const d = numberValue(divisor);
    const wasFloat = eitherIsFloat(number, divisor);
    const quotient = Math.ceil(n / d);
    const remainder = n - quotient * d;
    return values(quotient, maybeFloat(remainder, wasFloat));
}
