import {
    tValue,
    nilValue,
    toLispBoolean
} from './header';
import {
    values1,
    values
} from './values';

function isNumber(x) {
    return (typeof(x) === 'number');
}

export function CL_numberp(x) {
    return values1(toLispBoolean(isNumber(x)));
}

export function CL_integerp(x) {
    return values1(toLispBoolean(isNumber(x) && Number.isInteger(x)));
}

export function CL_add(x, y) {
    return values1(x + y);
}

export function CL_sub(x, y) {
    return values1(x - y);
}

export function CL_mul(x, y) {
    return values1(x * y);
}

export function CL_floor(number, divisor) {
  return values(Math.floor(number / divisor), number % divisor);
}

export function CL_rem(number, divisor) {
  return values1(number % divisor);
}

export function CL_numberEqual(x, y) {
    return values1(toLispBoolean(x === y));
}

export function CL_numberNotEqual(x, y) {
    return values1(toLispBoolean(x !== y));
}

export function CL_greaterThan(x, y) {
    return values1(toLispBoolean(x > y));
}

export function CL_lessThan(x, y) {
    return values1(toLispBoolean(x < y));
}

export function CL_greaterEqual(x, y) {
    return values1(toLispBoolean(x >= y));
}

export function CL_lessEqual(x, y) {
    return values1(toLispBoolean(x <= y));
}
