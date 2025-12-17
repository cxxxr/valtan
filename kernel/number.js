import {
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

export function CL_negate(x) {
    return values1(-x);
}

export function CL_mul(x, y) {
    return values1(x * y);
}

export function CL_floor(number, divisor) {
    const quotient = Math.floor(number / divisor);
    return values(quotient, number - quotient * divisor);
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

export function CL_round(number, divisor) {
    return values(Math.round(number / divisor), number - Math.round(number / divisor) * divisor);
}

export function CL_truncate(number, divisor) {
    return values(Math.trunc(number / divisor), number - Math.trunc(number / divisor) * divisor);
}

export function CL_ceiling(number, divisor) {
    return values(Math.ceil(number / divisor), number - Math.ceil(number / divisor) * divisor);
}
