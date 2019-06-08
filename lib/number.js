import {
    tValue,
    nilValue,
    typeError,
    toLispBoolean
} from './header';
import {
    values1
} from './values';

function isNumber(x) {
    return (typeof(x) === 'number');
}

function checkNumber(x) {
    if (!isNumber(x)) {
        typeError(x, "Number");
    }
}

export function CL_numberp(x) {
    return values1(toLispBoolean(isNumber(x)));
}

export function CL_integerp(x) {
    return values1(toLispBoolean(isNumber(x) && Number.isInteger(x)));
}

export function CL_add(...args) {
    let sum = 0;
    for (let arg of args) {
        checkNumber(arg);
        sum += arg;
    }
    return values1(sum);
}

export function CL_sub(...args) {
    let acc = args[0];
    for (let i = 1; i < args.length; i++) {
        checkNumber(args[i]);
        acc -= args[i];
    }
    return values1(acc);
}

export function CL_mul(...args) {
    let acc = 1;
    for (let arg of args) {
        checkNumber(arg);
        acc *= arg;
    }
    return values1(acc);
}

export function CL_numberEqual(number, ...numbers) {
    checkNumber(number);
    for (let n of numbers) {
        checkNumber(n);
        if (!(number === n)) {
            return values1(nilValue);
        }
        number = n;
    }
    return values1(tValue);
}

export function CL_greaterThan(number, ...numbers) {
    checkNumber(number);
    for (let n of numbers) {
        checkNumber(n);
        if (!(number > n)) {
            return values1(nilValue);
        }
        number = n;
    }
    return values1(tValue);
}

export function CL_lessThan(number, ...numbers) {
    checkNumber(number);
    for (let n of numbers) {
        checkNumber(n);
        if (!(number < n)) {
            return values1(nilValue);
        }
        number = n;
    }
    return values1(tValue);
}

export function CL_greaterEqual(number, ...numbers) {
    checkNumber(number);
    for (let n of numbers) {
        checkNumber(n);
        if (!(number >= n)) {
            return values1(nilValue);
        }
        number = n;
    }
    return values1(tValue);
}

export function CL_lessEqual(number, ...numbers) {
    checkNumber(number);
    for (let n of numbers) {
        checkNumber(n);
        if (!(number <= n)) {
            return values1(nilValue);
        }
        number = n;
    }
    return values1(tValue);
}
