import {
    values1
} from './values';
import {
    nilValue,
    toLispBoolean,
    typeError
} from './header';

class Cons {
    constructor(car, cdr) {
        this.car = car;
        this.cdr = cdr;
    }
}

export function isCons(x) {
    return x instanceof Cons;
}

export function makeCons(car, cdr) {
    return new Cons(car, cdr);
}

export function rplaca(cons, car) {
    cons.car = car;
    return car;
}

export function rplacd(cons, cdr) {
    cons.cdr = cdr;
    return cdr;
}

export function jsArrayToList(jsArguments, start = 0) {
    let acc = nilValue;
    for (let i = jsArguments.length - 1; i >= start; i--) {
        acc = makeCons(jsArguments[i], acc);
    }
    return acc;
}

function checkCons(x) {
    if (!isCons(x)) {
        typeError(x, "CONS");
    }
}

function checkList(x) {
    if (!isCons(x) && x !== nilValue) {
        typeError(x, "LIST");
    }
}

export function CL_consp(x) {
    return values1(toLispBoolean(isCons(x)));
}

export function CL_cons(car, cdr) {
    return values1(makeCons(car, cdr));
}

export function CL_car(list) {
    checkList(list);
    if (list === nilValue) {
        return values1(nilValue);
    } else {
        return values1(list.car);
    }
}

export function CL_cdr(list) {
    checkList(list);
    if (list === nilValue) {
        return values1(nilValue);
    } else {
        return values1(list.cdr);
    }
}

export function CL_rplaca(cons, car) {
    checkCons(cons);
    return values1(rplaca(cons, car));
}

export function CL_rplacd(cons, cdr) {
    checkCons(cons);
    return values1(rplacd(cons, cdr));
}