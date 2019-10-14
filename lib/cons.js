import {
    values1
} from './values';
import {
    S_nil,
    toLispBoolean,
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
    return cons;
}

export function rplacd(cons, cdr) {
    cons.cdr = cdr;
    return cons;
}

export function jsArrayToList(jsArguments, start = 0) {
    let acc = S_nil;
    for (let i = jsArguments.length - 1; i >= start; i--) {
        acc = makeCons(jsArguments[i], acc);
    }
    return acc;
}

export function listToJsArray(list) {
    const array = new Array();
    for (let x = list; isCons(x); x = x.cdr) {
        array.push(x.car);
    }
    return array;
}

export function CL_consp(x) {
    return values1(toLispBoolean(isCons(x)));
}

export function CL_cons(car, cdr) {
    return values1(makeCons(car, cdr));
}

export function CL_car(list) {
    if (list === S_nil) {
        return values1(S_nil);
    } else {
        return values1(list.car);
    }
}

export function CL_cdr(list) {
    if (list === S_nil) {
        return values1(S_nil);
    } else {
        return values1(list.cdr);
    }
}

export function CL_rplaca(cons, car) {
    return values1(rplaca(cons, car));
}

export function CL_rplacd(cons, cdr) {
    return values1(rplacd(cons, cdr));
}

export function CL_jsArrayToList(array) {
    return values1(jsArrayToList(array));
}

export function CL_listToJsArray(array) {
    return values1(listToJsArray(array));
}