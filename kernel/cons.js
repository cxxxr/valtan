import {
    values1,
    restoreValues
} from './values';
import {
    S_nil,
    toLispBoolean,
} from './header';
import { typeError } from './error';
import { intern, cl_package } from './package';

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

// VALUES-LIST needs a stable value snapshot, not an APPLY argument list.
// Validate the proper list while collecting its values, including cycles.
export function CL_valuesList(list) {
    const result = [];
    let cursor = list, slow = list;
    const improper = () => typeError(list, jsArrayToList([
        intern('SATISFIES', cl_package), intern('PROPER-LIST-P', cl_package)
    ]));
    while (cursor !== S_nil) {
        if (!isCons(cursor)) return improper();
        result.push(cursor.car);
        cursor = cursor.cdr;
        if (result.length % 2 === 0) slow = slow.cdr;
        if (cursor !== S_nil && cursor === slow) return improper();
    }
    restoreValues(result);
    return result.length ? result[0] : S_nil;
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

// Public accessors keep their Lisp type checks even when the compiler bypasses
// the Lisp wrapper. Internal %CAR/%CDR retain their existing unchecked ABI.
export function CL_checkedCar(list) {
    if (list === S_nil) return values1(S_nil);
    if (!isCons(list)) typeError(list, intern('LIST', cl_package));
    return values1(list.car);
}

export function CL_checkedCdr(list) {
    if (list === S_nil) return values1(S_nil);
    if (!isCons(list)) typeError(list, intern('LIST', cl_package));
    return values1(list.cdr);
}

// ELT must validate a proper list and its index. Do both in one native walk,
// retaining the selected cons for reads and SETF without repeated Lisp LENGTH
// and NTH traversals. The slow pointer also rejects circular lists.
export function CL_listEltCell(list, index) {
    const validIndex = (typeof index === 'number' && Number.isInteger(index) && index >= 0)
        || (typeof index === 'bigint' && index >= 0n);
    const wanted = validIndex ? Number(index) : -1;
    let cursor = list, slow = list, size = 0, selected = null;
    const improper = () => typeError(list, jsArrayToList([
        intern('SATISFIES', cl_package), intern('PROPER-LIST-P', cl_package)
    ]));
    while (cursor !== S_nil) {
        if (!isCons(cursor)) return improper();
        if (size === wanted) selected = cursor;
        cursor = cursor.cdr;
        size++;
        if (size % 2 === 0) slow = slow.cdr;
        if (cursor !== S_nil && cursor === slow) return improper();
    }
    if (!selected) return typeError(index, jsArrayToList([
        intern('INTEGER', cl_package), 0, jsArrayToList([size])
    ]));
    return values1(selected);
}

export function CL_null(x) { return values1(toLispBoolean(x === S_nil)); }
export function CL_listp(x) { return values1(toLispBoolean(x === S_nil || isCons(x))); }
export function CL_atom(x) { return values1(toLispBoolean(!isCons(x))); }

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
