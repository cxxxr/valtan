import {
    values1
} from './values';
import {
  toLispBoolean
} from './header';

export function CL_makeObject(...args) {
    let obj = {};
    for (let i = 0; i < args.length; i += 2) {
        obj[args[i]] = args[i + 1];
    }
    return values1(obj);
}

export function CL_instanceof(x, y) {
    return values1(toLispBoolean(x instanceof y));
}