import {
    values1
} from './values';
import {
    toLispBoolean, S_nil,
} from './header';
import { typeError } from './error';
import { intern, cl_package } from './package';

export function isCharacter(x) {
    return (typeof(x) === 'string' || x instanceof String) &&
        (x.length === 1 || x.length === 2 && x.codePointAt(0) > 0xffff);
}

export function makeCharacter(code) {
    return String.fromCodePoint(code);
}

export function CL_characterp(x) {
    return values1(toLispBoolean(isCharacter(x)));
}

export function CL_codeChar(code) {
    return values1(code >= 0 && code < 0x110000 ? String.fromCodePoint(Number(code)) : S_nil);
}

export function CL_charCode(char) {
    return values1(char.codePointAt(0));
}

export function CL_charEqual(x, y) {
    if (!isCharacter(x)) typeError(x, intern('CHARACTER', cl_package));
    if (!isCharacter(y)) typeError(y, intern('CHARACTER', cl_package));
    return values1(toLispBoolean(x.codePointAt(0) === y.codePointAt(0)));
}
