import {
    values1
} from './values';
import {
    nilValue,
    toLispBoolean,
    typeError
} from './header';

export function isCharacter(x) {
  return (typeof(x) === 'string' || x instanceof String) && x.length === 1;
}

export function makeCharacter(code) {
  return String.fromCharCode(code);
}

export function CL_characterp(x) {
  return values1(toLispBoolean(isCharacter(x)));
}

export function CL_codeChar(code) {
  if (!Number.isInteger(code)) {
    typeError(code, "INTEGER");
  }
  return values1(String.fromCharCode(code));
}

export function CL_charCode(char) {
  if (!isCharacter(char)) {
    typeError(code, "String");
  }
  return values1(char.charCodeAt(0));
}
