import {
    values1
} from './values';
import {
    nilValue,
    tValue,
    toLispBoolean,
    typeError
} from './header';

export function isCharacter(x) {
  return (typeof(x) === 'string' || x instanceof String) && x.length === 1;
}

export function makeCharacter(code) {
  return String.fromCharCode(code);
}

function checkCharacter(x) {
  if (!isCharacter(x)) {
    typeError(x, "CHARACTER");
  }
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
    typeError(char, "Character");
  }
  return values1(char.charCodeAt(0));
}

export function CL_charEql(char, ...characters) {
  checkCharacter(char);
  for (let c of characters) {
    checkCharacter(c);
    if (!(char === c)) {
      return values1(nilValue);
    }
  }
  return values1(tValue);
}

export function CL_charNotEql(char, ...characters) {
  checkCharacter(char);
  for (let c of characters) {
    checkCharacter(c);
    if (!(char !== c)) {
      return values1(nilValue);
    }
  }
  return values1(tValue);
}

export function CL_charLessThan(char, ...characters) {
  checkCharacter(char);
  for (let c of characters) {
    checkCharacter(c);
    if (!(char < c)) {
      return values1(nilValue);
    }
    char = c;
  }
  return values1(tValue);
}

export function CL_charGreaterThan(char, ...characters) {
  checkCharacter(char);
  for (let c of characters) {
    checkCharacter(c);
    if (!(char > c)) {
      return values1(nilValue);
    }
    char = c;
  }
  return values1(tValue);
}

export function CL_charLessEqual(char, ...characters) {
  checkCharacter(char);
  for (let c of characters) {
    checkCharacter(c);
    if (!(char <= c)) {
      return values1(nilValue);
    }
    char = c;
  }
  return values1(tValue);
}

export function CL_charGreaterEqual(char, ...characters) {
  checkCharacter(char);
  for (let c of characters) {
    checkCharacter(c);
    if (!(char >= c)) {
      return values1(nilValue);
    }
    char = c;
  }
  return values1(tValue);
}
