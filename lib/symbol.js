import { values1 } from './values';
import { nilValue, checkString, toLispBoolean } from './header';

class LispSymbol {
  constructor(options) {
    const {name, packageName} = options;
    this.name = name;
    this.packageName = packageName;
    this.value = null;
    this.func = null;
    this.plist = nilValue;
  }

  toString() {
    return this.name.toString();
  }

  isBound() {
    return this.value !== null;
  }

  isFBound() {
    return this.func !== null;
  }

  getFunction() {
    if (this.func === null) {
      throw new Error(`The function ${this.name} is undefined.`);
    }
    return this.func;
  }

  getValue() {
    if (this.value === null) {
      throw new Error(`The variable ${this.name} is unbound`);
    }
    return this.value;
  }
}

export function isSymbol(x) {
  return x instanceof LispSymbol;
}

export function makeSymbol(name, packageName = null) {
  return new LispSymbol({name, packageName});
}

export function symbolName(symbol) {
  return symbol.name;
}

export function symbolValue(symbol) {
  if (!symbol.isBound()) throw new Error(`The variable ${symbol.name} is unbound`);
  return symbol.value;
}

export function setSymbolValue(symbol, value) {
  symbol.value = value;
  return value;
}

export function setSymbolFunction(symbol, func) {
  symbol.func = func;
  return func;
}

function checkSymbol(value) {
  if (!isSymbol(value)) {
    typeError(value, "SYMBOL");
  }
}

export function CL_symbolp(x) {
  return values1(toLispBoolean(isSymbol(x)));
}

export function CL_makeSymbol(string) {
  checkString(string);
  return values1(makeSymbol(string));
}

export function CL_symbolPlist(symbol) {
  checkSymbol(symbol);
  return values1(symbol.plist);
}

export function CL_setSymbolPlist(symbol, plist) {
  checkSymbol(symbol);
  symbol.plist = plist;
  return values1(plist);
}

export function CL_boundp(x) {
  checkSymbol(x);
  return values1(toLispBoolean(x.isBound()));
}

export function CL_symbolPackage(symbol) {
  checkSymbol(symbol);
  return values1(symbol.packageName);
}

export function CL_symbolFunction(symbol) {
  checkSymbol(symbol);
  return values1(symbol.getFunction());
}

export function CL_setSymbolFunction(symbol, func) {
  checkSymbol(symbol);
  return values1(setSymbolFunction(symbol, func));
}

export function CL_symbolName(symbol) {
  checkSymbol(symbol);
  return values1(symbolName(symbol));
}
