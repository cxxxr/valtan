import { nilValue } from './header';

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

export function isSymbol(x) {
  return x instanceof LispSymbol;
}
