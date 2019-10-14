import {
    values1
} from './values';
import {
    S_nil,
    checkString,
    toLispBoolean,
    typeError,
    S_symbol
} from './header';
import {
  makeCons
} from './cons';
import {
  raise
} from './callstack';

class LispSymbol {
    constructor(options) {
        const {
            name,
            packageName
        } = options;
        this.name = name;
        this.packageName = packageName;
        this.value = null;
        this.func = null;
        this.plist = S_nil;
    }

    isBound() {
        return this.value !== null;
    }

    isFBound() {
        return this.func !== null;
    }

    getFunction() {
        if (this.func === null) {
          raise('The function ~S is undefined', this);
        }
        return this.func;
    }

    getValue() {
        if (this.value === null) {
          raise('The variable ~S is unbound', this);
        }
        return this.value;
    }

    getPackageName() {
      return this.packageName;
    }
}

export function isSymbol(x) {
    return x instanceof LispSymbol;
}

export function makeSymbol(name, packageName = null) {
    return new LispSymbol({
        name,
        packageName
    });
}

export function symbolName(symbol) {
    return symbol.name;
}

export function symbolValue(symbol) {
    if (!symbol.isBound()) raise('The variable ~S is unbound', symbol.name);
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

function checkSymbol(x) {
    if (!isSymbol(x)) {
        typeError(x, S_symbol);
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
    if (symbol.plist === undefined) symbol.plist = S_nil;
    return values1(symbol.plist);
}

export function CL_setSymbolPlist(symbol, plist) {
    checkSymbol(symbol);
    symbol.plist = plist;
    return values1(plist);
}

export function CL_put(symbol, key, value) {
    for (let plist = symbol.plist; plist !== S_nil; plist = plist.cdr.cdr) {
        if (plist.car === key) {
            plist.cdr.car = value;
            return values1(value);
        }
    }
    symbol.plist = makeCons(key, makeCons(value, symbol.plist));
    return values1(S_nil);
}

export function CL_boundp(x) {
    checkSymbol(x);
    return values1(toLispBoolean(x.isBound()));
}

export function CL_fboundp(x) {
  checkSymbol(x);
  return values1(toLispBoolean(x.isFBound()));
}

export function CL_symbolPackage(symbol) {
    checkSymbol(symbol);
    return values1(symbol.packageName);
}

export function CL_symbolValue(symbol) {
  checkSymbol(symbol);
  return values1(symbolValue(symbol));
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

export function CL_set(symbol, value) {
  checkSymbol(symbol);
  setSymbolValue(symbol, value);
  return values1(value);
}

export function CL_makunbound(symbol) {
  checkSymbol(symbol);
  symbol.value = null;
  return values1(symbol);
}

export function CL_fmakunbound(symbol) {
  checkSymbol(symbol);
  symbol.func = null;
  return values1(symbol);
}
