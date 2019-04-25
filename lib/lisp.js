class Cons {
  constructor(car, cdr) {
    this.car = car;
    this.cdr = cdr;
  }
}

class LispSymbol {
  constructor(options) {
    const {name} = options;
    this.name = name;
    this.value = null;
    this.func = null;
  }

  isBound() {
    return this.value !== null;
  }
}

export class BlockValue {
  constructor(symbol, value) {
    this.name = symbol;
    this.value = value;
  }
}

class Macro {
  constructor(func) {
    this.func = func;
  }
}

class Package {
  constructor(options = {}) {
    const {name, nicknames, use} = options;
    this.name = name;
    this.nicknames = nicknames || [];
    this.usePackages = use || [];
    this.symbolTable = {};
    this.exportTable = {};
  }

  is(name) {
    if (this.name === name) {
      return true;
    }
    for (let nickname of this.nicknames) {
      if (nickname === name) {
        return true;
      }
    }
    return false;
  }

  exportSymbol(symbol) {
    this.exportTable[symbol] = true;
  }

  findSymbol(name, sanePackage = this) {
    for (pkg of this.usePackages) {
      const s = pkg.findSymbol(name, sanePackage);
      if (s) {
        return s;
      }
    }
    if (this === sanePackage) {
      return this.symbolTable[name];
    } else if (this.symbolTable[name] && this.exportTable[name]) {
      return this.symbolTable[name];
    } else {
      return null;
    }
  }

  intern(name) {
    return this.findSymbol(name) || (this.symbolTable[name] = new LispSymbol({name}));
  }
}

const packages = [];
let current_package;

export function findPackage(name) {
  for (let p of packages) {
    if (p.is(name)) {
      return p;
    }
  }
  return null;
}

export function makePackage(name, options) {
  const p = new Package({name, ...options});
  packages.push(p);
  return p;
}

function ensurePackage(pkg) {
  if (typeof pkg === 'string') {
    const result = findPackage(pkg);
    if (!result) throw new Error(`The Package ${pkg} is undefined`);
    return result
  } else {
    return pkg;
  }
}

export function intern(name, pkg = current_package) {
  return ensurePackage(pkg).intern(name);
}

export function call_function(symbol, ...args) {
  if (symbol.func === null) throw new Error(`The function ${symbol.name} is undefined`);
  return symbol.func(...args);
}

export function symbol_value(symbol) {
  if (!symbol.isBound()) throw new Error(`The variable ${symbol.name} is unbound`);
  return symbol.value;
}

export function set_symbol_value(symbol, value) {
  symbol.value = value;
  return value;
}

const cl_package = makePackage('COMMON-LISP', { nicknames: ['CL'] });
const system_package = makePackage('SYSTEM');
const keyword_package = makePackage('KEYWORD');
const cl_user_package = makePackage('COMMON-LISP-USER', { nicknames: ['CL-USER'] });

current_package = cl_user_package;

export const tValue = intern('T', cl_package);
tValue.value = tValue;

export const nilValue = intern('NIL', cl_package);
nilValue.value = nilValue;

cl_package.exportSymbol(
  intern('+', cl_package).func = (...args) => {
    let sum = 0;
    for (let arg of args) {
      sum += arg;
    }
    return sum;
  }
);

cl_package.exportSymbol(
  intern('-', cl_package).func = (...args) => {
    let acc = args[0];
    for (let i = 1; i < args.length; i++) {
      acc -= args[i];
    }
    return acc;
  }
);

cl_package.exportSymbol(
  intern('*', cl_package).func = (...args) => {
    let acc = 1;
    for (let arg of args) {
      acc *= arg;
    }
    return acc;
  }
);

cl_package.exportSymbol(
  intern('=', cl_package).func = (x, y) => {
    return x === y ? tValue : nilValue;
  }
);

let multiple_values = null;

cl_package.exportSymbol(
  intern('VALUES', cl_package).func = (...args) => {
    if (args.length === 0) {
      return nilValue;
    } else {
      multiple_values = args;
      return args[0];
    }
  }
);

cl_package.exportSymbol(
  intern('BOUNDP', cl_package).func = (symbol) => {
    return symbol.isBound() ? tValue : nilValue;
  }
);

cl_package.exportSymbol(
  intern('SYMBOL-FUNCTION', cl_package).func = (symbol) => {
    return symbol.func;
  }
);

cl_package.exportSymbol(
  intern('MULTIPLE-VALUE-CALL', cl_package).func = (fn, ...args) => {
    const vector = [];
    for (let i = 0; i < args.length - 1; i++) {
      vector.push(args[i]);
    }
    return fn(...vector.concat(multiple_values));
  }
);

intern('CONSOLE.LOG', system_package).func = (...args) => {
  console.log(...args);
}

intern('FSET', system_package).func = (symbol, fn) => {
  symbol.func = fn;
}

intern('ADD-GLOBAL-MACRO', system_package).func = (symbol, fn) => {
  symbol.func = new Macro(fn);
}
