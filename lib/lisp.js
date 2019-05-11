export let tValue;
export let nilValue;

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
    this.plist = nilValue;
  }

  isBound() {
    return this.value !== null;
  }
}

class Structure {
  constructor(name, values) {
    this.name = name;
    this.values = values;
  }

  getValue(i) {
    return this.values[i];
  }

  setValue(i, value) {
    this.values[i] = value;
  }
}

export class BlockValue {
  constructor(symbol, value) {
    this.name = symbol;
    this.value = value;
  }
}

export class TagValue {
  constructor(level, index) {
    this.level = level;
    this.index = index;
  }
}

export class CatchValue {
  constructor(symbol, value) {
    this.symbol = symbol;
    this.value = value;
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

let current_values = [];

export function values1(x) {
  current_values = [x];
  return x;
}

function values(...args) {
  current_values = args;
  return args.length === 0 ? lisp.nilValue : args[0];
}

function multipleValueCall(fn, ...args) {
    const vector = [];
    for (let i = 0; i < args.length - 1; i++) {
      vector.push(args[i]);
    }
    return fn(...vector.concat(current_values));
}

const cl_package = makePackage('COMMON-LISP', { nicknames: ['CL'] });
const system_package = makePackage('SYSTEM');
const ffi_package = makePackage('FFI');
const keyword_package = makePackage('KEYWORD');
const cl_user_package = makePackage('COMMON-LISP-USER', { nicknames: ['CL-USER'] });

current_package = cl_user_package;

tValue = intern('T', cl_package);
tValue.value = tValue;

nilValue = intern('NIL', cl_package);
nilValue.value = nilValue;

export function symbolPackage(symbol) {
  for (let pkg of packages) {
    if (pkg.findSymbol(symbol.name) === symbol) {
      return pkg;
    }
  }
}

function checkArgs(args, min, max = min) {
  if (args.length < min || max < args.length) {
    throw new Error(`invalid number of arguments: ${args.length}`);
  }
}

function checkType(value, assert, name) {
  if (!assert(value)) {
    throw new Error(`The value ${value} is not of the expected type ${name}`);
  }
}

function makeCheckType(type) {
  return (function(value) {
    return (value instanceof type);
  });
}

function checkList(value) {
  checkType(
    value,
    function(value) {
      return (value instanceof Cons || value === nilValue)
    },
    "LIST");
}

function checkCons(value) {
  checkType(value, makeCheckType(Cons), "CONS")
}

function checkSymbol(value) {
  checkType(value, makeCheckType(LispSymbol), "SYMBOL");
}

export function consp(x) {
  checkArgs(arguments, 1);
  return values1((x instanceof Cons) ? tValue : nilValue);
}

export function cons(car, cdr) {
  checkArgs(arguments, 2);
  return values1(new Cons(car, cdr));
}

export function car(cons) {
  checkArgs(arguments, 1);
  checkList(cons);
  return values1(cons === nilValue ? nilValue : cons.car);
}

export function cdr(cons) {
  checkArgs(arguments, 1);
  checkList(cons);
  return values1(cons === nilValue ? nilValue : cons.cdr);
}

export function rplaca(cons, value) {
  checkArgs(arguments, 2);
  checkCons(cons);
  cons.car = value;
  return cons;
}

export function rplacd(cons, value) {
  checkArgs(arguments, 2);
  checkCons(cons);
  cons.cdr = value;
  return cons;
}

cl_package.exportSymbol(intern('CONSP', cl_package).func = consp);
cl_package.exportSymbol(intern('CONS', cl_package).func = cons);
cl_package.exportSymbol(intern('CAR', cl_package).func = car);
cl_package.exportSymbol(intern('CDR', cl_package).func = cdr);
cl_package.exportSymbol(intern('RPLACA', cl_package).func = rplaca);
cl_package.exportSymbol(intern('RPLACD', cl_package).func = rplacd);

cl_package.exportSymbol(
  intern('FUNCALL', cl_package).func = function (fn, ...args) {
    return fn(...args);
  }
);

cl_package.exportSymbol(
  intern('+', cl_package).func = function (...args) {
    let sum = 0;
    for (let arg of args) {
      sum += arg;
    }
    return values1(sum);
  }
);

cl_package.exportSymbol(
  intern('-', cl_package).func = function (...args) {
    let acc = args[0];
    for (let i = 1; i < args.length; i++) {
      acc -= args[i];
    }
    return values1(acc);
  }
);

cl_package.exportSymbol(
  intern('*', cl_package).func = function (...args) {
    let acc = 1;
    for (let arg of args) {
      acc *= arg;
    }
    return values1(acc);
  }
);

cl_package.exportSymbol(
  intern('=', cl_package).func = function (x, y) {
    return values1(x === y ? tValue : nilValue);
  }
);

cl_package.exportSymbol(
  intern('>', cl_package).func = function (x, y) {
    return values1(x > y ? tValue : nilValue);
  }
);

cl_package.exportSymbol(
  intern('<', cl_package).func = function (x, y) {
    return values1(x < y ? tValue : nilValue);
  }
);

cl_package.exportSymbol(
  intern('>=', cl_package).func = function (x, y) {
    return values1(x >= y ? tValue : nilValue);
  }
);

cl_package.exportSymbol(
  intern('<=', cl_package).func = function (x, y) {
    return values1(x <= y ? tValue : nilValue);
  }
);

cl_package.exportSymbol(
  intern('EQ', cl_package).func = function (x, y) {
    return values1(x === y ? tValue : nilValue);
  }
);

cl_package.exportSymbol(
  intern('EQL', cl_package).func = function (x, y) {
    return values1(x === y ? tValue : nilValue);
  }
);

cl_package.exportSymbol(intern('VALUES', cl_package).func = values);
cl_package.exportSymbol(intern('MULTIPLE-VALUE-CALL', cl_package).func = multipleValueCall);

cl_package.exportSymbol(
  intern('SYMBOL-PLIST', cl_package).func = function (symbol) {
    checkArgs(arguments, 1);
    checkSymbol(symbol);
    return values1(symbol.plist);
  }
);

cl_package.exportSymbol(
  intern('PUT-SYMBOL-PLIST', system_package).func = function (symbol, plist) {
    checkArgs(arguments, 2);
    checkSymbol(symbol);
    symbol.plist = plist;
    return values1(plist);
  }
);

cl_package.exportSymbol(
  intern('BOUNDP', cl_package).func = function (symbol) {
    checkSymbol(symbol);
    return values1(symbol.isBound() ? tValue : nilValue);
  }
);

cl_package.exportSymbol(
  intern('SYMBOL-FUNCTION', cl_package).func = function (symbol) {
    checkSymbol(symbol);
    return values1(symbol.func);
  }
);

intern('FSET', system_package).func = function (symbol, fn) {
  symbol.func = fn;
  return values1(fn);
}

intern('%ERROR', system_package).func = function (string) {
  throw new Error(string);
}

intern('MAKE-STRUCTURE', system_package).func = function (name, ...args) {
  return values1(new Structure(name, args));
}

intern('STRUCTURE-REF', system_package).func = function (structure, i) {
  return values1(structure.getValue(i));
}

intern('STRUCTURE-SET', system_package).func = function (structure, i, value) {
  structure.setValue(i, value);
  return values1(value);
}

intern('MAKE-OBJECT', ffi_package).func = function (...args) {
  let obj = {};
  for (let i = 0; i < args.length; i += 2) {
    obj[args[i]] = args[i+1];
  }
  return values1(obj);
}

intern('OBJECT-GET', ffi_package).func = function (obj, key) {
  return values1(obj[key]);
}

intern('OBJECT-SET', ffi_package).func = function (obj, key, value) {
  obj[key] = value;
  return values1(value);
}

intern('INSTANCEOF', ffi_package).func = function (x, y) {
  return values1(x instanceof y);
}

export function jsArrayToList(array, start = 0) {
  let acc = nilValue;
  for (let i = array.length - 1; i >= start; i--) {
    acc = cons(array[i], acc);
  }
  return acc;
}
