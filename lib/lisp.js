import {makeCharacter, isCharacter, charCode} from 'character';
export * from 'character';

import {makeCons, isCons, car, cdr, setCar, setCdr} from 'cons';
export * from 'cons';

export let tValue;
export let nilValue;

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

  copy() {
    return new Structure(this.name, this.values);
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
    return this.findSymbol(name) || (this.symbolTable[name] = new LispSymbol({name, packageName: this.name}));
  }
}

const packageNameTable = {};
const packages = [];
let current_package;

function findPackage(name) {
  for (let p of packages) {
    if (p.is(name)) {
      return p;
    }
  }
  return null;
}

function alreadyExistPackageName(name, nicknames) {
  if (packageNameTable[name]) {
    return name;
  }
  for (let n of nicknames) {
    if (packageNameTable[n]) {
      return n;
    }
  }
  return false;
}

function makePackage(name, options = {}) {
  const {nicknames = [], use} = options;
  const duplicateName = alreadyExistPackageName(name, nicknames);
  if (duplicateName) {
    throw new Error(`Package ${duplicateName} already exists`);
  }
  const p = new Package({name, nicknames, use});
  packages.push(p);
  return p;
}

function ensurePackage(pkg) {
  if (pkg instanceof Package) {
    return pkg;
  }

  if (pkg instanceof LispSymbol) {
    pkg = pkg.name;
  }

  if (isString(pkg)) {
    const result = findPackage(pkg);
    if (!result) throw new Error(`The Package ${pkg} is undefined`);
    return result;
  }

  typeError(pkg, 'Package');
}

export function intern(name, pkg = current_package.value) {
  return ensurePackage(pkg).intern(name);
}

export function makeSymbol(name) {
  return new LispSymbol({name});
}

export function callFunction(symbol, ...args) {
  const func = symbol.getFunction();
  return func(...args);
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

export function toLispBoolean(x) {
  return x ? tValue : nilValue;
}

function isString(x) {
  return typeof x === 'string' || x instanceof String;
}

function isSymbol(x) {
  return x instanceof LispSymbol;
}

const cl_package = makePackage('COMMON-LISP', { nicknames: ['CL'] });
const system_package = makePackage('SYSTEM');
const ffi_package = makePackage('FFI');
const keyword_package = makePackage('KEYWORD');
const cl_user_package = makePackage('COMMON-LISP-USER', { nicknames: ['CL-USER'] });
current_package = intern('*PACKAGE*', cl_package);
current_package.value = cl_user_package;

tValue = intern('T', cl_package);
tValue.value = tValue;

nilValue = intern('NIL', cl_package);
nilValue.value = nilValue;
nilValue.plist = nilValue;

export function jsArrayToList(array, start = 0) {
  let acc = nilValue;
  for (let i = array.length - 1; i >= start; i--) {
    acc = makeCons(array[i], acc);
  }
  return acc;
}

function listToJSArray(list) {
  if (list === nilValue) {
    return [];
  }
  const array = [];
  for (let ls = list; (isCons(ls)); ls = ls.cdr) {
    array.push(ls.car);
  }
  return array;
}

function argumentsError(name, args) {
  throw new Error(`${name} is invalid number of arguments for ${name}: ${args.length}`);
}

function typeError(value, typeName) {
  throw new Error(`The value ${value} is not of the expected type ${typeName}`);
}

function checkType(value, assert, name) {
  if (!assert(value)) {
    typeError(value, name);
  }
}

function typeChecker(type) {
  return (function(value) {
    return (value instanceof type);
  });
}

function checkList(value) {
  checkType(
    value,
    function(value) {
      return (isCons(value) || value === nilValue)
    },
    "LIST");
}

function checkCons(value) {
  if (!isCons(value)) {
    typeError(value, "CONS");
  }
}

function checkSymbol(value) {
  checkType(value, typeChecker(LispSymbol), "SYMBOL");
}

function checkPackage(value) {
  checkType(value, typeChecker(Package), "Package");
}

function checkStructure(value) {
  checkType(value, typeChecker(Structure), "STRUCTURE");
}

function checkNumber(value) {
  if (typeof(value) !== 'number') {
    typeError(value, "Number");
  }
}

function checkInteger(value) {
  if (!Number.isInteger(value)) {
    typeError(value, "integer");
  }
}

function checkString(value) {
  if (!isString(value)) {
    typeError(value, "String");
  }
}

function checkCharacter(value) {
  if (!isCharacter(value)) {
    typeError(value, "Character");
  }
}

function ensureFunction(x) {
  if (x instanceof Function) {
    return x;
  } else if (x instanceof LispSymbol) {
    return x.getFunction();
  } else {
    typeError(x, "Function");
  }
}

function registerFunction(pkg, name, fn, min = 0, max = min) {
  if (!min) {
    intern(name, pkg).func = function () {
      return fn(...arguments);
    }
  } else if (min === max) {
    intern(name, pkg).func = function () {
      if (arguments.length !== min) argumentsError(name, arguments);
      return fn(...arguments);
    }
  } else if (!max) {
    intern(name, pkg).func = function () {
      if (arguments.length < min) {
        argumentsError(name, arguments);
      }
      return fn(...arguments);
    }
  } else {
    intern(name, pkg).func = function () {
      if (arguments.length < min || max < arguments.length) {
        argumentsError(name, arguments);
      }
      return fn(...arguments);
    }
  }
}


//////////////////////////////////////////////////
// values
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
  fn = ensureFunction(fn);
  const vector = [];
  for (let i = 0; i < args.length - 1; i++) {
    vector.push(args[i]);
  }
  return fn(...vector.concat(current_values));
}

registerFunction(cl_package, 'VALUES', values, 0, null);
registerFunction(cl_package, 'MULTIPLE-VALUE-CALL', multipleValueCall, 1, null);


//////////////////////////////////////////////////
function eq(x, y) {
  return values1(toLispBoolean(x === y));
}

registerFunction(cl_package, 'EQ', eq, 2);
registerFunction(cl_package, 'EQL', eq, 2);
registerFunction(cl_package, 'EQUAL', eq, 2);


//////////////////////////////////////////////////
// error
registerFunction(system_package, '%ERROR', function (string) {
  throw new Error(string);
}, 1);


//////////////////////////////////////////////////
// function
function funcall(fn, ...args) {
  fn = ensureFunction(fn);
  return fn(...args);
}

function apply(fn, arg, ...args) {
  fn = ensureFunction(fn);
  if (args.length === 0) {
    checkList(arg);
    const tmp = listToJSArray(arg);
    return fn(...tmp);
  }
  const vector = [arg];
  const last = args[args.length - 1];
  checkList(last);
  for (let i = 0; i < args.length - 1; i++) {
    vector.push(args[i]);
  }
  return fn(...vector.concat(listToJSArray(last)));
}

registerFunction(cl_package, 'FUNCALL', funcall, 1, null);
registerFunction(cl_package, 'APPLY', apply, 2, null);


//////////////////////////////////////////////////
// symbol
registerFunction(cl_package, 'SYMBOLP', function (x) {
  return values1(toLispBoolean(isSymbol(x)));
}, 1);

registerFunction(cl_package, 'MAKE-SYMBOL', function (string) {
  checkString(name);
  return values1(makeSymbol(name));
}, 1);

registerFunction(cl_package, 'SYMBOL-PLIST', function (symbol) {
  checkSymbol(symbol);
  return values1(symbol.plist);
}, 1);

registerFunction(system_package, 'PUT-SYMBOL-PLIST', function (symbol, plist) {
  checkSymbol(symbol);
  symbol.plist = plist;
  return values1(plist);
}, 2);

registerFunction(cl_package, 'BOUNDP', function (symbol) {
  checkSymbol(symbol);
  return values1(toLispBoolean(symbol.isBound()));
}, 1);

registerFunction(cl_package, 'SYMBOL-FUNCTION', function (symbol) {
  checkSymbol(symbol);
  return values1(symbol.getFunction());
}, 1);

registerFunction(cl_package, 'SYMBOL-NAME', function (symbol) {
  checkSymbol(symbol);
  return values1(symbol.name);
}, 1);

registerFunction(cl_package, 'SYMBOL-PACKAGE', function (symbol) {
  checkSymbol(symbol);
  return values1(findPackage(symbol.packageName));
}, 1);

registerFunction(system_package, 'FSET', function (symbol, fn) {
  symbol.func = fn;
  return values1(fn);
}, 2);


//////////////////////////////////////////////////
// cons
export function CL_consp(x) {
  return values1(toLispBoolean(isCons(x)));
}

export function CL_cons(car, cdr) {
  return values1(makeCons(car, cdr));
}

export function CL_car(cons) {
  checkList(cons);
  return values1(cons === nilValue ? nilValue : cons.car);
}

export function CL_cdr(cons) {
  checkList(cons);
  return values1(cons === nilValue ? nilValue : cons.cdr);
}

export function CL_rplaca(cons, value) {
  checkCons(cons);
  setCar(cons, value);
  return cons;
}

export function CL_rplacd(cons, value) {
  checkCons(cons);
  setCdr(cons, value);
  return cons;
}

registerFunction(cl_package, 'CONSP', CL_consp, 1);
registerFunction(cl_package, 'CONS', CL_cons, 2);
registerFunction(cl_package, 'CAR', CL_car, 1);
registerFunction(cl_package, 'CDR', CL_cdr, 1);
registerFunction(cl_package, 'RPLACA', CL_rplaca, 2);
registerFunction(cl_package, 'RPLACD', CL_rplacd, 2);


//////////////////////////////////////////////////
// number
function plus(...args) {
  let sum = 0;
  for (let arg of args) {
    checkNumber(arg);
    sum += arg;
  }
  return values1(sum);
}

function minus(...args) {
  let acc = args[0];
  for (let i = 1; i < args.length; i++) {
    checkNumber(args[i]);
    acc -= args[i];
  }
  return values1(acc);
}

function times(...args) {
  let acc = 1;
  for (let arg of args) {
    checkNumber(arg);
    acc *= arg;
  }
  return values1(acc);
}

function numberEqual(number, ...numbers) {
  checkNumber(number);
  for (let n of numbers) {
    checkNumber(n);
    if (!(number === n)) {
      return values1(nilValue);
    }
    number = n;
  }
  return values1(tValue);
}

function gt(number, ...numbers) {
  checkNumber(number);
  for (let n of numbers) {
    checkNumber(n);
    if (!(number > n)) {
      return values1(nilValue);
    }
    number = n;
  }
  return values1(tValue);
}

function lt(number, ...numbers) {
  checkNumber(number);
  for (let n of numbers) {
    checkNumber(n);
    if (!(number < n)) {
      return values1(nilValue);
    }
    number = n;
  }
  return values1(tValue);
}

function ge(number, ...numbers) {
  checkNumber(number);
  for (let n of numbers) {
    checkNumber(n);
    if (!(number >= n)) {
      return values1(nilValue);
    }
    number = n;
  }
  return values1(tValue);
}

function le(number, ...numbers) {
  checkNumber(number);
  for (let n of numbers) {
    checkNumber(n);
    if (!(number <= n)) {
      return values1(nilValue);
    }
    number = n;
  }
  return values1(tValue);
}

registerFunction(cl_package, '+', plus, 0, null);
registerFunction(cl_package, '-', minus, 1, null);
registerFunction(cl_package, '*', times, 0, null);
registerFunction(cl_package, '=', numberEqual, 1, null);
registerFunction(cl_package, '>', gt, 1, null);
registerFunction(cl_package, '<', lt, 1, null);
registerFunction(cl_package, '>=', ge, 1, null);
registerFunction(cl_package, '<=', le, 1, null);


//////////////////////////////////////////////////
// character
registerFunction(cl_package, 'CHARACTERP', function (x) {
  return values1(isCharacter(x));
}, 1);

registerFunction(cl_package, 'CHAR-CODE', function (x) {
  checkCharacter(x);
  return values1(charCode(x));
}, 1);

registerFunction(cl_package, 'CODE-CHAR', function (x) {
  checkType(x, function (x) { return Number.isInteger(x) && x >= 0; }, "positive integer");
  return values1(makeCharacter(x));
}, 1);


//////////////////////////////////////////////////
// string
registerFunction(cl_package, 'STRINGP', function (x) {
  return values1(isString(x));
}, 1);


//////////////////////////////////////////////////
// package
registerFunction(cl_package, 'INTERN', function (name, pkg = current_package.value) {
  checkString(name);
  checkPackage(pkg);
  // TODO: 返り値が不十分
  return values1(intern(name, pkg));
}, 1, 2);

registerFunction(cl_package, 'PACKAGE-NAME', function (packageDesignator) {
  return values1(ensurePackage(packageDesignator).name);
}, 1);

registerFunction(cl_package, 'FIND-PACKAGE', function (packageDesignator) {
  return values1(ensurePackage(packageDesignator));
}, 1);


//////////////////////////////////////////////////
// structure
registerFunction(system_package, 'MAKE-STRUCTURE', function (name, ...args) {
  return values1(new Structure(name, args));
}, 1, null);

registerFunction(cl_package, 'COPY-STRUCTURE', function (x) {
  checkStructure(x);
  return values1(x.copy());
}, 1);

registerFunction(system_package, 'STRUCTURE-P', function (x) {
  return values1(toLispBoolean(x instanceof Structure));
}, 1);

registerFunction(system_package, 'STRUCTURE-NAME', function (structure) {
  checkStructure(structure);
  return values1(structure.name);
}, 1);

registerFunction(system_package, 'STRUCTURE-REF', function (structure, i) {
  checkStructure(structure);
  return values1(structure.getValue(i));
}, 2);

registerFunction(system_package, 'STRUCTURE-SET', function (structure, i, value) {
  checkStructure(structure);
  structure.setValue(i, value);
  return values1(value);
}, 3);


//////////////////////////////////////////////////
// ffi
registerFunction(ffi_package, 'MAKE-OBJECT', function (...args) {
  let obj = {};
  for (let i = 0; i < args.length; i += 2) {
    obj[args[i]] = args[i+1];
  }
  return values1(obj);
}, 0, null);

registerFunction(ffi_package, 'INSTANCEOF', function (x, y) {
  return values1(x instanceof y);
}, 2);
