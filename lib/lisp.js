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
    const {name, packageName} = options;
    this.name = name;
    this.packageName = packageName;
    this.value = null;
    this.func = null;
    this.plist = nilValue;
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
    if (this.func === null) {
      throw new Error(`The variable ${this.name} is unbound`);
    }
  }
}

export class Character {
  constructor(code) {
    this.code = code;
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
  if (typeof pkg === 'string') {
    const result = findPackage(pkg);
    if (!result) throw new Error(`The Package ${pkg} is undefined`);
    return result
  } else {
    return pkg;
  }
}

export function intern(name, pkg = current_package.value) {
  return ensurePackage(pkg).intern(name);
}

export function makeSymbol(name) {
  return new LispSymbol({name});
}

function isKeyword(x) {
  if (!(x instanceof LispSymbol)) {
    return false;
  }
  return x.packageName === 'KEYWORD';
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

export function toLispBoolean(x) {
  return x ? tValue : nilValue;
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

export function jsArrayToList(array, start = 0) {
  let acc = nilValue;
  for (let i = array.length - 1; i >= start; i--) {
    acc = cons(array[i], acc);
  }
  return acc;
}

function listToJSArray(list) {
  if (list === nilValue) {
    return [];
  }
  const array = [];
  for (let ls = list; (ls instanceof Cons); ls = ls.cdr) {
    array.push(ls.car);
  }
  return array;
}

function checkNumberOfArguments(args, min, max = min) {
  if (!(min <= args.length && (max === null | args.length <= max))) {
    throw new Error(`invalid number of arguments: ${args.length}`);
  }
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
      return (value instanceof Cons || value === nilValue)
    },
    "LIST");
}

function checkCons(value) {
  checkType(value, typeChecker(Cons), "CONS")
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
  if (typeof(value) !== 'string') {
    typeError(value, "String");
  }
}

function checkCharacter(value) {
  if (!(value instanceof Character)) {
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

function registerFunction(pkg, name, fn) {
  intern(name, pkg).func = fn;
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

registerFunction(cl_package, 'VALUES', values);
registerFunction(cl_package, 'MULTIPLE-VALUE-CALL', multipleValueCall);


//////////////////////////////////////////////////
function eq(x, y) {
  checkNumberOfArguments(arguments, 2);
  return values1(toLispBoolean(x === y));
}

registerFunction(cl_package, 'EQ', eq);
registerFunction(cl_package, 'EQL', eq);
registerFunction(cl_package, 'EQUAL', eq);


//////////////////////////////////////////////////
// error
registerFunction(system_package, '%ERROR', function (string) {
  throw new Error(string);
});


//////////////////////////////////////////////////
// function
function funcall(fn, ...args) {
  checkNumberOfArguments(arguments, 1, null);
  fn = ensureFunction(fn);
  return fn(...args);
}

function apply(fn, arg, ...args) {
  checkNumberOfArguments(arguments, 2, null);
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

registerFunction(cl_package, 'FUNCALL', funcall);
registerFunction(cl_package, 'APPLY', apply);


//////////////////////////////////////////////////
// symbol
registerFunction(cl_package, 'SYMBOLP', function (x) {
  checkNumberOfArguments(arguments, 1);
  return values1(toLispBoolean(x instanceof LispSymbol));
});

registerFunction(cl_package, 'KEYWORDP', function (x) {
  checkNumberOfArguments(arguments, 1);
  return values1(toLispBoolean(isKeyword(x)));
});

registerFunction(cl_package, 'SYMBOL-PLIST', function (symbol) {
  checkNumberOfArguments(arguments, 1);
  checkSymbol(symbol);
  return values1(symbol.plist);
});

registerFunction(system_package, 'PUT-SYMBOL-PLIST', function (symbol, plist) {
  checkNumberOfArguments(arguments, 2);
  checkSymbol(symbol);
  symbol.plist = plist;
  return values1(plist);
});

registerFunction(cl_package, 'BOUNDP', function (symbol) {
  checkNumberOfArguments(arguments, 1);
  checkSymbol(symbol);
  return values1(toLispBoolean(symbol.isBound()));
});

registerFunction(cl_package, 'SYMBOL-FUNCTION', function (symbol) {
  checkNumberOfArguments(arguments, 1);
  checkSymbol(symbol);
  return values1(symbol.getFunction());
});

registerFunction(cl_package, 'SYMBOL-NAME', function (symbol) {
  checkNumberOfArguments(arguments, 1);
  checkSymbol(symbol);
  return values1(symbol.name);
});

registerFunction(system_package, 'FSET', function (symbol, fn) {
  symbol.func = fn;
  return values1(fn);
});


//////////////////////////////////////////////////
// cons
export function consp(x) {
  checkNumberOfArguments(arguments, 1);
  return values1(toLispBoolean(x instanceof Cons));
}

export function cons(car, cdr) {
  checkNumberOfArguments(arguments, 2);
  return values1(new Cons(car, cdr));
}

export function car(cons) {
  checkNumberOfArguments(arguments, 1);
  checkList(cons);
  return values1(cons === nilValue ? nilValue : cons.car);
}

export function cdr(cons) {
  checkNumberOfArguments(arguments, 1);
  checkList(cons);
  return values1(cons === nilValue ? nilValue : cons.cdr);
}

export function rplaca(cons, value) {
  checkNumberOfArguments(arguments, 2);
  checkCons(cons);
  cons.car = value;
  return cons;
}

export function rplacd(cons, value) {
  checkNumberOfArguments(arguments, 2);
  checkCons(cons);
  cons.cdr = value;
  return cons;
}

registerFunction(cl_package, 'CONSP', consp);
registerFunction(cl_package, 'CONS', cons);
registerFunction(cl_package, 'CAR', car);
registerFunction(cl_package, 'CDR', cdr);
registerFunction(cl_package, 'RPLACA', rplaca);
registerFunction(cl_package, 'RPLACD', rplacd);


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
  checkNumberOfArguments(args, 1, null);
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
  checkNumberOfArguments(arguments, 1, null);
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
  checkNumberOfArguments(arguments, 1, null);
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
  checkNumberOfArguments(arguments, 1, null);
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
  checkNumberOfArguments(arguments, 1, null);
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
  checkNumberOfArguments(arguments, 1, null);
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

registerFunction(cl_package, '+', plus);
registerFunction(cl_package, '-', minus);
registerFunction(cl_package, '*', times);
registerFunction(cl_package, '=', numberEqual);
registerFunction(cl_package, '>', gt);
registerFunction(cl_package, '<', lt);
registerFunction(cl_package, '>=', ge);
registerFunction(cl_package, '<=', le);


//////////////////////////////////////////////////
// character
registerFunction(cl_package, 'CHARACTERP', function (x) {
  checkNumberOfArguments(arguments, 1);
  return values1(x instanceof Character);
});

registerFunction(cl_package, 'CHAR-CODE', function (x) {
  checkNumberOfArguments(arguments, 1);
  checkCharacter(x);
  return values1(x.code);
});

registerFunction(cl_package, 'CODE-CHAR', function (x) {
  checkNumberOfArguments(arguments, 1);
  checkType(x, function (x) { return Number.isInteger(x) && x >= 0; }, "positive integer");
  return values1(new Character(x));
});


//////////////////////////////////////////////////
// package
registerFunction(cl_package, 'INTERN', function (name, pkg = current_package.value) {
  checkNumberOfArguments(arguments, 1, 2);
  checkString(name);
  checkPackage(pkg);
  // TODO: 返り値が不十分
  return values1(intern(name, pkg));
});

registerFunction(cl_package, 'PACKAGE-NAME', function (packageDesignator) {
  checkNumberOfArguments(arguments, 1);
  if (packageDesignator instanceof Package) {
    return values1(packageDesignator.name);
  } else if (typeof packageDesignator === 'string') {
    const pkg = findPackage(packageDesignator);
    if (pkg === null) typeError(packageDesignator, 'Package');
    return values1(pkg);
  } else {
    typeError(packageDesignator, 'Package');
  }
});


//////////////////////////////////////////////////
// structure
registerFunction(system_package, 'MAKE-STRUCTURE', function (name, ...args) {
  return values1(new Structure(name, args));
});

registerFunction(cl_package, 'COPY-STRUCTURE', function (x) {
  checkStructure(x);
  return values1(x.copy());
});

registerFunction(system_package, 'STRUCTURE-P', function (x) {
  return values1(x instanceof Structure);
});

registerFunction(system_package, 'STRUCTURE-NAME', function (structure) {
  checkStructure(structure);
  return values1(structure.name);
});

registerFunction(system_package, 'STRUCTURE-REF', function (structure, i) {
  checkStructure(structure);
  return values1(structure.getValue(i));
});

registerFunction(system_package, 'STRUCTURE-SET', function (structure, i, value) {
  checkStructure(structure);
  structure.setValue(i, value);
  return values1(value);
});


//////////////////////////////////////////////////
// ffi
registerFunction(ffi_package, 'MAKE-OBJECT', function (...args) {
  let obj = {};
  for (let i = 0; i < args.length; i += 2) {
    obj[args[i]] = args[i+1];
  }
  return values1(obj);
});

registerFunction(ffi_package, 'INSTANCEOF', function (x, y) {
  return values1(x instanceof y);
});
