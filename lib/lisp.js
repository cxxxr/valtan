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

export function CL_apply(fn, args) {
  return fn(...args);
}

export function CL_functionp(x) {
  return values1(toLispBoolean(typeof x === 'function'));
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

export function CL_values(args) {
  current_values = args;
  return args.length === 0 ? nilValue : args[0];
}

export function CL_multipleValueCall(fn, ...args) {
  const vector = [];
  for (let i = 0; i < args.length - 1; i++) {
    vector.push(args[i]);
  }
  return fn(...vector.concat(current_values));
}


//////////////////////////////////////////////////
export function CL_eq(x, y) {
  return values1(toLispBoolean(x === y));
}


//////////////////////////////////////////////////
// error
export function CL_error(string) {
  throw new Error(string);
}

//////////////////////////////////////////////////
// number
export function CL_add(...args) {
  let sum = 0;
  for (let arg of args) {
    checkNumber(arg);
    sum += arg;
  }
  return values1(sum);
}

export function CL_sub(...args) {
  let acc = args[0];
  for (let i = 1; i < args.length; i++) {
    checkNumber(args[i]);
    acc -= args[i];
  }
  return values1(acc);
}

export function CL_mul(...args) {
  let acc = 1;
  for (let arg of args) {
    checkNumber(arg);
    acc *= arg;
  }
  return values1(acc);
}

export function CL_numberEqual(number, ...numbers) {
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

export function CL_greaterThan(number, ...numbers) {
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

export function CL_lessThan(number, ...numbers) {
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

export function CL_greaterEqual(number, ...numbers) {
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

export function CL_lessEqual(number, ...numbers) {
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


//////////////////////////////////////////////////
// structure
export function CL_makeStructure(name, ...args) {
  return values1(new Structure(name, args));
}

export function CL_copyStructure(x) {
  checkStructure(x);
  return values1(x.copy());
}

export function CL_structurep(x) {
  return values1(toLispBoolean(x instanceof Structure));
}

export function CL_structureName(structure) {
  checkStructure(structure);
  return values1(structure.name);
}

export function CL_structureRef(structure, i) {
  checkStructure(structure);
  return values1(structure.getValue(i));
}

export function CL_structureSet(structure, i, value) {
  checkStructure(structure);
  structure.setValue(i, value);
  return values1(value);
}


//////////////////////////////////////////////////
// ffi
export function CL_makeObject(...args) {
  let obj = {};
  for (let i = 0; i < args.length; i += 2) {
    obj[args[i]] = args[i+1];
  }
  return values1(obj);
}

export function CL_instanceof(x, y) {
  return values1(toLispBoolean(x instanceof y));
}
