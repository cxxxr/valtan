import {
  tValue, nilValue, initBoolean, argumentsError, typeError, checkType, typeChecker,
  toLispBoolean
} from './header';

export { tValue, nilValue } from './header';
import { values1 } from './values';
export * from './values';
import { makeSymbol, symbolName, isSymbol } from './symbol';
export * from './symbol';
export * from './ffi';
export * from './structure';
export * from './number';
export * from './control';

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
    return this.findSymbol(name) || (this.symbolTable[name] = makeSymbol(name, this.name));
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

  if (isSymbol(pkg)) {
    pkg = symbolName(pkg);
  }

  if (isString(pkg)) {
    const result = findPackage(pkg);
    if (!result) throw new Error(`The Package ${pkg} is undefined`);
    return result;
  }

  typeError(pkg, "Package");
}

export function intern(name, pkg = current_package.value) {
  return ensurePackage(pkg).intern(name);
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

function isString(x) {
  return typeof x === 'string' || x instanceof String;
}

const cl_package = makePackage('COMMON-LISP', { nicknames: ['CL'] });
const system_package = makePackage('SYSTEM');
const ffi_package = makePackage('FFI');
const keyword_package = makePackage('KEYWORD');
const cl_user_package = makePackage('COMMON-LISP-USER', { nicknames: ['CL-USER'] });
current_package = intern('*PACKAGE*', cl_package);
current_package.value = cl_user_package;

initBoolean(intern('T', cl_package), intern('NIL', cl_package));

function checkSymbol(value) {
  if (!isSymbol(value)) {
    typeError(value, "SYMBOL");
  }
}

function checkPackage(value) {
  checkType(value, typeChecker(Package), "Package");
}

function checkString(value) {
  if (!isString(value)) {
    typeError(value, "String");
  }
}

function ensureFunction(x) {
  if (x instanceof Function) {
    return x;
  } else if (isSymbol(x)) {
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
// symbol
registerFunction(cl_package, 'SYMBOLP', function (x) {
  return values1(toLispBoolean(isSymbol(x)));
}, 1);

registerFunction(system_package, '%MAKE-SYMBOL', function (string) {
  checkString(string);
  return values1(makeSymbol(string));
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
// package
registerFunction(cl_package, 'PACKAGE-NAME', function (packageDesignator) {
  return values1(ensurePackage(packageDesignator).name);
}, 1);

registerFunction(cl_package, 'FIND-PACKAGE', function (packageDesignator) {
  return values1(ensurePackage(packageDesignator));
}, 1);
