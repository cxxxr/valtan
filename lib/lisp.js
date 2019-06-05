import {
  tValue, nilValue, initBoolean, argumentsError, typeError, checkType, typeChecker,
  toLispBoolean, isString
} from './header';

export { tValue, nilValue } from './header';

import { values1 } from './values';
export * from './values';

import { makeSymbol, symbolName, isSymbol, setSymbolFunction } from './symbol';
export * from './symbol';

import { cl_package, system_package, intern, findPackage, ensurePackage } from './package';
export * from './package';

export * from './ffi';
export * from './structure';
export * from './number';
export * from './control';

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
  return values1(setSymbolFunction(symbol, fn));
}, 2);


//////////////////////////////////////////////////
// package
registerFunction(cl_package, 'PACKAGE-NAME', function (packageDesignator) {
  return values1(ensurePackage(packageDesignator).name);
}, 1);

registerFunction(cl_package, 'FIND-PACKAGE', function (packageDesignator) {
  return values1(ensurePackage(packageDesignator));
}, 1);
