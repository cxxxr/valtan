import {
  tValue, nilValue, initBoolean, argumentsError, typeError, checkType, typeChecker,
  toLispBoolean, isString
} from './header';

export { tValue, nilValue } from './header';

import { values1 } from './values';
export * from './values';

import {
  makeSymbol, symbolName, isSymbol, setSymbolFunction,
  CL_symbolp, CL_makeSymbol, CL_symbolPlist, CL_setSymbolPlist, CL_boundp, CL_symbolPackage,
  CL_symbolFunction, CL_setSymbolFunction, CL_symbolName
} from './symbol';
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

function checkPackage(value) {
  checkType(value, typeChecker(Package), "Package");
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
registerFunction(cl_package, 'SYMBOLP', CL_symbolp, 1);
registerFunction(system_package, '%MAKE-SYMBOL', CL_makeSymbol, 1);
registerFunction(cl_package, 'SYMBOL-PLIST', CL_symbolPlist, 1);
registerFunction(system_package, 'PUT-SYMBOL-PLIST', CL_setSymbolPlist, 2);
registerFunction(cl_package, 'BOUNDP', CL_boundp, 1);
registerFunction(cl_package, 'SYMBOL-FUNCTION', CL_symbolFunction, 1);
registerFunction(cl_package, 'SYMBOL-NAME', CL_symbolName, 1);
registerFunction(system_package, 'SYMBOL-PACKAGE-NAME', CL_symbolPackage, 1);
registerFunction(system_package, 'FSET', CL_setSymbolFunction, 2);


//////////////////////////////////////////////////
// package
registerFunction(cl_package, 'PACKAGE-NAME', function (packageDesignator) {
  return values1(ensurePackage(packageDesignator).name);
}, 1);

registerFunction(cl_package, 'FIND-PACKAGE', function (packageDesignator) {
  return values1(ensurePackage(packageDesignator));
}, 1);
