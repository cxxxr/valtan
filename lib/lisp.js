import {
    initSymbols,
    argumentsError
} from './header';

export {
    tValue,
    nilValue
}
from './header';

import {
    values1
} from './values';
export * from './values';

import {
    makeSymbol,
    symbolName,
    isSymbol,
    setSymbolFunction,
    CL_symbolp,
    CL_makeSymbol,
    CL_symbolPlist,
    CL_setSymbolPlist,
    CL_boundp,
    CL_symbolPackage,
    CL_symbolFunction,
    CL_setSymbolFunction,
    CL_symbolName
} from './symbol';
export * from './symbol';

import {
    cl_package,
    system_package,
    intern,
    findPackage,
    ensurePackage,
    CL_packageName,
    CL_findPackage
} from './package';
export * from './package';

export * from './ffi';
export * from './structure';

import {
    CL_numberp,
    CL_integerp,
    CL_add,
    CL_sub,
    CL_mul,
    CL_numberEqual,
    CL_greaterThan,
    CL_lessThan,
    CL_greaterEqual,
    CL_lessEqual
} from './number';
export * from './number';

import {
  CL_apply,
  CL_functionp
} from './function';
export * from './function';

export * from './control';

initSymbols({
  t: intern('T', cl_package),
  nil: intern('NIL', cl_package),
});

function registerFunction(pkg, name, fn, min = 0, max = min) {
    if (!min) {
        intern(name, pkg).func = function() {
            return fn(...arguments);
        }
    } else if (min === max) {
        intern(name, pkg).func = function() {
            if (arguments.length !== min) argumentsError(name, arguments);
            return fn(...arguments);
        }
    } else if (!max) {
        intern(name, pkg).func = function() {
            if (arguments.length < min) {
                argumentsError(name, arguments);
            }
            return fn(...arguments);
        }
    } else {
        intern(name, pkg).func = function() {
            if (arguments.length < min || max < arguments.length) {
                argumentsError(name, arguments);
            }
            return fn(...arguments);
        }
    }
}

// symbol.js
registerFunction(cl_package, 'SYMBOLP', CL_symbolp, 1);
registerFunction(system_package, '%MAKE-SYMBOL', CL_makeSymbol, 1);
registerFunction(cl_package, 'SYMBOL-PLIST', CL_symbolPlist, 1);
registerFunction(system_package, 'PUT-SYMBOL-PLIST', CL_setSymbolPlist, 2);
registerFunction(cl_package, 'BOUNDP', CL_boundp, 1);
registerFunction(cl_package, 'SYMBOL-FUNCTION', CL_symbolFunction, 1);
registerFunction(cl_package, 'SYMBOL-NAME', CL_symbolName, 1);
registerFunction(system_package, 'SYMBOL-PACKAGE-NAME', CL_symbolPackage, 1);
registerFunction(system_package, 'FSET', CL_setSymbolFunction, 2);

// package.js
registerFunction(cl_package, 'PACKAGE-NAME', CL_packageName, 1);
registerFunction(cl_package, 'FIND-PACKAGE', CL_findPackage, 1);

// number.js
registerFunction(cl_package, 'NUMBERP', CL_numberp, 1);
registerFunction(cl_package, 'INTEGERP', CL_integerp, 1);
registerFunction(cl_package, '+', CL_add, 0, null);
registerFunction(cl_package, '-', CL_sub, 1, null);
registerFunction(cl_package, '*', CL_mul, 0, null);
registerFunction(cl_package, '=', CL_numberEqual, 1, null);
registerFunction(cl_package, '>', CL_greaterThan, 1, null);
registerFunction(cl_package, '<', CL_lessThan, 1, null);
registerFunction(cl_package, '>=', CL_greaterEqual, 1, null);
registerFunction(cl_package, '<=', CL_lessEqual, 1, null);

// function.js
registerFunction(system_package, 'APPLY', CL_apply, 2, null);
registerFunction(cl_package, 'FUNCTIONP', CL_functionp, 1);
