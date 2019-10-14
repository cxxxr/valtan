import {
    CL_values,
    CL_multipleValueCall
} from './values';
export * from './values';

import {
    initSymbols,
} from './header';

import {
    argumentsError
} from './error';

export {
    S_t,
    S_nil
}
from './header';

export {
    raise,
    setLispFormatFunction
}
from './callstack';

import {
    CL_symbolp,
    CL_makeSymbol,
    CL_symbolPlist,
    CL_setSymbolPlist,
    CL_put,
    CL_boundp,
    CL_fboundp,
    CL_symbolPackage,
    CL_symbolValue,
    CL_symbolFunction,
    CL_set,
    CL_setSymbolFunction,
    CL_symbolName,
    CL_makunbound,
    CL_fmakunbound,
} from './symbol';
export * from './symbol';

import {
    cl_package,
    system_package,
    ffi_package,
    intern,
    CL_packagep,
    CL_packageName,
    CL_packageNicknames,
    CL_listAllPackages,
    CL_intern,
    CL_findSymbol,
    CL_makePackage,
    CL_mapPackageSymbols
} from './package';
export * from './package';

import {
    CL_instanceof
} from './ffi';
export * from './ffi';

import {
    CL_makeStructure,
    CL_copyStructure,
    CL_structurep,
    CL_structureName,
    CL_structureSlotCount,
    CL_structureRef,
    CL_structureSet
} from './structure';
export * from './structure';

import {
    CL_numberp,
    CL_integerp,
    CL_add,
    CL_sub,
    CL_negate,
    CL_mul,
    CL_rem,
    CL_numberEqual,
    CL_numberNotEqual,
    CL_greaterThan,
    CL_lessThan,
    CL_greaterEqual,
    CL_lessEqual,
    CL_floor
} from './number';
export * from './number';

import {
    CL_apply,
    CL_functionp,
} from './function';
export * from './function';

import {
    CL_consp,
    CL_cons,
    CL_car,
    CL_cdr,
    CL_rplaca,
    CL_rplacd,
    CL_jsArrayToList,
    CL_listToJsArray
} from './cons';
export * from './cons';

import {
    CL_eq,
    CL_error
} from './control';
export * from './control';

import {
    CL_characterp,
    CL_codeChar,
    CL_charCode,
} from './character';
export * from './character';

export * from './util';

function symbols() {
    let obj = {}
    for (let name of ["PACKAGE", "SYMBOL", "STRING", "TYPE-ERROR", "ARGUMENTS-ERROR"]) {
        obj[name] = intern(name, cl_package);
    }
    return obj;
}

initSymbols(Object.assign({
    t: intern('T', cl_package),
    nil: intern('NIL', cl_package),
}, symbols()));

function registerFunction(pkg, name, fn, min = 0, max = min) {
    if (min === null || (min === 0 && max === null)) {
        intern(name, pkg).func = function() {
            return fn(...arguments);
        }
    } else if (min === max) {
        intern(name, pkg).func = function() {
            if (arguments.length !== min) argumentsError(intern(name), arguments.length);
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
registerFunction(cl_package, 'FBOUNDP', CL_fboundp, 1);
registerFunction(cl_package, 'SYMBOL-VALUE', CL_symbolValue, 1);
registerFunction(cl_package, 'SYMBOL-FUNCTION', CL_symbolFunction, 1);
registerFunction(cl_package, 'SET', CL_set, 2);
registerFunction(cl_package, 'MAKUNBOUND', CL_makunbound, 1);
registerFunction(cl_package, 'FMAKUNBOUND', CL_fmakunbound, 1);
registerFunction(system_package, '%SYMBOL-NAME', CL_symbolName, 1);
registerFunction(system_package, 'SYMBOL-PACKAGE-NAME', CL_symbolPackage, 1);
registerFunction(system_package, 'FSET', CL_setSymbolFunction, 2);
registerFunction(system_package, 'MAP-PACKAGE-SYMBOLS', CL_mapPackageSymbols, 2);
registerFunction(system_package, '%PUT', CL_put, 3);

// package.js
registerFunction(cl_package, 'PACKAGEP', CL_packagep, 1);
registerFunction(system_package, '%PACKAGE-NAME', CL_packageName, 1);
registerFunction(system_package, '%PACKAGE-NICKNAMES', CL_packageNicknames, 1);
registerFunction(cl_package, 'LIST-ALL-PACKAGES', CL_listAllPackages, 0);
registerFunction(system_package, 'INTERN', CL_intern, 2);
registerFunction(system_package, 'FIND-SYMBOL', CL_findSymbol, 2);
registerFunction(system_package, 'MAKE-PACKAGE', CL_makePackage, 3);

// number.js
registerFunction(cl_package, 'NUMBERP', CL_numberp, 1);
registerFunction(cl_package, 'INTEGERP', CL_integerp, 1);
registerFunction(system_package, '%ADD', CL_add, 2);
registerFunction(system_package, '%SUB', CL_sub, 2);
registerFunction(system_package, '%NEGATE', CL_negate, 1);
registerFunction(system_package, '%MUL', CL_mul, 2);
registerFunction(system_package, '%REM', CL_rem, 2);
registerFunction(system_package, '%=', CL_numberEqual, 2);
registerFunction(system_package, '%/=', CL_numberNotEqual, 2);
registerFunction(system_package, '%>', CL_greaterThan, 2);
registerFunction(system_package, '%<', CL_lessThan, 2);
registerFunction(system_package, '%>=', CL_greaterEqual, 2);
registerFunction(system_package, '%<=', CL_lessEqual, 2);
registerFunction(system_package, '%FLOOR', CL_floor, 2);

// function.js
registerFunction(system_package, 'APPLY', CL_apply, 2, null);
registerFunction(cl_package, 'FUNCTIONP', CL_functionp, 1);

// cons.js
registerFunction(cl_package, 'CONSP', CL_consp, 1);
registerFunction(cl_package, 'CONS', CL_cons, 2);
registerFunction(system_package, '%CAR', CL_car, 1);
registerFunction(system_package, '%CDR', CL_cdr, 1);
registerFunction(system_package, '%RPLACA', CL_rplaca, 2);
registerFunction(system_package, '%RPLACD', CL_rplacd, 2);
registerFunction(system_package, 'JS-ARRAY-TO-LIST', CL_jsArrayToList, 1);
registerFunction(system_package, 'LIST-TO-JS-ARRAY', CL_listToJsArray, 1);

// values.js
registerFunction(cl_package, 'VALUES', CL_values, 0, null);
registerFunction(system_package, 'MULTIPLE-VALUE-CALL', CL_multipleValueCall, null);

// structure.js
registerFunction(system_package, 'MAKE-STRUCTURE', CL_makeStructure, 1, null);
registerFunction(system_package, '%COPY-STRUCTURE', CL_copyStructure, 1);
registerFunction(system_package, 'STRUCTURE-P', CL_structurep, 1);
registerFunction(system_package, '%STRUCTURE-NAME', CL_structureName, 1);
registerFunction(system_package, '%STRUCTURE-SLOT-COUNT', CL_structureSlotCount, 1);
registerFunction(system_package, '%STRUCTURE-REF', CL_structureRef, 2);
registerFunction(system_package, '%STRUCTURE-SET', CL_structureSet, 3);

// control.js
registerFunction(cl_package, 'EQ', CL_eq, 2);
registerFunction(system_package, 'ERROR', CL_error, null);

// character.js
registerFunction(cl_package, 'CHARACTERP', CL_characterp, 1);
registerFunction(system_package, '%CODE-CHAR', CL_codeChar, 1);
registerFunction(system_package, '%CHAR-CODE', CL_charCode, 1);

// ffi.js
registerFunction(ffi_package, 'INSTANCEOF', CL_instanceof, null);