import {
    CL_values,
    CL_multipleValueCall
} from './values';
export * from './values';

import {
    initSymbols,
    S_current_package,
} from './header';

import {
    argumentsError
} from './error';
export * from './error';

export {
    S_t,
    S_nil
}
from './header';

export {
    raise,
    setLispFormatFunction,
    getBacktrace,
}
from './callstack';

import {
    setSymbolValue,
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
    cl_user_package,
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
    CL_mapPackageSymbols,
    CL_mapPackageExternalSymbols,
    CL_mapPackageAllSymbols,
    CL_export,
    CL_deletePackage,
    CL_packageUsedByList,
    CL_packageUseList,
    CL_unexport,
    CL_usePackage,
    CL_unusePackage,
    CL_import,
    CL_shadowingImport,
    CL_shadow,
    CL_packageShadowingSymbols,
    CL_unintern,
    CL_renamePackage,
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
    CL_integerDecodeFloat,
    CL_decodeFloat,
    CL_floatRadix,
    CL_floatDigits,
    CL_floatPrecision,
    CL_floatSign,
    CL_scaleFloat,

    CL_rationalp,
    CL_numerator,
    CL_denominator,
    CL_rational,
    CL_floatRank,
    CL_float,
    CL_abs,
    CL_log,
    CL_transcendental,
    CL_atan,
    CL_phase,
    CL_cis,
    CL_numberPrintString,
    CL_complexp,
    CL_complex,
    CL_realpart,
    CL_imagpart,
    CL_integerp,
    CL_ash,
    CL_integerLength,
    CL_logand,
    CL_floatp,
    CL_add,
    CL_sub,
    CL_negate,
    CL_mul,
    CL_div,
    CL_rem,
    CL_numberEqual,
    CL_numberNotEqual,
    CL_greaterThan,
    CL_lessThan,
    CL_greaterEqual,
    CL_lessEqual,
    CL_floor,
    CL_round,
    CL_truncate,
    CL_ceiling,
    toFloat,
    numberValue,
    Float,
} from './number';
export * from './number';

import { CL_formatControl } from './format';
import { CL_initializeFileSystem, CL_fileOperation, CL_streamClosedp, CL_markStreamClosed } from './files';
import { CL_initializeEnvironment, CL_environmentOperation } from './environment';

import {
    CL_apply,
    CL_functionp,
    CL_funcall,
    startProfile,
    finishProfile,
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
    const names = [
        "PACKAGE",
        "SYMBOL",
        "INTEGER",
        "NUMBER",
        "STRING",
        "FUNCTION",
        "SETF",
        "TYPE-ERROR",
        "ARGUMENTS-ERROR",
        "SIMPLE-ERROR",
        "SIMPLE-MAKE-STRING",
        "UNDEFINED-FUNCTION",
        "UNBOUND-VARIABLE",
        "PROGRAM-ERROR",
        "*PACKAGE*",
    ];
    for (let name of names) {
        obj[name] = intern(name, cl_package);
    }
    return obj;
}

initSymbols(Object.assign({
    t: intern('T', cl_package),
    nil: intern('NIL', cl_package),
    'FDEFINITION-SETF': intern('FDEFINITION-SETF', system_package),
}, symbols()));

setSymbolValue(S_current_package, cl_user_package);

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
                argumentsError(intern(name), arguments.length);
            }
            return fn(...arguments);
        }
    } else {
        intern(name, pkg).func = function() {
            if (arguments.length < min || max < arguments.length) {
                argumentsError(intern(name), arguments.length);
            }
            return fn(...arguments);
        }
    }
}

// symbol.js
registerFunction(cl_package, 'SYMBOLP', CL_symbolp, 1);
registerFunction(system_package, 'MAKE-SYMBOL', CL_makeSymbol, 1);
registerFunction(system_package, 'SYMBOL-PLIST', CL_symbolPlist, 1);
registerFunction(system_package, 'PUT-SYMBOL-PLIST', CL_setSymbolPlist, 2);
registerFunction(cl_package, 'BOUNDP', CL_boundp, 1);
registerFunction(cl_package, 'FBOUNDP', CL_fboundp, 1);
registerFunction(system_package, 'SYMBOL-VALUE', CL_symbolValue, 1);
registerFunction(system_package, 'SYMBOL-FUNCTION', CL_symbolFunction, 1);
registerFunction(system_package, '%SET', CL_set, 2);
registerFunction(cl_package, 'MAKUNBOUND', CL_makunbound, 1);
registerFunction(cl_package, 'FMAKUNBOUND', CL_fmakunbound, 1);
registerFunction(system_package, 'SYMBOL-NAME', CL_symbolName, 1);
registerFunction(system_package, 'SYMBOL-PACKAGE-NAME', CL_symbolPackage, 1);
registerFunction(system_package, 'FSET', CL_setSymbolFunction, 2);
registerFunction(system_package, 'MAP-PACKAGE-SYMBOLS', CL_mapPackageSymbols, 2);
registerFunction(system_package, 'MAP-PACKAGE-EXTERNAL-SYMBOLS', CL_mapPackageExternalSymbols, 2);
registerFunction(system_package, 'MAP-PACKAGE-ALL-SYMBOLS', CL_mapPackageAllSymbols, 2);
registerFunction(system_package, 'PUT', CL_put, 3);

// package.js
registerFunction(cl_package, 'PACKAGEP', CL_packagep, 1);
registerFunction(system_package, 'PACKAGE-NAME', CL_packageName, 1);
registerFunction(system_package, 'PACKAGE-NICKNAMES', CL_packageNicknames, 1);
registerFunction(cl_package, 'LIST-ALL-PACKAGES', CL_listAllPackages, 0);
registerFunction(system_package, 'INTERN', CL_intern, 2);
registerFunction(system_package, 'FIND-SYMBOL', CL_findSymbol, 2);
registerFunction(system_package, 'MAKE-PACKAGE', CL_makePackage, 3);
registerFunction(system_package, 'EXPORT', CL_export, 1, 2);
registerFunction(system_package, '%DELETE-PACKAGE', CL_deletePackage, 1);
registerFunction(system_package, 'PACKAGE-USED-BY-LIST', CL_packageUsedByList, 1);
registerFunction(system_package, 'PACKAGE-USE-LIST', CL_packageUseList, 1);
registerFunction(system_package, 'UNEXPORT', CL_unexport, 2);
registerFunction(system_package, 'USE-PACKAGE', CL_usePackage, 2);
registerFunction(system_package, 'UNUSE-PACKAGE', CL_unusePackage, 2);
registerFunction(system_package, 'IMPORT', CL_import, 2);
registerFunction(system_package, 'SHADOWING-IMPORT', CL_shadowingImport, 2);
registerFunction(system_package, 'SHADOW', CL_shadow, 2);
registerFunction(system_package, 'PACKAGE-SHADOWING-SYMBOLS', CL_packageShadowingSymbols, 1);
registerFunction(system_package, 'UNINTERN', CL_unintern, 2);
registerFunction(system_package, 'RENAME-PACKAGE', CL_renamePackage, 3);


registerFunction(system_package, 'INITIALIZE-FILE-SYSTEM', CL_initializeFileSystem, 2);
registerFunction(system_package, 'INITIALIZE-ENVIRONMENT', CL_initializeEnvironment, 1);
registerFunction(system_package, 'ENVIRONMENT-OP', CL_environmentOperation, 1, null);
registerFunction(system_package, 'FILE-OP', CL_fileOperation, 1, null);
registerFunction(system_package, 'STREAM-CLOSED-P', CL_streamClosedp, 1);
registerFunction(system_package, 'MARK-STREAM-CLOSED', CL_markStreamClosed, 1);

// number.js
registerFunction(cl_package, 'NUMBERP', CL_numberp, 1);
registerFunction(cl_package, 'INTEGER-DECODE-FLOAT', CL_integerDecodeFloat, 1);
registerFunction(cl_package, 'DECODE-FLOAT', CL_decodeFloat, 1);
registerFunction(cl_package, 'FLOAT-RADIX', CL_floatRadix, 1);
registerFunction(cl_package, 'FLOAT-DIGITS', CL_floatDigits, 1);
registerFunction(cl_package, 'FLOAT-PRECISION', CL_floatPrecision, 1);
registerFunction(cl_package, 'FLOAT-SIGN', CL_floatSign, 1, 2);
registerFunction(cl_package, 'SCALE-FLOAT', CL_scaleFloat, 2);

registerFunction(system_package, 'RATIONALP', CL_rationalp, 1);
registerFunction(cl_package, 'NUMERATOR', CL_numerator, 1);
registerFunction(cl_package, 'DENOMINATOR', CL_denominator, 1);
registerFunction(cl_package, 'RATIONAL', CL_rational, 1);
registerFunction(system_package, 'FLOAT-RANK', CL_floatRank, 2);
registerFunction(cl_package, 'FLOAT', CL_float, 1, 2);
registerFunction(cl_package, 'ABS', CL_abs, 1);
registerFunction(cl_package, 'LOG', CL_log, 1, 2);
for (const op of ['sqrt', 'exp', 'sin', 'cos', 'tan', 'asin', 'acos', 'sinh', 'cosh', 'tanh', 'asinh', 'acosh', 'atanh']) {
    registerFunction(cl_package, op.toUpperCase(), x => CL_transcendental(op, x), 1);
}
registerFunction(cl_package, 'ATAN', CL_atan, 1, 2);
registerFunction(cl_package, 'PHASE', CL_phase, 1);
registerFunction(cl_package, 'CIS', CL_cis, 1);
registerFunction(system_package, 'FORMAT-CONTROL', CL_formatControl, 3);
registerFunction(system_package, 'NUMBER-PRINT-STRING', CL_numberPrintString, 3);
registerFunction(cl_package, 'COMPLEXP', CL_complexp, 1);
registerFunction(cl_package, 'COMPLEX', CL_complex, 1, 2);
registerFunction(cl_package, 'REALPART', CL_realpart, 1);
registerFunction(cl_package, 'IMAGPART', CL_imagpart, 1);
registerFunction(cl_package, 'INTEGERP', CL_integerp, 1);
registerFunction(cl_package, 'ASH', CL_ash, 2);
registerFunction(cl_package, 'INTEGER-LENGTH', CL_integerLength, 1);
registerFunction(system_package, '%LOGAND', CL_logand, 2);
registerFunction(system_package, 'FLOATP', CL_floatp, 1);
registerFunction(system_package, 'MAKE-FLOAT', toFloat, 1);
registerFunction(system_package, 'FLOAT-VALUE', numberValue, 1);
registerFunction(system_package, '%ADD', CL_add, 2);
registerFunction(system_package, '%SUB', CL_sub, 2);
registerFunction(system_package, '%NEGATE', CL_negate, 1);
registerFunction(system_package, '%MUL', CL_mul, 2);
registerFunction(system_package, '%DIV', CL_div, 2);
registerFunction(system_package, '%REM', CL_rem, 2);
registerFunction(system_package, '%=', CL_numberEqual, 2);
registerFunction(system_package, '%/=', CL_numberNotEqual, 2);
registerFunction(system_package, '%>', CL_greaterThan, 2);
registerFunction(system_package, '%<', CL_lessThan, 2);
registerFunction(system_package, '%>=', CL_greaterEqual, 2);
registerFunction(system_package, '%<=', CL_lessEqual, 2);
registerFunction(system_package, '%FLOOR', CL_floor, 2);
registerFunction(system_package, '%ROUND', CL_round, 2);
registerFunction(system_package, '%TRUNCATE', CL_truncate, 2);
registerFunction(system_package, '%CEILING', CL_ceiling, 2);

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
registerFunction(system_package, 'RAW-ARRAY-TO-LIST', CL_jsArrayToList, 1);
registerFunction(system_package, 'LIST-TO-RAW-ARRAY', CL_listToJsArray, 1);

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
