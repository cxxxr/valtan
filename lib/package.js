import {
    values1,
    values
} from './values';
import {
    nilValue,
    typeError,
    isString,
    toLispBoolean,
    checkString
} from './header';
import {
    makeSymbol,
    isSymbol,
    symbolName,
    symbolValue,
    setSymbolValue
} from './symbol';
import {
    jsArrayToList
} from './cons';
import {
  raise
} from './callstack';

class Package {
    constructor(options = {}) {
        const {
            name,
            nicknames,
            usePackageNames
        } = options;
        this.name = name;
        this.nicknames = nicknames || [];
        this.usePackageNames = usePackageNames || [];
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

    exportSymbol(name) {
        if (typeof name !== 'string') {
          raise(`Internal Error: ${name} is not string`);
        }
        this.exportTable[name] = true;
    }

    addNickname(nickname) {
        if (this.nicknames.indexOf(nickname) === -1) {
            this.nicknames.push(nickname);
        }
    }

    mapSymbols(fn) {
      for (let name in this.symbolTable) {
        fn(this.symbolTable[name]);
      }
    }

    _findSymbol(name, sanePackage) {
        for (let pkgName of this.usePackageNames) {
            const pkg = ensurePackage(pkgName);
            const s = pkg._findSymbol(name, sanePackage);
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

    findSymbol(name, sanePackage = this) {
        const symbol = this._findSymbol(name, sanePackage);
        if (!symbol) {
            return [null, null];
        }
        if (symbol.getPackageName() !== this.name) {
            return [symbol, 'INHERITED'];
        }
        if (this.exportTable[symbolName(symbol)]) {
            return [symbol, 'EXTERNAL'];
        }
        return [symbol, 'INTERNAL'];
    }

    intern(name) {
        const [symbol, status] = this.findSymbol(name);
        if (status === null) {
            const newSymbol = makeSymbol(name, this.name);
            this.symbolTable[name] = newSymbol;
            return [newSymbol, null];
        }
        return [symbol, status];
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

function alreadyExistPackageName(name, nicknames) {
    let pkg;
    if (pkg = findPackage(name)) {
        return [name, pkg];
    }
    for (let name of nicknames) {
        if (pkg = findPackage(name)) {
            return [name, pkg];
        }
    }
    return [null, null];
}

function makePackage(name, nicknames = [], usePackageNames = []) {
    const [duplicateName, _] = alreadyExistPackageName(name, nicknames);
    if (duplicateName) {
        raise(`Package ${duplicateName} already exists`);
    }
    const p = new Package({
        name,
        nicknames,
        usePackageNames,
    });
    packages.push(p);
    return p;
}

export function defpackage(name, {
    exportNames = [],
    nicknames = [],
    usePackageNames = []
}) {
    let [_, pkg] = alreadyExistPackageName(name, nicknames);
    if (!pkg) {
        pkg = makePackage(name, nicknames, usePackageNames);
    }
    for (let name of exportNames) {
        pkg.exportSymbol(name);
    }
    for (let name of nicknames) {
        pkg.addNickname(name);
    }
    return pkg;
}

export function ensurePackage(pkg) {
    if (pkg instanceof Package) {
        return pkg;
    }

    if (isSymbol(pkg)) {
        pkg = symbolName(pkg);
    }

    if (isString(pkg)) {
        const result = findPackage(pkg);
        if (!result) raise(`The Package ${pkg} is undefined`);
        return result;
    }

    typeError(pkg, "Package");
}

export function intern(name, pkg = symbolValue(current_package)) {
    const [symbol, status] = ensurePackage(pkg).intern(name);
    return symbol;
}

export function changeCurrentPackage(pkg) {
    return setSymbolValue(current_package, ensurePackage(pkg));
}

export const cl_package = makePackage('COMMON-LISP', ['CL']);
export const system_package = makePackage('SYSTEM');
export const ffi_package = makePackage('FFI');
export const keyword_package = makePackage('KEYWORD');
export const cl_user_package = makePackage('COMMON-LISP-USER', ['CL-USER'], ['COMMON-LISP']);
current_package = intern('*PACKAGE*', cl_package);
setSymbolValue(current_package, cl_user_package);

function toKeyword(name) {
    const [symbol, status] = keyword_package.intern(name);
    return symbol;
}

export function CL_packagep(x) {
    return values1(toLispBoolean(x instanceof Package));
}

function checkPackage(x) {
    if (!(x instanceof Package)) {
        typeError(x, "PACKAGE");
    }
}

export function CL_packageName(pkg) {
    checkPackage(pkg);
    return values1(pkg.name);
}

export function CL_packageNicknames(pkg) {
    checkPackage(pkg);
    return values1(pkg.nicknames);
}

export function CL_listAllPackages() {
    return values1(jsArrayToList(packages));
}

export function CL_intern(name, pkg) {
    checkString(name);
    checkPackage(pkg);
    const [symbol, status] = pkg.intern(name);
    if (pkg === keyword_package) {
      symbol.value = symbol;
    }
    if (status === null) {
        return values(symbol, nilValue);
    }
    return values(symbol, toKeyword(status));
}

export function CL_findSymbol(name, pkg) {
    checkString(name);
    checkPackage(pkg);
    const [symbol, status] = pkg.findSymbol(name);
    if (status === null) {
        return values(nilValue, nilValue);
    }
    return values(symbol, toKeyword(status));
}

export function CL_makePackage(name, nicknames, usePackageNames) {
    checkString(name);
    for (let name of nicknames) {
        checkString(name);
    }
    for (let name of usePackageNames) {
        checkString(name);
    }
    return values1(makePackage(name, nicknames, usePackageNames));
}

export function CL_mapPackageSymbols(pkg, fn) {
  checkPackage(pkg);
  pkg.mapSymbols(fn);
  return values1(nilValue);
}