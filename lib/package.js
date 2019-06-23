import {
    values1
} from './values';
import {
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

    exportSymbol(symbol) {
        this.exportTable[symbol] = true;
    }

    findSymbol(name, sanePackage = this) {
        for (pkg of this.usePackageNames) {
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

export function findPackage(name) {
    for (let p of packages) {
        if (p.is(name)) {
            return p;
        }
    }
    return null;
}

function alreadyExistPackageName(name, nicknames) {
    if (packageNameTable[name]) {
        return [name, packageNameTable[name]];
    }
    for (let n of nicknames) {
        if (packageNameTable[n]) {
            return [n, packageNameTable[n]];
        }
    }
    return [null, null];
}

function makePackageInternal(name, nicknames, usePackageNames) {
    const p = new Package({
        name,
        nicknames,
        usePackageNames,
    });
    packages.push(p);
    return p;
}

function makePackage(name, nicknames = [], usePackageNames = []) {
    const [duplicateName, _] = alreadyExistPackageName(name, nicknames);
    if (duplicateName) {
        throw new Error(`Package ${duplicateName} already exists`);
    }
    return makePackageInternal(name, nicknames, usePackageNames);
}

export function defpackage(name, nicknames = [], usePackageNames = []) {
    const [_, pkg] = alreadyExistPackageName(name, nicknames);
    if (pkg) {
      return pkg;
    }
    return makePackageInternal(name, nicknames, usePackageNames);
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
        if (!result) throw new Error(`The Package ${pkg} is undefined`);
        return result;
    }

    typeError(pkg, "Package");
}

export function intern(name, pkg = symbolValue(current_package)) {
    return ensurePackage(pkg).intern(name);
}

export const cl_package = makePackage('COMMON-LISP', ['CL']);
export const system_package = makePackage('SYSTEM');
export const ffi_package = makePackage('FFI');
export const keyword_package = makePackage('KEYWORD');
export const cl_user_package = makePackage('COMMON-LISP-USER', ['CL-USER']);
current_package = intern('*PACKAGE*', cl_package);
setSymbolValue(current_package, cl_user_package);

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
  return values1(intern(name, pkg));
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
