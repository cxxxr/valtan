import {
    values1
} from './values';
import {
    typeError,
    isString,
    toLispBoolean
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
            use
        } = options;
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
    const {
        nicknames = [], use
    } = options;
    const duplicateName = alreadyExistPackageName(name, nicknames);
    if (duplicateName) {
        throw new Error(`Package ${duplicateName} already exists`);
    }
    const p = new Package({
        name,
        nicknames,
        use
    });
    packages.push(p);
    return p;
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

export const cl_package = makePackage('COMMON-LISP', {
    nicknames: ['CL']
});
export const system_package = makePackage('SYSTEM');
export const ffi_package = makePackage('FFI');
export const keyword_package = makePackage('KEYWORD');
export const cl_user_package = makePackage('COMMON-LISP-USER', {
    nicknames: ['CL-USER']
});
current_package = intern('*PACKAGE*', cl_package);
setSymbolValue(current_package, cl_user_package);

export function CL_packagep(x) {
  return values1(toLispBoolean(x instanceof Package));
}

export function CL_packageName(packageDesignator) {
    return values1(ensurePackage(packageDesignator).name);
}

export function CL_findPackage(packageDesignator) {
    return values1(ensurePackage(packageDesignator));
}

export function CL_listAllPackages() {
  return values1(jsArrayToList(packages));
}
