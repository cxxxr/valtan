import {
    values1,
    values
} from './values';
import {
    S_package,
    S_symbol,
    S_t,
    S_nil,
    S_current_package,
    isString,
    toLispBoolean,
} from './header';
import {
    typeError,
    checkString,
    simpleError,
} from './error';
import {
    makeSymbol,
    isSymbol,
    symbolName,
    symbolValue,
    setSymbolValue
} from './symbol';
import {
    isCons,
    listToJsArray,
    jsArrayToList
} from './cons';

class Package {
    constructor({ name, nicknames = [] } = {}) {
        this.name = name;
        this.nicknames = [...new Set(nicknames)];
        this.useList = [];
        this.symbolTable = Object.create(null);
        this.exportTable = Object.create(null);
        this.shadowing = new Set();
    }

    is(name) { return this.name === name || this.nicknames.includes(name); }
    exportSymbol(name) { this.exportTable[name] = true; }
    addNickname(name) { if (!this.nicknames.includes(name)) this.nicknames.push(name); }
    mapSymbols(fn) { for (const symbol of Object.values(this.symbolTable)) fn(symbol); }

    findSymbol(name) {
        if (Object.hasOwn(this.symbolTable, name)) {
            return [this.symbolTable[name], this.exportTable[name] ? 'EXTERNAL' : 'INTERNAL'];
        }
        let inherited = null;
        for (const used of this.useList) {
            if (used.exportTable[name]) {
                const symbol = used.symbolTable[name];
                if (inherited && inherited !== symbol) packageError(this, `Conflicting inherited symbol ${name}`);
                inherited = symbol;
            }
        }
        return inherited ? [inherited, 'INHERITED'] : [null, null];
    }

    intern(name) {
        const [symbol, status] = this.findSymbol(name);
        if (status !== null) return [symbol, status];
        const result = makeSymbol(name, this.name);
        this.symbolTable[name] = result;
        if (this.name === 'KEYWORD') {
            this.exportSymbol(name);
            result.value = result;
        }
        return [result, null];
    }
}

function packageError(pkg, message) {
    const handler = cl_package.symbolTable['PACKAGE-OPERATION-ERROR'];
    if (handler?.func) handler.func(pkg, message);
    simpleError(message);
}

function checkSymbol(symbol) {
    if (!isSymbol(symbol)) typeError(symbol, S_symbol);
}

function symbolList(symbols) {
    const result = symbols === S_nil ? [] : isCons(symbols) ? listToJsArray(symbols) : [symbols];
    result.forEach(checkSymbol);
    return result;
}

function usePackages(usedPackages, pkg) {
    const combined = [...new Set([...pkg.useList, ...usedPackages])];
    for (const used of combined) {
        for (const name of Object.keys(used.exportTable)) {
            if (pkg.shadowing.has(name)) continue;
            const candidates = new Set();
            if (Object.hasOwn(pkg.symbolTable, name)) candidates.add(pkg.symbolTable[name]);
            for (const source of combined) {
                if (source.exportTable[name]) candidates.add(source.symbolTable[name]);
            }
            if (candidates.size > 1) packageError(pkg, `USE-PACKAGE conflict for ${name}`);
        }
    }
    pkg.useList = combined;
}

const packages = [];

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
    const [duplicateName] = alreadyExistPackageName(name, nicknames);
    if (duplicateName) packageError(name, `Package ${duplicateName} already exists`);
    const used = usePackageNames.map(ensurePackage);
    const p = new Package({ name, nicknames });
    usePackages(used, p);
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
        // Exported but not yet implemented CL names must still be accessible
        // as CL symbols, rather than being interned in each using package.
        const [symbol] = pkg.intern(name);
        pkg.symbolTable[name] = symbol;
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
        if (!result) simpleError(`The Package ${pkg} is undefined`);
        return result;
    }

    typeError(pkg, S_package);
}

export function intern(name, pkg = symbolValue(S_current_package)) {
    const [symbol, status] = ensurePackage(pkg).intern(name);
    return symbol;
}

export function changeCurrentPackage(pkg) {
    return setSymbolValue(S_current_package, ensurePackage(pkg));
}

export const cl_package = makePackage('COMMON-LISP', ['CL']);
export const system_package = makePackage('SYSTEM');
export const ffi_package = makePackage('FFI');
export const keyword_package = makePackage('KEYWORD');
export const cl_user_package = makePackage('COMMON-LISP-USER', ['CL-USER', 'VALTAN-USER'], ['COMMON-LISP']);
makePackage('COMPILER');

function toKeyword(name) {
    const [symbol, status] = keyword_package.intern(name);
    return symbol;
}

export function CL_packagep(x) {
    return values1(toLispBoolean(x instanceof Package));
}

function checkPackage(x) {
    if (!(x instanceof Package)) {
        typeError(x, S_package);
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
        return values(symbol, S_nil);
    }
    return values(symbol, toKeyword(status));
}

export function CL_findSymbol(name, pkg) {
    checkString(name);
    checkPackage(pkg);
    const [symbol, status] = pkg.findSymbol(name);
    if (status === null) {
        return values(S_nil, S_nil);
    }
    return values(symbol, toKeyword(status));
}

export function CL_makePackage(name, nicknames, usePackageNames) {
    checkString(name);
    for (let name of nicknames) {
        checkString(name);
    }
    usePackageNames.forEach(ensurePackage);
    return values1(makePackage(name, nicknames, usePackageNames));
}

export function CL_export(symbols, pkg) {
    checkPackage(pkg);
    const entries = symbolList(symbols);
    for (const symbol of entries) {
        const [accessible] = pkg.findSymbol(symbol.name);
        if (accessible !== symbol) packageError(pkg, `Symbol ${symbol.name} is not accessible for EXPORT`);
        for (const user of packages.filter(p => p.useList.includes(pkg))) {
            if (user.shadowing.has(symbol.name)) continue;
            const [old] = user.findSymbol(symbol.name);
            if (old && old !== symbol) packageError(user, `EXPORT conflict for ${symbol.name}`);
        }
    }
    for (const symbol of entries) {
        pkg.symbolTable[symbol.name] = symbol;
        pkg.exportSymbol(symbol.name);
    }
    return values1(S_t);
}

export function CL_unexport(symbols, pkg) {
    checkPackage(pkg);
    for (const symbol of symbolList(symbols)) {
        const [accessible] = pkg.findSymbol(symbol.name);
        if (accessible !== symbol) packageError(pkg, `Symbol ${symbol.name} is not accessible for UNEXPORT`);
        delete pkg.exportTable[symbol.name];
    }
    return values1(S_t);
}

export function CL_usePackage(used, pkg) {
    checkPackage(pkg);
    usePackages(listToJsArray(used).map(ensurePackage), pkg);
    return values1(S_t);
}

export function CL_unusePackage(used, pkg) {
    checkPackage(pkg);
    const removed = listToJsArray(used).map(ensurePackage);
    pkg.useList = pkg.useList.filter(p => !removed.includes(p));
    return values1(S_t);
}

export function CL_import(symbols, pkg, shadowing = false) {
    checkPackage(pkg);
    const entries = symbolList(symbols);
    if (!shadowing) for (const symbol of entries) {
        const [old] = pkg.findSymbol(symbol.name);
        if (old && old !== symbol) packageError(pkg, `IMPORT conflict for ${symbol.name}`);
    }
    for (const symbol of entries) {
        const old = pkg.symbolTable[symbol.name];
        if (old && old !== symbol) {
            if (old.packageName === pkg.name) old.packageName = null;
            delete pkg.exportTable[symbol.name];
        }
        pkg.symbolTable[symbol.name] = symbol;
        if (symbol.packageName === null) symbol.packageName = pkg.name;
        if (shadowing) pkg.shadowing.add(symbol.name);
    }
    return values1(S_t);
}

export function CL_shadowingImport(symbols, pkg) { return CL_import(symbols, pkg, true); }

export function CL_shadow(names, pkg) {
    checkPackage(pkg);
    for (const name of names) {
        checkString(name);
        if (!Object.hasOwn(pkg.symbolTable, name)) pkg.symbolTable[name] = makeSymbol(name, pkg.name);
        pkg.shadowing.add(name);
    }
    return values1(S_t);
}

export function CL_packageShadowingSymbols(pkg) {
    checkPackage(pkg);
    return values1(jsArrayToList([...pkg.shadowing].map(name => pkg.symbolTable[name])));
}

export function CL_unintern(symbol, pkg) {
    checkSymbol(symbol); checkPackage(pkg);
    if (pkg.symbolTable[symbol.name] !== symbol) return values1(S_nil);
    if (pkg.shadowing.has(symbol.name)) {
        const inherited = new Set(pkg.useList.filter(p => p.exportTable[symbol.name]).map(p => p.symbolTable[symbol.name]));
        if (inherited.size > 1) packageError(pkg, `UNINTERN would expose a conflict for ${symbol.name}`);
    }
    delete pkg.symbolTable[symbol.name];
    delete pkg.exportTable[symbol.name];
    pkg.shadowing.delete(symbol.name);
    if (symbol.packageName === pkg.name) symbol.packageName = null;
    return values1(S_t);
}

export function CL_renamePackage(pkg, name, nicknames) {
    checkPackage(pkg); checkString(name); nicknames.forEach(checkString);
    for (const candidate of [name, ...nicknames]) {
        const old = findPackage(candidate);
        if (old && old !== pkg) packageError(pkg, `Package name ${candidate} already exists`);
    }
    for (const symbol of Object.values(pkg.symbolTable)) {
        if (symbol.packageName === pkg.name) symbol.packageName = name;
    }
    pkg.name = name;
    pkg.nicknames = [...new Set(nicknames)];
    return values1(pkg);
}

export function CL_mapPackageSymbols(pkg, fn) {
    checkPackage(pkg);
    pkg.mapSymbols(fn);
    return values1(S_nil);
}

// Map only over external symbols (exported symbols)
export function CL_mapPackageExternalSymbols(pkg, fn) {
    checkPackage(pkg);
    for (let name in pkg.symbolTable) {
        if (pkg.exportTable[name]) {
            fn(pkg.symbolTable[name]);
        }
    }
    return values1(S_nil);
}

// Map over all accessible symbols (present + inherited from used packages)
export function CL_mapPackageAllSymbols(pkg, fn) {
    checkPackage(pkg);
    const seen = Object.create(null);

    // First, iterate over present symbols
    for (let name in pkg.symbolTable) {
        seen[name] = true;
        fn(pkg.symbolTable[name]);
    }

    // Then, iterate over inherited symbols from used packages
    for (const usedPkg of pkg.useList) {
        for (let name in usedPkg.symbolTable) {
            if (usedPkg.exportTable[name] && !seen[name]) {
                seen[name] = true;
                fn(usedPkg.symbolTable[name]);
            }
        }
    }
    return values1(S_nil);
}

// The Lisp wrapper handles the correctable error when other packages use it.
export function CL_deletePackage(pkgDesignator) {
    const pkg = ensurePackage(pkgDesignator);
    if (pkg.name === null) return values1(S_nil);
    const index = packages.indexOf(pkg);
    if (index < 0) return values1(S_nil);
    for (const p of packages) p.useList = p.useList.filter(used => used !== pkg);
    for (const symbol of Object.values(pkg.symbolTable)) {
        if (symbol.packageName === pkg.name) symbol.packageName = null;
    }
    packages.splice(index, 1);
    pkg.name = null;
    pkg.nicknames = [];
    pkg.useList = [];
    pkg.symbolTable = Object.create(null);
    pkg.exportTable = Object.create(null);
    pkg.shadowing.clear();
    return values1(S_t);
}

export function CL_packageUsedByList(pkgDesignator) {
    const pkg = ensurePackage(pkgDesignator);
    return values1(jsArrayToList(packages.filter(p => p.useList.includes(pkg))));
}

export function CL_packageUseList(pkgDesignator) {
    return values1(jsArrayToList(ensurePackage(pkgDesignator).useList));
}
