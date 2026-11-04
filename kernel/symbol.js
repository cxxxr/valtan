import {
    values1
} from './values';
import {
    S_nil,
    toLispBoolean,
    S_symbol,
    S_setf,
    S_fdefinition_setf
} from './header';
import {
    typeError,
    checkString,
    unboundVariable,
    undefinedFunction,
} from './error';
import {
    makeCons,
    isCons
} from './cons';

class LispSymbol {
    constructor(options) {
        const {
            name,
            packageName
        } = options;
        this.name = name;
        this.packageName = packageName;
        this.value = null;
        this.func = null;
        this.plist = S_nil;
        this.compilerProperties = new Map();
    }

    isBound() {
        return this.value !== null;
    }

    isFBound() {
        return this.func !== null ||
            (this.compilerProperties.has('MACRO') && this.compilerProperties.get('MACRO') !== S_nil) ||
            (this.compilerProperties.has('TRANSFORM') && this.compilerProperties.get('TRANSFORM') !== S_nil) ||
            (this.compilerProperties.has('SPECIAL-OPERATOR') && this.compilerProperties.get('SPECIAL-OPERATOR') !== S_nil);
    }

    getFunction() {
        if (this.func === null) {
            undefinedFunction(this);
        }
        this.func.lisp_symbol = this;
        return this.func;
    }

    getValue() {
        if (this.value === null) {
            unboundVariable(this);
        }
        return this.value;
    }

    getPackageName() {
        return this.packageName;
    }
}

export function isSymbol(x) {
    return x instanceof LispSymbol;
}

export function makeSymbol(name, packageName = null) {
    return new LispSymbol({
        name,
        packageName
    });
}

export function symbolName(symbol) {
    return symbol.name;
}

export function symbolValue(symbol) {
    if (!symbol.isBound()) unboundVariable(symbol);
    return symbol.value;
}

export function setSymbolValue(symbol, value) {
    symbol.value = value;
    return value;
}

export function setSymbolFunction(symbol, func) {
    symbol.compilerProperties.delete('MACRO');
    symbol.func = func;
    func.lisp_name = symbol.name;
    symbol.func.lisp_symbol = symbol;
    return func;
}

function checkSymbol(x) {
    if (!isSymbol(x)) {
        typeError(x, S_symbol);
    }
}

export function CL_symbolp(x) {
    return values1(toLispBoolean(isSymbol(x)));
}

export function CL_makeSymbol(string) {
    checkString(string);
    return values1(makeSymbol(string));
}

export function CL_symbolPlist(symbol) {
    checkSymbol(symbol);
    if (symbol.plist === undefined) symbol.plist = S_nil;
    return values1(symbol.plist);
}

export function CL_setSymbolPlist(symbol, plist) {
    checkSymbol(symbol);
    symbol.plist = plist;
    return values1(plist);
}

export function CL_put(symbol, key, value) {
    checkSymbol(symbol);
    for (let plist = symbol.plist; plist !== S_nil; plist = plist.cdr.cdr) {
        if (plist.car === key) {
            plist.cdr.car = value;
            return values1(value);
        }
    }
    symbol.plist = makeCons(key, makeCons(value, symbol.plist));
    return values1(S_nil);
}

export function CL_compilerProperty(symbol, key) {
    checkSymbol(symbol);
    return values1(symbol.compilerProperties.get(key.name) ?? S_nil);
}

export function CL_setCompilerProperty(symbol, key, value) {
    checkSymbol(symbol);
    symbol.compilerProperties.set(key.name, value);
    if (key.name === 'MACRO' && value !== S_nil) symbol.func = null;
    return values1(value);
}

export function CL_boundp(x) {
    checkSymbol(x);
    return values1(toLispBoolean(x.isBound()));
}

function setfFunctionCell(name, setfSymbol, setfKey) {
    if (isCons(name) && name.car === setfSymbol && isCons(name.cdr) &&
        name.cdr.cdr === S_nil && name.cdr.car instanceof LispSymbol) {
        for (let plist = name.cdr.car.plist; plist !== S_nil; plist = plist.cdr.cdr) {
            if (plist.car === setfKey) return plist.cdr;
        }
        return null;
    }
    checkSymbol(name);
    return null;
}

export function CL_fboundp(x) {
    if (isCons(x)) {
        const cell = setfFunctionCell(x, S_setf, S_fdefinition_setf);
        return values1(toLispBoolean(cell !== null && cell.car !== S_nil));
    }
    checkSymbol(x);
    return values1(toLispBoolean(x.isFBound()));
}

export function CL_symbolPackage(symbol) {
    checkSymbol(symbol);
    return values1(symbol.packageName);
}

export function CL_symbolValue(symbol) {
    checkSymbol(symbol);
    return values1(symbolValue(symbol));
}

export function CL_symbolFunction(symbol) {
    checkSymbol(symbol);
    if (symbol.func === null && symbol.isFBound()) {
        const descriptor = ['MACRO', 'TRANSFORM'].map(key => symbol.compilerProperties.get(key))
            .find(value => value !== undefined && value !== S_nil);
        return values1(descriptor || symbol);
    }
    return values1(symbol.getFunction());
}

export function CL_setSymbolFunction(symbol, func) {
    checkSymbol(symbol);
    return values1(setSymbolFunction(symbol, func));
}

export function CL_symbolName(symbol) {
    checkSymbol(symbol);
    return values1(symbolName(symbol));
}

export function CL_set(symbol, value) {
    checkSymbol(symbol);
    setSymbolValue(symbol, value);
    return values1(value);
}

export function CL_makunbound(symbol) {
    checkSymbol(symbol);
    symbol.value = null;
    return values1(symbol);
}

export function CL_fmakunbound(symbol) {
    if (isCons(symbol)) {
        const cell = setfFunctionCell(symbol, S_setf, S_fdefinition_setf);
        if (cell !== null) cell.car = S_nil;
        return values1(symbol);
    }
    checkSymbol(symbol);
    symbol.func = null;
    symbol.compilerProperties.delete('MACRO');
    return values1(symbol);
}
