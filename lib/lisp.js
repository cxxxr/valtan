class Cons {
  constructor(car, cdr) {
    this.car = car;
    this.cdr = cdr;
  }
}

class LispSymbol {
  constructor(options) {
    const {name} = options;
    this.name = name;
    this.value = null;
    this.func = null;
  }
}

class Macro {
  constructor(func) {
    this.func = func;
  }
}

class Package {
  constructor(options = {}) {
    const {name, nicknames, use} = options;
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
    return this.findSymbol(name) || (this.symbolTable[name] = new LispSymbol({name}));
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

export function makePackage(name, options) {
  const p = new Package({name, ...options});
  packages.push(p);
  return p;
}

export function intern(name, pkg = current_package) {
  return pkg.intern(name);
}

export function call_function(packageName, name, ...args) {
  const pkg = findPackage(packageName);
  if (!pkg) throw new Error(`The package ${packageName} is undefined`);
  const s = intern(name, pkg);
  if (s.func === null) throw new Error(`The function ${name} is undefined`);
  return s.func(...args);
}

export function global_variable(name) {
  const s = intern(name);
  return s.value;
}

export function set_global_variable(name, value) {
  const s = intern(name);
  s.value = value;
  return value;
}

const cl_package = makePackage('COMMON-LISP', { nicknames: ['CL'] });
const system_package = makePackage('SYSTEM');
const cl_user_package = makePackage('COMMON-LISP-USER', { nicknames: ['CL-USER'] });

current_package = cl_user_package;

export const tValue = intern('T', cl_package);
tValue.value = tValue;

export const nilValue = intern('NIL', cl_package);
nilValue.value = nilValue;

cl_package.exportSymbol(
  intern('+', cl_package).func = (...args) => {
    let sum = 0;
    for (let arg of args) {
      sum += arg;
    }
    return sum;
  }
);

cl_package.exportSymbol(
  intern('-', cl_package).func = (...args) => {
    let acc = args[0];
    for (let i = 1; i < args.length; i++) {
      acc -= args[i];
    }
    return acc;
  }
);

cl_package.exportSymbol(
  intern('*', cl_package).func = (...args) => {
    let acc = 1;
    for (let arg of args) {
      acc *= arg;
    }
    return acc;
  }
);

cl_package.exportSymbol(
  intern('=', cl_package).func = (x, y) => {
    return x === y ? tValue : nilValue;
  }
);

intern('CONSOLE.LOG', system_package).func = (...args) => {
  console.log(...args);
}

intern('FSET', system_package).func = (symbol, fn) => {
  symbol.func = fn;
}

intern('ADD-GLOBAL-MACRO', system_package).func = (symbol, fn) => {
  symbol.func = new Macro(fn);
}
