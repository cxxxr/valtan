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

class Package {
  constructor(options = {}) {
    const {name, nicknames} = options;
    this.name = name;
    this.nicknames = nicknames || [];
    this.table = {};
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

  findSymbol(name) {
    return this.table[name];
  }

  intern(name) {
    return this.table[name] || (this.table[name] = new LispSymbol({name}));
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
  const s = intern(name, findPackage(packageName));
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

current_package = cl_package;

export const nilValue = intern('NIL', cl_package);
nilValue.value = nilValue;

intern('+', cl_package).func = (...args) => {
  let sum = 0;
  for (let arg of args) {
    sum += arg;
  }
  return sum;
}

intern('CONSOLE.LOG', system_package).func = (...args) => {
  console.log(...args);
}

intern('FSET', system_package).func = (symbol, fn) => {
  symbol.func = fn;
}
