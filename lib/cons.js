class Cons {
  constructor(car, cdr) {
    this.car = car;
    this.cdr = cdr;
  }
}

export function makeCons(car, cdr) {
  return new Cons(car, cdr);
}

export function isCons(x) {
  return x instanceof Cons;
}

export function car(x) {
  return x.car;
}

export function cdr(x) {
  return x.cdr;
}

export function setCar(x, value) {
  x.car = value;
}

export function setCdr(x, value) {
  x.cdr = value;
}
