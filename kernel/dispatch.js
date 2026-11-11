import { S_nil } from './header';

// Private CLOS dispatch keys. A canonical object avoids hashing and comparing
// newly allocated lists of class objects on every generic-function call.
// Roots are per generic function and reset with its effective-method cache.
// Weak edges do not keep discarded classes or EQL specializers alive.
const roots = new WeakMap();
const node = () => ({ children: new WeakMap() });

export function resetMethodCacheKeys(genericFunction) {
    roots.delete(genericFunction);
}

export function internMethodCacheKey(genericFunction, classes, specializers) {
    let current = roots.get(genericFunction);
    if (!current) roots.set(genericFunction, current = node());
    for (let rest = classes; rest !== S_nil; rest = rest.cdr) {
        let next = current.children.get(rest.car);
        if (!next) current.children.set(rest.car, next = node());
        current = next;
    }
    if (specializers !== S_nil) {
        // Keep the two sequences distinct, including prefix/empty keys.
        current = current.specializers ??= node();
        for (let rest = specializers; rest !== S_nil; rest = rest.cdr) {
            let next = current.children.get(rest.car);
            if (!next) current.children.set(rest.car, next = node());
            current = next;
        }
    }
    return current;
}
