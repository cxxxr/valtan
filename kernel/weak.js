import {S_nil, toLispBoolean} from './header';
import {values1} from './values';
import {simpleError} from './error';
import {CL_funcall} from './function';
import {Float, Ratio, Complex} from './number';

const weakable = value => value !== null && (typeof value === 'object' || typeof value === 'function');
const ref = value => weakable(value) ? new WeakRef(value) : {deref: () => value};
function numericKey(value) {
    if (typeof value === 'bigint' || (typeof value === 'number' && Number.isInteger(value))) return `i:${value}`;
    if (value instanceof Float) return `f:${Object.is(value.value, -0) ? '-0' : value.value}`;
    if (value instanceof Ratio) return `r:${value.numerator}/${value.denominator}`;
    if (value instanceof Complex) return `c:${numericKey(value.real)};${numericKey(value.imag)}`;
    return null;
}

// Entries are never strongly retained by the iteration index. Weak-key maps
// are ephemerons, so a value referring back to its key does not pin that key.
// KEY-OR-VALUE uses two ephemeron indexes: either live endpoint retains both.
export class WeakHashMap {
    constructor(weakness, test = 'eql') {
        if (!['key', 'value', 'key-and-value', 'key-or-value'].includes(weakness)) throw new Error('Invalid weakness');
        this.weakness = weakness;
        this.test = test;
        this.clear();
    }
    clear() {
        this.objects = new WeakMap();
        this.reverse = new WeakMap();
        this.strong = new Map();
        this.numbers = new Map();
        this.records = new Set();
    }
    lookup(key) {
        const code = this.test === 'eql' ? numericKey(key) : null;
        if (code !== null) return this.numbers.get(code);
        return (weakable(key) ? this.objects.get(key) : undefined) ?? this.strong.get(key);
    }
    endpoints(record) {
        return [record.weakKey ? record.key.deref() : record.key,
                record.weakValue ? record.value.deref() : record.value];
    }
    removeRecord(record) {
        const [key, value] = this.endpoints(record);
        if (key !== undefined) record.index.delete(record.indexKey ?? key);
        if (this.weakness === 'key-or-value' && weakable(value)) {
            const records = this.reverse.get(value);
            records?.delete(record);
            if (!records?.size) this.reverse.delete(value);
        }
        this.records.delete(record.ref);
    }
    get(key) {
        const record = this.lookup(key);
        if (!record) return undefined;
        const [storedKey, value] = this.endpoints(record);
        if (storedKey === undefined || value === undefined) {
            this.removeRecord(record); return undefined;
        }
        return value;
    }
    has(key) { return this.get(key) !== undefined; }
    set(key, value) {
        this.delete(key);
        const code = this.test === 'eql' ? numericKey(key) : null;
        const weakKey = code === null && weakable(key) && this.weakness !== 'value';
        const either = this.weakness === 'key-or-value';
        const weakValue = weakable(value) && ['value', 'key-and-value'].includes(this.weakness);
        // A primitive endpoint cannot be collected. For KEY-OR-VALUE it keeps
        // the entire pair alive, including when the other endpoint is an object.
        const index = code !== null ? this.numbers :
            weakKey && !(either && !weakable(value)) ? this.objects : this.strong;
        const record = {key: weakKey && !either ? ref(key) : key,
            value: weakValue ? ref(value) : value,
            weakKey: weakKey && !either, weakValue, index,
            indexKey: code !== null ? code : undefined};
        record.ref = new WeakRef(record);
        index.set(code ?? key, record);
        this.records.add(record.ref);
        if (either && weakable(value)) {
            let records = this.reverse.get(value);
            if (!records) this.reverse.set(value, records = new Set());
            records.add(record);
        }
        return this;
    }
    delete(key) {
        const record = this.lookup(key);
        if (!record) return false;
        this.removeRecord(record); return true;
    }
    *entries() {
        for (const reference of [...this.records]) {
            const record = reference.deref();
            if (!record) { this.records.delete(reference); continue; }
            const [key, value] = this.endpoints(record);
            if (key === undefined || value === undefined) { this.removeRecord(record); continue; }
            yield [key, value];
        }
    }
    get size() { let count = 0; for (const entry of this.entries()) count++; return count; }
    forEach(fn) { for (const [key, value] of this.entries()) fn(value, key, this); }
}

export class WeakPointer {
    constructor(value) { this.reference = ref(value); }
    deref() { return this.reference.deref(); }
}

const finalizerTokens = new WeakMap();
let finalizers;
const finalizerRegistry = () => finalizers ??= new FinalizationRegistry(callbacks => {
    for (const callback of [...callbacks].reverse()) {
        try { CL_funcall(callback); }
        catch (error) { console.error('Valtan finalizer error:', error); }
    }
});

export function CL_weakOperation(operation, ...args) {
    switch (operation) {
        case 'map': return values1(new WeakHashMap(...args));
        case 'pointer': return values1(new WeakPointer(args[0]));
        case 'pointer-p': return values1(toLispBoolean(args[0] instanceof WeakPointer));
        case 'value':
            if (!(args[0] instanceof WeakPointer)) return simpleError('Not a weak pointer');
            return values1(args[0].deref() ?? S_nil);
        case 'gc': globalThis.gc?.(); return values1(S_nil);
        case 'finalize': {
            const [object, callback] = args;
            if (!weakable(object)) return simpleError('Finalization requires a heap object');
            let token = finalizerTokens.get(object);
            if (!token) {
                token = {callbacks: []};
                finalizerTokens.set(object, token);
                finalizerRegistry().register(object, token.callbacks, token);
            }
            token.callbacks.push(callback);
            return values1(object);
        }
        case 'cancel': {
            const object = args[0], token = weakable(object) && finalizerTokens.get(object);
            if (token) { finalizers.unregister(token); finalizerTokens.delete(object); }
            return values1(object);
        }
        default: return simpleError(`Unknown weak operation ${operation}`);
    }
}
