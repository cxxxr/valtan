import { typeError } from './error';
import { intern, cl_package } from './package';
import {
    toLispBoolean, S_integer, S_number, S_nil
} from './header';
import {
    values1,
    values
} from './values';

// Float class to distinguish floats from integers in JavaScript
// JavaScript treats 3 and 3.0 as identical, but Common Lisp distinguishes them
export class Float {
    constructor(value) {
        this.value = Number(value);
    }

    valueOf() {
        return this.value;
    }
}

export class Complex {
    constructor(real, imag) {
        this.real = real;
        this.imag = imag;
    }
}

// Ratios are canonical: coprime BigInt parts, positive denominator, and
// integral results represented by the existing integer representation.
export class Ratio {
    constructor(numerator, denominator) {
        this.numerator = numerator;
        this.denominator = denominator;
    }
    valueOf() {
        const negative = this.numerator < 0n;
        const n = negative ? -this.numerator : this.numerator, d = this.denominator;
        // Scale both operands before converting, avoiding Infinity / Infinity.
        const ns = Math.max(0, n.toString(2).length - 54);
        const ds = Math.max(0, d.toString(2).length - 54);
        return (negative ? -1 : 1) * Number(n >> BigInt(ns)) / Number(d >> BigInt(ds)) * 2 ** (ns - ds);
    }
}

const isRational = x => isInteger(x) || x instanceof Ratio;
const rationalParts = x => x instanceof Ratio ? [x.numerator, x.denominator] : [BigInt(x), 1n];
const realType = () => intern('REAL', cl_package);
function checkReal(x) {
    if (!isNumber(x) || x instanceof Complex) typeError(x, realType());
}
function checkRational(x) {
    if (!isRational(x)) typeError(x, intern('RATIONAL', cl_package));
}
function divisionByZero() { intern('ERROR', cl_package).getFunction()(intern('DIVISION-BY-ZERO', cl_package)); }
export function makeRatio(n, d) {
    n = BigInt(n); d = BigInt(d);
    if (d === 0n) divisionByZero();
    if (d < 0n) { n = -n; d = -d; }
    let a = n < 0n ? -n : n, b = d;
    while (b !== 0n) [a, b] = [b, a % b];
    n /= a; d /= a;
    return d === 1n ? normalizeInteger(n) : new Ratio(n, d);
}
export function CL_rationalp(x) { return values1(toLispBoolean(isRational(x))); }
export function CL_numerator(x) { checkRational(x); return values1(normalizeInteger(rationalParts(x)[0])); }
export function CL_denominator(x) { checkRational(x); return values1(normalizeInteger(rationalParts(x)[1])); }
export function CL_float(x, prototype) {
    checkReal(x);
    if (prototype !== undefined && !(prototype instanceof Float)) typeError(prototype, intern('FLOAT', cl_package));
    return values1(toFloat(x));
}
// IEEE binary64 decomposition, used for exact rational conversion/comparison.
function exactRealParts(x) {
    if (isRational(x)) return rationalParts(x);
    const value = Number(numberValue(x));
    if (!Number.isFinite(value)) return null;
    const view = new DataView(new ArrayBuffer(8));
    view.setFloat64(0, value);
    const bits = view.getBigUint64(0), exponent = Number((bits >> 52n) & 2047n);
    let mantissa = bits & ((1n << 52n) - 1n);
    if (exponent) mantissa |= 1n << 52n;
    if (bits >> 63n) mantissa = -mantissa;
    const shift = (exponent || 1) - 1023 - 52;
    return shift >= 0 ? [mantissa << BigInt(shift), 1n] : [mantissa, 1n << BigInt(-shift)];
}
export function CL_rational(x) {
    checkReal(x);
    if (isRational(x)) return values1(x);
    const parts = exactRealParts(x);
    if (!parts) intern('ERROR', cl_package).getFunction()(intern('ARITHMETIC-ERROR', cl_package));
    return values1(makeRatio(...parts));
}

// Order every finite binary64 value, keeping the two signed zeros adjacent
// and distinct. Interval bounds use numeric comparison, so a closed zero
// bound includes both; EQL singleton types use the exact bit pattern.
export function CL_floatRank(bound, mode) {
    const maximumMagnitude = 0x7fefffffffffffffn;
    const lastRank = 2n * maximumMagnitude + 1n;
    const view = new DataView(new ArrayBuffer(8));
    if (mode === 0) {
        if (!(bound instanceof Float)) typeError(bound, intern('FLOAT', cl_package));
        view.setFloat64(0, bound.value);
        const bits = view.getBigUint64(0), magnitude = bits & 0x7fffffffffffffffn;
        if (magnitude > maximumMagnitude) intern('ERROR', cl_package).getFunction()('Non-finite float type bound');
        return values1(normalizeInteger(bits >> 63n ? maximumMagnitude - magnitude : maximumMagnitude + 1n + magnitude));
    }
    const lower = mode === 1 || mode === 2;
    const open = mode === 2 || mode === 4;
    if (bound === S_nil) return values1(normalizeInteger(lower ? 0n : lastRank));
    checkReal(bound);
    let left = 0n, right = lastRank + 1n;
    while (left < right) {
        const middle = (left + right) >> 1n;
        const bits = middle <= maximumMagnitude ?
            (1n << 63n) | (maximumMagnitude - middle) : middle - maximumMagnitude - 1n;
        view.setBigUint64(0, bits);
        const comparison = compareReal(new Float(view.getFloat64(0)), bound);
        // Search for the first accepted lower bound, or first excluded upper bound.
        const before = lower ? (open ? comparison <= 0 : comparison < 0) :
            (open ? comparison < 0 : comparison <= 0);
        if (before) left = middle + 1n;
        else right = middle;
    }
    return values1(normalizeInteger(lower ? left : left - 1n));
}

// Get the raw numeric value from a number (whether Float or primitive)
export function numberValue(x) {
    return (x instanceof Float || x instanceof Ratio) ? x.valueOf() : x;
}

// Check if x is any kind of number (primitive or Float)
function isNumber(x) {
    return (typeof(x) === 'number') || (typeof x === 'bigint') || (x instanceof Float) || (x instanceof Complex) || (x instanceof Ratio);
}

function complexParts(x) {
    return (x instanceof Complex) ? [x.real, x.imag] : [x, 0];
}

// Create a Float if needed (for preserving floatness in operations)
// If value is already a Float, return it as-is to avoid nesting
export function toFloat(value) {
    if (value instanceof Float) {
        return value;
    }
    return new Float(value);
}

export function CL_numberp(x) {
    return values1(toLispBoolean(isNumber(x)));
}

function logarithmParts(x) {
    if (!isNumber(x)) typeError(x, S_number);
    if (x instanceof Complex) {
        const real = Number(numberValue(x.real)), imag = Number(numberValue(x.imag));
        return [Math.log(Math.hypot(real, imag)), Math.atan2(imag, real)];
    }
    if (typeof x === 'bigint' && x !== 0n) {
        const magnitude = x < 0n ? -x : x;
        const shift = Math.max(0, magnitude.toString(2).length - 53);
        return [Math.log(Number(magnitude >> BigInt(shift))) + shift * Math.LN2, x < 0n ? Math.PI : 0];
    }
    const value = Number(numberValue(x));
    return [Math.log(Math.abs(value)), value < 0 ? Math.PI : 0];
}

export function CL_log(x, base) {
    let [real, imag] = logarithmParts(x);
    let complex = x instanceof Complex || imag !== 0;
    if (base !== undefined) {
        if (base === 0n || numberValue(base) === 0 || base instanceof Complex &&
            Number(numberValue(base.real)) === 0 && Number(numberValue(base.imag)) === 0) return values1(new Float(0));
        const [br, bi] = logarithmParts(base), denominator = br * br + bi * bi;
        if (bi === 0) [real, imag] = [real / br, imag / br];
        else [real, imag] = [(real * br + imag * bi) / denominator, (imag * br - real * bi) / denominator];
        complex ||= base instanceof Complex || bi !== 0;
    }
    return values1(complex ? new Complex(new Float(real), new Float(imag)) : new Float(real));
}

export function CL_abs(x) {
    if (!isNumber(x)) typeError(x, S_number);
    if (x instanceof Complex) {
        return values1(new Float(Math.hypot(Number(numberValue(x.real)), Number(numberValue(x.imag)))));
    }
    if (isRational(x)) return values1(rationalParts(x)[0] < 0n ? CL_negate(x) : x);
    return values1(new Float(Math.abs(numberValue(x))));
}

export function CL_numberPrintString(x, base, radix) {
    if (!isNumber(x)) typeError(x, S_number);
    if (x instanceof Complex) {
        return values1(`#C(${CL_numberPrintString(x.real, base, radix)} ${CL_numberPrintString(x.imag, base, radix)})`);
    }
    if (x instanceof Ratio) {
        const numerator = CL_numberPrintString(normalizeInteger(x.numerator), base, S_nil);
        const denominator = CL_numberPrintString(normalizeInteger(x.denominator), base, S_nil);
        const prefix = radix === S_nil ? '' : ({ 2: '#b', 8: '#o', 16: '#x' }[base] || `#${base}r`);
        return values1(prefix + numerator + '/' + denominator);
    }
    if (isInteger(x)) {
        if (!Number.isInteger(base) || base < 2 || base > 36) typeError(base, S_integer);
        const digits = BigInt(x).toString(base).toUpperCase();
        if (radix === S_nil) return values1(digits);
        if (base === 10) return values1(digits + '.');
        return values1(({ 2: '#b', 8: '#o', 16: '#x' }[base] || `#${base}r`) + digits);
    }
    const value = numberValue(x);
    let text = Object.is(value, -0) ? '-0' : String(value);
    if (Number.isFinite(value) && !text.includes('.')) {
        const exponent = text.indexOf('e');
        text = exponent < 0 ? text + '.0' : text.slice(0, exponent) + '.0' + text.slice(exponent);
    }
    return values1(text);
}

export function CL_complexp(x) {
    return values1(toLispBoolean(x instanceof Complex));
}

export function CL_complex(real, imag = 0) {
    checkReal(real); checkReal(imag);
    const floatParts = !isRational(real) || !isRational(imag);
    if (!floatParts && (imag === 0 || imag === 0n)) {
        return values1(real);
    }
    return values1(new Complex(floatParts ? toFloat(real) : real,
                               floatParts ? toFloat(imag) : imag));
}

export function CL_realpart(x) {
    if (!isNumber(x)) typeError(x, S_number);
    return values1((x instanceof Complex) ? x.real : x);
}

export function CL_imagpart(x) {
    if (!isNumber(x)) typeError(x, S_number);
    return values1((x instanceof Complex) ? x.imag : isRational(x) ? 0 : new Float(Number(numberValue(x)) * 0));
}

export function CL_integerp(x) {
    // Float objects are never integers, even if the value is an integer
    if (x instanceof Float) {
        return values1(toLispBoolean(false));
    }
    return values1(toLispBoolean(isInteger(x)));
}

export function CL_floatp(x) {
    // Float objects are always floats
    // Primitive numbers that aren't integers are also floats
    if (x instanceof Float) {
        return values1(toLispBoolean(true));
    }
    return values1(toLispBoolean(typeof(x) === 'number' && !Number.isInteger(x)));
}

export function isInteger(x) {
    return typeof x === 'bigint' || (typeof x === 'number' && Number.isInteger(x));
}

export function normalizeInteger(x) {
    return x >= -9007199254740991n && x <= 9007199254740991n ? Number(x) : x;
}

function integerBinary(x, y, op) {
    if (typeof x === 'number' && typeof y === 'number') {
        const result = op(x, y);
        if (Number.isSafeInteger(result)) return result;
    }
    return normalizeInteger(op(BigInt(x), BigInt(y)));
}

function arithmetic(x, y, op) {
    if (!isNumber(x)) typeError(x, S_number);
    if (!isNumber(y)) typeError(y, S_number);
    if (x instanceof Complex || y instanceof Complex) {
        let [a, b] = complexParts(x), [c, d] = complexParts(y);
        if (![a, b, c, d].every(isRational)) [a, b, c, d] = [a, b, c, d].map(toFloat);
        if (op === '+') return CL_complex(CL_add(a, c), CL_add(b, d));
        if (op === '-') return CL_complex(CL_sub(a, c), CL_sub(b, d));
        if (op === '*') return CL_complex(CL_sub(CL_mul(a, c), CL_mul(b, d)), CL_add(CL_mul(a, d), CL_mul(b, c)));
        const denominator = CL_add(CL_mul(c, c), CL_mul(d, d));
        return CL_complex(CL_div(CL_add(CL_mul(a, c), CL_mul(b, d)), denominator),
                          CL_div(CL_sub(CL_mul(b, c), CL_mul(a, d)), denominator));
    }
    if (isRational(x) && isRational(y)) {
        if (isInteger(x) && isInteger(y) && op !== '/') {
            return integerBinary(x, y, op === '+' ? (a,b) => a+b : op === '-' ? (a,b) => a-b : (a,b) => a*b);
        }
        const [a, b] = rationalParts(x), [c, d] = rationalParts(y);
        if (op === '+') return makeRatio(a*d+c*b, b*d);
        if (op === '-') return makeRatio(a*d-c*b, b*d);
        if (op === '*') return makeRatio(a*c, b*d);
        return makeRatio(a*d, b*c);
    }
    const a = Number(numberValue(x)), b = Number(numberValue(y));
    if (op === '/' && b === 0) divisionByZero();
    return new Float(op === '+' ? a+b : op === '-' ? a-b : op === '*' ? a*b : a/b);
}
export function CL_add(x, y) { return values1(arithmetic(x, y, '+')); }
export function CL_sub(x, y) { return values1(arithmetic(x, y, '-')); }
export function CL_mul(x, y) { return values1(arithmetic(x, y, '*')); }
export function CL_negate(x) {
    if (!isNumber(x)) typeError(x, S_number);
    if (x instanceof Complex) return values1(CL_complex(CL_negate(x.real), CL_negate(x.imag)));
    if (x instanceof Ratio) return values1(new Ratio(-x.numerator, x.denominator));
    return values1(isInteger(x) ? normalizeInteger(-BigInt(x)) : new Float(-numberValue(x)));
}

function checkInteger(x) {
    if (!isInteger(x)) typeError(x, S_integer);
    return BigInt(x);
}

export function CL_ash(integer, count) {
    return values1(normalizeInteger(checkInteger(integer) << checkInteger(count)));
}

export function CL_integerLength(integer) {
    let n = checkInteger(integer);
    if (n < 0n) n = ~n;
    return values1(n === 0n ? 0 : n.toString(2).length);
}

export function CL_logand(x, y) {
    return values1(normalizeInteger(checkInteger(x) & checkInteger(y)));
}

export function CL_div(x, y) { return values1(arithmetic(x, y, '/')); }

function divideInteger(number, divisor, mode) {
    const n = BigInt(number), d = BigInt(divisor);
    if (d === 0n) divisionByZero();
    let q = n / d;
    const r = n % d;
    if (r !== 0n) {
        const sameSign = (n < 0n) === (d < 0n);
        if (mode === 'floor' && !sameSign) q--;
        if (mode === 'ceiling' && sameSign) q++;
        if (mode === 'round') {
            const ar = (r < 0n ? -r : r) * 2n;
            const ad = d < 0n ? -d : d;
            if (ar > ad || (ar === ad && q % 2n !== 0n)) q += sameSign ? 1n : -1n;
        }
    }
    return values(normalizeInteger(q), normalizeInteger(n - q * d));
}

function divideRounded(number, divisor, mode) {
    checkReal(number); checkReal(divisor);
    if (isRational(number) && isRational(divisor)) {
        const [a,b] = rationalParts(number), [c,d] = rationalParts(divisor);
        const q = divideInteger(a*d, b*c, mode);
        return values(q, CL_sub(number, CL_mul(q, divisor)));
    }
    const n = Number(numberValue(number)), d = Number(numberValue(divisor));
    if (d === 0) divisionByZero();
    const quotient = mode === 'round'
        ? (() => { const f = Math.floor(n / d), r = n / d - f;
                   return r > 0.5 || (r === 0.5 && f % 2 !== 0) ? f + 1 : f; })()
        : Math[mode === 'ceiling' ? 'ceil' : mode](n / d);
    return values(normalizeInteger(BigInt(quotient)), new Float(n - quotient * d));
}

export function CL_floor(number, divisor) { return divideRounded(number, divisor, 'floor'); }
export function CL_round(number, divisor) { return divideRounded(number, divisor, 'round'); }
export function CL_truncate(number, divisor) { return divideRounded(number, divisor, 'trunc'); }
export function CL_ceiling(number, divisor) { return divideRounded(number, divisor, 'ceiling'); }

export function CL_rem(number, divisor) {
    const q = divideRounded(number, divisor, 'trunc');
    return values1(CL_sub(number, CL_mul(q, divisor)));
}

function compareReal(x, y) {
    checkReal(x); checkReal(y);
    if (isInteger(x) && isInteger(y)) return x < y ? -1 : x > y ? 1 : 0;
    if (!isRational(x) && !isRational(y)) {
        const a = numberValue(x), b = numberValue(y);
        return a < b ? -1 : a > b ? 1 : a === b ? 0 : NaN;
    }
    const xp = exactRealParts(x), yp = exactRealParts(y);
    if (xp && yp) {
        const a = xp[0] * yp[1], b = yp[0] * xp[1];
        return a < b ? -1 : a > b ? 1 : 0;
    }
    const a = Number(numberValue(x)), b = Number(numberValue(y));
    return a < b ? -1 : a > b ? 1 : a === b ? 0 : NaN;
}
function numberEqual(x, y) {
    if (!isNumber(x)) typeError(x, S_number);
    if (!isNumber(y)) typeError(y, S_number);
    const [a,b] = complexParts(x), [c,d] = complexParts(y);
    return compareReal(a, c) === 0 && compareReal(b, d) === 0;
}
export function CL_numberEqual(x, y) { return values1(toLispBoolean(numberEqual(x, y))); }
export function CL_numberNotEqual(x, y) { return values1(toLispBoolean(!numberEqual(x, y))); }
export function CL_greaterThan(x, y) { return values1(toLispBoolean(compareReal(x, y) > 0)); }
export function CL_lessThan(x, y) { return values1(toLispBoolean(compareReal(x, y) < 0)); }
export function CL_greaterEqual(x, y) { return values1(toLispBoolean(compareReal(x, y) >= 0)); }
export function CL_lessEqual(x, y) { return values1(toLispBoolean(compareReal(x, y) <= 0)); }

function floatBits(x) {
    if (!(x instanceof Float)) typeError(x, intern('FLOAT', cl_package));
    if (!Number.isFinite(x.value)) intern('ERROR', cl_package).getFunction()(intern('ARITHMETIC-ERROR', cl_package));
    const view = new DataView(new ArrayBuffer(8));
    view.setFloat64(0, x.value);
    const bits = view.getBigUint64(0), exponent = Number((bits >> 52n) & 2047n);
    const significand = (bits & ((1n << 52n) - 1n)) | (exponent ? 1n << 52n : 0n);
    return [significand, significand ? (exponent || 1) - 1075 : 0, bits >> 63n ? -1 : 1];
}
function decodedFloat(x) {
    const [sig, exp, sign] = floatBits(x);
    const precision = sig === 0n ? 0 : sig.toString(2).length;
    return [Number(sig) / 2 ** precision, exp + precision, sign];
}
export function CL_integerDecodeFloat(x) {
    const [sig, exp, sign] = floatBits(x);
    return values(normalizeInteger(sig), exp, sign);
}
export function CL_decodeFloat(x) {
    const [sig, exp, sign] = decodedFloat(x);
    return values(new Float(sig), exp, new Float(sign));
}
export function CL_floatRadix(x) { floatBits(x); return values1(2); }
export function CL_floatDigits(x) { floatBits(x); return values1(53); }
export function CL_floatPrecision(x) {
    const [sig] = floatBits(x);
    return values1(sig === 0n ? 0 : sig.toString(2).length);
}
export function CL_floatSign(x, y = new Float(1)) {
    const [, , sign] = floatBits(x); floatBits(y);
    return values1(new Float(sign * Math.abs(y.value)));
}
export function CL_scaleFloat(x, exponent) {
    const [sig, exp, sign] = decodedFloat(x);
    const shift = checkInteger(exponent);
    if (sig === 0) return values1(x);
    const power = BigInt(exp) + shift;
    let result;
    if (power > 1024n) result = Infinity;
    else if (power < -1074n) result = 0;
    else if (power === 1024n) result = sig * 2 ** 1023 * 2;
    else if (power < -1021n) result = sig * 2 ** Number(power + 1074n) * Number.MIN_VALUE;
    else result = sig * 2 ** Number(power);
    if (!Number.isFinite(result)) intern('ERROR', cl_package).getFunction()(intern('FLOATING-POINT-OVERFLOW', cl_package));
    return values1(new Float(sign * result));
}

const negative = x => x < 0 || Object.is(x, -0);
const signed = (magnitude, sign) => negative(sign) ? -magnitude : magnitude;
function complexSqrt(a, b) {
    if (a === 0 && b === 0) return [0, b];
    const t = Math.sqrt(Math.hypot(a, b) / 2 + Math.abs(a) / 2);
    return a >= 0 ? [t, b / (2 * t)] : [Math.abs(b) / (2 * t), signed(t, b)];
}
function complexAsin(a, b) {
    const scale = Math.max(1, Math.hypot(a + 1, b) / 2 + Math.hypot(a - 1, b) / 2);
    const real = Math.asin(Math.max(-1, Math.min(1, a / scale)));
    // CL's real-axis cut is continuous with quadrant IV at x > 1.
    const imag = b === 0 && Math.abs(a) > 1 ? -Math.sign(a) * Math.acosh(scale)
        : signed(Math.acosh(scale), b);
    return [real, imag];
}
function complexMath(op, a, b) {
    switch (op) {
    case 'sqrt': return complexSqrt(a, b);
    case 'exp': return [Math.exp(a) * Math.cos(b), Math.exp(a) * Math.sin(b)];
    case 'sin': return [Math.sin(a) * Math.cosh(b), Math.cos(a) * Math.sinh(b)];
    case 'cos': return [Math.cos(a) * Math.cosh(b), -Math.sin(a) * Math.sinh(b)];
    case 'tan': {
        if (Math.abs(b) > 350) return [0, Math.sign(b)];
        const denominator = Math.cos(2 * a) + Math.cosh(2 * b);
        return [Math.sin(2 * a) / denominator, Math.sinh(2 * b) / denominator];
    }
    case 'sinh': return [Math.sinh(a) * Math.cos(b), Math.cosh(a) * Math.sin(b)];
    case 'cosh': return [Math.cosh(a) * Math.cos(b), Math.sinh(a) * Math.sin(b)];
    case 'tanh': {
        if (Math.abs(a) > 350) return [Math.sign(a), 0];
        const denominator = Math.cosh(2 * a) + Math.cos(2 * b);
        return [Math.sinh(2 * a) / denominator, Math.sin(2 * b) / denominator];
    }
    case 'asin': return complexAsin(a, b);
    case 'acos': { const [r, i] = complexAsin(a, b); return [Math.PI / 2 - r, -i]; }
    case 'asinh': { const [r, i] = complexAsin(-b, a); return [i, -r]; }
    case 'acosh': {
        const scale = Math.max(1, Math.hypot(a + 1, b) / 2 + Math.hypot(a - 1, b) / 2);
        return [Math.acosh(scale), signed(Math.acos(Math.max(-1, Math.min(1, a / scale))), b)];
    }
    case 'atan':
        return [(Math.atan2(a, 1 - b) + Math.atan2(a, 1 + b)) / 2,
                (Math.log(Math.hypot(a, 1 + b)) - Math.log(Math.hypot(a, 1 - b))) / 2];
    case 'atanh':
        return [(Math.log(Math.hypot(1 + a, b)) - Math.log(Math.hypot(1 - a, b))) / 2,
                (Math.atan2(b, 1 + a) + Math.atan2(b, 1 - a)) / 2];
    default: throw new Error(`Unknown numeric operation ${op}`);
    }
}
export function CL_transcendental(op, x) {
    if (!isNumber(x)) typeError(x, S_number);
    if (!(x instanceof Complex)) {
        const value = Number(numberValue(x)), result = Math[op](value);
        if (!Number.isNaN(result)) return values1(new Float(result));
    }
    const [r, i] = complexParts(x).map(part => Number(numberValue(part)));
    const [real, imag] = complexMath(op, r, i);
    return values1(new Complex(new Float(real), new Float(imag)));
}
export function CL_atan(x, y) {
    if (y === undefined) return CL_transcendental('atan', x);
    checkReal(x); checkReal(y);
    return values1(new Float(Math.atan2(Number(numberValue(x)), Number(numberValue(y)))));
}
export function CL_phase(x) {
    if (!isNumber(x)) typeError(x, S_number);
    const [r, i] = complexParts(x).map(part => Number(numberValue(part)));
    return values1(new Float(Math.atan2(i, r)));
}
export function CL_cis(x) {
    checkReal(x);
    const value = Number(numberValue(x));
    return values1(new Complex(new Float(Math.cos(value)), new Float(Math.sin(value))));
}
