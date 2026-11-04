import { S_nil, S_t, S_simple_make_string } from './header';
import { intern, cl_package, system_package, keyword_package, findPackage } from './package';
import { isCons, listToJsArray } from './cons';
import { values1, currentValues } from './values';
import { simpleError, typeError } from './error';
import { numberValue } from './number';

function expandDecimal(text) {
    if (!/[eE]/.test(text)) return text;
    const [coefficient, power] = text.toLowerCase().split('e');
    const point = (coefficient.indexOf('.') < 0 ? coefficient.length : coefficient.indexOf('.')) + Number(power);
    const digits = coefficient.replace('.', '');
    return point <= 0 ? '0.' + '0'.repeat(-point) + digits :
        point >= digits.length ? digits + '0'.repeat(point - digits.length) : digits.slice(0, point) + '.' + digits.slice(point);
}
function fixedDecimal(value, digits, width, signLength) {
    let text = expandDecimal(String(value));
    if (digits == null) {
        if (width != null) {
            const integral = value < 1 ? 1 : Math.floor(Math.log10(value)) + 1;
            const available = Math.max(0, width - signLength - integral - 1);
            const shortest = text.includes('.') ? text.length - text.indexOf('.') - 1 : 0;
            text = expandDecimal(value.toFixed(Math.min(100, shortest, available)));
            if (text.includes('.')) text = text.replace(/0+$/, '').replace(/\.$/, '');
        }
        if (!text.includes('.')) text += '.0';
    } else {
        text = expandDecimal(value.toFixed(digits));
        if (!text.includes('.')) text += '.' + '0'.repeat(digits);
    }
    if (width != null && text.length + signLength > width && text.startsWith('0.') && digits !== 0) text = text.slice(1);
    return text;
}
function exponentialDecimal(value, digits, width, signLength, exponentDigits, scale, marker) {
    let parts = value.toExponential().split('e');
    let exponent = Number(parts[1]) - scale + 1;
    const suffix = () => marker + (exponent < 0 ? '-' : '+') + String(Math.abs(exponent)).padStart(exponentDigits, '0');
    let fraction;
    if (digits == null) {
        const shortest = parts[0].replace('.', '').length;
        fraction = Math.max(1, shortest - scale);
        if (width != null) fraction = Math.max(0, Math.min(fraction, width - signLength - Math.max(1, scale) - 1 - suffix().length));
    } else fraction = scale > 0 ? Math.max(0, digits - scale + 1) : digits;
    const precision = Math.max(1, scale + fraction);
    parts = value.toExponential(Math.min(100, precision - 1)).split('e');
    exponent = Number(parts[1]) - scale + 1;
    const mantissa = parts[0].replace('.', '');
    let text = scale <= 0 ? '0.' + '0'.repeat(-scale) + mantissa :
        mantissa.padEnd(scale, '0').slice(0, scale) + '.' + mantissa.slice(scale);
    if (digits == null && text.includes('.') && text.length - text.indexOf('.') - 1 > 1) text = text.replace(/0+$/, '').replace(/\.$/, '.0');
    return text + suffix();
}

const cache = new Map();
const cl = (name, ...args) => intern(name, cl_package).getFunction()(...args);
const keyword = name => intern(name, keyword_package);
const raw = value => intern('ARRAY-TO-RAW-STRING', system_package).getFunction()(value);
const string = value => S_simple_make_string.getFunction()(value);
const fail = message => simpleError(`FORMAT: ${message}`);

// Parse independently of the arguments: V and # are evaluated when the
// directive is reached, including inside recursive controls.
function parse(control) {
    if (cache.has(control)) return cache.get(control);
    let pos = 0;
    function directive() {
        const params = [];
        let colon = false, at = false, needParameter = false;
        while (pos < control.length) {
            const c = control[pos];
            if (c === ',') { params.push(null); pos++; needParameter = true; continue; }
            let value;
            if (c === "'") { pos++; if (pos >= control.length) fail('missing character parameter'); value = control[pos++]; }
            else if (c === 'v' || c === 'V' || c === '#') { value = { kind: c.toUpperCase() }; pos++; }
            else if (/[0-9+-]/.test(c)) {
                const match = /^[+-]?\d+/.exec(control.slice(pos));
                if (!match) fail('invalid numeric parameter');
                value = Number(match[0]); pos += match[0].length;
            } else {
                if (needParameter) params.push(null);
                break;
            }
            params.push(value); needParameter = false;
            if (control[pos] !== ',') break;
            pos++; needParameter = true;
        }
        while (control[pos] === ':' || control[pos] === '@') {
            if (control[pos++] === ':') colon = true; else at = true;
        }
        if (pos >= control.length) fail('unterminated directive');
        const op = control[pos++].toUpperCase();
        const result = { op, params, colon, at };
        if (op === '/') {
            const end = control.indexOf('/', pos);
            if (end < 0) fail('unterminated function name');
            result.name = control.slice(pos, end).toUpperCase(); pos = end + 1;
        }
        return result;
    }
    function sequence(end) {
        const clauses = [[]], separators = [];
        while (pos < control.length) {
            if (control[pos] !== '~') {
                const start = pos;
                while (pos < control.length && control[pos] !== '~') pos++;
                clauses.at(-1).push(control.slice(start, pos)); continue;
            }
            pos++;
            const item = directive();
            if (item.op === end) return { clauses, separators, close: item };
            if (item.op === ';') {
                if (end !== ']' && end !== '>') fail('misplaced separator');
                separators.push(item); clauses.push([]); continue;
            }
            const closing = { '[': ']', '{': '}', '(': ')', '<': '>' }[item.op];
            if (closing) Object.assign(item, sequence(closing));
            else if (']})>'.includes(item.op)) fail('unmatched closing directive');
            if (item.op === '\n' && !item.colon) {
                while (pos < control.length && /[ \t\r\n\f]/.test(control[pos])) pos++;
            }
            clauses.at(-1).push(item);
        }
        if (end) fail('unclosed directive');
        return { clauses, separators };
    }
    const result = sequence(null).clauses[0];
    function validate(nodes, justification = false) {
        let dynamic = false, conditionalJustification = false;
        for (const node of nodes) {
            if (typeof node === 'string') continue;
            const logical = node.op === '<' && node.close.colon;
            const isDynamic = logical || ['W', '_', 'I'].includes(node.op) || node.op === 'T' && node.colon;
            if (justification && isDynamic) fail('pretty directives cannot be nested in justification');
            dynamic ||= isDynamic;
            if (logical) {
                if (node.clauses.length > 3) fail('too many logical block segments');
                const constants = node.clauses.length === 1 ? [] : [node.clauses[0], ...(node.clauses.length === 3 ? [node.clauses[2]] : [])];
                if (constants.some(clause => clause.some(part => typeof part !== 'string'))) fail('logical block prefix and suffix must be constant');
            }
            if (node.op === '<' && !logical && node.separators.some(s => s.colon)) conditionalJustification = true;
            for (const clause of node.clauses || []) {
                const child = validate(clause, justification || node.op === '<' && !logical);
                dynamic ||= child.dynamic; conditionalJustification ||= child.conditionalJustification;
            }
        }
        if (dynamic && conditionalJustification) fail('conditional justification cannot be combined with pretty directives');
        return { dynamic, conditionalJustification };
    }
    validate(result);
    if (cache.size >= 512) cache.delete(cache.keys().next().value);
    cache.set(control, result);
    return result;
}

class Arguments {
    constructor(list, prettyContext = null) {
        this.items = []; this.tails = []; this.index = 0;
        this.prettyContext = prettyContext;
        const seen = new Set();
        let tail = list;
        while (isCons(tail) && !seen.has(tail)) {
            seen.add(tail); this.tails.push(tail); this.items.push(tail.car); tail = tail.cdr;
        }
        if (tail !== S_nil && !prettyContext) typeError(tail, intern('LIST', cl_package));
        this.tails.push(tail);
    }
    take() {
        if (this.prettyContext) {
            this.prettyContext.flush();
            const value = cl('%PRETTY-POP', cl('LIST', this.tail(), this.index), this.prettyContext.stream);
            if (currentValues()[1] !== S_nil) throw new LogicalEscape();
            if (this.index < this.items.length) this.index++;
            return value;
        }
        if (!this.remaining()) fail('not enough arguments');
        return this.items[this.index++];
    }
    remaining() { return this.items.length - this.index + Number(this.tails.at(-1) !== S_nil); }
    tail() { return this.tails[this.index]; }
    move(index) {
        if (!Number.isInteger(index) || index < 0 || index > this.items.length) fail('argument index out of range');
        this.index = index;
    }
}

class Escape { constructor(outer) { this.outer = outer; } }
class LogicalEscape {}

function integerText(value, radix, signed = false, grouped = false, comma = ',', interval = 3) {
    const n = BigInt(value);
    let digits = (n < 0n ? -n : n).toString(radix).toUpperCase();
    if (grouped) {
        if (interval <= 0) fail('group interval must be positive');
        let result = '';
        for (let i = 0; i < digits.length; i++) {
            if (i && (digits.length - i) % interval === 0) result += comma;
            result += digits[i];
        }
        digits = result;
    }
    return (n < 0n ? '-' : signed ? '+' : '') + digits;
}

const small = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine',
    'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen'];
const tens = ['', '', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety'];
const scales = ['', 'thousand', 'million', 'billion', 'trillion', 'quadrillion', 'quintillion',
    'sextillion', 'septillion', 'octillion', 'nonillion', 'decillion', 'undecillion', 'duodecillion',
    'tredecillion', 'quattuordecillion', 'quindecillion', 'sexdecillion', 'septendecillion', 'octodecillion', 'novemdecillion', 'vigintillion'];
function english(value, ordinal) {
    let n = BigInt(value), result;
    if (n < 0n) return 'negative ' + english(-n, ordinal);
    if (n < 20n) result = small[Number(n)];
    else if (n < 100n) result = tens[Number(n / 10n)] + (n % 10n ? '-' + english(n % 10n, false) : '');
    else if (n < 1000n) result = english(n / 100n, false) + ' hundred' + (n % 100n ? ' ' + english(n % 100n, false) : '');
    else {
        let power = 1000n, scale = 1;
        while (n >= power * 1000n && scale < scales.length - 1) { power *= 1000n; scale++; }
        result = english(n / power, false) + ' ' + scales[scale] + (n % power ? ' ' + english(n % power, false) : '');
    }
    if (ordinal) {
        const special = { one: 'first', two: 'second', three: 'third', five: 'fifth', eight: 'eighth', nine: 'ninth', twelve: 'twelfth' };
        result = result.replace(/[a-z]+$/, word => special[word] || (word.endsWith('y') ? word.slice(0, -1) + 'ieth' : word + 'th'));
    }
    return result;
}

function roman(value, old) {
    let n = Number(value);
    if (!Number.isInteger(n) || n <= 0 || n >= (old ? 5000 : 4000)) fail('Roman numeral out of range');
    let output = '';
    for (const [amount, text] of (old
        ? [[1000, 'M'], [500, 'D'], [100, 'C'], [50, 'L'], [10, 'X'], [5, 'V'], [1, 'I']]
        : [[1000, 'M'], [900, 'CM'], [500, 'D'], [400, 'CD'], [100, 'C'], [90, 'XC'], [50, 'L'], [40, 'XL'], [10, 'X'], [9, 'IX'], [5, 'V'], [4, 'IV'], [1, 'I']])) {
        while (n >= amount) { output += text; n -= amount; }
    }
    return output;
}

function run(nodes, args, ctx, outerRemaining = null) {
    const render = (object, escape, colon = false, base = S_nil) => raw(cl('FORMAT-OBJECT-STRING', object, escape ? S_t : S_nil, colon ? S_t : S_nil, base));
    const capture = fn => {
        const child = makeContext(cl('MAKE-STRING-OUTPUT-STREAM'));
        fn(child); child.flush();
        return raw(cl('GET-OUTPUT-STREAM-STRING', child.stream));
    };
    for (const node of nodes) {
        if (typeof node === 'string') {
            if (ctx.fillText) {
                for (const part of node.match(/ +|[^ ]+/g) || []) {
                    ctx.emit(part);
                    if (/^ +$/.test(part)) { ctx.flush(); cl('PPRINT-NEWLINE', keyword('FILL'), ctx.stream); }
                }
            } else ctx.emit(node);
            continue;
        }
        const params = node.params.map(p => p?.kind === 'V' ? args.take() : p?.kind === '#' ? args.remaining() : p);
        const p = (index, fallback) => {
            const value = params[index] == null || params[index] === S_nil ? fallback : params[index];
            return typeof value === 'bigint' ? Number(value) : value;
        };
        const repeat = (text, count) => { if (!Number.isSafeInteger(count) || count < 0) fail('invalid repetition count'); ctx.emit(text.repeat(count)); };
        const { op, colon, at } = node;
        switch (op) {
        case 'W':
            { const object = args.take(); ctx.flush();
              cl('FORMAT-WRITE-OBJECT', object, colon ? S_t : S_nil, at ? S_t : S_nil, ctx.stream); }
            break;
        case 'A': case 'S': {
            if (!params.length) {
                const object = args.take(); ctx.flush();
                cl('FORMAT-OUTPUT-OBJECT', ctx.stream, object, op === 'S' ? S_t : S_nil, colon ? S_t : S_nil);
                ctx.column = cl('STREAM-COLUMN', ctx.stream);
                break;
            }
            const text = render(args.take(), op === 'S', colon);
            const mincol = p(0, 0), colinc = p(1, 1), minpad = p(2, 0), pad = p(3, ' ');
            if (colinc <= 0) fail('column increment must be positive');
            let count = Math.max(0, minpad);
            if (text.length + count < mincol) count += Math.ceil((mincol - text.length - count) / colinc) * colinc;
            if (at) repeat(pad, count);
            ctx.emit(text);
            if (!at) repeat(pad, count);
            break;
        }
        case 'D': case 'B': case 'O': case 'X': case 'R': {
            const value = args.take();
            if (op === 'R' && p(0, null) === null) {
                ctx.emit(at ? roman(value, colon) : english(value, colon)); break;
            }
            const offset = op === 'R' ? 1 : 0;
            const radix = op === 'R' ? p(0, 10) : { D: 10, B: 2, O: 8, X: 16 }[op];
            if (!Number.isInteger(radix) || radix < 2 || radix > 36) fail('invalid radix');
            const integral = typeof value === 'bigint' || typeof value === 'number' && Number.isInteger(value);
            const text = integral ? integerText(value, radix, at, colon, p(offset + 2, ','), p(offset + 3, 3)) : render(value, false, false, radix);
            repeat(p(offset + 1, ' '), Math.max(0, p(offset, 0) - text.length)); ctx.emit(text); break;
        }
        case 'C': {
            const value = args.take();
            if (typeof value !== 'string' || value.length !== 1) fail('character required');
            const name = cl('CHAR-NAME', value);
            const named = name !== S_nil ? raw(name) : value;
            ctx.emit(colon ? named : at ? '#\\' + named : value); break;
        }
        case '%': repeat('\n', p(0, 1)); break;
        case '|': repeat('\f', p(0, 1)); break;
        case '~': repeat('~', p(0, 1)); break;
        case '&': {
            const count = p(0, 1);
            if (count > 0) { if (ctx.column !== 0) ctx.emit('\n'); repeat('\n', count - 1); }
            break;
        }
        case '\n': if (at) ctx.emit('\n'); break;
        case '_':
            ctx.flush();
            cl('PPRINT-NEWLINE', keyword(colon ? at ? 'MANDATORY' : 'FILL' : at ? 'MISER' : 'LINEAR'), ctx.stream);
            break;
        case 'I':
            ctx.flush(); cl('PPRINT-INDENT', keyword(colon ? 'CURRENT' : 'BLOCK'), p(0, 0), ctx.stream);
            break;
        case 'T': {
            const col = p(0, 1), increment = p(1, 1);
            if (colon || cl('%PRETTY-CURRENT', ctx.stream) !== S_nil) {
                ctx.flush();
                cl('PPRINT-TAB', keyword(colon ? at ? 'SECTION-RELATIVE' : 'SECTION' : at ? 'LINE-RELATIVE' : 'LINE'), col, increment, ctx.stream);
                break;
            }
            let count;
            if (at) count = col + (increment ? (increment - (ctx.column + col) % increment) % increment : 0);
            else count = ctx.column < col ? col - ctx.column : increment ? increment - (ctx.column - col) % increment : 0;
            repeat(' ', count); break;
        }
        case '*': args.move(at ? p(0, 0) : args.index + (colon ? -1 : 1) * p(0, 1)); break;
        case 'P': {
            if (colon) args.move(args.index - 1);
            const one = args.take() === 1;
            ctx.emit(at ? one ? 'y' : 'ies' : one ? '' : 's'); break;
        }
        case '?': {
            const control = args.take(), nested = at ? args : new Arguments(args.take());
            if (typeof control === 'function') {
                ctx.flush();
                const tail = control(ctx.stream, ...nested.items.slice(nested.index));
                nested.move(nested.items.length - listToJsArray(tail).length);
                ctx.column = cl('STREAM-COLUMN', ctx.stream);
            } else {
                try { run(parse(raw(control)), nested, ctx); } catch (e) { if (!(e instanceof Escape)) throw e; }
            }
            break;
        }
        case '^': {
            // NIL parameter values are omitted; trailing omissions select the
            // lower-arity termination rule, including the no-parameter rule.
            while (params.length && (params.at(-1) == null || params.at(-1) === S_nil)) params.pop();
            let escape;
            if (!params.length) escape = colon ? outerRemaining === 0 : args.remaining() === 0;
            else if (params.length === 1) escape = Number(p(0, 0)) === 0;
            else if (params.length === 2) escape = p(0, 0) === p(1, 0);
            else escape = p(0, 0) <= p(1, 0) && p(1, 0) <= p(2, 0);
            if (escape) throw new Escape(colon);
            break;
        }
        case '[': {
            let index;
            if (at) { if (args.take() === S_nil) break; args.move(args.index - 1); index = 0; }
            else if (colon) index = args.take() === S_nil ? 0 : 1;
            else index = p(0, null) ?? args.take();
            if (index < 0 || index >= node.clauses.length) {
                index = node.separators.findIndex(separator => separator.colon);
                if (index < 0) break;
                index++;
            }
            run(node.clauses[index], args, ctx, outerRemaining); break;
        }
        case '{': {
            let body = node.clauses[0];
            if (!body.length) { const control = args.take(); body = typeof control === 'function' ? control : parse(raw(control)); }
            const nested = at ? args : new Arguments(args.take()), limit = p(0, Infinity);
            let iteration = 0;
            while (iteration < limit && (nested.remaining() || iteration === 0 && node.close.colon)) {
                const before = nested.index;
                const current = colon ? new Arguments(nested.take()) : nested;
                try {
                    if (typeof body === 'function') {
                        ctx.flush();
                        const tail = body(ctx.stream, ...current.items.slice(current.index));
                        current.move(current.items.length - listToJsArray(tail).length);
                        ctx.column = cl('STREAM-COLUMN', ctx.stream);
                    } else run(body, current, ctx, colon ? nested.remaining() : null);
                }
                catch (e) { if (!(e instanceof Escape)) throw e; if (!colon || e.outer) break; }
                iteration++;
                if (!colon && nested.index === before && limit === Infinity && nested.remaining()) fail('iteration does not consume arguments');
            }
            break;
        }
        case '(': {
            let escape;
            const text = capture(child => {
                try { run(node.clauses[0], args, child, outerRemaining); }
                catch (e) { if (!(e instanceof Escape)) throw e; escape = e; }
            });
            ctx.emit(colon && at ? text.toUpperCase() : colon ? text.toLowerCase().replace(/\b[a-z]/g, c => c.toUpperCase()) : at ? text.toLowerCase().replace(/[a-z]/, c => c.toUpperCase()) : text.toLowerCase());
            if (escape) throw escape;
            break;
        }
        case '<': {
            if (node.close.colon) {
                const object = at ? args.tail() : args.take();
                if (at) args.move(args.items.length);
                const prefix = node.clauses.length > 1 ? node.clauses[0].join('') : colon ? '(' : '';
                const suffix = node.clauses.length > 2 ? node.clauses[2].join('') : colon ? ')' : '';
                const body = node.clauses[node.clauses.length > 1 ? 1 : 0];
                ctx.flush();
                cl('%CALL-PRETTY-BLOCK', ctx.stream, object, string(prefix), string(suffix),
                    node.separators[0]?.at ? string(prefix) : S_nil, stream => {
                        const child = makeContext(stream);
                        child.fillText = node.close.at;
                        const nested = new Arguments(object, child);
                        try { run(body, nested, child); }
                        catch (e) { if (!(e instanceof Escape) && !(e instanceof LogicalEscape)) throw e; }
                        child.flush();
                        return S_nil;
                    });
                ctx.column = cl('STREAM-COLUMN', ctx.stream);
                break;
            }
            const parts = [];
            for (const clause of node.clauses) {
                try { parts.push(capture(child => run(clause, args, child, outerRemaining))); }
                catch (e) { if (!(e instanceof Escape)) throw e; break; }
            }
            const mincol = p(0, 0), colinc = p(1, 1), minpad = p(2, 0), pad = p(3, ' ');
            let gaps = Math.max(0, parts.length - 1) + Number(colon) + Number(at);
            const left = colon || gaps === 0;
            if (gaps === 0) gaps = 1;
            const size = parts.reduce((n, part) => n + part.length, 0);
            let padding = gaps * minpad;
            if (colinc <= 0) fail('column increment must be positive');
            if (size + padding < mincol) padding += Math.ceil((mincol - size - padding) / colinc) * colinc;
            const gap = () => { const n = Math.floor(padding / gaps); repeat(pad, n); padding -= n; gaps--; };
            if (left) gap();
            parts.forEach((part, i) => { ctx.emit(part); if (i < parts.length - 1) gap(); });
            if (at) gap(); break;
        }
        case 'F': case 'E': case 'G': case '$': {
            const object = args.take(), value = Number(numberValue(object));
            if (!Number.isFinite(value)) { ctx.emit(render(object, false)); break; }
            const money = op === '$', width = p(money ? 2 : 0, null), d = p(money ? 0 : 1, null);
            const sign = value < 0 || Object.is(value, -0) ? '-' : at ? '+' : '';
            let magnitude = Math.abs(value), text;
            if (op === 'F') magnitude *= 10 ** p(2, 0);
            if (money) text = magnitude.toFixed(d ?? 2).padStart(p(1, 1) + (d ?? 2) + 1, '0');
            else if (op === 'F') text = fixedDecimal(magnitude, d, width, sign.length);
            else if (op === 'E') text = exponentialDecimal(magnitude, d, width, sign.length, p(2, 1), p(3, 1), p(6, 'E'));
            else {
                const digits = d ?? Math.max(1, String(magnitude).replace(/[^0-9]/g, '').length);
                text = op === 'G' ? magnitude.toPrecision(digits) : magnitude.toExponential(d ?? 6);
                text = text.replace(/e([+-])(\d+)/, (_, sign, exponent) => (p(6, 'E') + sign + exponent.padStart(p(2, 1), '0')));
            }
            if (!/[.Ee]/.test(text)) text += '.';
            text = sign + text;
            const overflow = p(op === 'F' ? 3 : 4, null), pad = p(money ? 3 : op === 'F' ? 4 : 5, ' ');
            if (!money && overflow != null && width && text.length > width) repeat(overflow, width);
            else if (money && colon) { ctx.emit(sign); repeat(pad, Math.max(0, width - text.length)); ctx.emit(text.slice(sign.length)); }
            else { repeat(pad, Math.max(0, width - text.length)); ctx.emit(text); }
            break;
        }
        case '/': {
            const [packageName, symbolName] = node.name.includes(':') ? node.name.split(/::?/) : ['COMMON-LISP-USER', node.name];
            const pkg = findPackage(packageName);
            if (!pkg) fail('unknown function package');
            const [symbol] = pkg.findSymbol(symbolName);
            if (!symbol) fail('unknown formatting function');
            const object = args.take(); ctx.flush();
            symbol.getFunction()(ctx.stream, object, colon ? S_t : S_nil, at ? S_t : S_nil, ...params.map(x => x ?? S_nil));
            ctx.column = cl('STREAM-COLUMN', ctx.stream); break;
        }
        default: fail(`unsupported directive ${op}`);
        }
    }
}

function makeContext(stream) {
    const ctx = { stream, output: '', column: cl('STREAM-COLUMN', stream) };
    ctx.emit = text => {
        ctx.output += text;
        const i = text.lastIndexOf('\n');
        ctx.column = i < 0 ? ctx.column + text.length : text.length - i - 1;
    };
    ctx.flush = () => { if (ctx.output) { cl('WRITE-STRING', string(ctx.output), stream); ctx.output = ''; } };
    return ctx;
}

export function CL_formatControl(stream, control, list) {
    const args = new Arguments(list), ctx = makeContext(stream);
    try { run(parse(String(control)), args, ctx); }
    catch (e) { if (!(e instanceof Escape)) throw e; }
    ctx.flush();
    return values1(args.tail());
}
