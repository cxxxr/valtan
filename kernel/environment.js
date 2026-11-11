import { S_nil, toLispBoolean } from './header';
import { values, values1 } from './values';
import { normalizeInteger, numberValue } from './number';
import { simpleError } from './error';
import { setResumableImplementation } from './resumable';

let hostOS;
let subprocessHost, subprocessFS;
export function CL_initializeEnvironment(os) { hostOS = os; return values1(S_nil); }
const epochOffset = 2208988800n;
const leapYear = year => year % 4n === 0n && (year % 100n !== 0n || year % 400n === 0n);
const monthLengths = year => [31, leapYear(year) ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
function daysBeforeYear(year) {
    const y = year - 1n;
    return 365n * y + y / 4n - y / 100n + y / 400n;
}
const epochDays = daysBeforeYear(1900n);
const integer = x => normalizeInteger(x);
function dateForLocalTime(time) {
    const date = new Date(Number(BigInt(time) - epochOffset) * 1000);
    if (!Number.isFinite(date.getTime())) simpleError('Local time is outside the host calendar range; supply an explicit time zone');
    return date;
}
function decode(time, offset) {
    if (offset === S_nil) {
        const date = dateForLocalTime(time), year = date.getFullYear();
        const january = new Date(0), july = new Date(0);
        january.setFullYear(year, 0, 1); july.setFullYear(year, 6, 1);
        const standardOffset = Math.max(january.getTimezoneOffset(), july.getTimezoneOffset());
        return values(date.getSeconds(), date.getMinutes(), date.getHours(), date.getDate(),
            date.getMonth() + 1, year, (date.getDay() + 6) % 7,
            toLispBoolean(date.getTimezoneOffset() !== standardOffset), standardOffset * 60);
    }
    // Integer calendar arithmetic retains exact seconds even beyond Date's range.
    const local = BigInt(time) - BigInt(offset);
    let days = local / 86400n, seconds = local % 86400n;
    if (seconds < 0n) { days--; seconds += 86400n; }
    const absolute = days + epochDays;
    let year = absolute * 400n / 146097n + 1n;
    while (daysBeforeYear(year) > absolute) year--;
    while (daysBeforeYear(year + 1n) <= absolute) year++;
    let remaining = Number(absolute - daysBeforeYear(year)), month = 0;
    const lengths = monthLengths(year);
    while (remaining >= lengths[month]) remaining -= lengths[month++];
    return values(Number(seconds % 60n), Number(seconds / 60n % 60n), Number(seconds / 3600n),
        remaining + 1, month + 1, integer(year), Number((days % 7n + 7n) % 7n), S_nil, offset);
}
function encode(second, minute, hour, day, month, year, offset) {
    year = BigInt(year);
    if (year < 100n) {
        const current = BigInt(new Date().getFullYear());
        year += current / 100n * 100n;
        if (year < current - 50n) year += 100n;
        if (year >= current + 50n) year -= 100n;
    }
    const lengths = monthLengths(year);
    if (day > lengths[month - 1]) simpleError('Invalid day for the given month and year');
    if (offset === S_nil) {
        const date = new Date(0);
        date.setFullYear(Number(year), month - 1, day);
        date.setHours(hour, minute, second, 0);
        if (!Number.isFinite(date.getTime())) simpleError('Local time is outside the host calendar range; supply an explicit time zone');
        return values1(integer(BigInt(Math.floor(date.getTime() / 1000)) + epochOffset));
    }
    let days = daysBeforeYear(year) - epochDays + BigInt(day - 1);
    for (let i = 0; i < month - 1; i++) days += BigInt(lengths[i]);
    return values1(integer(days * 86400n + BigInt(hour * 3600 + minute * 60 + second) + BigInt(offset)));
}
export function CL_environmentOperation(operation, ...args) {
    const proc = globalThis.process;
    switch (operation.name) {
    case 'INITIALIZE-SUBPROCESS':
        [subprocessHost, subprocessFS] = args;
        return values1(S_nil);
    case 'SUBPROCESS-AVAILABLE': return values1(toLispBoolean(!!subprocessHost));
    case 'RUN-PROGRAM': {
        if (!subprocessHost) return simpleError('Subprocess execution requires Node.js');
        const [command, argv, shell, directory, input, output, errorOutput] = args;
        const opened = [];
        try {
            function descriptor(spec, index) {
                if (spec[0] === 'ignore') return 'ignore';
                if (spec[0] === 'inherit') return index;
                if (spec[0] !== 'file') throw new Error(`Invalid subprocess I/O: ${spec[0]}`);
                const fd = subprocessFS.openSync(spec[1], index === 0 ? 'r' : 'a');
                opened.push(fd);
                return fd;
            }
            const stdio = [descriptor(input, 0), descriptor(output, 1)];
            stdio.push(errorOutput[0] === 'stdout' ? stdio[1] : descriptor(errorOutput, 2));
            const result = subprocessHost.spawnSync(command, argv, {
                shell, cwd: directory ?? undefined, stdio, windowsHide: true,
            });
            if (result.error) throw result.error;
            if (result.status !== null) return values1(result.status);
            const signal = hostOS?.constants?.signals?.[result.signal];
            if (signal) return values1(128 + signal);
            throw new Error(`Subprocess ended without an exit status: ${result.signal}`);
        } catch (error) { return simpleError(`Subprocess failed: ${error.message}`); }
        finally { for (const fd of opened) subprocessFS.closeSync(fd); }
    }
    case 'GETENV': return values1(proc?.env?.[args[0]] ?? S_nil);
    case 'SETENV':
        if (!proc?.env) return simpleError('Environment variables require Node.js');
        if (args[1] === S_nil) delete proc.env[args[0]];
        else proc.env[args[0]] = args[1];
        return values1(args[1]);
    case 'CHDIR':
        if (!proc?.chdir) return simpleError('Changing directories requires Node.js');
        try { proc.chdir(args[0]); } catch (error) { return simpleError(error.message); }
        return values1(S_nil);
    case 'FEATURES': {
        const little = new Uint8Array(new Uint16Array([1]).buffer)[0] === 1;
        const result = [little ? 'LITTLE-ENDIAN' : 'BIG-ENDIAN'];
        if (hostOS) {
            const type = hostOS.type();
            result.push(type === 'Windows_NT' ? 'WINDOWS' : type.toUpperCase());
            if (type !== 'Windows_NT') result.push('UNIX');
            if (['Darwin', 'FreeBSD', 'OpenBSD', 'NetBSD', 'DragonFly'].includes(type)) result.push('BSD');
            const arch = hostOS.arch();
            const cpu = { x64: 'X86-64', ia32: 'X86', arm: 'ARM', arm64: 'ARM64',
                ppc: 'PPC', ppc64: 'PPC64', mips: 'MIPS', mipsel: 'MIPS',
                s390x: 'S390X', riscv64: 'RISCV64', loong64: 'LOONGARCH64' }[arch];
            if (cpu) result.push(cpu);
            if (['x64', 'arm64', 'ppc64', 's390x', 'riscv64', 'loong64', 'mips64el'].includes(arch)) result.push('64-BIT');
            else if (['ia32', 'arm', 'ppc', 'mips', 'mipsel'].includes(arch)) result.push('32-BIT');
        }
        return values1(result);
    }
    case 'UNIVERSAL-TIME': return values1(integer(BigInt(Math.floor(Date.now() / 1000)) + epochOffset));
    case 'REAL-TIME': return values1(Math.floor(globalThis.performance?.now() ?? Date.now()));
    case 'RUN-TIME': {
        if (!proc?.cpuUsage) return values1(Math.floor(globalThis.performance?.now() ?? Date.now()));
        const usage = proc.cpuUsage();
        return values1(Math.floor((usage.user + usage.system) / 1000));
    }
    case 'SLEEP': {
        const duration = Number(numberValue(args[0])) * 1000;
        const now = () => globalThis.performance?.now() ?? Date.now();
        const deadline = now() + duration;
        if (duration > 0 && typeof SharedArrayBuffer !== 'undefined') {
            try {
                const cell = new Int32Array(new SharedArrayBuffer(4));
                while (now() < deadline) Atomics.wait(cell, 0, 0, deadline - now());
                return values1(S_nil);
            } catch (error) { if (!(error instanceof TypeError)) throw error; }
        }
        // Browser main threads forbid Atomics.wait; synchronous Lisp cannot yield.
        while (now() < deadline) { /* wait for the requested duration */ }
        return values1(S_nil);
    }
    case 'DECODE': return decode(...args);
    case 'ENCODE': return encode(...args);
    case 'HOME': return values1(hostOS ? hostOS.homedir().replace(/\\/g, '/').replace(/\/?$/, '/') : S_nil);
    case 'VERSION': return values1(`JavaScript runtime ${proc?.version ?? 'browser'}`);
    case 'MACHINE-INSTANCE': return values1(hostOS?.hostname() ?? S_nil);
    case 'MACHINE-TYPE': return values1(hostOS?.arch() ?? proc?.arch ?? S_nil);
    case 'MACHINE-VERSION': return values1(hostOS?.cpus()?.[0]?.model ?? S_nil);
    case 'SOFTWARE-TYPE': return values1(hostOS?.type() ?? proc?.platform ?? 'Web browser');
    case 'SOFTWARE-VERSION': return values1(hostOS?.release() ?? globalThis.navigator?.userAgent ?? S_nil);
    case 'ROOM': return values1(proc?.memoryUsage ? JSON.stringify(proc.memoryUsage()) : 'Host memory statistics are unavailable');
    case 'DISASSEMBLE': return values1(Function.prototype.toString.call(args[0]));
    default: return simpleError(`Unknown environment operation: ${operation.name}`);
    }
}

setResumableImplementation(CL_environmentOperation, function*(operation, ...args) {
    if (operation.name !== 'SLEEP') return CL_environmentOperation(operation, ...args);
    const requested = Number(numberValue(args[0])) * 1000;
    const duration = requested > 0 ? requested : 0;
    const deadline = performance.now() + duration;
    let timer = null;
    try {
        yield { kind: 'await', promise: new Promise(resolve => {
            const finish = () => {
                const remaining = deadline - performance.now();
                if (remaining <= 0) resolve();
                else timer = setTimeout(finish, Math.min(remaining, 2147483647));
            };
            // Even zero seconds yields a host turn. Chunk large durations so
            // host timer overflow cannot turn a long sleep into a short one.
            timer = setTimeout(finish, Math.min(duration, 2147483647));
        }) };
        return values1(S_nil);
    } finally {
        if (timer !== null) clearTimeout(timer);
    }
});
