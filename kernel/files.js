import { S_nil, S_t, S_simple_make_string, toLispBoolean } from './header';
import { intern, cl_package, keyword_package } from './package';
import { values1 } from './values';
import { normalizeInteger } from './number';
import { resourcePath, embeddedFile, embeddedTruename, embeddedEntries } from './resources';
export { registerEmbeddedFile } from './resources';

// Node supplies these modules at startup; browser bundles do not import Node APIs.
let fs, paths;
export function CL_initializeFileSystem(fileSystem, pathModule) {
    fs = fileSystem; paths = pathModule;
    return values1(S_nil);
}
const keyword = x => x === S_nil ? null : x.name;
const absolutePath = raw => paths ? paths.resolve(raw) : resourcePath(raw);
function writable(raw, pathname) {
    if (embeddedTruename(absolutePath(raw))) fileError(pathname, 'Embedded resources are read-only');
}
function fileError(pathname, message) {
    const str = S_simple_make_string.getFunction()(message);
    intern('ERROR', cl_package).getFunction()(intern('FILE-ERROR', cl_package),
        intern('PATHNAME', keyword_package), pathname, intern('MESSAGE', keyword_package), str);
}
function guarded(pathname, fn) {
    if (!fs) fileError(pathname, 'File operations are unavailable on this host');
    try { return fn(); }
    catch (error) {
        if (error?.code) fileError(pathname, error.message);
        throw error;
    }
}
function exists(path) {
    try { return fs.statSync(path); }
    catch (e) { if (e.code === 'ENOENT' || e.code === 'ENOTDIR') return null; throw e; }
}
function open(raw, pathname, direction, ifExists, ifMissing, bytes, signed) {
    const path = absolutePath(raw), resource = embeddedFile(path);
    if (resource) {
        const mode = keyword(direction);
        if (mode === 'OUTPUT' || mode === 'IO') {
            if (ifExists === S_nil) return S_nil;
            fileError(pathname, 'Embedded resources are read-only');
        }
        return {path, pathname, mode, bytes, signed: signed !== S_nil,
                content: bytes === 0 ? Array.from(new TextDecoder().decode(resource.data)) : Array.from(resource.data),
                position: 0, original: null, created: false, reset: false, dirty: false, closed: mode === 'PROBE'};
    }
    if (embeddedTruename(path) && !fs) fileError(pathname, 'Not a regular file');
    if (!fs && ifMissing === S_nil) return S_nil;
    return guarded(pathname, () => {
        const path = paths.resolve(raw), stat = exists(path);
        const mode = keyword(direction), present = keyword(ifExists), missing = keyword(ifMissing);
        const output = mode === 'OUTPUT' || mode === 'IO';
        if (!stat) {
            if (missing === null) return S_nil;
            if (missing === 'ERROR') fileError(pathname, 'File does not exist');
            fs.writeFileSync(path, new Uint8Array());
        } else if (!stat.isFile()) fileError(pathname, 'Not a regular file');
        else if (output) {
            if (present === null) return S_nil;
            if (present === 'ERROR') fileError(pathname, 'File already exists');
        }
        const original = stat ? fs.readFileSync(path) : null;
        let backup = null;
        if (stat && output && present === 'RENAME') {
            backup = path + '.bak'; fs.renameSync(path, backup);
        }
        const reset = output && !['OVERWRITE', 'APPEND'].includes(present);
        if (reset) fs.writeFileSync(path, new Uint8Array());
        const data = reset ? new Uint8Array() : fs.readFileSync(path);
        const content = bytes === 0 ? Array.from(new TextDecoder().decode(data)) : Array.from(data);
        return { path, pathname, mode, bytes, signed: signed !== S_nil, content,
                 position: present === 'APPEND' && output ? content.length : 0,
                 original, backup, created: !stat, reset, dirty: false, closed: mode === 'PROBE' };
    });
}
function flush(file) {
    if (file.dirty) guarded(file.pathname, () => {
        fs.writeFileSync(file.path, file.bytes === 0 ? file.content.join('') : Uint8Array.from(file.content));
        file.dirty = false;
    });
}
function read(file, peek = false) {
    if (file.position >= file.content.length) return S_nil;
    if (file.bytes === 0) {
        const value = file.content[file.position];
        if (!peek) file.position++;
        return value;
    }
    if (file.bytes === 1) {
        const value = file.content[file.position];
        if (!peek) file.position++;
        return file.signed && value >= 128 ? value - 256 : value;
    }
    let n = 0n;
    if (file.position + file.bytes > file.content.length) return S_nil;
    for (let i = 0; i < file.bytes; i++) n = (n << 8n) | BigInt(file.content[file.position + i]);
    if (file.signed) n = BigInt.asIntN(file.bytes * 8, n);
    if (!peek) file.position += file.bytes;
    return normalizeInteger(n);
}
function write(file, value) {
    writable(file.path, file.pathname);
    if (file.bytes === 0) {
        while (file.position > file.content.length) file.content.push('\0');
        for (const character of value) file.content[file.position++] = character;
    } else if (file.bytes === 1) {
        while (file.position > file.content.length) file.content.push(0);
        file.content[file.position++] = Number(value) & 255;
    } else {
        const n = BigInt.asUintN(file.bytes * 8, BigInt(value));
        while (file.content.length < file.position + file.bytes) file.content.push(0);
        for (let i = 0; i < file.bytes; i++) file.content[file.position + i] = Number((n >> BigInt((file.bytes - i - 1) * 8)) & 255n);
        file.position += file.bytes;
    }
    file.dirty = true;
    return value;
}
function directoryPattern(absolute) {
    return new RegExp('^' + absolute.replace(/[.+^${}()|[\]\\]/g, '\\$&')
        .replace(/\*\*\//g, '\x01').replace(/\*\*/g, '\x02').replace(/\*/g, '[^/]*').replace(/\?/g, '[^/]')
        .replace(/\x01/g, '(?:.*/)?').replace(/\x02/g, '.*') + '$');
}
function directory(pattern) {
    const absolute = paths.resolve(pattern);
    const directoryOnly = /[\\/]$/.test(pattern);
    if (!/[?*]/.test(absolute)) {
        const stat = exists(absolute);
        return stat && (!directoryOnly || stat.isDirectory()) ? [fs.realpathSync(absolute) + (stat.isDirectory() ? '/' : '')] : [];
    }
    const first = absolute.search(/[?*]/), slash = absolute.lastIndexOf('/', first);
    const root = absolute.slice(0, slash + 1), results = [];
    const re = directoryPattern(absolute);
    function walk(dir) {
        for (const entry of fs.readdirSync(dir, {withFileTypes: true})) {
            const full = paths.join(dir, entry.name);
            if ((!directoryOnly || entry.isDirectory()) &&
                (re.test(full) || entry.isDirectory() && re.test(full + '/'))) results.push(fs.realpathSync(full) + (entry.isDirectory() ? '/' : ''));
            if (entry.isDirectory() && absolute.includes('**')) walk(full);
        }
    }
    if (exists(root)) walk(root);
    return results;
}
export function CL_fileOperation(operation, ...args) {
    const [a,b,c] = args;
    let result;
    switch (operation.name) {
    case 'CWD': result = paths ? paths.resolve('.') + '/' : '/'; break;
    case 'OPEN': result = open(...args); break;
    case 'READ': result = read(a); break;
    case 'PEEK': result = read(a, true); break;
    case 'UNREAD': a.position -= a.bytes || 1; result = S_nil; break;
    case 'WRITE': result = write(a,b); break;
    case 'FLUSH': flush(a); result = S_nil; break;
    case 'CLOSE':
        if (!a.closed) {
            if (b !== S_nil && (a.created || a.reset)) guarded(a.pathname, () => {
                if (a.original === null) { if (exists(a.path)) fs.unlinkSync(a.path); }
                else fs.writeFileSync(a.path, a.original);
            });
            else flush(a);
            a.closed = true;
        }
        result = S_t; break;
    case 'POSITION':
        if (b === S_nil) result = a.position / (a.bytes || 1);
        else { a.position = b.name === 'END' ? a.content.length : b.name === 'START' ? 0 : Number(b) * (a.bytes || 1); result = S_t; }
        break;
    case 'LENGTH': result = a.content.length / (a.bytes || 1); break;
    // Text positions count Lisp characters, including supplementary code points.
    case 'STRING-LENGTH': result = Array.from(b).length; break;
    case 'PROBE': result = embeddedTruename(absolutePath(a)) || (fs ? guarded(b, () => exists(a) ? fs.realpathSync(a) + (fs.statSync(a).isDirectory() ? '/' : '') : S_nil) : S_nil); break;
    case 'TRUENAME': result = embeddedTruename(absolutePath(a)) || guarded(b, () => fs.realpathSync(a) + (fs.statSync(a).isDirectory() ? '/' : '')); break;
    case 'DIRECTORY': {
        const re = directoryPattern(absolutePath(a) + (/[\\/]$/.test(a) && absolutePath(a) !== '/' ? '/' : ''));
        const embedded = embeddedEntries().filter(name => re.test(name) || (!/[\\/]$/.test(a) && re.test(name.replace(/\/$/, ''))));
        result = [...new Set([...embedded, ...(fs ? guarded(b, () => directory(a)) : [])])]; break;
    }
    case 'DELETE': writable(a,b); result = guarded(b, () => { fs.unlinkSync(a); return S_t; }); break;
    case 'DELETE-DIRECTORY': writable(a,b); result = guarded(b, () => { fs.rmdirSync(a); return S_t; }); break;
    case 'DELETE-DIRECTORY-TREE': writable(a,b); result = guarded(b, () => { fs.rmSync(a, { recursive: true }); return S_t; }); break;
    case 'RENAME': writable(a,c); writable(b,c); result = guarded(c, () => { fs.renameSync(a,b); return fs.realpathSync(b); }); break;
    case 'ENSURE-DIRECTORIES': result = guarded(b, () => {
        const dir = paths.dirname(paths.resolve(a)), existed = Boolean(exists(dir));
        fs.mkdirSync(dir, {recursive: true}); return toLispBoolean(!existed);
    }); break;
    case 'WRITE-DATE': result = embeddedFile(absolutePath(a))?.writeDate ?? (fs ? guarded(b, () => { const stat = exists(a); return stat ? Math.floor(stat.mtimeMs / 1000) + 2208988800 : S_nil; }) : S_nil); break;
    case 'AUTHOR': result = embeddedTruename(absolutePath(a)) ? S_nil : guarded(b, () => { const stat = exists(a); return stat ? String(stat.uid) : S_nil; }); break;
    default: throw new Error(`Unknown file operation ${operation.name}`);
    }
    return values1(result);
}

// Closing a stream must not keep the stream or its buffers alive indefinitely.
const closedStreams = new WeakSet();
export function CL_streamClosedp(stream) { return values1(toLispBoolean(closedStreams.has(stream))); }
export function CL_markStreamClosed(stream) { closedStreams.add(stream); return values1(S_t); }
