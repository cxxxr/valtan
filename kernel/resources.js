// Immutable data shipped with an ASDF system. Paths retain their build-time
// names so ASDF metadata and *LOAD-TRUENAME* can locate them after deployment.
const files = new Map();
const directories = new Set();

export function resourcePath(raw) {
    const parts = [];
    for (const part of raw.replaceAll('\\', '/').split('/')) {
        if (!part || part === '.') continue;
        if (part === '..') parts.pop(); else parts.push(part);
    }
    return '/' + parts.join('/');
}

export function registerEmbeddedFile(raw, hex, writeDate) {
    const path = resourcePath(raw);
    // A shared dependency may appear in several ASDF plans.
    const existing = files.get(path);
    if (existing) {
        if (existing.hex !== hex || existing.writeDate !== writeDate)
            throw new Error(`Conflicting embedded resource: ${path}`);
        return;
    }
    if (hex.length % 2 || /[^0-9a-f]/i.test(hex)) throw new Error(`Invalid resource data: ${path}`);
    const data = new Uint8Array(hex.length / 2);
    for (let i = 0; i < data.length; i++) data[i] = parseInt(hex.slice(i * 2, i * 2 + 2), 16);
    files.set(path, {data, hex, writeDate});
    for (let index = path.lastIndexOf('/'); index >= 0; index = path.lastIndexOf('/', index - 1)) {
        directories.add(path.slice(0, index) || '/');
        if (index === 0) break;
    }
}

export function embeddedFile(path) { return files.get(resourcePath(path)); }
export function embeddedTruename(path) {
    const name = resourcePath(path);
    if (files.has(name)) return name;
    if (directories.has(name)) return name === '/' ? name : name + '/';
    return null;
}
export function embeddedEntries() {
    return [...files.keys(), ...Array.from(directories, x => x === '/' ? x : x + '/')];
}
