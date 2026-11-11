export function codeArrayToString(array) {
    return array.map(c => String.fromCodePoint(c)).join('');
}

// Internal storage copy. Callers have already checked Lisp array bounds and
// resolved displacement. Avoid a Lisp function call for every emitted character
// when the compiler appends a large JavaScript fragment to its output stream.
export function copyRawArrayRange(source, sourceStart, destination, destinationStart, count) {
    if (source === destination && destinationStart > sourceStart && destinationStart < sourceStart + count) {
        for (let i = count - 1; i >= 0; --i) destination[destinationStart + i] = source[sourceStart + i];
    } else {
        for (let i = 0; i < count; ++i) destination[destinationStart + i] = source[sourceStart + i];
    }
    return destination;
}
