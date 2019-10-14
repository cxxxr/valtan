export function codeArrayToString(array) {
    return array.map(c => String.fromCharCode(c)).join('');
}