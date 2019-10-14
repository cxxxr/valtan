import {
    S_arguments_error,
    S_type_error,
    S_string,
    isString,
} from './header';

export function argumentsError(name, numArgs) {
    S_arguments_error.func(name, numArgs);
}

export function typeError(datum, expectedType) {
    S_type_error.func(datum, expectedType);
}

export function checkString(value) {
    if (!isString(value)) {
        typeError(value, S_string);
    }
}
