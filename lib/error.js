import {
    S_arguments_error,
    S_type_error,
    S_string,
    S_simple_make_string,
    S_simple_error,
    S_undefined_function,
    S_unbound_variable,
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

export function simpleError(string, ...args) {
    S_simple_error.func(S_simple_make_string.func(string), ...args);
}

export function undefinedFunction(name) {
    S_undefined_function.func(name);
}

export function unboundVariable(name) {
    S_unbound_variable.func(name);
}
