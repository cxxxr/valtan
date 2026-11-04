export let S_t;
export let S_nil;
export let S_package;
export let S_symbol;
export let S_integer;
export let S_number;
export let S_string;
export let S_function;
export let S_setf;
export let S_fdefinition_setf;
export let S_type_error;

export let S_arguments_error;
export let S_simple_error;
export let S_simple_make_string;
export let S_undefined_function;
export let S_unbound_variable;
export let S_program_error;

export let S_current_package;

export function initSymbols({
    t,
    nil,
    ...symbols
}) {
    S_t = t;
    S_t.value = t;
    S_t.plist = nil;
    S_nil = nil;
    S_nil.value = nil;
    S_nil.plist = nil;
    // Symbols interned while bootstrapping predate the NIL object.
    for (const symbol of Object.values(symbols)) {
        if (symbol.plist === undefined) symbol.plist = nil;
    }
    S_package = symbols["PACKAGE"];
    S_symbol = symbols["SYMBOL"];
    S_integer = symbols["INTEGER"];
    S_number = symbols["NUMBER"];
    S_string = symbols["STRING"];
    S_function = symbols["FUNCTION"];
    S_setf = symbols["SETF"];
    S_fdefinition_setf = symbols["FDEFINITION-SETF"];
    S_type_error = symbols["TYPE-ERROR"];

    S_arguments_error = symbols["ARGUMENTS-ERROR"];
    S_simple_error = symbols["SIMPLE-ERROR"];
    S_simple_make_string = symbols["SIMPLE-MAKE-STRING"];
    S_undefined_function = symbols["UNDEFINED-FUNCTION"];
    S_unbound_variable = symbols["UNBOUND-VARIABLE"];
    S_program_error = symbols["PROGRAM-ERROR"];

    S_current_package = symbols["*PACKAGE*"];
    S_current_package.plist = nil;
}

export function toLispBoolean(x) {
    return x ? S_t : S_nil;
}

export function isString(x) {
    return typeof x === 'string' || x instanceof String;
}
