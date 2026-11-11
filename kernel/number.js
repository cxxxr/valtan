// Preserve the public numeric API while keeping condition delivery at the
// boundary. Calls within number-operations.js use raw helpers, so a nested
// complex/ratio operation cannot signal through an ordinary Lisp call.
import * as operations from './number-operations';
import { makeNativeFunction } from './native-error';

export { Float, Complex, Ratio, CL_rationalp, numberValue, toFloat, CL_numberp, CL_parseFloatString, CL_complexp, CL_integerp, CL_floatp, isInteger, normalizeInteger } from './number-operations';

export const makeRatio = makeNativeFunction(operations.makeRatio);
export const CL_numerator = makeNativeFunction(operations.CL_numerator);
export const CL_denominator = makeNativeFunction(operations.CL_denominator);
export const CL_float = makeNativeFunction(operations.CL_float);
export const CL_rational = makeNativeFunction(operations.CL_rational);
export const CL_floatRank = makeNativeFunction(operations.CL_floatRank);
export const CL_log = makeNativeFunction(operations.CL_log);
export const CL_abs = makeNativeFunction(operations.CL_abs);
export const CL_numberPrintString = makeNativeFunction(operations.CL_numberPrintString);
export const CL_complex = makeNativeFunction(operations.CL_complex);
export const CL_realpart = makeNativeFunction(operations.CL_realpart);
export const CL_imagpart = makeNativeFunction(operations.CL_imagpart);
export const CL_add = makeNativeFunction(operations.CL_add);
export const CL_sub = makeNativeFunction(operations.CL_sub);
export const CL_mul = makeNativeFunction(operations.CL_mul);
export const CL_negate = makeNativeFunction(operations.CL_negate);
export const CL_ash = makeNativeFunction(operations.CL_ash);
export const CL_integerLength = makeNativeFunction(operations.CL_integerLength);
export const CL_logand = makeNativeFunction(operations.CL_logand);
export const CL_div = makeNativeFunction(operations.CL_div);
export const CL_floor = makeNativeFunction(operations.CL_floor);
export const CL_round = makeNativeFunction(operations.CL_round);
export const CL_truncate = makeNativeFunction(operations.CL_truncate);
export const CL_ceiling = makeNativeFunction(operations.CL_ceiling);
export const CL_rem = makeNativeFunction(operations.CL_rem);
export const CL_numberEqual = makeNativeFunction(operations.CL_numberEqual);
export const CL_numberNotEqual = makeNativeFunction(operations.CL_numberNotEqual);
export const CL_greaterThan = makeNativeFunction(operations.CL_greaterThan);
export const CL_lessThan = makeNativeFunction(operations.CL_lessThan);
export const CL_greaterEqual = makeNativeFunction(operations.CL_greaterEqual);
export const CL_lessEqual = makeNativeFunction(operations.CL_lessEqual);
export const CL_integerDecodeFloat = makeNativeFunction(operations.CL_integerDecodeFloat);
export const CL_decodeFloat = makeNativeFunction(operations.CL_decodeFloat);
export const CL_floatRadix = makeNativeFunction(operations.CL_floatRadix);
export const CL_floatDigits = makeNativeFunction(operations.CL_floatDigits);
export const CL_floatPrecision = makeNativeFunction(operations.CL_floatPrecision);
export const CL_floatSign = makeNativeFunction(operations.CL_floatSign);
export const CL_scaleFloat = makeNativeFunction(operations.CL_scaleFloat);
export const CL_transcendental = makeNativeFunction(operations.CL_transcendental);
export const CL_atan = makeNativeFunction(operations.CL_atan);
export const CL_phase = makeNativeFunction(operations.CL_phase);
export const CL_cis = makeNativeFunction(operations.CL_cis);
