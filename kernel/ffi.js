import {
    values1
} from './values';
import {
    toLispBoolean
} from './header';

export function CL_instanceof(x, y) {
    return values1(toLispBoolean(x instanceof y));
}