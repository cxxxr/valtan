import {
    toLispBoolean
} from './header';
import {
    values1
} from './values';

class Structure {
    constructor(name, values) {
        this.name = name;
        this.values = values;
    }

    getValue(i) {
        return this.values[i];
    }

    setValue(i, value) {
        this.values[i] = value;
    }

    copy() {
        return new Structure(this.name, this.values);
    }
}

export function CL_makeStructure(name, ...args) {
    return values1(new Structure(name, args));
}

export function CL_copyStructure(x) {
    return values1(x.copy());
}

export function CL_structurep(x) {
    return values1(toLispBoolean(x instanceof Structure));
}

export function CL_structureName(structure) {
    return values1(structure.name);
}

export function CL_structureSlotCount(structure) {
  return values1(structure.values.length);
}

export function CL_structureRef(structure, i) {
    return values1(structure.getValue(i));
}

export function CL_structureSet(structure, i, value) {
    structure.setValue(i, value);
    return values1(value);
}
