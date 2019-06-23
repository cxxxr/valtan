import {
    toLispBoolean,
    typeError
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

function checkStructure(value) {
    if (!(value instanceof Structure)) {
        typeError(value, "STRUCTURE");
    }
}

export function CL_makeStructure(name, ...args) {
    return values1(new Structure(name, args));
}

export function CL_copyStructure(x) {
    checkStructure(x);
    return values1(x.copy());
}

export function CL_structurep(x) {
    return values1(toLispBoolean(x instanceof Structure));
}

export function CL_structureName(structure) {
    checkStructure(structure);
    return values1(structure.name);
}

export function CL_structureRef(structure, i) {
    checkStructure(structure);
    return values1(structure.getValue(i));
}

export function CL_structureSet(structure, i, value) {
    checkStructure(structure);
    structure.setValue(i, value);
    return values1(value);
}
