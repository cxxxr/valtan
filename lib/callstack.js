const callStack = [];

var lispFormatFunction = null;

export function setLispFormatFunction(f) {
    lispFormatFunction = f;
}

export function pushCallStack(frame) {
    if (callStack.length > 1000) {
        raise('stack over flow');
    }
    callStack.push(frame);
}

export function popCallStack() {
    callStack.pop();
}

export function raise(...args) {
    if (lispFormatFunction === null) {
        console.log(args);
        throw new Error();
    }
    let s = lispFormatFunction.apply(null, args);
    s += '\n\n';
    s += 'Backtrace:\n';
    callStack.reverse();
    console.log(callStack);
    for (let i = 0; i < callStack.length; i++) {
        const frame = callStack[i];
        s += `${i}: `;
        try {
            s += lispFormatFunction("~S", frame);
        } catch (e) {
            s += `#<error printing ${e}>`
        }
        if (i < callStack.length - 1) {
            s += '\n';
        }
    }
    //console.log(s)
    throw new Error(s);
}
