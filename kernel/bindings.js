// Dynamic bindings are stack frames, not copies of the global value. Moving a
// suspended binding stack off the symbols reveals the shared global cells;
// reinstalling it captures any global changes made by another context.
class BindingContext {
    constructor() { this.top = null; }
}
let currentContext = new BindingContext();

export function createBindingContext() { return new BindingContext(); }

export function bindSpecial(symbol) {
    const frame = { symbol, savedValue: symbol.value, previous: currentContext.top };
    currentContext.top = frame;
    return frame;
}

export function unbindSpecial(frame) {
    if (currentContext.top !== frame) throw new Error('Dynamic binding stack mismatch');
    frame.symbol.value = frame.savedValue;
    currentContext.top = frame.previous;
}

function activate(context) {
    if (!(context instanceof BindingContext)) throw new TypeError('Expected a binding context');
    const previous = currentContext;
    if (context === previous) return previous;
    for (let frame = previous.top; frame; frame = frame.previous) {
        frame.localValue = frame.symbol.value;
        frame.symbol.value = frame.savedValue;
    }
    const frames = [];
    for (let frame = context.top; frame; frame = frame.previous) frames.push(frame);
    for (let i = frames.length - 1; i >= 0; i--) {
        const frame = frames[i];
        frame.savedValue = frame.symbol.value;
        frame.symbol.value = frame.localValue;
    }
    currentContext = context;
    return previous;
}

export function withBindingContext(context, function_) {
    const previous = activate(context);
    try { return function_(); }
    finally { activate(previous); }
}

