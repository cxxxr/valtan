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
  callStack.reverse();
  for (let frame of callStack) {
    console.log(lispFormatFunction("~S", frame));
  }
  throw new Error(lispFormatFunction.apply(null, args));
}
