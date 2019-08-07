const callStack = [];

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
    const [fn, ...args] = frame;
    console.log(fn);
  }
  throw new Error(args);
}
