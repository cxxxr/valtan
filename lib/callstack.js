const callStack = [];

export function pushCallStack(frame) {
  callStack.push(frame);
}

export function popCallStack() {
  callStack.pop();
}

export function raise(...args) {
  callStack.reverse();
  for (let frame of callStack) {
    const [symbol, ...args] = frame;
    console.log(`${symbol.packageName}::${symbol.name}`);
  }
  throw new Error(args);
}
