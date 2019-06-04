export let tValue;
export let nilValue;

export function initBoolean(t, nil) {
  tValue = t;
  tValue.value = t;
  nilValue = nil;
  nilValue.value = nil;
}
