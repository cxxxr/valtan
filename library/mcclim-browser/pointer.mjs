// Worker commands and page-side routing for CLIM's explicit pointer grabs.
// Browser capture is limited to the document; it cannot grab other OS windows.
export function pointerGrabCommands(send) {
  return {
    grabPointer: (window, multipleWindow = false) => {
      send({type:'pointer-grab', id:window.id, multipleWindow:!!multipleWindow});
      return true;
    },
    ungrabPointer: () => send({type:'pointer-ungrab'}),
  };
}

export class BrowserPointerGrab {
  constructor(windows, input) {
    this.windows = windows;
    this.input = input;
    this.active = null;
    this.controller = new AbortController();
    for (const type of ['pointerdown','pointermove','pointerup','wheel'])
      document.addEventListener(type, event => this.route(event),
        {capture:true, passive:false, signal:this.controller.signal});
    for (const type of ['pointerenter','pointerleave'])
      document.addEventListener(type, event => {
        if (this.active) event.stopImmediatePropagation();
      }, {capture:true, signal:this.controller.signal});
    for (const type of ['click','auxclick','contextmenu'])
      document.addEventListener(type, event => {
        if (this.active) { event.preventDefault(); event.stopImmediatePropagation(); }
      }, {capture:true, signal:this.controller.signal});
  }
  grab(id, multipleWindow) {
    const state = this.windows.get(id);
    if (state) {
      this.release();
      this.active = {state, multipleWindow, previousFocus:document.activeElement};
      state.canvas.focus({preventScroll:true});
    }
  }
  release() {
    const saved = this.active;
    this.active = null;
    if (saved?.previousFocus?.isConnected &&
        (document.activeElement === saved.state.canvas || document.activeElement === saved.state.input ||
         document.activeElement === document.body))
      saved.previousFocus.focus({preventScroll:true});
  }
  route(event) {
    if (!this.active) return;
    let {state, multipleWindow} = this.active;
    if (!this.windows.has(state.id)) { this.release(); return; }
    if (multipleWindow) {
      // A previous button press may still have native DOM pointer capture on
      // another canvas. Use hit testing to implement CLIM owner-events routing.
      const hit = document.elementFromPoint(event.clientX, event.clientY);
      if (hit?.tagName === 'CANVAS') {
        const id = Number(hit.closest('.mcclim-window')?.dataset.windowId);
        state = this.windows.get(id) ?? state;
      }
    }
    const {canvas} = state, rect = canvas.getBoundingClientRect();
    if (!rect.width || !rect.height) { this.release(); return; }
    const modifiers = (event.shiftKey ? 1 : 0) | (event.ctrlKey ? 2 : 0) |
      (event.altKey ? 4 : 0) | (event.metaKey ? 8 : 0);
    const raw = {type:event.type, window:state.id,
      x:(event.clientX-rect.left)*canvas.width/rect.width,
      y:(event.clientY-rect.top)*canvas.height/rect.height,
      modifiers, time:event.timeStamp};
    if (event.type === 'wheel') { raw.dx = event.deltaX; raw.dy = event.deltaY; }
    else { raw.button = event.button; raw.buttons = event.buttons; }
    this.input(raw);
    event.preventDefault(); event.stopImmediatePropagation();
  }
  dispose() { this.release(); this.controller.abort(); }
}
