// Browser windows use CLIM mirror coordinates: the canvas origin, not the
// decoration's upper-left corner, is (x,y). Child mirrors are relative to the
// parent's canvas and clipped by it. Unmanaged popups have no title decoration.
export class BrowserWindows {
  constructor(container, input) {
    this.windows = new Map();
    this.input = input;
    this.desktop = document.createElement('div');
    this.desktop.className = 'mcclim-desktop';
    Object.assign(this.desktop.style, {position:'relative', marginTop:'40px', minHeight:'320px'});
    this.taskbar = document.createElement('div');
    this.taskbar.className = 'mcclim-taskbar';
    container.append(this.desktop, this.taskbar);
  }
  create(message) {
    const section = document.createElement('section');
    section.className = 'mcclim-window'; section.dataset.windowId = message.id;
    Object.assign(section.style, {position:'absolute', left:'0px', top:'0px', border:'0', margin:'0'});
    const title = document.createElement('h2'); title.textContent = message.title;
    Object.assign(title.style, {position:'absolute', bottom:'100%', left:'0', right:'0', margin:'0',
      cursor:'move', userSelect:'none', touchAction:'none'});
    const viewport = document.createElement('div');
    Object.assign(viewport.style, {position:'relative', overflow:'hidden',
      width:message.width+'px', height:message.height+'px'});
    const canvas = document.createElement('canvas');
    canvas.width = message.width; canvas.height = message.height; canvas.tabIndex = 0;
    canvas.setAttribute('aria-label', message.title);
    Object.assign(canvas.style, {display:'block', touchAction:'none'});
    const parent = message.parent == null ? null : this.windows.get(message.parent);
    if (message.parent != null && !parent) throw new Error(`Missing parent mirror ${message.parent}`);
    const state = {id:message.id, parent:message.parent ?? null, section, title, viewport, canvas,
      context:canvas.getContext('bitmaprenderer'), x:0, y:0, minimized:false};
    title.hidden = message.managed === false || !!parent;
    viewport.append(canvas); section.append(title, viewport);
    (parent?.viewport ?? this.desktop).append(section);
    this.windows.set(message.id, state);
    this.raise(state);
    this.fit();
    let drag;
    title.addEventListener('pointerdown', event => {
      if (event.button !== 0) return;
      drag = {id:event.pointerId, clientX:event.clientX, clientY:event.clientY, x:state.x, y:state.y};
      this.raise(state); title.setPointerCapture(event.pointerId); canvas.focus(); event.preventDefault();
    });
    title.addEventListener('pointermove', event => {
      if (!drag || event.pointerId !== drag.id) return;
      this.move(state, Math.round(drag.x + event.clientX - drag.clientX),
                       Math.round(drag.y + event.clientY - drag.clientY));
      this.input({type:'window-configuration', window:state.id, x:state.x, y:state.y,
        width:canvas.width, height:canvas.height});
      event.preventDefault();
    });
    const endDrag = () => {drag = undefined;};
    title.addEventListener('pointerup', endDrag);
    title.addEventListener('pointercancel', endDrag);
    title.addEventListener('lostpointercapture', endDrag);
    return state;
  }
  move(state, x, y) {
    state.x = Number(x); state.y = Number(y);
    state.section.style.left = state.x+'px'; state.section.style.top = state.y+'px';
    this.fit();
  }
  resize(state, width, height) {
    state.canvas.width = width; state.canvas.height = height;
    state.viewport.style.width = width+'px'; state.viewport.style.height = height+'px';
    this.fit();
  }
  fit() {
    let height = 320;
    for (const state of this.windows.values())
      if (state.parent === null && !state.section.hidden) height = Math.max(height, state.y+state.canvas.height);
    this.desktop.style.minHeight = height+'px';
  }
  viewportSize() {
    const rect = this.desktop.getBoundingClientRect();
    return {width:Math.max(1, Math.floor(rect.width)),
      height:Math.max(1, Math.floor(innerHeight-rect.top))};
  }
  reorder(state, front) {
    const parent = state.section.parentElement;
    // Changing DOM parents during pointerdown disrupts Chrome's pointer capture
    // and focus. Stacking is a visual operation; keep the mirror tree attached.
    const siblings = [...parent.children]
      .filter(element => element !== state.section && element.classList.contains('mcclim-window'))
      .sort((a, b) => Number(a.style.zIndex) - Number(b.style.zIndex));
    if (front) siblings.push(state.section);
    else siblings.unshift(state.section);
    siblings.forEach((element, index) => { element.style.zIndex = index + 1; });
  }
  raise(state) {this.reorder(state, true);}
  bury(state) {this.reorder(state, false);}
  rename(state, title) {
    state.title.textContent = title; state.canvas.setAttribute('aria-label', title);
    state.input?.setAttribute('aria-label', title);
    if (state.restore) state.restore.textContent = title;
  }
  visible(state, visible) {
    state.section.hidden = !visible;
    if (visible) {state.minimized = false; state.restore?.remove(); state.restore = undefined;}
    this.fit();
  }
  minimize(state) {
    if (state.minimized) return;
    state.minimized = true; this.visible(state, false);
    const restore = document.createElement('button');
    restore.type = 'button'; restore.textContent = state.title.textContent;
    restore.setAttribute('aria-label', `Restore ${state.title.textContent}`);
    restore.addEventListener('click', () => {
      this.input({type:'window-restore', window:state.id});
    });
    state.restore = restore; this.taskbar.append(restore);
  }
  destroy(state) {
    for (const child of [...this.windows.values()]) if (child.parent === state.id) this.destroy(child);
    state.restore?.remove(); state.section.remove(); this.windows.delete(state.id); this.fit();
  }
  dispose() {
    for (const state of [...this.windows.values()]) if (this.windows.has(state.id)) this.destroy(state);
    this.desktop.remove(); this.taskbar.remove();
  }
}
