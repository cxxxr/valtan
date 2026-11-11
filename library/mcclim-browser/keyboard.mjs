// Canvas is not an editable element: browsers cannot start an IME composition
// on it, even with contenteditable. Each mirror owns a small editable input;
// CLIM remains responsible for the actual text buffer and its rendered cursor.
export function attachTextInput(state, input) {
  const editor = document.createElement('textarea');
  editor.className = 'mcclim-text-input';
  editor.tabIndex = -1;
  editor.setAttribute('aria-label', state.canvas.getAttribute('aria-label') ?? 'McCLIM');
  editor.autocapitalize = 'off'; editor.autocomplete = 'off'; editor.spellcheck = false;
  Object.assign(editor.style, {position:'absolute', left:'0', top:'0', width:'1px', height:'1px',
    padding:'0', border:'0', opacity:'0', pointerEvents:'none', resize:'none'});
  state.viewport.append(editor);
  state.input = editor;
  state.canvas.addEventListener('focus', () => editor.focus({preventScroll:true}));
  let composing = false, committed = false;
  const commit = text => {
    // One Unicode character per entry also keeps supplementary text within the
    // shared queue's UTF-8 entry limit.
    for (const character of text ?? '') input({type:'text-input', window:state.id, text:character});
    editor.value = '';
  };
  for (const type of ['keydown','keyup']) editor.addEventListener(type, event => {
    // Key 229/Process is an IME command, not a CLIM gesture. Dead keys must be
    // allowed to reach the browser's composition machinery as well.
    if (composing || event.isComposing || event.keyCode === 229 ||
        event.key === 'Process' || event.key === 'Dead') return;
    const modifiers = (event.shiftKey ? 1 : 0) | (event.ctrlKey ? 2 : 0) |
      (event.altKey ? 4 : 0) | (event.metaKey ? 8 : 0);
    input({type, window:state.id, key:event.key, code:event.code,
      modifiers, repeat:event.repeat, time:event.timeStamp});
    event.preventDefault();
  });
  editor.addEventListener('compositionstart', () => {composing = true;});
  editor.addEventListener('compositionend', event => {
    composing = false; committed = true; commit(event.data);
    // Some engines send the final input after compositionend in the same task.
    queueMicrotask(() => {committed = false;});
  });
  editor.addEventListener('input', event => {
    if (composing || event.isComposing) return;
    if (!committed) commit(event.data ?? editor.value);
    else editor.value = '';
  });
  return editor;
}
