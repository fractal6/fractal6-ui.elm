// jsdom doesn't implement document.execCommand. Provide enough for the
// replaceRange happy path; tests that need the fallback toggle the global
// flag (`true` → returns false, `'throw'` → throws).
globalThis.__failExecCommand = false;

document.execCommand = function (cmd, _ui, value) {
    if (globalThis.__failExecCommand === 'throw') throw new Error('execCommand disabled');
    if (globalThis.__failExecCommand === true) return false;
    if (cmd !== 'insertText') return false;
    const el = document.activeElement;
    if (!el || (el.tagName !== 'TEXTAREA' && el.tagName !== 'INPUT')) return false;
    const s = el.selectionStart;
    const e = el.selectionEnd;
    el.value = el.value.slice(0, s) + value + el.value.slice(e);
    const caret = s + value.length;
    el.selectionStart = caret;
    el.selectionEnd = caret;
    el.dispatchEvent(new Event('input', { bubbles: true }));
    return true;
};
