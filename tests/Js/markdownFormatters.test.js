import { toggleMarkup, pushLine, insertBlock } from '../../assets/js/ports.js';

function makeTextarea(value, selStart = value.length, selEnd = selStart) {
    const ta = document.createElement('textarea');
    ta.value = value;
    document.body.appendChild(ta);
    ta.focus();
    ta.setSelectionRange(selStart, selEnd);
    return ta;
}

beforeEach(() => {
    document.body.innerHTML = '';
    globalThis.__failExecCommand = false;
});

test('toggleMarkup wraps a selection in `**…**` and lands caret after closing marker', () => {
    const ta = makeTextarea('foo', 0, 3);
    toggleMarkup(ta, '**');
    expect(ta.value).toBe('**foo**');
    expect(ta.selectionStart).toBe(7);
});

test('toggleMarkup unwraps when selection is already surrounded by `**`', () => {
    const ta = makeTextarea('**foo**', 2, 5);
    toggleMarkup(ta, '**');
    expect(ta.value).toBe('foo');
});

test('pushLine prefixes a single empty line with the marker', () => {
    const ta = makeTextarea('');
    pushLine(ta, '### ');
    expect(ta.value).toBe('### ');
});

test('insertBlock inserts <details>/<summary> template into empty buffer', () => {
    const ta = makeTextarea('');
    insertBlock(ta, '<details>', '<summary></summary>\n', '</details>');
    expect(ta.value).toBe('<details><summary></summary>\n</details>');
    // Cursor sits at the seam between <details> and <summary>.
    expect(ta.selectionStart).toBe('<details>'.length);
});
