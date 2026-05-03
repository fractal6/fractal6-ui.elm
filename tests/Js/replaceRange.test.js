import { replaceRange } from '../../assets/js/textutils.js';

function makeTextarea(value = '', selStart = value.length, selEnd = selStart) {
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

test('inserts text and lands caret at start + text.length by default', () => {
    const ta = makeTextarea('hello');
    let inputEvents = 0;
    ta.addEventListener('input', () => inputEvents++);

    replaceRange(ta, 5, 5, ' world');

    expect(ta.value).toBe('hello world');
    expect(ta.selectionStart).toBe(11);
    expect(ta.selectionEnd).toBe(11);
    expect(inputEvents).toBeGreaterThanOrEqual(1);
});

test('honors explicit caretStart / caretEnd', () => {
    const ta = makeTextarea('foo');
    replaceRange(ta, 3, 3, 'bar', 4, 6);

    expect(ta.value).toBe('foobar');
    expect(ta.selectionStart).toBe(4);
    expect(ta.selectionEnd).toBe(6);
});

test('replaces an existing range (non-empty [start, end])', () => {
    const ta = makeTextarea('hello world');
    replaceRange(ta, 6, 11, 'jest');

    expect(ta.value).toBe('hello jest');
    expect(ta.selectionStart).toBe(10);
});

test('falls back to el.value assignment when execCommand throws', () => {
    globalThis.__failExecCommand = 'throw';
    const ta = makeTextarea('abc');
    let inputEvents = 0;
    ta.addEventListener('input', () => inputEvents++);

    replaceRange(ta, 3, 3, 'XYZ');

    expect(ta.value).toBe('abcXYZ');
    expect(inputEvents).toBe(1);
});

test('falls back to el.value assignment when execCommand returns false', () => {
    globalThis.__failExecCommand = true;
    const ta = makeTextarea('abc');
    let inputEvents = 0;
    ta.addEventListener('input', () => inputEvents++);

    replaceRange(ta, 1, 2, 'XX', 3);

    expect(ta.value).toBe('aXXc');
    expect(ta.selectionStart).toBe(3);
    expect(inputEvents).toBe(1);
});
