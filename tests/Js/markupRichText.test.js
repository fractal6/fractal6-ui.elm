import { markupRichText } from '../../assets/js/bulma_drivers.js';

function makeTextarea(value, selStart = value.length, selEnd = selStart) {
    const ta = document.createElement('textarea');
    ta.id = 'ta';
    ta.value = value;
    document.body.appendChild(ta);
    ta.focus();
    ta.setSelectionRange(selStart, selEnd);
    return ta;
}

function fakeEvent(props) {
    return {
        isComposing: false,
        ctrlKey: false,
        shiftKey: false,
        altKey: false,
        keyCode: 0,
        defaultPrevented: false,
        preventDefault() { this.defaultPrevented = true; },
        stopPropagation() {},
        ...props,
    };
}

const stubApp = { ports: {
    changePatternFromJs: { send() {} },
    changeEmojiPatternFromJs: { send() {} },
    arrowFromJs: { send() {} },
    selectActiveItemFromJs: { send() {} },
} };

beforeEach(() => {
    document.body.innerHTML = '';
    globalThis.__failExecCommand = false;
});

test('skips during IME composition', () => {
    const ta = makeTextarea('1. foo');
    markupRichText(fakeEvent({ key: 'Enter', isComposing: true }), ta, stubApp);
    expect(ta.value).toBe('1. foo');
});

test('Enter on ordered list increments counter and preserves indent', () => {
    const ta = makeTextarea('  1. foo');
    markupRichText(fakeEvent({ key: 'Enter' }), ta, stubApp);
    expect(ta.value).toBe('  1. foo\n  2. ');
    expect(ta.selectionStart).toBe(14);
});

test('Enter on empty `- ` line exits the list (drops the marker)', () => {
    // Buffer needs length >= 3 to enter the Enter handler at all.
    // Use `\n- ` so the cursor sits on an empty list item.
    const ta = makeTextarea('\n- ');
    markupRichText(fakeEvent({ key: 'Enter' }), ta, stubApp);
    expect(ta.value).not.toContain('- ');
});

test('Enter on `* [ ] foo` continues checkbox (not just `-`)', () => {
    const ta = makeTextarea('* [ ] foo');
    markupRichText(fakeEvent({ key: 'Enter' }), ta, stubApp);
    expect(ta.value).toBe('* [ ] foo\n* [ ] ');
});

test('Backspace at `\\n  - ` dedents to `\\n- `, caret moves -2', () => {
    const ta = makeTextarea('\n  - ');
    markupRichText(fakeEvent({ key: 'Backspace' }), ta, stubApp);
    expect(ta.value).toBe('\n- ');
    expect(ta.selectionStart).toBe(3);
});

test('Backspace at `\\n- ` removes the marker entirely', () => {
    const ta = makeTextarea('\n- ');
    markupRichText(fakeEvent({ key: 'Backspace' }), ta, stubApp);
    expect(ta.value).toBe('\n');
    expect(ta.selectionStart).toBe(1);
});

test('Tab inside a list inserts two spaces at the right offset', () => {
    const ta = makeTextarea('\n- ');
    markupRichText(fakeEvent({ key: 'Tab' }), ta, stubApp);
    expect(ta.value).toBe('\n  - ');
    expect(ta.selectionStart).toBe(5);
});
