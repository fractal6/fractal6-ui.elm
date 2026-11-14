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

// --- 0x tension picker ---

function makeTensionTooltip(visible) {
    const tip = document.createElement('span');
    tip.id = 'tatensionInput';
    document.body.appendChild(tip);
    // jsdom has no layout: offsetParent is always null, which isHidden() reads as hidden.
    if (visible) Object.defineProperty(tip, 'offsetParent', { get: () => document.body });
    return tip;
}

function tensionApp() {
    const sent = [];
    const port = (name) => ({ send: (v) => sent.push([name, v]) });
    return { sent, ports: {
        openTensionPickerFromJs: port('open'),
        closeTensionPickerFromJs: port('close'),
        changeTensionPatternFromJs: port('pattern'),
        arrowFromJs: port('arrow'),
        selectActiveItemFromJs: port('select'),
    } };
}

test.each([
    ['0', true],
    ['see 0', true],
    ['line\n0', true],
    ['a0', false],
    ['10', false],
    ['', false],
])('typing x after %j opens the tension picker: %s', (value, opens) => {
    // jsdom's computed style is not iterable, which getCaretCoordinates needs.
    const style = jest.spyOn(window, 'getComputedStyle').mockReturnValue([]);
    const ta = makeTextarea(value);
    makeTensionTooltip(false);
    const app = tensionApp();
    markupRichText(fakeEvent({ key: 'x' }), ta, app);
    style.mockRestore();
    expect(app.sent.some(([name]) => name === 'open')).toBe(opens);
});

test('open tension picker forwards the text typed after 0x', () => {
    const ta = makeTextarea('see 0xdep');
    makeTensionTooltip(true);
    const app = tensionApp();
    markupRichText(fakeEvent({ key: 'l' }), ta, app);
    expect(app.sent).toEqual([['pattern', 'depl']]);
});

test.each([
    ['see 0xdeploy', ' ', 'deploy '],
    ['see 0xdeploy ap', 'i', 'deploy api'],
    ['see 0x33e6f0 and 0xde', 'p', 'dep'],
])('tension pattern %j + %j is forwarded as %j', (value, key, pattern) => {
    const ta = makeTextarea(value);
    makeTensionTooltip(true);
    const app = tensionApp();
    markupRichText(fakeEvent({ key }), ta, app);
    expect(app.sent).toEqual([['pattern', pattern]]);
});

test('a pattern longer than the lookback closes the tension picker', () => {
    const ta = makeTextarea('0x' + 'word '.repeat(10));
    makeTensionTooltip(true);
    const app = tensionApp();
    markupRichText(fakeEvent({ key: 'a' }), ta, app);
    expect(app.sent).toEqual([['close', null]]);
});

test('a second consecutive space closes the tension picker', () => {
    const ta = makeTextarea('see 0xdeploy ');
    makeTensionTooltip(true);
    const app = tensionApp();
    markupRichText(fakeEvent({ key: ' ' }), ta, app);
    expect(app.sent).toEqual([['close', null]]);
});

test('backspacing the 0x prefix closes the tension picker', () => {
    const ta = makeTextarea('see 0x');
    makeTensionTooltip(true);
    const app = tensionApp();
    markupRichText(fakeEvent({ key: 'Backspace' }), ta, app);
    expect(app.sent).toEqual([['close', null]]);
});
