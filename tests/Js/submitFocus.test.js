import { InitBulma } from '../../assets/js/bulma_drivers.js';

const stubApp = { ports: new Proxy({}, { get: () => ({ send() {} }) }) };

function init() {
    InitBulma(stubApp, { bulmaHandlers: [] }, "");
    jest.advanceTimersByTime(500);
}

function press(el, key, opts = {}) {
    const ev = new KeyboardEvent('keydown', { key, bubbles: true, cancelable: true, ...opts });
    el.dispatchEvent(ev);
    return ev;
}

function setup(submitAttrs = '') {
    document.body.innerHTML = `
      <div class="modal-card submitFocus">
        <textarea id="textAreaModal" class="textarea">hello world</textarea>
        <button class="button defaultSubmit" ${submitAttrs}>submit</button>
      </div>`;
    init();
    const ta = document.getElementById('textAreaModal');
    ta.focus();
    ta.setSelectionRange(11, 11);
    return ta;
}

beforeEach(() => { jest.useFakeTimers(); document.body.innerHTML = ''; });

test('Tab focuses the defaultSubmit button', () => {
    const ta = setup();
    const ev = press(ta, 'Tab');
    expect(ev.defaultPrevented).toBe(true);
    expect(document.activeElement.className).toContain('defaultSubmit');
});

test('Tab is not swallowed when the defaultSubmit button is disabled', () => {
    // A disabled button cannot take focus: preventing the default would trap the Tab.
    const ta = setup('disabled');
    const ev = press(ta, 'Tab');
    expect(ev.defaultPrevented).toBe(false);
    expect(document.activeElement).toBe(ta);
});
