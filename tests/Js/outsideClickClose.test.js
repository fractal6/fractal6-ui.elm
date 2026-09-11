// OUTSIDE_CLICK_CLOSE registers document listeners and unregisters them when it
// closes. Elm's `Ports.click ""` (the CLICK action) is the only way it gets
// unregistered on a close that comes from a click inside the panel (cross
// button, "add selected items", picking a color): the synthetic click has no
// mousedown, so it must bypass the mousedown-inside guard.
import { actions } from '../../assets/js/ports.js';

const mouse = (el, type) => el.dispatchEvent(new MouseEvent(type, { bubbles: true, button: 0 }));

const keydownHandlers = session => session.bulmaHandlers.filter(h => h[0] === 'keydown');

const setup = (hasEsc = false) => {
    document.body.innerHTML = `
        <div id="body">
            <div id="cardPanelContainer"><div id="cardPanel"><button id="cross">x</button></div></div>
            <div id="board"><div id="tile">tile</div></div>
        </div>`;
    const app = { ports: { closeCardPanelFromJs: { send: jest.fn() } } };
    const session = { bulmaHandlers: [] };
    actions['OUTSIDE_CLICK_CLOSE'](app, session, { msg: 'closeCardPanelFromJs', target: 'cardPanel', hasEsc });
    jest.advanceTimersByTime(60);
    return { app, session };
};

const clickOutside = el => {
    mouse(el, 'mousedown');
    mouse(el, 'mouseup');
    mouse(el, 'click');
};

beforeEach(() => jest.useFakeTimers());

test('synthetic click from a cross-button close unregisters the listener', () => {
    const { app, session } = setup();

    // User clicks the cross (inside the panel): Elm runs OnClose, which sends
    // `Ports.click ""` -> CLICK action -> #body click.
    const cross = document.getElementById('cross');
    mouse(cross, 'mousedown');
    mouse(cross, 'mouseup');
    actions['CLICK'](app, session, '');
    expect(app.ports.closeCardPanelFromJs.send).toHaveBeenCalledTimes(1);

    // Panel is closed: the stale listener must not fire on later clicks.
    app.ports.closeCardPanelFromJs.send.mockClear();
    clickOutside(document.getElementById('tile'));
    expect(app.ports.closeCardPanelFromJs.send).not.toHaveBeenCalled();
});

test('hasEsc toggles the Escape listener (CardPanel owns Escape in Elm)', () => {
    expect(keydownHandlers(setup(false).session)).toHaveLength(0);
    expect(keydownHandlers(setup(true).session)).toHaveLength(1);
});

test('mousedown inside the panel, click ending outside, does not close', () => {
    const { app } = setup();

    // Text selection drag: mousedown inside the panel, click event outside it.
    const cross = document.getElementById('cross');
    mouse(cross, 'mousedown');
    mouse(document.getElementById('tile'), 'click');
    expect(app.ports.closeCardPanelFromJs.send).not.toHaveBeenCalled();

    // Listeners are still attached, so a genuine outside click still closes.
    clickOutside(document.getElementById('tile'));
    expect(app.ports.closeCardPanelFromJs.send).toHaveBeenCalledTimes(1);
});
