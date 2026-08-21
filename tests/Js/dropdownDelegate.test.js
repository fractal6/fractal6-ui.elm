import { BulmaDriver } from '../../assets/js/bulma_drivers.js';

const stubApp = { ports: {} };

function addDropdown(id, cls = 'dropdown') {
    const el = document.createElement('div');
    el.id = id;
    el.className = cls;
    el.innerHTML = '<div class="dropdown-trigger"><span class="ellipsis">x</span></div>' +
        '<div class="dropdown-menu"><a class="dropdown-item">item</a></div>';
    document.body.appendChild(el);
    return el;
}

function click(el) {
    el.dispatchEvent(new window.MouseEvent('click', { bubbles: true }));
}

// Mirrors session.bulmaHandlers: one array for the whole app lifetime.
const handlers = [];

beforeEach(() => {
    document.body.innerHTML = '';
});

test('toggles on trigger click', () => {
    const dd = addDropdown('dd1');
    BulmaDriver(stubApp, null, handlers);

    click(dd.querySelector('.ellipsis'));
    expect(dd.classList.contains('is-active')).toBe(true);

    click(dd.querySelector('.ellipsis'));
    expect(dd.classList.contains('is-active')).toBe(false);
});

test('works for dropdowns rendered after the driver ran', () => {
    BulmaDriver(stubApp, null, handlers);
    const dd = addDropdown('late');

    click(dd.querySelector('.ellipsis'));
    expect(dd.classList.contains('is-active')).toBe(true);
});

test('survives the element being replaced (Elm re-render)', () => {
    let dd = addDropdown('dd1');
    BulmaDriver(stubApp, null, handlers);

    dd.remove();
    dd = addDropdown('dd1');

    click(dd.querySelector('.ellipsis'));
    expect(dd.classList.contains('is-active')).toBe(true);
});

test('closes the others, and all on outside click', () => {
    const a = addDropdown('a');
    const b = addDropdown('b');
    BulmaDriver(stubApp, null, handlers);

    click(a.querySelector('.ellipsis'));
    click(b.querySelector('.ellipsis'));
    expect(a.classList.contains('is-active')).toBe(false);
    expect(b.classList.contains('is-active')).toBe(true);

    click(document.body);
    expect(b.classList.contains('is-active')).toBe(false);
});

test('ignores elm-controlled and hoverable dropdowns', () => {
    const elm = addDropdown('elm1', 'dropdown elm');
    const hov = addDropdown('hov1', 'dropdown is-hoverable');
    BulmaDriver(stubApp, null, handlers);

    click(elm.querySelector('.ellipsis'));
    click(hov.querySelector('.ellipsis'));
    expect(elm.classList.contains('is-active')).toBe(false);
    expect(hov.classList.contains('is-active')).toBe(false);
});

test('a nested dropdown does not close its parent', () => {
    const parent = addDropdown('parent');
    const child = addDropdown('child');
    parent.querySelector('.dropdown-menu').appendChild(child);
    BulmaDriver(stubApp, null, handlers);

    click(parent.querySelector('.ellipsis'));
    click(child.querySelector('.ellipsis'));
    expect(parent.classList.contains('is-active')).toBe(true);
    expect(child.classList.contains('is-active')).toBe(true);
});
