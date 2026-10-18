/*
 * Board scroll restoration on browser back/forward (assets/js/scrollRestore.js).
 */

import { initScrollRestore } from '../../assets/js/scrollRestore.js';

jest.useFakeTimers();
global.requestAnimationFrame = cb => setTimeout(cb, 16);

const session = {};
initScrollRestore(session);

const go = path => history.pushState({}, '', path);
const back = path => { history.replaceState({}, '', path); window.dispatchEvent(new PopStateEvent('popstate')); };
const scroll = (el, x, y) => { el.scrollLeft = x; el.scrollTop = y; el.dispatchEvent(new Event('scroll')); };
const frame = () => jest.advanceTimersByTime(16);
const mount = (id, cls) => {
    const el = document.createElement('div');
    el.id = id;
    if (cls) el.className = cls;
    document.body.appendChild(el);
    return el;
};

test('restores the board scroll on back, wins over auto-scroll, released by user input', () => {
    go('/p/1');
    scroll(mount('board', 'kb-board'), 300, 0);
    frame();

    go('/p/2');
    document.getElementById('board').remove();

    back('/p/1');
    const board = mount('board', 'kb-board'); // re-rendered later by Elm
    scroll(board, 0, 0); // auto-scroll while loading
    frame();
    expect(board.scrollLeft).toBe(300);
    expect(session.scrollPositions.get(location.href).board).toEqual([300, 0]);

    window.dispatchEvent(new Event('wheel'));
    expect(session.restoringScroll).toBe(false);
    board.remove();
});

test('ignores non-board elements', () => {
    go('/x/1');
    scroll(mount('other'), 100, 100);
    frame();
    expect(session.scrollPositions.has(location.href)).toBe(false);
    document.getElementById('other').remove();
});

test('gives up after the timeout', () => {
    go('/t/1');
    const board = mount('board', 'kb-board');
    scroll(board, 100, 0);
    frame();
    go('/t/2');
    back('/t/1');
    jest.advanceTimersByTime(3100);
    expect(session.restoringScroll).toBe(false);
    board.remove();
});

test('keeps only the last 10 URLs', () => {
    const board = mount('board', 'kb-board');
    for (let i = 0; i < 15; i++) {
        go(`/u/${i}`);
        scroll(board, i + 1, 0);
        frame();
    }
    expect(session.scrollPositions.size).toBe(10);
    expect(session.scrollPositions.has('http://localhost/u/4')).toBe(false);
    expect(session.scrollPositions.has('http://localhost/u/14')).toBe(true);
    board.remove();
});
