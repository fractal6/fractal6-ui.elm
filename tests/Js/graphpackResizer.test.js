/*
 * Column splitting of the graphpack resizer grips (setColWidth).
 */

import { GraphPack } from '../../assets/js/graphpack_d3.js';

// jsdom has no layout: offsetWidth falls back to the inline width, else the given size.
function mkCols(wLeft, wRight) {
    const fakeWidth = (el, w) =>
        Object.defineProperty(el, 'offsetWidth', { get: () => parseInt(el.style.width, 10) || w });

    const left = document.createElement('div');
    const right = document.createElement('div');
    fakeWidth(left, wLeft);
    fakeWidth(right, wRight);

    const gp = Object.create(GraphPack);
    gp.$canvasParent = left.appendChild(document.createElement('div'));
    gp.$nextToChart = right.appendChild(document.createElement('div'));
    return { gp, left, right };
}

test('splits the row at the given width', () => {
    const { gp, left, right } = mkCols(500, 500);
    gp.setColWidth(700);
    expect(left.style.width).toBe('700px');
    expect(right.style.width).toBe('300px');
    expect(gp.userColWidth).toBe(700);
});

test('clamps both columns to minWidth, keeping the row total', () => {
    const { gp, left, right } = mkCols(500, 500);

    gp.setColWidth(10);
    expect(left.style.width).toBe(gp.minWidth + 'px');
    expect(right.style.width).toBe(1000 - gp.minWidth + 'px');

    gp.setColWidth(5000);
    expect(left.style.width).toBe(1000 - gp.minWidth + 'px');
    expect(right.style.width).toBe(gp.minWidth + 'px');
});
