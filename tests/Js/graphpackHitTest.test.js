/*
 * Geometric hit-testing of the graphpack canvas (getNodeUnderPointer).
 * Replaces the former hidden-canvas pixel picking, which broke in browsers
 * with canvas fingerprinting protection (e.g. Brave farbling getImageData).
 */

import { GraphPack } from '../../assets/js/graphpack_d3.js';

// Build a d3-hierarchy-like node (only the fields hit-testing reads).
function mkNode(data, x, y, r, depth, children) {
    const n = { data, x, y, r, depth };
    if (children && children.length) n.children = children;
    return n;
}
const circle = (x, y, r, depth, children) => mkNode({ type_: 'Circle' }, x, y, r, depth, children);
const role = (x, y, r, depth, role_type) => mkNode({ type_: 'Role', role_type }, x, y, r, depth);
const hidden = (x, y, r, depth) => mkNode({ type_: 'Hidden', role_type: 'Bot' }, x, y, r, depth);

/*
 * Fixture tree (graph/pack coordinates):
 *
 *   root (100,100 r100 d0)
 *   ├── circleA (60,100 r40 d1)
 *   │   ├── circleC (50,100 r15 d2)
 *   │   │   └── circleD (50,100 r7 d3)
 *   │   │       └── circleE (50,100 r3 d4)
 *   │   └── circleF (85,100 r10 d2)
 *   │       └── circleG (85,100 r4 d3)
 *   ├── roleB (150,100 r20 d1, Peer)
 *   └── hiddenH (100,160 r10 d1)
 */
function mkGraph() {
    const circleE = circle(50, 100, 3, 4);
    const circleD = circle(50, 100, 7, 3, [circleE]);
    const circleC = circle(50, 100, 15, 2, [circleD]);
    const circleG = circle(85, 100, 4, 3);
    const circleF = circle(85, 100, 10, 2, [circleG]);
    const circleA = circle(60, 100, 40, 1, [circleC, circleF]);
    const roleB = role(150, 100, 20, 1, 'Peer');
    const hiddenH = hidden(100, 160, 10, 1);
    const root = circle(100, 100, 100, 0, [circleA, roleB, hiddenH]);
    return { root, circleA, circleC, circleD, circleE, circleF, circleG, roleB, hiddenH };
}

// GraphPack instance with an identity canvas->graph transform (mouseX == gx).
function mkGp(g, focus) {
    const gp = Object.create(GraphPack);
    gp.rootNode = g.root;
    gp.focusedNode = focus;
    gp.centerX = 0;
    gp.centerY = 0;
    gp.nodeOffsetY = 0;
    gp.zoomCtx = { centerX: 0, centerY: 0, scale: 1 };
    return gp;
}
const at = (x, y) => ({ mouseX: x, mouseY: y });

describe('getNodeUnderPointer (geometric hit-testing)', () => {
    test('returns undefined outside the root circle', () => {
        const g = mkGraph();
        const gp = mkGp(g, g.root);
        expect(gp.getNodeUnderPointer(null, at(300, 300))).toBeUndefined();
    });

    test('returns the root in a dead zone (no child under pointer)', () => {
        const g = mkGraph();
        const gp = mkGp(g, g.root);
        expect(gp.getNodeUnderPointer(null, at(100, 30))).toBe(g.root);
    });

    test('descends to the deepest drawn circle, capped at focus depth + 3', () => {
        const g = mkGraph();
        const gp = mkGp(g, g.root);
        // circleE (d4) contains the point but is beyond root cap (0 + 3) -> circleD
        expect(gp.getNodeUnderPointer(null, at(50, 100))).toBe(g.circleD);
    });

    test('descends deeper once inside the focused subtree', () => {
        const g = mkGraph();
        const gp = mkGp(g, g.circleC);
        // focus circleC (d2): cap = 5 -> circleE (d4) is reachable
        expect(gp.getNodeUnderPointer(null, at(50, 100))).toBe(g.circleE);
    });

    test('outside the focused subtree, stops at the siblings level (mirrors drawOutside)', () => {
        const g = mkGraph();
        const gp = mkGp(g, g.circleC);
        // pointer in circleG (d3), child of sibling circleF: only circleF is drawn
        expect(gp.getNodeUnderPointer(null, at(85, 100))).toBe(g.circleF);
    });

    test('skips Hidden filler nodes', () => {
        const g = mkGraph();
        const gp = mkGp(g, g.root);
        expect(gp.getNodeUnderPointer(null, at(100, 160))).toBe(g.root);
    });

    test('applies the role rayon factor', () => {
        const g = mkGraph();
        const gp = mkGp(g, g.root);
        const rEff = 20 * GraphPack.rayonFactorRole; // drawn radius of roleB
        expect(gp.getNodeUnderPointer(null, at(150 + rEff - 0.5, 100))).toBe(g.roleB);
        expect(gp.getNodeUnderPointer(null, at(150 + rEff + 0.5, 100))).toBe(g.root);
    });

    test('inverts the zoom transform (pan + scale)', () => {
        const g = mkGraph();
        const gp = mkGp(g, g.root);
        gp.centerX = 500;
        gp.centerY = 300;
        gp.zoomCtx = { centerX: 100, centerY: 100, scale: 2 };
        // graph point (50,100) -> canvas ((50-100)*2+500, (100-100)*2+300)
        expect(gp.getNodeUnderPointer(null, at(400, 300))).toBe(g.circleD);
    });

    test('returns undefined when the graph is not initialized', () => {
        const gp = Object.create(GraphPack);
        gp.rootNode = null;
        expect(gp.getNodeUnderPointer(null, at(0, 0))).toBeUndefined();
    });
});
