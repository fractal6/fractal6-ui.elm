import { timer } from 'd3-timer';
import { GraphPack } from '../../assets/js/graphpack_d3.js';
import { actions, UCTX_KEY } from '../../assets/js/ports.js';

jest.mock('d3-timer', () => ({
    timer: jest.fn(tick => ({ tick, stop: jest.fn() })),
}));

const circle = (nameid, parent) => ({ nameid, name: nameid, type_: 'Circle', parent });
const role = (nameid, parent) => ({ nameid, name: 'Role', type_: 'Role', role_type: 'Peer', parent });
const parent = nameid => ({ nameid });
const tree = nameid => [
    circle('org', null),
    circle('org#a', parent('org')),
    circle('org#b', parent('org')),
    role(nameid, parent(nameid.split('#').slice(0, 2).join('#'))),
];
const geometry = ({ x, y, r }) => ({ x, y, r });
const payload = (data, focusid = 'org', nodeRenames = {}) => ({ data, focusid, nodeRenames });
let graphs, reduced;

function canvasDom() {
    document.body.innerHTML = `
        <div><div id="canvasParent"><canvas id="canvasOrga"></canvas>
            <div id="canvasButtons"></div><div id="canvasResizer"></div>
            <div id="nodeTooltip" data-event-tension="doTension" data-event-action="doAction">
                <span id="doTension"><span></span></span><span id="doAction"></span>
            </div>
        </div></div><div><div id="nextToChart"></div></div>`;
    Object.defineProperty(document.getElementById('canvasParent'), 'offsetWidth', { value: 600 });
    const ctx = Object.fromEntries(['setTransform', 'fillRect', 'beginPath', 'fill', 'stroke', 'setLineDash', 'fillText']
        .map(name => [name, jest.fn()]));
    ctx.measureText = text => ({ width: text.length * 10 });
    ctx.arc = jest.fn((x, y, r) => {
        if (![x, y, r].every(Number.isFinite) || r < 0) throw new Error('Invalid circle geometry');
    });
    document.getElementById('canvasOrga').getContext = () => ctx;
    document.getElementById('canvasOrga').getBoundingClientRect = () => ({ left: 0, top: 0, width: 600, height: 444 });
    return ctx;
}

function graph(data = tree('org#a#role'), focusid = 'org#a#role') {
    const ctx = canvasDom();
    const gp = Object.create(GraphPack);
    gp.computeCircleColorRange = jest.fn();
    gp.colorCircleRange = Array(8).fill('#cccccc');
    const app = { ports: Object.fromEntries([
        'nodeHoveredFromJs', 'nodeFocusedFromJs', 'nodeClickedFromJs', 'nodeLeftClickedFromJs',
        'nodeRightClickedFromJs', 'nodeDraggedFromJs', 'sendToggleGraphReverse',
    ].map(name => [name, { send: jest.fn(), subscribe: jest.fn() }])) };
    gp.init(app, payload(data, focusid), true);
    graphs.push(gp);
    const session = { gp, isInit: false, qsn: { removeAll: jest.fn(), addAll: jest.fn() } };
    return { gp, ctx, app, session };
}

beforeEach(() => {
    jest.useFakeTimers();
    timer.mockClear();
    graphs = [];
    reduced = false;
    window.matchMedia = jest.fn(() => ({ get matches() { return reduced; } }));
});
afterEach(() => {
    graphs.forEach(gp => gp.dispose());
    localStorage.clear();
    jest.useRealTimers();
});

test.each(['nodeSizeTopDown', 'nodeSizeBottomUp'])('packing order and geometry survive shuffled snapshots with %s', nodeSize => {
    const data = [
        circle('org', null),
        ...['org#a', 'org#b'].map(id => ({ ...circle(id, parent('org')), name: 'Circle' })),
        circle('org#large', parent('org')),
        circle('org#small', parent('org#large')),
        role('org#small#role', parent('org#small')),
        role('org##a', parent('org')),
        role('org##b', parent('org')),
        { ...role('org##c', parent('org')), name: 'Alpha' },
    ];
    const { gp } = graph(data, 'org');
    reduced = true;
    gp.nodeSize = gp[nodeSize];
    gp.resetGraphPack(data, 'org');
    const expected = Object.fromEntries(Object.entries(gp.nodesDict).map(([id, node]) => [id, geometry(node)]));
    const kids = gp.rootNode.children;
    // Circles first, largest first; then roles, ordered by name then nameid.
    expect(kids.map(n => n.data.type_)).toEqual(['Circle', 'Circle', 'Circle', 'Role', 'Role', 'Role']);
    expect(kids.slice(0, 3).map(n => n.r)).toEqual([...kids.slice(0, 3).map(n => n.r)].sort((x, y) => y - x));
    expect(kids.slice(3).map(n => n.data.nameid)).toEqual(['org##c', 'org##a', 'org##b']);
    expect(gp.nodesDict['org#small'].children.map(n => n.data.type_)).toEqual(['Role']);
    for (const snapshot of [[...data].reverse(), [...data.slice(3), ...data.slice(0, 3)]]) {
        gp.resetGraphPack(snapshot, 'org');
        expect(Object.fromEntries(Object.entries(gp.nodesDict).map(([id, node]) => [id, geometry(node)]))).toEqual(expected);
    }
});

test('sparse circles share a minimum footprint, and sizing ignores names', () => {
    const bot = (nameid, p) => ({ ...role(nameid, p), role_type: 'Bot' });
    const roles = (id, n) => Array.from({ length: n }, (_, i) => role(`${id}#r${i}`, parent(id)));
    const data = [
        circle('org', null),
        ...['org#e', 'org#1', 'org#2', 'org#3', 'org#m'].map(id => circle(id, parent('org'))),
        ...roles('org#1', 1), ...roles('org#2', 2), ...roles('org#3', 3),
        ...roles('org#m', 3), ...[1, 2, 3].map(i => bot(`org#m#b${i}`, parent('org#m'))),
    ];
    const { gp } = graph(data, 'org');
    const r = id => gp.nodesDict[id].target.r;
    // Empty, one role and two roles all get the two-role footprint; a third role grows it.
    expect(r('org#1')).toBeCloseTo(r('org#e'));
    expect(r('org#2')).toBeCloseTo(r('org#e'));
    expect(r('org#3')).toBeGreaterThan(r('org#e'));
    const mixed = r('org#m');
    // Siblings pack by size, so renaming a role cannot reorder or reshape its circle.
    gp.resetGraphPack(data.map(n => n.nameid === 'org#m#r0' ? { ...n, name: 'Zzz' } : n), 'org');
    expect(gp.nodesDict['org#m'].children.map(n => n.data.role_type))
        .toEqual(['Peer', 'Peer', 'Peer', 'Bot', 'Bot', 'Bot']);
    expect(r('org#m')).toBeCloseTo(mixed);
});

test('redraw maps renamed focus and starts at displayed geometry, without leaking the map', () => {
    const { gp, app, session } = graph();
    const old = geometry(gp.focusedNode);
    const viewport = [...gp.viewport];
    const moved = tree('org#b#role');
    actions.DRAW_GRAPHPACK(app, session, payload(moved, '', { 'org#a#role': 'org#b#role' }));
    expect(gp.focusedNode.data.nameid).toBe('org#b#role');
    expect(gp.nodesDict['org#a#role']).toBeUndefined();
    expect(geometry(gp.focusedNode)).toEqual(old);
    expect(gp.viewport).toEqual(viewport);
    expect(gp.exitingNodes).toEqual([]);
    const tracks = gp.motion.tracks;
    const targetViewport = gp.motion.vp;
    const motionTimer = gp.motionTimer;
    motionTimer.tick(200);
    tracks.forEach(({ node, from, to }) => {
        for (const key of ['x', 'y', 'r']) expect(node[key]).toBeCloseTo((from[key] + to[key]) / 2);
    });
    gp.viewport.forEach((v, i) => expect(v).toBeCloseTo((viewport[i] + targetViewport[i]) / 2));
    motionTimer.tick(400);
    expect(gp.motion).toBeNull();
    expect(gp.motionTimer).toBeNull();
    expect(motionTimer.stop).toHaveBeenCalled();
    tracks.forEach(({ node, to }) => expect(geometry(node)).toEqual(geometry(to)));
    expect(gp.viewport).toEqual(targetViewport);

    actions.DRAW_GRAPHPACK(app, session, payload([...moved, role('org#a#role', parent('org#a'))]));
    expect(gp.focusedNode.data.nameid).toBe('org#b#role');
    const hidden = gp.nodesDict['org#a#role'];
    expect(hidden.opacity).toBe(0);
    expect(geometry(hidden)).toEqual(hidden.target);
    expect(gp.motion.tracks.some(t => t.node === hidden)).toBe(false);
    expect(session.qsn.addAll).toHaveBeenCalledWith(gp.dataNodes);
});

test('a moved circle and descendants retain geometry while both parents reflow', () => {
    const data = [
        ...tree('org#a#role'),
        circle('org#moving', parent('org#a')),
        role('org#moving#child', parent('org#moving')),
    ];
    const { gp, app, session } = graph(data, 'org');
    const before = Object.fromEntries(Object.entries(gp.nodesDict).map(([id, node]) => [id, geometry(node)]));
    const moved = data.map(n => n.nameid === 'org#moving' ? { ...n, parent: parent('org#b') } : n);
    actions.DRAW_GRAPHPACK(app, session, payload(moved, '', {
        'org#moving': 'org#moving', 'org#moving#child': 'org#moving#child',
    }));
    for (const id of ['org#a', 'org#b', 'org#moving', 'org#moving#child']) {
        expect(geometry(gp.nodesDict[id])).toEqual(before[id]);
        expect(gp.nodesDict[id].target).not.toEqual(before[id]);
    }
    expect(gp.exitingNodes).toEqual([]);
    gp.motionTimer.tick(400);
    for (const node of Object.values(gp.nodesDict)) expect(geometry(node)).toEqual(node.target);
});

test('entries grow and exits shrink/fade outside the live hierarchy, including interruptions', () => {
    const { gp, app, session } = graph(tree('org#a#role'), 'org');
    const removed = gp.nodesDict['org#a#role'];
    const radius = removed.r;
    const data = tree('org#b#new');
    actions.DRAW_GRAPHPACK(app, session, payload(data));
    expect(gp.nodesDict['org#a#role']).toBeUndefined();
    expect(gp.exitingNodes).toEqual([removed]);
    expect(gp.nodesDict['org#b#new'].r).toBe(0);
    gp.motionTimer.tick(200);
    expect(removed.r).toBeCloseTo(radius / 2);
    expect(removed.opacity).toBe(0.5);
    const displayed = geometry(gp.nodesDict['org#b#new']);
    const oldTimer = gp.motionTimer;
    actions.DRAW_GRAPHPACK(app, session, payload([...data, circle('org#extra', parent('org'))]));
    expect(oldTimer.stop).toHaveBeenCalled();
    expect(gp.exitingNodes).toEqual([removed]);
    expect(removed.r).toBeCloseTo(radius / 2);
    expect(removed.opacity).toBe(0.5);
    expect(geometry(gp.nodesDict['org#b#new'])).toEqual(displayed);
    oldTimer.tick(400); // Even a stale queued callback cannot advance the replacement motion.
    expect(geometry(gp.nodesDict['org#b#new'])).toEqual(displayed);
    gp.motionTimer.tick(400);
    expect(gp.exitingNodes).toEqual([]);
    expect(removed.r).toBe(0);
    expect(removed.opacity).toBe(0);
});

test('zoom interrupts layout and exits from the displayed frame; resize does not reset either', () => {
    const { gp, app, session } = graph(tree('org#a#role'), 'org');
    const removed = gp.nodesDict['org#a#role'];
    actions.DRAW_GRAPHPACK(app, session, payload([
        ...gp.dataNodes.filter(n => n.type_ !== 'Role'), circle('org#extra', parent('org')),
    ]));
    gp.motionTimer.tick(150);
    const viewport = [...gp.viewport];
    const oldTimer = gp.motionTimer;
    const nodes = [...Object.values(gp.nodesDict), removed].map(node => [node, geometry(node)]);
    const opacity = removed.opacity;
    gp.zoomToNode('org#a');
    expect(oldTimer.stop).toHaveBeenCalled();
    expect(gp.viewport).toEqual(viewport);
    nodes.forEach(([node, before]) => expect(geometry(node)).toEqual(before));
    expect(gp.motion.changingRadii).toBe(true);
    expect(gp.motion.tracks.find(t => t.node === removed).geometry).toBe(true);
    const newTimer = gp.motionTimer;
    gp.resizeMe();
    expect(gp.motionTimer).toBe(newTimer);
    expect(gp.viewport).toEqual(viewport);
    newTimer.tick(200);
    gp.motion.tracks.forEach(({ node, from, to }) => {
        for (const key of ['x', 'y', 'r']) expect(node[key]).toBeCloseTo((from[key] + to[key]) / 2);
    });
    expect(removed.opacity).toBeCloseTo(opacity / 2);
    newTimer.tick(400);
    Object.values(gp.nodesDict).forEach(node => expect(geometry(node)).toEqual(node.target));
    expect(removed.r).toBe(0);
    expect(removed.opacity).toBe(0);
    expect(gp.exitingNodes).toEqual([]);
    expect(gp.zoomCtx.centerX).toBe(gp.zoomedNode.x);
    expect(gp.zoomCtx.centerY).toBe(gp.zoomedNode.y);
});

test('motion caches the old/new visible union and immediately settles nodes outside it', () => {
    const data = [circle('org', null)];
    for (let i = 1; i < 127; i++)
        data.push(circle(`org#n${i}`, parent(i < 3 ? 'org' : `org#n${Math.floor((i - 1) / 2)}`)));
    const { gp, app, session } = graph(data, 'org#n7');
    const before = gp.visibleNodes().map(n => n.data.nameid);
    actions.DRAW_GRAPHPACK(app, session, payload([...data, role('org#n126#new', parent('org#n126'))]));
    gp.zoomToNode('org#n15');
    const visible = new Set([...before, ...gp.visibleNodes().map(n => n.data.nameid)]);
    const { tracks, drawNodes } = gp.motion;
    expect(new Set(tracks.map(t => t.node.data.nameid))).toEqual(visible);
    expect(new Set(drawNodes)).toEqual(new Set(tracks.map(t => t.node)));
    expect(tracks.length).toBeLessThan(gp.nodes.length / 2);
    const hidden = Object.values(gp.nodesDict).filter(n => !visible.has(n.data.nameid));
    expect(hidden).toContain(gp.nodesDict['org#n126#new']);
    hidden.forEach(n => {
        expect(geometry(n)).toEqual(n.target);
        expect(n.opacity).toBe(0);
        expect(n.ctx).toBeNull();
        Object.freeze(n); // Frame updates must not touch nodes outside the cached visible set.
    });
    gp.motionTimer.tick(200);
    expect(gp.motion.drawNodes).toBe(drawNodes);
    gp.motionTimer.tick(400);
});

test('static zoom keeps geometry and opacity untouched and reuses its sorted draw list', () => {
    const { gp } = graph(tree('org#a#role'), 'org');
    gp.zoomToNode('org#a');
    const { tracks, drawNodes, changingRadii } = gp.motion;
    expect(changingRadii).toBe(false);
    expect(tracks.every(t => !t.geometry)).toBe(true);
    expect(drawNodes.map(n => n.r)).toEqual(drawNodes.map(n => n.r).sort((a, b) => b - a));
    const sort = jest.spyOn(drawNodes, 'sort');
    tracks.forEach(({ node }) => {
        for (const key of ['x', 'y', 'r', 'opacity']) Object.defineProperty(node, key, { writable: false });
    });
    const roleNode = gp.nodesDict['org#a#role'];
    const ctx = roleNode.ctx;
    gp.motionTimer.tick(200);
    expect(gp.motion.drawNodes).toBe(drawNodes);
    expect(roleNode.ctx).not.toEqual(ctx);
    expect(roleNode.ctx.rayon).toBeCloseTo(gp.nodeRayon(roleNode) * gp.zoomCtx.scale);
    gp.motionTimer.tick(400);
    expect(sort).not.toHaveBeenCalled();
    expect(gp.getNodeUnderPointer(null, {
        mouseX: roleNode.ctx.centerX, mouseY: roleNode.ctx.centerY,
    })).toBe(roleNode);
});

test('changing radii remain sorted each frame when their ordering crosses', () => {
    const crowded = [...tree('org#a#role'), ...[1, 2, 3].map(i => role(`org#b#r${i}`, parent('org#b')))];
    const { gp } = graph(crowded, 'org');
    const a = gp.nodesDict['org#b'], b = gp.nodesDict['org#a'];
    expect(a.r).toBeGreaterThan(b.r);
    a.target = { ...a.target, r: b.r / 2 };
    b.target = { ...b.target, r: a.r * 2 };
    gp.startMotion(true);
    expect(gp.motion.changingRadii).toBe(true);
    const draw = jest.spyOn(gp, 'drawNode');
    for (const elapsed of [100, 300]) {
        draw.mockClear();
        gp.motionTimer.tick(elapsed);
        const drawn = draw.mock.calls.map(([node]) => node);
        expect(drawn.indexOf(a) < drawn.indexOf(b)).toBe(a.r > b.r);
    }
    expect(a.r).toBeLessThan(b.r);
});

test('moving a visible subtree beyond the new depth cap fades it rather than dropping it mid-flight', () => {
    const data = [
        ...tree('org#a#role'),
        circle('org#deep', parent('org#b')),
        circle('org#deeper', parent('org#deep')),
    ];
    const { gp, app, session } = graph(data, 'org');
    const moved = data.map(n => n.nameid === 'org#a' ? { ...n, parent: parent('org#deeper') } : n);
    actions.DRAW_GRAPHPACK(app, session, payload(moved, '', { 'org#a': 'org#a', 'org#a#role': 'org#a#role' }));
    const child = gp.nodesDict['org#a#role'];
    expect(gp.visibleNodes()).not.toContain(child);
    expect(child.opacity).toBe(1);
    gp.motionTimer.tick(200);
    expect(child.opacity).toBe(0.5);
    const draw = jest.spyOn(gp, 'drawNode');
    gp.drawCanvas();
    const drawn = draw.mock.calls.map(([node]) => node);
    expect(drawn).toContain(child);
    expect(drawn.indexOf(gp.rootNode)).toBeLessThan(drawn.indexOf(child));
    gp.motionTimer.tick(400);
    expect(child.opacity).toBe(0);
});

test('reduced motion applies exact layout and zoom endpoints synchronously, also when changed mid-flight', () => {
    const { gp, app, session } = graph(tree('org#a#role'), 'org');
    reduced = true;
    actions.DRAW_GRAPHPACK(app, session, payload([...gp.dataNodes, circle('org#extra', parent('org'))]));
    gp.zoomToNode('org#a');
    expect(timer).not.toHaveBeenCalled();
    expect(gp.isZooming).toBe(false);
    expect(gp.viewport[0]).toBe(gp.zoomedNode.x);
    Object.values(gp.nodesDict).forEach(n => expect(geometry(n)).toEqual(n.target));
    reduced = false;
    gp.zoomToNode('org');
    const active = gp.motionTimer;
    reduced = true;
    active.tick(1);
    expect(active.stop).toHaveBeenCalled();
    expect(gp.motion).toBeNull();
});

test('small owned roles never draw a negative dashed-border radius', () => {
    localStorage.setItem(UCTX_KEY, JSON.stringify({ username: 'alice' }));
    const data = tree('org#a#role').map(n => n.type_ === 'Role' ? { ...n, first_link: { username: 'alice' } } : n);
    const { gp, app, session, ctx } = graph(data, 'org');
    actions.DRAW_GRAPHPACK(app, session, payload(data.filter(n => n.type_ !== 'Role')));
    gp.motionTimer.tick(390);
    expect(gp.exitingNodes[0].r * gp.zoomCtx.scale).toBeLessThan(1);
    expect(() => gp.drawCanvas()).not.toThrow();
    expect(ctx.arc).toHaveBeenCalled();
});

test('reflow clears delayed tooltip/drag state and blocks pointer, menu, tooltip and keyboard actions', () => {
    const { gp, app, session } = graph(tree('org#a#role'), 'org');
    gp.drawNodeHover(gp.nodesDict['org#a#role'], true);
    gp.pressed = true;
    gp.dragCandidate = { node: gp.nodesDict['org#a#role'], x: 10, y: 10 };
    actions.DRAW_GRAPHPACK(app, session, payload([...gp.dataNodes, circle('org#extra', parent('org'))]));
    expect(gp.dragCandidate).toBeNull();
    expect(gp.pressed).toBe(false);
    jest.advanceTimersByTime(30);
    expect(gp.$tooltip.classList.contains('is-invisible')).toBe(true);
    expect(gp.$tooltip.inert).toBe(true);
    expect(gp.getNodeUnderPointer(null, { mouseX: 100, mouseY: 100 })).toBeUndefined();
    ['pointerdown', 'pointermove', 'pointerup', 'pointerleave', 'contextmenu'].forEach(type =>
        gp.$canvas.dispatchEvent(new MouseEvent(type, { bubbles: true, button: 0 })));
    gp.$canvas.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowDown', cancelable: true }));
    document.getElementById('doTension').dispatchEvent(new MouseEvent('mousedown', { button: 0 }));
    for (const name of ['nodeClickedFromJs', 'nodeDraggedFromJs', 'nodeLeftClickedFromJs', 'nodeRightClickedFromJs'])
        expect(app.ports[name].send).not.toHaveBeenCalled();
});

test('a newer redraw cancels delayed INIT, while identical same-canvas INIT does not restart its motion', () => {
    const { gp, app, session } = graph();
    actions.INIT_GRAPHPACK(app, session, payload(gp.dataNodes, 'org#a#role'));
    const moved = payload(tree('org#b#role'), 'org#b#role', { 'org#a#role': 'org#b#role' });
    actions.DRAW_GRAPHPACK(app, session, moved);
    const active = gp.motionTimer;
    jest.advanceTimersByTime(150);
    expect(gp.motionTimer).toBe(active);
    expect(gp.focusedNode.data.nameid).toBe('org#b#role');
    actions.INIT_GRAPHPACK(app, session, payload([], ''));
    actions.INIT_GRAPHPACK(app, session, { ...moved, nodeRenames: {} });
    jest.advanceTimersByTime(150);
    expect(gp.motionTimer).toBe(active);
    expect(active.stop).not.toHaveBeenCalled();
});

test('canvas replacement cancels old timers/listeners without canceling initialization of the new canvas', async () => {
    const { gp, app, session } = graph(tree('org#a#role'), 'org');
    gp.zoomToNode('org#a');
    const oldTimer = gp.motionTimer;
    const oldCanvas = gp.$canvas;
    const removeListener = jest.spyOn(oldCanvas, 'removeEventListener');
    window.dispatchEvent(new Event('resize'));
    actions.DRAW_BUTTONS_GRAPHPACK(app, session);
    canvasDom();
    actions.INIT_GRAPHPACK(app, session, payload(tree('org#b#role')));
    await Promise.resolve(); // Deliver the old canvas's removal observer.
    expect(oldTimer.stop).toHaveBeenCalled();
    expect(removeListener).toHaveBeenCalled();
    jest.advanceTimersByTime(150);
    expect(gp.$canvas).not.toBe(oldCanvas);
    expect(gp.nodesDict['org#b#role']).toBeDefined();
    expect(gp.nodesDict['org#a#role']).toBeUndefined();
    expect(gp.motionTimer).toBeNull();
    gp.$canvas.remove();
    await Promise.resolve();
    expect(gp.handlers).toEqual([]);
    expect(gp.observer).toBeNull();
    expect(gp.resizeTimer).toBeNull();
    expect(gp.buttonsTimer).toBeNull();
});

test('reverse interrupts through the same timer, preserving focus and not accumulating hidden fillers', () => {
    const { gp, app } = graph();
    const reverse = app.ports.sendToggleGraphReverse.subscribe.mock.calls[0][0];
    const count = gp.nodes.length;
    reverse();
    const active = gp.motionTimer;
    const viewport = [...gp.viewport];
    reverse();
    expect(active.stop).toHaveBeenCalled();
    expect(gp.viewport).toEqual(viewport);
    expect(gp.focusedNode.data.nameid).toBe('org#a#role');
    expect(gp.nodes).toHaveLength(count);
    gp.motionTimer.tick(400);
    expect(gp.isZooming).toBe(false);
});

test('disconnected private roots are selected without mutating the snapshot used by same-canvas INIT', () => {
    const data = [circle('org#a', parent('org#private')), circle('org#other', null), role('org#a#role', parent('org#a'))];
    const before = JSON.stringify(data);
    const { gp, app } = graph(data, 'org#a#role');
    expect(gp.rootNode.data.nameid).toBe('org#a');
    expect(JSON.stringify(data)).toBe(before);
    const nodes = gp.nodes;
    gp.init(app, payload(data, 'org#a#role'), false);
    expect(gp.nodes).toBe(nodes);
    expect(gp.motion).toBeNull();
    gp.zoomToNode('org#other');
    expect(gp.rootNode.data.nameid).toBe('org#other');
    gp.motionTimer.tick(400);
    expect(gp.exitingNodes).toEqual([]);
});

test('initial percent-encoded focus is decoded and deleted focus falls back to its surviving parent', () => {
    const { gp, app, session } = graph(tree('org#a#role'), 'org%23a%23role');
    expect(gp.focusedNode.data.nameid).toBe('org#a#role');
    const viewport = [...gp.viewport];
    actions.DRAW_GRAPHPACK(app, session, payload(gp.dataNodes.filter(n => n.type_ !== 'Role')));
    expect(gp.focusedNode.data.nameid).toBe('org#a');
    expect(gp.viewport).toEqual(viewport);
    gp.motionTimer.tick(400);
    expect(gp.exitingNodes).toEqual([]);
});

test.each(['org#a#role', 'org%23a%23role'])('redraw before initialization remaps pending focus %s', focusid => {
    const { gp, app, session } = graph();
    gp.dispose();
    actions.INIT_GRAPHPACK(app, session, payload(tree('org#a#role'), focusid));
    actions.DRAW_GRAPHPACK(app, session, payload(tree('org#b#role'), '', { 'org#a#role': 'org#b#role' }));
    actions.INIT_GRAPHPACK(app, session, payload([], ''));
    jest.advanceTimersByTime(150);
    expect(gp.focusedNode.data.nameid).toBe('org#b#role');
    expect(gp.nodesDict['org#a#role']).toBeUndefined();
});

test('a token refresh updates ownership styling without repacking or canceling motion', () => {
    const { gp, app, session } = graph();
    gp.zoomToNode('org');
    const active = gp.motionTimer;
    const nodes = gp.nodes;
    localStorage.setItem(UCTX_KEY, JSON.stringify({ username: 'alice' }));
    actions.FLUSH_GRAPHPACK(app, session, '');
    expect(gp.uctx.username).toBe('alice');
    expect(gp.nodes).toBe(nodes);
    expect(gp.motionTimer).toBe(active);
});

test.each([
    ['org#a', 'org#a', 167],
    ['org#a', 'org', 127],
    ['org', 'org#a', 127],
])('tooltip placement requires matching focus %s and zoom %s', (focusid, zoomid, top) => {
    const { gp } = graph();
    const node = gp.nodesDict['org#a'];
    node.ctx = { centerX: 200, centerY: 200, rayon: 30 };
    gp.focusedNode = gp.nodesDict[focusid];
    gp.zoomedNode = gp.nodesDict[zoomid];
    Object.defineProperties(gp.$tooltip, {
        clientWidth: { value: 120 },
        clientHeight: { value: 40 },
    });
    gp.drawNodeTooltip_(node);
    expect(gp.$tooltip.style.top).toBe(`${top}px`);
    expect(gp.$tooltip.style.left).toBe('139px');
    expect(gp.$tooltip.inert).toBe(false);
});

test('hidden tooltip placement settles before revealing, without forcing visible replacements to settle', () => {
    const { gp } = graph(tree('org#a#role'), 'org');
    const settle = jest.fn(() => {
        expect(gp.$tooltip.classList.contains('is-invisible')).toBe(true);
        expect(gp.$tooltip.style.left).not.toBe('');
        expect(gp.$tooltip.style.top).not.toBe('');
    });
    gp.$tooltip.getBoundingClientRect = settle;
    gp.drawNodeHover(gp.nodesDict['org#a'], true);
    jest.advanceTimersToNextFrame();
    expect(settle).toHaveBeenCalledTimes(1);
    gp.drawNodeHover(gp.nodesDict['org#b'], true);
    jest.advanceTimersToNextFrame();
    expect(settle).toHaveBeenCalledTimes(1);
});

test('hover replacements stay visible and commit the latest title, options and position after rendering', () => {
    const { gp, app } = graph(tree('org#a#role'), 'org');
    const title = document.querySelector('#doTension > span');
    const options = document.getElementById('doAction');
    Object.defineProperties(gp.$tooltip, {
        clientWidth: { get: () => 100 + options.textContent.length * 10 },
        clientHeight: { value: 40 },
    });
    // Model the port's Elm render, queued before JS measures the conditional options.
    app.ports.nodeHoveredFromJs.send.mockImplementation(id => {
        requestAnimationFrame(() => { options.textContent = id; });
    });
    const draw = jest.spyOn(gp, 'drawNodeTooltip_');
    const a = gp.nodesDict['org#a'], b = gp.nodesDict['org#b'], roleNode = gp.nodesDict['org#a#role'];
    gp.drawNodeHover(a, true);
    jest.advanceTimersToNextFrame();
    expect(gp.$tooltip.classList.contains('is-invisible')).toBe(false);
    expect(title.textContent).toBe(a.data.name);

    gp.drawNodeHover(b, true);
    expect(gp.$tooltip.classList.contains('is-invisible')).toBe(false);
    expect(gp.$tooltip.inert).toBe(true);
    expect(title.textContent).toBe(a.data.name);
    document.getElementById('doTension').dispatchEvent(new MouseEvent('mousedown', { button: 0 }));
    gp.$tooltip.dispatchEvent(new MouseEvent('contextmenu', { bubbles: true, cancelable: true }));
    gp.$canvas.dispatchEvent(new MouseEvent('contextmenu', { cancelable: true }));
    expect(app.ports.nodeLeftClickedFromJs.send).not.toHaveBeenCalled();
    expect(app.ports.nodeRightClickedFromJs.send).not.toHaveBeenCalled();

    gp.drawNodeHover(roleNode, true);
    jest.advanceTimersToNextFrame();
    expect(draw.mock.calls.map(([node]) => node.data.nameid)).toEqual([a.data.nameid, roleNode.data.nameid]);
    expect(options.textContent).toBe(roleNode.data.nameid);
    expect(title.textContent).toBe(roleNode.data.name);
    expect(gp.$tooltip.style.left).toBe(`${roleNode.ctx.centerX - (gp.$tooltip.clientWidth / 2 + 1)}px`);
    expect(gp.$tooltip.inert).toBe(false);
    expect(gp.tooltipFrame).toBeNull();
    document.getElementById('doTension').dispatchEvent(new MouseEvent('mousedown', { button: 0 }));
    expect(app.ports.nodeLeftClickedFromJs.send).toHaveBeenLastCalledWith(roleNode.data.nameid);
});

test('entry and rapid node-gap-node movement use the picked target without hiding the tooltip', () => {
    const { gp } = graph(tree('org#a#role').filter(n => n.type_ === 'Circle'), 'org');
    jest.advanceTimersToNextFrame();
    const a = gp.nodesDict['org#a'], b = gp.nodesDict['org#b'];
    const gap = { centerX: (a.ctx.centerX + b.ctx.centerX) / 2, centerY: (a.ctx.centerY + b.ctx.centerY) / 2 };
    for (const [type, point, expected] of [
        ['pointerenter', a.ctx, a],
        ['pointermove', b.ctx, b],
        ['pointermove', gap, gp.rootNode],
        ['pointermove', a.ctx, a],
    ]) {
        gp.$canvas.dispatchEvent(new MouseEvent(type, { clientX: point.centerX, clientY: point.centerY }));
        expect(gp.hoveredNode).toBe(expected);
        expect(gp.$tooltip.classList.contains('is-invisible')).toBe(false);
    }
    jest.advanceTimersToNextFrame();
    expect(document.querySelector('#doTension > span').textContent).toBe(a.data.name);
});

test.each([[100, 160], [260, 240]])('tooltip handoff follows its actual rectangle at y=%s, including canvas offsets', (top, gapY) => {
    const { gp } = graph(tree('org#a#role'), 'org');
    const node = gp.nodesDict['org#a'];
    gp.drawNodeHover(node, true);
    jest.advanceTimersToNextFrame();
    node.ctx = { centerX: 200, centerY: 200, rayon: 30 };
    gp.$canvas.getBoundingClientRect = () => ({ left: 80, top: 50, width: 600, height: 444 });
    gp.$tooltip.getBoundingClientRect = () => ({ left: 220, top: top + 50, width: 120, height: 40 });
    const gap = { mouseX: 200, mouseY: gapY };
    expect(gp.checkIf({ mouseX: 200, mouseY: top + 20 }, 'InTooltip', node)).toBe(true);
    expect(gp.checkIf(gap, 'InTooltip', node)).toBe(true);
    expect(gp.checkIf({ mouseX: 80, mouseY: gapY }, 'InTooltip', node)).toBe(false);
    expect(gp.checkIf({ mouseX: 200, mouseY: 200 }, 'InTooltip', node)).toBe(false);
    gp.$tooltip.inert = true;
    expect(gp.checkIf(gap, 'InTooltip', node)).toBe(false);
    gp.clearNodeTooltip();
    gp.$tooltip.inert = false;
    expect(gp.checkIf(gap, 'InTooltip', node)).toBe(false);
});

test('canvas-to-tooltip handoff and menu interaction retain their target until the menu closes', () => {
    const { gp, app, session } = graph(tree('org#a#role').filter(n => n.type_ === 'Circle'), 'org');
    const a = gp.nodesDict['org#a'], b = gp.nodesDict['org#b'];
    gp.drawNodeHover(a, true);
    jest.advanceTimersToNextFrame();
    document.getElementById('doAction').innerHTML = '<span class="clickMe"><i></i></span><div class="menu-item"></div>';
    const trigger = document.querySelector('#doAction i');
    gp.$canvas.dispatchEvent(new MouseEvent('pointerleave', { clientX: 620, clientY: 200, relatedTarget: trigger }));
    expect(gp.hoveredNode).toBe(a);
    trigger.dispatchEvent(new MouseEvent('mousedown', { bubbles: true, button: 0 }));
    document.querySelector('.menu-item').dispatchEvent(new MouseEvent('mousedown', { bubbles: true, button: 0 }));
    expect(gp.isFrozen).toBe(true);
    gp.$canvas.dispatchEvent(new MouseEvent('pointerleave', { clientX: 620, clientY: 200, relatedTarget: document.body }));
    expect(gp.hoveredNode).toBe(a);
    gp.resizeMe();
    jest.advanceTimersToNextFrame();
    expect(gp.hoveredNode).toBe(a);
    actions.CLEAR_CONTEXT_MENU(app, session);
    gp.$canvas.dispatchEvent(new MouseEvent('pointermove', { clientX: b.ctx.centerX, clientY: b.ctx.centerY }));
    expect(gp.hoveredNode).toBe(b);
});

test('the connecting gap outside the canvas does not reset hover to focus', () => {
    const { gp } = graph(tree('org#a#role'), 'org');
    const node = gp.nodesDict['org#a'];
    gp.drawNodeHover(node, true);
    jest.advanceTimersToNextFrame();
    node.ctx = { centerX: gp.centerX, centerY: 40, rayon: 20 };
    gp.$tooltip.getBoundingClientRect = () => ({ left: gp.centerX - 60, top: -70, width: 120, height: 40 });
    gp.$canvas.dispatchEvent(new MouseEvent('pointerleave', { clientX: gp.centerX, clientY: -1, relatedTarget: document.body }));
    expect(gp.hoveredNode).toBe(node);
    gp.$canvas.dispatchEvent(new MouseEvent('pointerleave', { clientX: 620, clientY: 200, relatedTarget: document.body }));
    expect(gp.hoveredNode).toBe(gp.focusedNode);
});

test.each(['dismiss', 'navigate', 'drag', 'dispose'])('%s cancels pending tooltip rendering', action => {
    const { gp } = graph(tree('org#a#role').filter(n => n.type_ === 'Circle'), 'org');
    const node = gp.nodesDict['org#a'];
    const tooltip = gp.$tooltip;
    const draw = jest.spyOn(gp, 'drawNodeTooltip_');
    gp.drawNodeHover(node, true);
    if (action === 'dismiss') gp.clearNodeTooltip();
    else if (action === 'navigate') gp.nodeClickedFromJs(node);
    else if (action === 'dispose') gp.dispose();
    else {
        gp.$canvas.dispatchEvent(new MouseEvent('pointerdown', { clientX: node.ctx.centerX, clientY: node.ctx.centerY, button: 0 }));
        gp.$canvas.dispatchEvent(new MouseEvent('pointermove', { clientX: node.ctx.centerX + 10, clientY: node.ctx.centerY + 10 }));
        expect(gp.isDragging).toBe(true);
    }
    expect(gp.tooltipFrame).toBeNull();
    jest.advanceTimersToNextFrame();
    expect(draw).not.toHaveBeenCalled();
    expect(tooltip.classList.contains('is-invisible')).toBe(true);
    expect(tooltip.inert).toBe(true);
});

test('focus tooltip and its actions resume after layout, keyboard zoom, reduced motion and resize', () => {
    const { gp, app, session } = graph();
    actions.DRAW_GRAPHPACK(app, session, payload(tree('org#b#role'), '', { 'org#a#role': 'org#b#role' }));
    gp.motionTimer.tick(400);
    gp.$canvas.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowUp', cancelable: true }));
    expect(app.ports.nodeClickedFromJs.send).toHaveBeenLastCalledWith('org#b');
    actions.FOCUS_GRAPHPACK(app, session, 'org#b');
    gp.motionTimer.tick(400);
    reduced = true;
    gp.zoomToNode('org#b#role');
    gp.resizeMe();
    jest.advanceTimersByTime(25);
    expect(gp.hoveredNode).toBe(gp.focusedNode);
    expect(app.ports.nodeHoveredFromJs.send).toHaveBeenLastCalledWith('org#b#role');
    expect(gp.$tooltip.inert).toBe(false);
    document.getElementById('doTension').dispatchEvent(new MouseEvent('mousedown', { button: 0 }));
    expect(app.ports.nodeLeftClickedFromJs.send).toHaveBeenLastCalledWith('org#b#role');
});

test('authoritative metadata refresh preserves the running layout and updates live node data', () => {
    const { gp, app, session } = graph();
    const data = tree('org#b#role');
    actions.DRAW_GRAPHPACK(app, session, payload(data, '', { 'org#a#role': 'org#b#role' }));
    gp.motionTimer.tick(200);
    const active = gp.motionTimer;
    const focused = gp.focusedNode;
    const viewport = gp.viewport;
    const fresh = data.map(n => ({ ...n, visibility: 'Secret', mode: 'Agile', n_open_tensions: 7 }));
    actions.DRAW_GRAPHPACK(app, session, payload(fresh));
    expect(gp.motionTimer).toBe(active);
    expect(gp.focusedNode).toBe(focused);
    expect(gp.viewport).toBe(viewport);
    expect(focused.data.n_open_tensions).toBe(7);
    expect(focused.data.visibility).toBe('Secret');
    active.tick(400);
    expect(geometry(focused)).toEqual(focused.target);
});

test('encoded focus preserves active layout during same-canvas metadata initialization and focus commands', () => {
    const { gp, app, session } = graph();
    const data = [...gp.dataNodes, circle('org#extra', parent('org'))];
    actions.DRAW_GRAPHPACK(app, session, payload(data));
    gp.motionTimer.tick(200);
    const active = gp.motionTimer;
    const nodes = gp.nodes;
    const viewport = gp.viewport;
    const focused = gp.focusedNode;
    const fresh = data.map(n => ({ ...n, n_open_tensions: 7 }));
    actions.INIT_GRAPHPACK(app, session, payload(fresh, 'org%23a%23role'));
    jest.advanceTimersByTime(150);
    actions.FOCUS_GRAPHPACK(app, session, 'org%23a%23role');
    expect(gp.motionTimer).toBe(active);
    expect(active.stop).not.toHaveBeenCalled();
    expect(gp.nodes).toBe(nodes);
    expect(gp.viewport).toBe(viewport);
    expect(gp.focusedNode).toBe(focused);
    expect(focused.data.n_open_tensions).toBe(7);
});

test('malformed percent-encoded focus safely falls back to the root', () => {
    const { gp, app, session } = graph(tree('org#a#role'), 'org%ZZ');
    expect(gp.focusedNode).toBe(gp.rootNode);
    reduced = true;
    gp.zoomToNode(gp.nodesDict['org#a#role']);
    expect(gp.focusedNode.data.nameid).toBe('org#a#role');
    actions.FOCUS_GRAPHPACK(app, session, 'org%ZZ');
    expect(gp.focusedNode).toBe(gp.rootNode);
});

test.each([{ data: [] }, { data: [circle('cycle', parent('cycle'))] }])('unusable redraw $data clears old data and cancels every canvas timer', ({ data }) => {
    const { gp, app, session, ctx } = graph();
    gp.zoomToNode('org');
    const stale = gp.motionTimer;
    actions.DRAW_BUTTONS_GRAPHPACK(app, session);
    actions.SAVE_SESSION_ITEM({ ports: { updateMenuTreeFromJs: { send: jest.fn() } } }, session, { key: 'tree_menu', val: {} });
    actions.DRAW_GRAPHPACK(app, session, payload(data));
    expect(gp.graph).toBeNull();
    expect(gp.nodesDict).toBeNull();
    expect(gp.exitingNodes).toEqual([]);
    expect(gp.handlers).toEqual([]);
    expect(gp.motionTimer).toBeNull();
    expect(gp.resizeTimer).toBeNull();
    expect(gp.buttonsTimer).toBeNull();
    ctx.arc.mockClear();
    stale.tick(400);
    jest.advanceTimersByTime(500);
    expect(ctx.arc).not.toHaveBeenCalled();
});

test('a disconnected snapshot without the old focus selects a current root rather than retaining old nodes', () => {
    const { gp, app, session } = graph();
    actions.DRAW_GRAPHPACK(app, session, payload([circle('org#public', parent('org#secret')), circle('org#other', null)]));
    expect(gp.focusedNode.data.nameid).toBe('org#public');
    gp.motionTimer.tick(400);
    expect(Object.keys(gp.nodesDict)).toEqual(['org#public']);
    expect(gp.exitingNodes).toEqual([]);
});

test('pending focus takes precedence over the old canvas when a newer redraw cancels initialization', () => {
    const { gp, app, session } = graph(tree('org#a#role'), 'org');
    actions.INIT_GRAPHPACK(app, session, payload(tree('org#b#role'), 'org'));
    actions.FOCUS_GRAPHPACK(app, session, 'org#b#role');
    expect(gp.focusedNode.data.nameid).toBe('org');
    actions.DRAW_GRAPHPACK(app, session, payload(tree('org#b#role'), '', { 'org#a#role': 'org#b#role' }));
    const active = gp.motionTimer;
    jest.advanceTimersByTime(150);
    expect(gp.focusedNode.data.nameid).toBe('org#b#role');
    expect(gp.motionTimer).toBe(active);
});

test('a different-org loading placeholder clears the retained canvas even if loading subsequently fails', () => {
    const { gp, app, session } = graph();
    gp.zoomToNode('org');
    const active = gp.motionTimer;
    actions.INIT_GRAPHPACK(app, session, payload([], 'other#circle'));
    expect(active.stop).toHaveBeenCalled();
    expect(gp.graph).toBeNull();
    expect(gp.handlers).toEqual([]);
});

test('saving user context recolors after storage is updated, independent of Elm batch ordering', () => {
    const { gp, app, session } = graph();
    app.ports.loadUserCtxFromJs = app.ports.reloadNotifFromJs = { send: jest.fn() };
    actions.FLUSH_GRAPHPACK(app, session);
    actions.SAVE_USERCTX(app, session, { data: { username: 'alice' } });
    expect(gp.uctx.username).toBe('alice');
});
