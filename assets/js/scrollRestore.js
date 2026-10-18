/*
 * Board scroll restoration on browser back/forward (see docs/scroll-position-detection.md).
 *
 * Saves, per URL, the scroll of `.kb-board` elements (and their children) that have an `id`.
 * On popstate, re-applies it every frame for RESTORE_MS so it wins over the async board
 * render and auto-scrolls; any user input releases it.
 *
 * ponytail: keyed by URL (Elm pushes an empty history state), same URL twice shares one entry.
 * ponytail: board auto-scrolls fired after RESTORE_MS win; raise it if data loads slower.
 */

const MAX_URLS = 10;
const RESTORE_MS = 3000;
const STOP_EVENTS = ['wheel', 'touchstart', 'keydown', 'mousedown'];

export function initScrollRestore(session) {
    session.scrollPositions = new Map(); // href -> { id: [x, y] }, oldest first
    session.restoringScroll = false;
    session.stopRestore = null;

    const pending = new Map(); // scrolled element -> href at event time
    const save = () => {
        if (!session.restoringScroll) {
            pending.forEach((href, el) => {
                if (!el.isConnected) return;
                const pos = session.scrollPositions.get(href) || {};
                pos[el.id] = [el.scrollLeft, el.scrollTop];
                session.scrollPositions.delete(href); // move to the end (most recent)
                session.scrollPositions.set(href, pos);
            });
            while (session.scrollPositions.size > MAX_URLS) {
                session.scrollPositions.delete(session.scrollPositions.keys().next().value);
            }
        }
        pending.clear();
    };

    // Capture phase: element scroll events don't bubble.
    document.addEventListener('scroll', e => {
        const el = e.target;
        if (session.restoringScroll || !el.id || !el.closest || !el.closest('.kb-board')) return;
        if (pending.size === 0) requestAnimationFrame(save);
        pending.set(el, location.href);
    }, { capture: true, passive: true });

    window.addEventListener('popstate', () => {
        if (session.stopRestore) session.stopRestore();
        const saved = session.scrollPositions.get(location.href);
        if (!saved) return;

        const deadline = Date.now() + RESTORE_MS;
        const stop = () => {
            session.restoringScroll = false;
            session.stopRestore = null;
            STOP_EVENTS.forEach(ev => window.removeEventListener(ev, stop, true));
        };
        const tick = () => {
            if (session.stopRestore !== stop) return;
            if (Date.now() > deadline) return stop();
            for (const id in saved) {
                const [x, y] = saved[id];
                const el = document.getElementById(id);
                if (el && (Math.abs(el.scrollLeft - x) > 1 || Math.abs(el.scrollTop - y) > 1)) {
                    el.scrollLeft = x;
                    el.scrollTop = y;
                }
            }
            requestAnimationFrame(tick);
        };

        session.restoringScroll = true;
        session.stopRestore = stop;
        STOP_EVENTS.forEach(ev => window.addEventListener(ev, stop, { capture: true, passive: true }));
        requestAnimationFrame(tick);
    });
}
