/*
 * Fractale - Self-organisation for humans.
 * Copyright (C) 2026 Fractale Co
 *
 * This file is part of Fractale.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
 */

import MiniSearch from 'minisearch'
import { InitBulma, catchEsc, getThemePref, updateLang, showSearchInput, hideSearchInput, showEmojiInput, hideEmojiInput } from './bulma_drivers'
import { replaceRange } from './textutils'
import { GraphPack } from './graphpack_d3'
import { sleep } from './custom.js'

// Drag&Drop support
// @DEBUG: how to import ?
//import { DragPorts } from './DragPorts.js'


/*
 *
 * Elm outgoing ports
 *
 */

// @TODO/future: user  {username} in key to support multiple session
export const UCTX_KEY = "user_ctx";

// Items that will be removed from localstorage when sign-out.
export const VOLATILE_SESSION_ITEMS = [
    UCTX_KEY,
    "window_pos",
    "welcome_cards",
    "recent_activity_tab",
    "orga_menu",
    // "tree_menu",
    // "lang",
    // "theme",
];

function initQuickSearch(qs, data) {
    qs.removeAll();
    qs.addAll(data);
}

// On load, listen to Elm!
window.addEventListener('load', _ => {
    window.ports = {
        init: (app) => {
            // Show the footbar
            //document.getElementById("footBar").style.display= "none";
            //setTimeout( function () {
            //    document.getElementById("footBar").style.display= "block";
            //}, 0.5);

            // Session Object
            var session = {
                isInit: true,
                bulmaHandlers: [],
                // Resizing
                rtime: null,
                timeout: false,
                delta: 200,
                // Scroll position tracking
                scrollTicking: false,
                lastScrollPosition: "top",
                // Graphpack
                gp: Object.create(GraphPack),

                /*** QuickSearch ***/
                // Node Quick Search
                qsn: new MiniSearch({
                    idField: 'nameid',
                    fields: ['nameid', 'name', 'first_link'],
                    storeFields: ['nameid'],
                    searchOptions: {
                        fuzzy: 0.3,
                        boost: { name: 2 },
                    },
                }),
                // User Quick Search
                qsu: new MiniSearch({
                    idField: 'username',
                    storeFields: ['username', 'name'],
                    fields: ['username', 'name'],
                    searchOptions: { fuzzy: 0.3, },
                }),
                // Label Quick Search
                qsl: new MiniSearch({
                    idField: 'id',
                    fields: ['name'],
                    storeFields: ['id', 'name', 'color'],
                    searchOptions: { fuzzy: 0.3 },
                }),
                // Orga Quick Search
                qso: new MiniSearch({
                    idField: 'nameid',
                    fields: ['name', 'nameid'],
                    storeFields: ['nameid'],
                    searchOptions: { fuzzy: 0.3, boost: { name: 2 } },
                }),
            };

            //
            // OUTGOING PORTS
            // --
            // Subscribe to Elm outgoing ports
            //
            app.ports.outgoing.subscribe(({ action, data }) => {
                if (actions[action]) {
                    actions[action](app, session, data)
                } else {
                    console.warn(`I didn't recognize action "${action}".`)
                }
            });

            // setup the dragstart and dragover ports subscriptions.
            //DragPorts.setup( app );

            // In "system" mode the stylesheet follows the OS on its own, but the graphpack
            // canvas samples the css variables at draw time and needs a redraw.
            window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', _ => {
                if (getThemePref() === "system") app.ports.flushGraphPackFromJs.send(null);
            });

            // Paste-capture: any element with [data-paste-capture] forwards
            // file items from the clipboard to Elm. The element id (if any)
            // is sent so the receiver can disambiguate multiple editors.
            document.addEventListener('paste', function (e) {
                var t = e.target;
                if (!t || typeof t.matches !== 'function') return;
                if (!t.matches('[data-paste-capture]')) return;
                var cd = e.clipboardData;
                if (!cd) return;
                var items = cd.items || [];
                var files = [];
                var objectUrls = [];
                // Stamp is shared within a single paste event; the per-file
                // index keeps names unique. Date.now() is live (unlike a stale
                // model.session.now relayed through Elm), so two paste events
                // ≥1ms apart never collide.
                var stamp = Date.now();
                for (var i = 0; i < items.length; i++) {
                    if (items[i].kind === 'file') {
                        var f = items[i].getAsFile();
                        if (f) {
                            // Pick an extension from the original name first,
                            // then fall back to the MIME subtype. The backend
                            // sniffs the actual MIME server-side; we just need
                            // a stable suffix.
                            var ext = '';
                            var origName = f.name || '';
                            var dot = origName.lastIndexOf('.');
                            if (dot > -1) {
                                ext = origName.substring(dot);
                            } else if (f.type && f.type.indexOf('/') > -1) {
                                ext = '.' + f.type.split('/')[1].toLowerCase();
                            }
                            var fname = 'paste-' + stamp + '-' + i + ext;
                            // Rebuild the File so the multipart upload sends
                            // OUR filename — the backend's rewrite logic looks
                            // up `![](<filename>)` placeholders by it.
                            var renamed = new File([f], fname, {
                                type: f.type,
                                lastModified: f.lastModified || stamp,
                            });
                            files.push(renamed);
                            objectUrls.push(URL.createObjectURL(renamed));
                        }
                    }
                }
                if (files.length === 0) return;
                e.preventDefault();
                app.ports.pastedFilesFromJs.send({
                    targetId: t.id || '',
                    files: files,
                    objectUrls: objectUrls,
                });
            });

            // Scroll position detection with throttling
            window.addEventListener('scroll', function() {
                if (!session.scrollTicking) {
                    window.requestAnimationFrame(function() {
                        var scrollY = window.scrollY;
                        var windowHeight = window.innerHeight;
                        var documentHeight = document.documentElement.scrollHeight;

                        var position;
                        // It seems that in modern browser, we have ~100 px per wheel notch
                        if (scrollY <= 142) {
                            position = "top";
                        } else if (scrollY + windowHeight >= documentHeight - 5) {
                            position = "bottom";
                        } else {
                            position = "middle";
                        }

                        // Only send if position changed (reduce Elm updates)
                        if (position !== session.lastScrollPosition) {
                            session.lastScrollPosition = position;
                            app.ports.scrollPositionFromJs.send(position);
                        }
                        session.scrollTicking = false;
                    });
                    session.scrollTicking = true;
                }
            }, { passive: true });

        }
    }
})

// Elm outgoing Ports Actions.
// Maps actions to functions!
export const actions = {
    'BULMA': (app, session, id) => {
        InitBulma(app, session, id);

        // Check if jwt token has expired
        var uctx = JSON.parse(localStorage.getItem(UCTX_KEY))
        if (uctx !== null && (uctx.expiresAt === undefined || new Date(uctx.expiresAt) < new Date())) {
            // refresh session
            app.ports.openAuthModalFromJs.send({uctx:uctx, refresh:true});
        }
    },

    //
    // Modal
    //
    'OPEN_MODAL': (app, session, modalid) => {
        document.documentElement.classList.add('has-modal-active');
        document.getElementById("navbarTop").classList.add('has-modal-active');
        InitBulma(app, session, modalid)
    },
    'CLOSE_MODAL': (app, session, _) => {
        document.documentElement.classList.remove('has-modal-active');
        document.getElementById("navbarTop").classList.remove('has-modal-active');
        InitBulma(app, session, "")
    },
    'OPEN_AUTH_MODAL': (app, session, message) => {
        document.documentElement.classList.add('has-modal-active2');
        document.getElementById("navbarTop").classList.add('has-modal-active2');
        //setTimeout(() => {
        //    // @DEBUG: the class does not persist without the setTimeout. Why ?
        //    document.documentElement.classList.add('has-modal-active2');
        //    document.getElementById("navbarTop").classList.add('has-modal-active2');
        //}, 50);
    },
    'CLOSE_AUTH_MODAL': (app, session, message) => {
        document.documentElement.classList.remove('has-modal-active2');
        document.getElementById("navbarTop").classList.remove('has-modal-active2');
        InitBulma(app, session, "")
    },

    'RAISE_AUTH_MODAL': (app, session, uctx) => {
        app.ports.openAuthModalFromJs.send({uctx:uctx});
    },

    'RAISE_AUTH_NEEDED': (app, session, _) => {
        app.ports.openAuthNeededFromJs.send(null);
    },

    //
    // Quick Search
    //
    'INIT_USERSEARCH': (app, session, data) => {
        // Setup User quickSearch
        initQuickSearch(session.qsu, data);
    },
    'INIT_USERSEARCHSEEK': (app, session, data) => {
        // Setup and search
        var qs = session.qsu;
        // Setup User quickSearch
        initQuickSearch(qs, data.users);
        // And return a search result
        var res = qs.search(data.pattern, {prefix:true}).slice(0,11);
        app.ports.lookupUserFromJs.send(res);
    },
    'INIT_LABELSEARCH': (app, session, data) => {
        // Setup User quickSearch
        initQuickSearch(session.qsl, data);
    },
    'ADD_QUICKSEARCH_NODES': (app, session, nodes) => {
        session.qsn.addAll(nodes);
    },
    'ADD_QUICKSEARCH_USERS': (app, session, users) => {
        session.qsu.addAll(users);
    },
    'REMOVE_QUICKSEARCH_NODES': (app, session, nodes) => {
        session.qsn.removeAll(nodes);
    },
    'REMOVE_QUICKSEARCH_USERS': (app, session, users) => {
        session.qsu.removeAll(users);
    },
    'SEARCH_NODES': (app, session, pattern) => {
        var qs = session.qsn;
        var nodes = session.gp.nodesDict || {};
        var res = qs.search(pattern, {prefix:true}).slice(0,11).map(n => {
            // Ignore Filtered Node (Owner, Member, etc)
            if (nodes[n.nameid]) {
                return nodes[n.nameid].data;
                //return {
                //    ...data,
                //    firstLink: (d.first_link)? data.first_link.username : "" }
            } else {
                return undefined
            }
        });
        app.ports.lookupNodeFromJs.send(res.filter(x => x));
    },
    'SEARCH_USERS': (app, session, pattern) => {
        var qs = session.qsu;
        var res = qs.search(pattern, {prefix:true}).slice(0,11);
        app.ports.lookupUserFromJs.send(res);
    },
    'SEARCH_LABELS': (app, session, pattern) => {
        var qs = session.qsl;
        var res = qs.search(pattern, {prefix:true}).slice(0,11);
        app.ports.lookupLabelFromJs.send(res);
    },
    'INIT_ORGASEARCH': (app, session, data) => {
        initQuickSearch(session.qso, data);
    },
    'SEARCH_ORGAS': (app, session, pattern) => {
        var qs = session.qso;
        var res = qs.search(pattern, {prefix:true}).slice(0,11);
        app.ports.lookupOrgaFromJs.send(res);
    },
    'PUSH_INPUT_SELECTION': (app, session, name) => {
        var $i = document.activeElement;
        var start = $i.selectionStart;
        var end = $i.selectionEnd;
        if (!$i || !start) return

        // Push the new value
        var pattern = "";
        var m = $i.value.slice(Math.max(0, start-50), start).match(/@[\w-\.]*$/);
        if (m) {
            pattern = m[m.length -1].slice(1);
            start -= pattern.length;
            end +=  pattern.length;
        }
        var replacer = name + " ";
        replaceRange($i, start, end, replacer, start + replacer.length);

        // Remove the search input
        const userTooltip = document.getElementById($i.id + "searchInput");
        hideSearchInput(userTooltip, app);
    },
    'INSERT_AT_CARET': (app, session, data) => {
        // data: { targetId: string, text: string }
        var $i = document.getElementById(data.targetId);
        if (!$i) return;
        var start = $i.selectionStart != null ? $i.selectionStart : $i.value.length;
        var end = $i.selectionEnd != null ? $i.selectionEnd : start;
        replaceRange($i, start, end, data.text, start + data.text.length);
    },
    'REVOKE_OBJECT_URL': (app, session, data) => {
        // data: string (blob: URL previously returned by URL.createObjectURL)
        if (typeof data === 'string' && data.indexOf('blob:') === 0) {
            try { URL.revokeObjectURL(data); } catch (_) { /* noop */ }
        }
    },
    'PUSH_EMOJI_SELECTION': (app, session, emoji) => {
        var $i = document.activeElement;
        var start = $i.selectionStart;
        var end = $i.selectionEnd;
        if (!$i || start == null) return

        // Find :pattern before cursor and replace with emoji
        var m = $i.value.slice(Math.max(0, start - 50), start).match(/:[\w-]*$/);
        if (m) {
            var matchLen = m[m.length - 1].length;
            start -= matchLen;
        }
        var replacer = emoji;
        replaceRange($i, start, end, replacer, start + replacer.length);

        // Remove the emoji input
        const emojiTooltip = document.getElementById($i.id + "emojiInput");
        hideEmojiInput(emojiTooltip, app);
    },

    //
    // GraphPack
    //
    'INIT_GRAPHPACK': (app, session, data) => {
        var gp = session.gp;

        if (gp.$canvas && !gp.isActive()) gp.dispose();
        var previousFocus = gp.pendingInit?.focusid || gp.rootNode?.data.nameid;
        if (data.focusid && previousFocus && data.focusid.split(/#|%23/i)[0] !== previousFocus.split(/#|%23/i)[0]) {
            gp.dispose();
            initQuickSearch(session.qsn, []);
            gp.init_canvas();
        }
        // Same-org loading placeholders must not erase a confirmed transition.
        if (!data.data || data.data.length === 0) {
            gp.init_canvas();
            return
        }

        clearTimeout(gp.initTimer);
        var canvas = document.getElementById(gp.canvasId);
        gp.pendingInit = data;
        gp.initTimer = setTimeout(() => {
            gp.initTimer = null;
            var snapshot = gp.pendingInit;
            gp.pendingInit = null;
            if (!snapshot || (canvas && canvas !== document.getElementById(gp.canvasId))) return
            if (gp.init(app, snapshot, session.isInit)) {
                session.isInit = false;
                initQuickSearch(session.qsn, snapshot.data);
            }
        }, 150);
    },
    'FOCUS_GRAPHPACK': (app, session, focusid) => {
        var gp = session.gp;
        if (gp.pendingInit) gp.pendingInit = { ...gp.pendingInit, focusid };
        else gp.zoomToNode(focusid);
    },
    'FLUSH_GRAPHPACK': (app, session, focusid) => {
        var gp = session.gp;
        if (gp.isActive()) {
            gp.uctx = JSON.parse(localStorage.getItem(UCTX_KEY));
            gp.computeCircleColorRange()
            gp.drawCanvas();
        }
    },
    'DRAW_GRAPHPACK' : (app, session, data) => {
        var gp = session.gp;
        var focusid = gp.normalizeFocusId(gp.pendingInit?.focusid || gp.focusedNode?.data.nameid || data.focusid);
        focusid = data.nodeRenames[focusid] || focusid;
        clearTimeout(gp.initTimer);
        gp.initTimer = gp.pendingInit = null;
        initQuickSearch(session.qsn, data.data);
        if (!gp.isActive() || !gp.graph) {
            actions.INIT_GRAPHPACK(app, session, { ...data, focusid });
            return
        }
        gp.resetGraphPack(data.data, focusid, data.nodeRenames);
    },
    'REMOVEDRAW_GRAPHPACK' : (app, session, data) => {
        actions.DRAW_GRAPHPACK(app, session, {
            ...data,
            data: data.data.filter(n => n.nameid !== data.focusid),
        });
    },
    'DRAW_BUTTONS_GRAPHPACK' : (app, session, _) => {
        var gp = session.gp;
        var canvas = gp.$canvas;
        clearTimeout(gp.buttonsTimer);
        gp.buttonsTimer = setTimeout(() => {
            gp.buttonsTimer = null;
            if (gp.isActive() && gp.graph && gp.$canvas === canvas) gp.drawButtons();
        }, 333);
    },
    'CLEAR_TOOLTIP': (app, session, message) => {
        var $canvas = document.getElementById("canvasOrga");
        if ($canvas) {
            var gp = session.gp;
            gp.clearNodeTooltip();
        }
    },
    'CLEAR_CONTEXT_MENU': (app, session, message) => {
        var $canvas = document.getElementById("canvasOrga");
        if ($canvas) {
            var gp = session.gp;
            gp.clearContextMenu();
        }
    },

    //
    // User Ctx -- Localstorage
    //
    'SAVE_USERCTX' : (app, session, user_ctx) => {
        // Save session
        localStorage.setItem(UCTX_KEY, JSON.stringify(user_ctx.data));

        // @deprecated: done in {getOrgaInfo} now
        // If version is outdated, reload.
        //if (user_ctx.data.client_version != "" && VERSION != "" && user_ctx.data.client_version != VERSION) {
        //    // Prevent bad redirection because /new/orga send a navigate redirection with a timeout.
        //    // @debug: catch redirection here.
        //    if (window.location.pathname != "/new/orga") {
        //        window.location.reload(true);
        //    }
        //}

        // Update Page/Components accordingly
        app.ports.loadUserCtxFromJs.send(user_ctx.data);
        app.ports.reloadNotifFromJs.send(null);
        actions.FLUSH_GRAPHPACK(app, session);
    },
    'SAVE_SESSION_ITEM' : (app, session, data) => {
        if (data.val == null) {
            localStorage.removeItem(data.key);
        } else {
            localStorage.setItem(data.key, JSON.stringify(data.val));
        }

        // Update Page/Components accordingly
        var resizePage = false;
        if (data.key == "orga_menu") {
            app.ports.updateMenuOrgaFromJs.send(data.val);
            resizePage = true;
        } else if (data.key == "tree_menu") {
            app.ports.updateMenuTreeFromJs.send(data.val);
            resizePage = true;
        }

        if (resizePage) {
            clearTimeout(session.gp.resizeTimer);
            session.gp.resizeTimer = setTimeout(() => session.gp.resizeMe(), 333);
        }
    },
    'SAVE_DRAFTS' : (app, session, data) => {
        localStorage.setItem('drafts', JSON.stringify(data));
    },
    'REMOVE_SESSION' : (app, session, _) => {
        // Remove volatile items
        for (var i=0; i<VOLATILE_SESSION_ITEMS.length; i++) {
            localStorage.removeItem(VOLATILE_SESSION_ITEMS[i]);
        }
        // Remove classes available only if logged-in.
        // i.e. tree_menu do not depend of a session.
        var $t = document.getElementById("body");
        if ($t) {
            $t.classList.remove('has-orga-menu');
        }
        // Won't work for httpOnly cookie
        document.cookie = "jwt=; expires=Thu, 01 Jan 1970 00:00:01 GMT; Path=/";
        app.ports.loggedOutOkFromJs.send(null);
    },
    'UPDATE_NOTIF' : (app, session, notif) => {
        app.ports.updateNotifFromJs.send(notif);
    },
    'PATH_CHANGED' : (app, session, _) => {
        app.ports.reloadPathFromJs.send(null);
    },
    'PROPAGATE_PATH' : (app, session, data) => {
        app.ports.propagatePathFromJs.send(data.data);
    },

    //
    // Popups
    //
    'INHERIT_WIDTH' : (app, session, target) => {
        const inheritWidth = () => {
            var $target = document.getElementById(target);
            if ($target) {
                $target.style.width = $target.parentNode.clientWidth + "px";
                return true
            }
            return false
        }
        sleep(10).then(() => {
            if (!inheritWidth()) {
                setTimeout(inheritWidth, 100);
            }
        });

    },
    'FOCUS_ON' : (app, session, target) => {
        setTimeout( () => {
            var $tt = document.getElementById(target);
            if ($tt) { $tt.focus(); }
        }, 100);
    },
    'OUTSIDE_CLICK_CLOSE' : (app, session, data) => {
        var id = data.target; // close the given target if a click occurs outside the div or if ESC is pressed
        var msg = data.msg; // automatically send the given msg to Elm

        // @debug: breaks the "close on click" event of burgers and dropdowns
        //InitBulma(app, session, id);

        const closeEvent = () => {
            app.ports[msg].send(null);
            removeClickListener();
        }

        // Track whether mousedown started inside the panel.
        // If so, ignore the subsequent click (e.g. text selection drag ending outside).
        let mouseDownInsideTarget = false;
        const mouseDownListener = event => {
            mouseDownInsideTarget = event.target.closest("#"+id) !== null;
        }

        // outside click listener
        const outsideClickListener = event => {
            if (mouseDownInsideTarget) {
                mouseDownInsideTarget = false;
                return;
            }
            if (event.target.closest("#"+id) === null) {
                // @debug; doesnt work with elm events!
                //event.stopPropagation();
                if (msg) closeEvent();
            }
        }

        // Escape listener
        const escListener = event => {
            catchEsc(event, closeEvent);
        }

        // Remove the listener on close
        const removeClickListener = () => {
            document.removeEventListener('mousedown', mouseDownListener);
            document.removeEventListener('click', outsideClickListener);
            document.removeEventListener('keydown', escListener);
        }

        // add the listener
        setTimeout(() => {
            document.addEventListener('mousedown', mouseDownListener);
            document.addEventListener('click', outsideClickListener);
            document.addEventListener('keydown', escListener);

            // add listenner to global handlers LUT to clean it on navigation
            var handlers = session.bulmaHandlers;
            handlers.push(["mousedown", mouseDownListener, document, mouseDownListener])
            handlers.push(["click", outsideClickListener, document, outsideClickListener])
            handlers.push(["keydown", escListener, document, escListener])

        }, 50);

    },

    //
    // Menus
    //
    'OPEN_ORGA_MENU': (app, session, _) => {
        var $t = document.getElementById("body");
        if ($t) {
            $t.classList.add('has-orga-menu');
        }
    },
    'CLOSE_ORGA_MENU': (app, session, _) => {
        var $t = document.getElementById("body");
        if ($t) {
            $t.classList.remove('has-orga-menu');
        }
    },
    'RELOAD_ORGA_MENU': (app, session, _) => {
        app.ports.reloadOrgaMenuFromJs.send(null);
    },
    'OPEN_TREE_MENU': (app, session, _) => {
        var $t = document.getElementById("body");
        if ($t) {
            $t.classList.add('has-tree-menu');
        }
        $t = document.getElementById("helperBar");
        if ($t) {
            $t.classList.add('has-tree-menu');
        }
        $t = document.getElementById("mainPane");
        if ($t) {
            $t.classList.add('has-tree-menu');
        }
    },
    'CLOSE_TREE_MENU': (app, session, _) => {
        var $t = document.getElementById("body");
        if ($t) {
            $t.classList.remove('has-tree-menu');
        }
        $t = document.getElementById("helperBar");
        if ($t) {
            $t.classList.remove('has-tree-menu');
        }
        $t = document.getElementById("mainPane");
        if ($t) {
            $t.classList.remove('has-tree-menu');
        }
    },
    'REQUIRE_TREE_DATA': (app, session, _) => {
        app.ports.requireTreeDataFromJs.send(null);
    },

    //
    // Utils
    //
    'LOG': (app, session, message) => {
        console.log(`From Elm:`, message);
    },
    'LOGERR': (app, session, message) => {
        console.warn(`Error from Elm:`, message);
    },
    'SHOW': (app, session, id) => {
        var $e = document.getElementById(id);
        if (!$e) { return }
        $e.style.display = "";
        //$e.style.visibility = "hidden";
    },
    'HIDE': (app, session, id) => {
        var $e = document.getElementById(id);
        if (!$e) { return }
        $e.style.display = "none";
        //$e.style.visibility = "hidden";
    },
    'OPENINNEWTAB': (app, session, url) => {
        window.open(url, '_blank');
    },
    'CLICK': (app, session, target) => {
        if (target == "") {
            target = "body";
        }
        var elt = document.getElementById(target);
        if (elt) elt.click();
    },
    'FORCE_RELOAD': (app, session, target) => {
        // Clear service worker caches and navigate with a cache-busting query param
        Promise.all([
            ('serviceWorker' in navigator)
                ? navigator.serviceWorker.getRegistrations().then(regs =>
                    Promise.all(regs.map(r => r.unregister()))
                )
                : Promise.resolve(),
            ('caches' in window)
                ? caches.keys().then(keys =>
                    Promise.all(keys.map(k => caches.delete(k)))
                )
                : Promise.resolve()
        ]).finally(() => {
            var url = new URL(window.location.href);
            url.searchParams.set('version', Date.now());
            window.location.href = url.toString();
        });
    },
    'RELOAD_LANG': (app, session, lang) => {
        updateLang(app, lang);
    },
    'FIT_HEIGHT': (app, session, id) => {
        var fitElement = id => {
            // SOlved with Browser.Dom.getElement
            var $e = document.getElementById(id);
            if (!$e) { return }

            var doc_h = document.body.scrollHeight;
            var screen_h = window.innerHeight;
            var elt_h = $e.offsetHeight; // $e.clientHeight -> smaller
            var x = doc_h - elt_h; // header height (above the target)
            var h = screen_h - x; // target size tha fit in screen

            if (doc_h > screen_h) {
                $e.style.height = h + "px";
            } else {
                var rect = $e.getBoundingClientRect();
                $e.style.height = elt_h + (screen_h - (rect.top+elt_h)) + "px";
            }
            //document.getElementsByTagName('html')[0].style.overflow = "hidden"; // @debug: html overflow stay disable...
            //document.body.style.overflowY = "hidden";

            //$e.style.maxHeight = 0.8*screen_h + "px";

            //console.log("document client:", document.body.clientHeight);
            //console.log("document scroll:", document.body.scrollHeight);
            //console.log("window inner:", window.innerHeight);
            //console.log("window outer:", window.outerHeight);
            //console.log("screen:", screen.height);
            //console.log("screen avail:", screen.availHeight);
            //console.log("elt client:", $e.clientHeight);
            //console.log("elt scrol:", $e.scrollHeight);
            //console.log("elt style:", $e.style.height);
            //console.log("elt top:", $e.offsetTop);
            //console.log("elt bottom:", $e.offsetTop + $e.clientHeight);
        }

        setTimeout(() => {
            fitElement(id);
        }, 333)
    },
    'RESET_SCROLL': (app, session, _) => {
        window.scroll(0, 0);
    },
    'SEND_IF_MOBILE': (app, session, msg) => {
        // Use case that motivate the use of availableWidth or window.innerWidth instead ?
        if (window.screen.width < 769) {
            setTimeout(() => {
                app.ports[msg].send(null);
            }, 500)
        }
    },
    'RICH_TEXT': (app, session, msg) => {
        // Markdown insert from the tool menu

        var target = msg.target;
        var c = msg.command;
        var $input = document.getElementById(target);
        if (!$input) return
        $input.focus();

        if (c == "Heading") {
            pushLine($input, "### ")
        } else if (c == "Bold") {
            toggleMarkup($input, "**")
        } else if (c == "Italic") {
            toggleMarkup($input, "_")
        } else if (c == "Strikethrough") {
            toggleMarkup($input, "~~")
        } else if (c == "Quote" ) {
            pushLine($input, "> ")
        } else if (c == "Link") {
            toggleMarkup($input, "[", " ", "()")
        } else if (c == "List-ul") {
            pushLine($input, "- ")
        } else if (c == "List-ol") {
            pushLine($input, "1. ")
        } else if (c == "List-check") {
            pushLine($input, "- [ ] ")
        } else if (c == "MentionUser") {
            pushLine($input, "@", true)
            showSearchInput($input, document.getElementById(target + "searchInput"), app);
        } else if (c == "MentionTension") {
            pushLine($input, "0x", true)
        } else if (c == "Details") {
            insertBlock($input,
                "<details>\n<summary>", "</summary>\n\n", "\n</details>")
        } else {
            console.warn("Rich text command not found.")
            return
        }

        // Immediately propagate change to Elm
        $input.dispatchEvent(new Event('input', {
            bubbles: true,
            cancelable: true,
        }));
    },
}

// Toggle simple markup on the line
export function toggleMarkup(obj, mark, prefix, suffix) {
    var value = obj.value;
    var start = obj.selectionStart;
    var end = obj.selectionEnd;
    var selection = value.substring(start, end);

	// Count leading spaces
    var space_left = selection.match(/^\s*/)[0].length
	// Count trailing spaces
    var space_right = selection.match(/\s*$/)[0].length
    // re-compute space
    start = obj.selectionStart + space_left;
    end = obj.selectionEnd - space_right;
    selection = value.substring(start, end);

    var mark_r
    if (mark == "[") {
        mark_r = "]"
    } else {
        mark_r = mark
    }
    if (!prefix) prefix = ""
    if (!suffix) suffix = ""
    if (value[start-1] == prefix) prefix = ""

    var pad = mark.length;
    var surrounding = value.substring(start-pad, start) + value.substring(end, end+pad);
    if (surrounding == mark+mark_r) { // remove
        replaceRange(obj, start - pad, end + pad, selection, start - pad);
    } else { // add
        var replacement = " ".repeat(space_left) + mark + selection + mark_r + " ".repeat(space_right);
        var fullText = prefix + replacement + suffix;
        var origStart = obj.selectionStart;
        var origEnd = obj.selectionEnd;

        // Caret depends on mark type and whether there was a selection
        var caret;
        if (mark == "[") {
            // Link with selection: cursor inside () to type URL → [text](|)
            // Link without selection: cursor inside [] → [|]()
            caret = selection.length > 0
                ? origStart + prefix.length + replacement.length + 1
                : origStart + prefix.length + mark.length;
        } else {
            // Bold/italic with selection: cursor after closing marker → **text**|
            // Bold/italic without selection: cursor inside delimiters → **|**
            caret = selection.length > 0
                ? origStart + fullText.length
                : origStart + prefix.length + mark.length;
        }
        replaceRange(obj, origStart, origEnd, fullText, caret);
    }
}

// PushLine add a markup with new lines before and eventually after.
// isInline: stay on the line when the line start by the same mark.
export function pushLine(obj, mark, isInline) {
    var value = obj.value;
    var start = obj.selectionStart;
    var end = obj.selectionEnd;
    var selection = value.substring(start, end);

    // Ignore if cursor already start with mark
    var startsWith = value.substring(start-mark.length, start) == mark ||
        value.substring(start, start+mark.length) == mark;
    if (startsWith) return

    // Multi lines selections
    var newline_count = (selection.match(/\n/g) || []).length;
    if (newline_count > 0) {
        // For multiline selections, add the mark to the beginning of each line
        let lines = selection.split('\n');
        let markedLines = lines.map(line => {
            // Skip empty lines or only add mark to non-empty lines
            return line.trim() ? mark + line : line;
        });
        replacement = markedLines.join('\n');

        replaceRange(obj, start, end, replacement);
        return
    }

    // Get surrounding line break
    var prefix = "\n\n";
    var suffix = "\n\n";
    var prevLine = value.substring(0, start).lastIndexOf("\n");
    if (prevLine < 0) {
        prevLine = 0
    }
    var nextLine = value.substring(end).search("\n");
    if (nextLine < 0) {
        nextLine = value.length;
        suffix = ""
    } else {
        nextLine += end
    }

    var prev2 = value.substring(start-2, start)
    var next2 = value.substring(end, end+2)

    // Check if the selection is surrounded by empty strings or newlines
    var isSurroundedByWhitespace = (
        prev2.search(/(^$|^\n|\n\n)/) >= 0 &&
        next2.search(/^$|\n$|\n\n/) >= 0
    );

    // Check if the selection is a full line
    var isFullLineSelection = (
        // Selection starts at beginning of line (either at position 0 or right after a newline)
        (start === 0 || value.charAt(start-1) === '\n') &&
        // Selection ends at end of line (either at end of text or right before a newline)
        (end === value.length || value.charAt(end) === '\n')
    );

    if (isSurroundedByWhitespace || isFullLineSelection) {
        // Stay on the line if space and full line selected
        var replacement = mark + selection;
        replaceRange(obj, start, end, replacement);
    } else {
        // Add new section
        // Adapt prefix and suffix
        var x = prevLine == 0 ? 0 : 1
        if (isInline && value.substring(prevLine+x, prevLine+x+mark.length) == mark) {
            if (value[start-1] != " ") prefix = " "
            else prefix = ""
            suffix = ""
        } else {
            prev2 = value.substring(nextLine-2, nextLine)
            next2 = value.substring(nextLine, nextLine+2)
            if (prev2 == "\n\n") prefix = ""
            else if (prev2 == "") prefix = ""
            else if (prev2[prev2.length-1] == "\n") prefix = "\n"
            if (next2 == "\n\n") suffix = ""
            else if (next2 == "") suffix = ""
            else if (next2[0] == "\n") suffix = "\n"
        }

        var replacement = mark + selection;
        var caret = nextLine + replacement.length + prefix.length;
        replaceRange(obj, nextLine, nextLine, prefix + replacement + suffix, caret);
    }
}

// Insert a block template (e.g. <details><summary>).
// prefix is inserted before the selection/cursor, middle after the selection,
// and suffix closes the block. Cursor is placed where the selection content goes.
export function insertBlock(obj, prefix, middle, suffix) {
    var value = obj.value;
    var start = obj.selectionStart;
    var end = obj.selectionEnd;
    var selection = value.substring(start, end);

    // Ensure block starts on its own line
    var before = "";
    if (start > 0 && value[start - 1] !== "\n") {
        before = value[start - 2] === "\n" ? "\n" : "\n\n";
    }

    // Ensure block ends before next content
    var after = "";
    if (end < value.length && value[end] !== "\n") {
        after = value[end + 1] === "\n" ? "\n" : "\n\n";
    }

    var replacement;
    var cursorPos;
    if (selection) {
        // Wrap selection: <details>\n<summary></summary>\n\nSELECTION\n</details>
        replacement = before + prefix + middle + selection + suffix + after;
        // Place cursor inside the summary tag
        cursorPos = start + before.length + prefix.length;
    } else {
        // No selection: insert full template with placeholder
        replacement = before + prefix + middle + suffix + after;
        // Place cursor after prefix (in the summary area)
        cursorPos = start + before.length + prefix.length;
    }

    replaceRange(obj, start, end, replacement, cursorPos);
}

