/*
 * Fractale - Self-organisation for humans.
 * Copyright (C) 2022 Fractale Co
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
import { InitBulma, catchEsc, updateLang } from './bulma_drivers'
import { GraphPack } from './graphpack_d3'
import { sleep } from './custom.js'

/*
 *
 * Elm outgoing ports
 *
 */

// @TODO/future: user  {username} in key to support multiple session
export const UCTX_KEY = "user_ctx";

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
                // Graphpack
                gp: Object.create(GraphPack),

                /*** QuickSearch ***/
                // Node Quick Search
                qsn: new MiniSearch({
                    idField: 'nameid',
                    storeFields: ['nameid'],
                    fields: ['nameid', 'name', 'first_link'],
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
                    storeFields: ['id', 'name', 'color'],
                    fields: ['name'],
                    searchOptions: { fuzzy: 0.3, },
                }),
            };

            // Subscribe to Elm outgoing ports
            app.ports.outgoing.subscribe(({ action, data }) => {
                if (actions[action]) {
                    actions[action](app, session, data)
                } else {
                    console.warn(`I didn't recognize action "${action}".`)
                }
            });

        }
    }

})

// Elm outgoing Ports Actions.
// Maps actions to functions!
export const actions = {
    'BULMA': (app, session, id) => {
        InitBulma(app, session, id);

        // Unlock tooltip (GP)
        session.gp.isFrozen = false;

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
        var res = qs.search(data.pattern, {prefix:true}).slice(0,20);
        app.ports.lookupUserFromJs_.send(res);
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
        var nodes = session.gp.nodesDict;
        var res = qs.search(pattern, {prefix:true}).slice(0,20).map(n => {
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
        app.ports.lookupNodeFromJs_.send(res.filter(x => x));
    },
    'SEARCH_USERS': (app, session, pattern) => {
        var qs = session.qsu;
        var res = qs.search(pattern, {prefix:true}).slice(0,20);
        app.ports.lookupUserFromJs_.send(res);
    },
    'SEARCH_LABELS': (app, session, pattern) => {
        var qs = session.qsl;
        var res = qs.search(pattern, {prefix:true}).slice(0,20);
        app.ports.lookupLabelFromJs_.send(res);
    },
    //
    // GraphPack
    //
    'INIT_GRAPHPACK': (app, session, data) => {
        var gp = session.gp;

        // Loading empty canvas
        if (!data.data || data.data.length == 0 ) {
            gp.isLoading = true;
            gp.init_canvas()
            return
        }

        setTimeout(() => { // to wait that layout is ready
            // Setup Graphpack
            var ok = gp.init(app, data, session.isInit);
            if (ok) {
                session.isInit = false;
                gp.zoomToNode(data.focusid, 0.5);
            } else {
                gp.isLoading = true;
                gp.init_canvas()
            }

            // Setup Node quickSearch
            initQuickSearch(session.qsn, data.data);
        }, 150);
    },
    'FOCUS_GRAPHPACK': (app, session, focusid) => {
        var $canvas = document.getElementById("canvasOrga");
        if ($canvas) {
            var gp = session.gp;
            gp.zoomToNode(focusid);
        }
    },
    'FLUSH_GRAPHPACK': (app, session, focusid) => {
        var $canvas = document.getElementById("canvasOrga");
        if ($canvas) {
            var gp = session.gp;
            gp.computeCircleColorRange()
            gp.drawCanvas();
        }
    },
    'DRAW_GRAPHPACK' : (app, session, data) => {
        var $canvas = document.getElementById("canvasOrga");
        if ($canvas) {
            var gp = session.gp;
            var focusid = gp.focusedNode ? gp.focusedNode.data.nameid : null;
            gp.resetGraphPack(data.data, true, focusid);
            gp.drawCanvas();
            gp.drawCanvas(true);

            // Fix bad drawing... (observed when adding, moving or removing node)
            gp.resizeMe();
        } // else
        // Some hidden data here...
    },
    'REMOVEDRAW_GRAPHPACK' : (app, session, data) => {
        var $canvas = document.getElementById("canvasOrga");
        if ($canvas) {
            // Remove a node
            for (var i=0; i<data.data.length; i++) {
                if (data.data[i].nameid == data.focusid) {
                    data.data.splice(i, 1);
                    break
                }
            }

            var gp = session.gp;
            gp.resetGraphPack(data.data, true, gp.focusedNode.data.nameid);
            gp.drawCanvas();
            gp.drawCanvas(true);
        }
    },
    'DRAW_BUTTONS_GRAPHPACK' : (app, session, _) => {
        var $canvas = document.getElementById("canvasOrga");
        if ($canvas) {
            var gp = session.gp;
            setTimeout( () => {
                gp.drawButtons()}, 333);
        }
    },
    'CLEAR_TOOLTIP': (app, session, message) => {
        var $canvas = document.getElementById("canvasOrga");
        if ($canvas) {
            var gp = session.gp;
            gp.clearNodeTooltip();
        }
    },
    //
    // User Ctx -- Localstorage
    //
    'SAVE_USERCTX' : (app, session, user_ctx) => {
        // Save session
        localStorage.setItem(UCTX_KEY, JSON.stringify(user_ctx.data));

        // If version is outdated, reload.
        if (user_ctx.data.client_version != "" && VERSION != "" && user_ctx.data.client_version != VERSION) {
            // Prevent bad redirection because /new/orga send a navigate redirection with a timeout.
            // @debug: catch redirection here.
            if (window.location.pathname != "/new/orga") {
                window.location.reload(true);
            }
        }

        // Update Page/Components accordingly
        app.ports.loadUserCtxFromJs.send(user_ctx.data);
        app.ports.reloadNotifFromJs.send(null);
    },
    'SAVE_SESSION_ITEM' : (app, session, data) => {
        localStorage.setItem(data.key, JSON.stringify(data.val));
        if (data.key == "orga_menu") {
            app.ports.updateMenuOrgaFromJs.send(data.val);
        } else if (data.key == "tree_menu") {
            app.ports.updateMenuTreeFromJs.send(data.val);
        }
        setTimeout(() => session.gp.resizeMe(), 333);
    },
    'REMOVE_SESSION' : (app, session, _) => {
        // @TODO: make a list of item to delete instead !
        // see also static/index.js
        localStorage.removeItem(UCTX_KEY);
        localStorage.removeItem("window_pos");
        //localStorage.removeItem("theme");
        // --
        // Remove classes available only if logged-in.
        // i.e. tree_menu do not depend of a session.
        //localStorage.removeItem("tree_menu");
        localStorage.removeItem("orga_menu");
        var $t;
        $t = document.getElementById("body");
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

        // outside click listener
        const outsideClickListener = event => {
            if (event.target.closest("#"+id) === null) {
                // @debug; doesnt work with elm events!
                //event.stopPropagation();
                closeEvent();
            }
        }

        // Escape listener
        const escListener = event => {
            catchEsc(event, closeEvent);
        }

        // Remove the listener on close
        const removeClickListener = () => {
            document.removeEventListener('click', outsideClickListener);
            document.removeEventListener('keydown', escListener);
        }

        // add the listener
        setTimeout(() => {
            document.addEventListener('click', outsideClickListener);
            document.addEventListener('keydown', escListener);

            // add listenner to global handlers LUT to clean it on navigation
            var handlers = session.bulmaHandlers;
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
    'LOGERR': (app, session, message) => {
        console.warn(`Error from Elm:`, message);
    },
    'CLICK': (app, session, target) => {
        var elt = document.getElementById(target);
        if (elt) elt.click();
    },
    'FORCE_RELOAD': (app, session, target) => {
        window.location.reload(true);
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
        } else if (c == "MentionTension") {
            pushLine($input, "0x", true)
        } else {
            console.warn("Rich text command not found.")
            return
        }

        //app.ports[msg.toMsg].send($input.value);
        var e = new Event('input', {
                bubbles: true,
                cancelable: true,
        });
        $input.dispatchEvent(e);
    },
}

// Toggle simple markup on the line
function toggleMarkup(obj, mark, prefix, suffix) {
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
        var replacement = selection;

        /* Loose undo/redo stack
        * // Remove the replacer
        * obj.value = value.substring(0, start-pad) + replacement + value.substring(end+pad);
        */
        // Works...
        obj.setSelectionRange(start-pad, end+pad);
        obj.setRangeText("");
        // @deprecated...
        document.execCommand("insertText", false, replacement);

        // Put caret at right position again
        obj.selectionStart =
            obj.selectionEnd = start - pad;
    } else { // add
        var replacement = " ".repeat(space_left) + mark + selection + mark_r + " ".repeat(space_right);

        /* Loose undo/redo stack
        * // Set textarea value to: text before caret + replacer + text after caret
        * obj.value = value.substring(0, start) + replacement + value.substring(end);
        */
        // Create range (do not suport texarea range + not undo :/
        //var range = document.createRange();
        //console.log(start, end)
        ////range.deleteContents();
        ////range.selectNodeContents(obj);
        //range.setStart(obj, start);
        //range.setEnd(obj, end);
        //range.insertNode(document.createTextNode(replacement));
        //// Replace selection
        //var selection = window.getSelection();
        //selection.removeAllRanges();
        //selection.addRange(range);
        //
        // Works...
        obj.setRangeText("");
        // @deprecated...
        document.execCommand("insertText", false, prefix + replacement + suffix);

        // Put caret at right position again
        obj.selectionStart =
            obj.selectionEnd = end + pad + prefix.length;
    }
}

// PushLine add a markup with new lines before and eventually after.
// isInline: stay on the line when the line start by the same mark.
function pushLine(obj, mark, isInline) {
    var value = obj.value;
    var start = obj.selectionStart;
    var end = obj.selectionEnd;
    var selection = value.substring(start, end);

    // Ignore if cursor already start with mark
    var startsWith = value.substring(start-mark.length, start) == mark ||
        value.substring(start, start+mark.length) == mark;
    if (startsWith) return

    // Ignore if multiple line selected
    var newline_count = (selection.match(/\n/g) || []).length;
    if (newline_count > 0) return

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
    if (prev2.search(/(^$|^\n|\n\n)/) >= 0 && next2.search(/^$|\n$|\n\n/) >= 0) {
        // Stay on the line if space and full line selected
        // --
        var replacement = mark + selection;
        obj.setRangeText("");
        // @deprecated...
        document.execCommand("insertText", false, replacement);
    } else {
        // Add new section
        // --
        // Adapt prefix andsuffix
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
        obj.setSelectionRange(nextLine, nextLine);
        obj.setRangeText("");
        // @deprecated...
        document.execCommand("insertText", false, prefix+replacement+suffix);

        // Put caret at right position again
        obj.selectionStart =
            obj.selectionEnd = nextLine + replacement.length + prefix.length;
    }
}

