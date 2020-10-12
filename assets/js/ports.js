import MiniSearch from 'minisearch'
import { BulmaDriver, InitBulma, catchEsc } from './bulma_drivers'
import { GraphPack } from './graphpack_d3'
import { sleep } from './custom.js'

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
                user_ctx: null, // from localstorage
                bulmaHandlers: [],
                // Graphpack
                gp: Object.create(GraphPack),
                // QuickSearch
                qsn: new MiniSearch({
                    idField: 'nameid',
                    storeFields: ['nameid'],
                    fields: ['nameid', 'name', 'first_link'],
                    searchOptions: {
                        fuzzy: 0.2,
                        boost: { name: 2 },
                    },
                }),
                qsu: new MiniSearch({
                    idField: 'username',
                    storeFields: ['username'],
                    fields: ['username', 'name'],
                    searchOptions: { fuzzy: 0.2, },
                })
            };

            // Suscribe to Elm outgoing ports
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
const actions = {
    //
    // Utils
    //
    'LOG': (app, session, message) => {
        console.log(`From Elm:`, message);
    },
    'BULMA': (app, session, eltId) => {
        InitBulma(app, session, eltId);
    },
    'TOGGLE_TH': (app, session, message) => {
        var $tt = document.getElementById("themeButton_port");
        if ($tt) {
            $tt.addEventListener("click", function(){
                toggleTheme();
            });
        }
    },
    //
    // Modal
    //
    'OPEN_MODAL': (app, session, message) => {
        document.documentElement.classList.add('has-modal-active');
        document.getElementById("navbarTop").classList.add('has-modal-active');
    },
    'CLOSE_MODAL': (app, session, message) => {
        document.documentElement.classList.remove('has-modal-active');
        document.getElementById("navbarTop").classList.remove('has-modal-active');
        InitBulma(app, session, "")
    },
    'OPEN_AUTH_MODAL': (app, session, message) => {
        document.documentElement.classList.add('has-modal-active2');
        document.getElementById("navbarTop").classList.add('has-modal-active2');
    },
    'CLOSE_AUTH_MODAL': (app, session, message) => {
        document.documentElement.classList.remove('has-modal-active2');
        document.getElementById("navbarTop").classList.remove('has-modal-active2');
        InitBulma(app, session, "")
    },
    //
    // Quick Search
    //
    'INIT_USERSEARCH': (app, session, data) => {
        // Setup User quickSearch
        initQuickSearch(session.qsu, data);
    },
    'ADD_QUICKSEARCH_NODES': (app, session, nodes) => {
        session.qsn.addAll(nodes);
    },
    'ADD_QUICKSEARCH_USERS': (app, session, users) => {
        session.qsu.addAll(users);
    },
    'SEARCH_NODES': (app, session, pattern) => {
        var qs = session.qsn;
        var nodes = session.gp.nodesDict;
        var res = qs.search(pattern, {prefix:true}).slice(0,10).map(n => {
            var d = nodes[n.nameid].data;
            return d
            //return {
            //    ...d,
            //    firstLink: (d.first_link)? d.first_link.username : ""
        //}
        });
        app.ports.lookupNodeFromJs_.send(res);
    },
    'SEARCH_USERS': (app, session, pattern) => {
        var qs = session.qsu;
        var res = qs.search(pattern, {prefix:true}).slice(0,10);
        app.ports.lookupUserFromJs_.send(res);
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
                gp.zoomToNode(data.focusid, 0.5);
                session.isInit = false;
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
    'CLEAR_TOOLTIP': (app, session, message) => {
        var $canvas = document.getElementById("canvasOrga");
        if ($canvas) {
            var gp = session.gp;
            gp.clearNodeTooltip();
        }
    },
    'DRAW_GRAPHPACK' : (app, session, data) => {
        var $canvas = document.getElementById("canvasOrga");
        if ($canvas) {
            var gp = session.gp;
            gp.resetGraphPack(data.data, true, gp.focusedNode.data.nameid);
            gp.drawCanvas();
            gp.drawCanvas(true);
        }
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
                gp.drawButtons()}, 300);
        }
    },
    //
    // User Ctx -- Localstorage
    //
    'SAVE_USERCTX' : (app, session, user_ctx) => {
        // @DEBUG: Maybe List encoder ?
        if (user_ctx.roles && user_ctx.roles.length == 0) delete user_ctx.roles
        localStorage.setItem(user_ctx.key, JSON.stringify(user_ctx.data));
    },
    'LOAD_USERCTX' : (app, session, user_ctx_key) => {
        app.ports.loadUserCtxFromJs.send(JSON.parse(localStorage.getItem(user_ctx_key)));
    },
    'REMOVE_USERCTX' : (app, session, user_ctx_key) => {
        localStorage.removeItem(user_ctx_key);
        document.cookie = "jwt=; expires=Thu, 01 Jan 1970 00:00:01 GMT; Path=/";
        app.ports.loggedOutOkFromJs.send(null);
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
        }, 50);
    },
    'CLICK': (app, session, target) => {
        document.getElementById(target).click();
    },
}
