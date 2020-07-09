import MiniSearch from 'minisearch'
import { BulmaDriver } from './bulma_drivers'
import { GraphPack } from './graphpack_d3'


// On load, listen to Elm!
window.addEventListener('load', _ => {
    window.ports = {
        init: (app) => {
            // Ephemere Objects
            var session = {
                isInit: true,
                user_ctx: null, // from localstorage
                bulmaHandlers: [],
                // Graphpack
                gp: Object.create(GraphPack),
                // QuickSearch
                qs: new MiniSearch({
                    idField: 'nameid',
                    storeFields: ['nameid'],
                    fields: ['name', 'first_link'],
                    searchOptions: {
                        fuzzy: 0.2,
                        boost: { name: 2 },
                    }
                })
            };
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
        console.log(`From Elm:`, message)
    },
    'BULMA': (app, session, eltId) => {
        console.log(`Activate Bulma driver...`);
        document.getElementById("footBar").style.display= "none";
        setTimeout( function () {
            document.getElementById("footBar").style.display= "block";
        }, 0.5);

        // This timeout is needed when bulma driver is called by elm Cmd,
        // to wait foe the Html Msg to be updated by elm in order
        // to have new node accessible by Javascript.
        //document.addEventListener('DOMContentLoaded', () => {
        var handlers = session.bulmaHandlers;
        setTimeout(BulmaDriver, 300, app, eltId, handlers);
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
    },
    //
    // Quick Search
    //
    'SEARCH_NODES': (app, session, pattern) => {
        var qs = session.qs;
        var nodes = session.gp.nodesDict;
        var res = qs.search(pattern, {prefix:true}).slice(0,10).map(n => {
            var d = nodes[n.nameid].data;
            return d
            //return {
            //    ...d,
            //    firstLink: (d.first_link)? d.first_link.username : ""
        //}
        });
        app.ports.lookupFromJs.send(res);
    },
    //
    // GraphPack
    //
    'INIT_GRAPHPACK': (app, session, data) => {
        setTimeout(() => {
            var gp = session.gp;
            //var data = JSON.parse(data);

            var ok = gp.init(app, data, session.isInit);
            if (ok) {
                gp.zoomToNode(data.focusid, 1.5);
                session.isInit = false;
            }

            var qs = session.qs;
            qs.removeAll();
            qs.addAll(data.data);
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
            gp.resetGraphPack(data.data, true);
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
            gp.resetGraphPack(data.data, true);
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
}
