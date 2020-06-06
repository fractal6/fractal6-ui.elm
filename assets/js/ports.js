
// On load, listen to Elm!
window.addEventListener('load', _ => {
    window.ports = {
        init: (app) => {

            // Ephemere Objects
            var session = {
                user_ctx: null, // from localstorage
                bulmaHandlers: [],
                // Graphpack
                gp: Object.create(GraphPack),
                // QuickSearch
                qs: new MiniSearch({
                    fields: ['name', 'first_link'],
                    storeFields: ['nameid'],
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
        // This timeout is needed when bulma driver is called by elm Cmd,
        // to wait foe the Html Msg to be updated by elm in order
        // to have new node accessible by Javascript.
        var handlers = session.bulmaHandlers;
        setTimeout(BulmaDriver, 300, app, eltId, handlers);
    },
    'TOGGLE_TH': (app, session, message) => {
        $tt = document.getElementById("themeButton_port");
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
        //window.addEventListener('DOMContentReady',function(){
        var $canvas = document.getElementById("canvasOrga");
        if (!$canvas) {
            var gp = session.gp;
            //var data = JSON.parse(data);

            var err = gp.init(app, data);
            if (err) {
                gp.zoomToNode(data.focusid, 0.5);
            }
        }
        //});

        var qs = session.qs;
        qs.removeAll();
        qs.addAll(data.data);
    },
    'FOCUS_GRAPHPACK': (app, session, focusid) => {
        var gp = session.gp;
        gp.zoomToNode(focusid);
    },
    'CLEAR_TOOLTIP': (app, session, message) => {
        var gp = session.gp;
        gp.clearNodeTooltip();
    },
    'DEBUG_CANVAS' : (app, session, user_ctx_key) => {
        var gp = session.gp;
        gp.replaceButtons();
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
