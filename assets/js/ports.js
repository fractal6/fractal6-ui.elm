
// On load, listen to Elm!
window.addEventListener('load', _ => {
    window.ports = {
        init: (app) => {

            // Ephemere Objects
            var session = {
                gp: Object.create(GraphPack),
                user_ctx: null // from localstorage
            };

            app.ports.outgoing.subscribe(({ action, data }) =>
                actions[action]
                ? actions[action](app, session, data)
                : console.warn(`I didn't recognize action "${action}".`)
            )

        }
    }
})

// Elm outgoing Ports Actions.
// Maps actions to functions!
const actions = {
    'LOG': (app, session, message) => {
        console.log(`From Elm:`, message)
    },
    'BULMA': (app, session, eltId) => {
        console.log(`Activate Bulma driver...`);
        // This timeout is needed when bulma driver is called by elm Cmd,
        // to wait foe the Html Msg to be updated by elm in order
        // to have new node accessible by Javascript.
        setTimeout(BulmaDriver, 300, eltId);
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
    // GraphPack
    //
    'INIT_GRAPHPACK': (app, session, data) => {
        //window.addEventListener('DOMContentReady',function(){
        var $canvas = document.getElementById("canvasOrga");
        if (!$canvas) {
            var gp = session.gp;
            //var data = JSON.parse(data);

            gp.init(app, data);
            gp.zoomToNode(data.focusid, 0.5);
        }
        //});
    },
    'FOCUS_GRAPHPACK': (app, session, focusid) => {
        var gp = session.gp;
        gp.zoomToNode(focusid);
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
        app.ports.loadUserCtx.send(JSON.parse(localStorage.getItem(user_ctx_key)));
    },
    'REMOVE_USERCTX' : (app, session, user_ctx_key) => {
        localStorage.removeItem(user_ctx_key);
    },
}
