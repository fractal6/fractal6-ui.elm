
// On load, listen to Elm!
window.addEventListener('load', _ => {
    window.ports = {
        init: (app) => {
            // Session Objects
            var session = {
                gp : Object.create(GraphPack)
            };

            app.ports.outgoing.subscribe(({ action, data }) =>
                actions[action]
                ? actions[action](app, session,  data)
                : console.warn(`I didn't recognize action "${action}".`)
            )

        }
    }
})

// maps actions to functions!
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
        document.getElementById("themeButton_port").addEventListener("click", function(){
            toggleTheme();
        });
    },
    'INIT_GRAPHPACK': (app, session, data) => {
        //window.addEventListener('DOMContentReady',function(){
        var $canvas = document.getElementById("canvasOrga");
        if (!$canvas) {
            var gp = session.gp;
            var data = JSON.parse(data);

            gp.init(app, data);
            gp.zoomToNode(data.focusid, 0.5);
        }
        //});
    },
    'FOCUS_GRAPHPACK': (app, session, focusid) => {
        var gp = session.gp;
        gp.zoomToNode(focusid);
    },
}
