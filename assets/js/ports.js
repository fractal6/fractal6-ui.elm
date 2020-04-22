
// On load, listen to Elm!
window.addEventListener('load', _ => {
    window.ports = {
        init: (app) =>
        app.ports.outgoing.subscribe(({ action, data }) =>
            actions[action]
            ? actions[action](app, data)
            : console.warn(`I didn't recognize action "${action}".`)
        )
    }
})

// maps actions to functions!
const actions = {
    'LOG': (app, message) => {
        console.log(`From Elm:`, message)
    },
    'BULMA': (app, message) => {
        console.log(`Activate Bulma driver...`);
        // This timeout is needed when bulma driver is called by elm Cmd,
        // to wait foe the Html Msg to be updated by elm in order
        // to have new node accessible by Javascript.
        setTimeout(BulmaDriver, 750);
    },
    'TOGGLE_TH': (app, message) => {
        document.getElementById("themeButton_port").addEventListener("click", function(){
            toggleTheme();
        });
    },
    'INIT_CIRCLEPACKING': (app, message) => {
        var data = JSON.parse(message);
        //window.addEventListener('DOMContentReady',function(){
        var $canvas = document.getElementById("canvasOrga");
        if (!$canvas) {
            drawAll(app, data);

            // On Resize handle
            var rtime;
            var timeout = false;
            var delta = 200;
            window.onresize = function () {
                rtime = new Date();
                if (timeout === false) {
                    timeout = true;
                    setTimeout(resizeend, delta);
                }
            };

            function resizeend() {
                if (new Date() - rtime < delta) {
                    setTimeout(resizeend, delta);
                } else {
                    timeout = false;
                    clearAll();
                    drawAll(app, data);
                }
            }
        }

        //});
    },
}
