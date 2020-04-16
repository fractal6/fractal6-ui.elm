
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
        BulmaDriver();
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
        if (!$canvas) drawAll(app, data);
        //});
    },
}
