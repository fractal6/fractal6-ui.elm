// pull in desired CSS/SASS files
require( '../assets/sass/main.scss' );
// JS entry point
require( '../assets/js/ports.js' )

// inject bundled Elm app into div#main
//var Elm = require( '../elm/Main.elm' );
//Elm.Main.embed( document.getElementById( 'main' ) );
var App = require( '../src/Main' );
//Elm.Elm.Main.init({
//      node: document.getElementById("main")
//});

window.addEventListener('load', _ => {

    // Local session
    var uctx = JSON.parse(localStorage.getItem("user_ctx"));
    var window_pos = JSON.parse(localStorage.getItem("window_pos"));
    var theme = localStorage.getItem("theme");
    if (theme) document.documentElement.className = theme;
    else document.documentElement.className = "dark";

    // Init Elm
    window.ports.init(App.Elm.Main.init({
        node: document.getElementById('main'),
        flags: {
            uctx: uctx,
            window_pos: window_pos,
            apis: {
                auth: AUTH_API,
                gql: GRAPHQL_API,
                rest: REST_API,
                doc: DOC_API,
                version: VERSION
            },
            screen: { w: window.innerWidth, h: window.innerHeight }
        }
    }));

});
