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
    // --
    // User session
    var uctx = JSON.parse(localStorage.getItem("user_ctx"));
    // Window pos
    var window_pos = JSON.parse(localStorage.getItem("window_pos"));
    // Menu left
    var menu_left = JSON.parse(localStorage.getItem("menu_left"));
    // Theme
    var theme = localStorage.getItem("theme");
    if (theme) document.documentElement.className = theme;
    else document.documentElement.className = "dark";
    // Lang
    // ...

    // Setup the layout
    if (menu_left == true) {
        setTimeout(() => {
            var $o = document.getElementById("body");
            if ($o) {
                $o.classList.add('has-menu-left');
            }
        }, 333);
    }

    // Init Elm
    // --
    window.ports.init(
        App.Elm.Main.init({
            node: document.getElementById('main'),
            flags: {
                uctx: uctx,
                window_pos: window_pos,
                menu_left: menu_left,
                apis: {
                    auth: AUTH_API,
                    gql: GRAPHQL_API,
                    rest: REST_API,
                    doc: DOC_API,
                    version: VERSION
                },
                screen: { w: window.innerWidth, h: window.innerHeight }
            }
        })
    );

});
