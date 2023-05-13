// pull in desired CSS/SASS files
require( '../assets/sass/main.scss' );
// JS entry point
var Ports = require( '../assets/js/ports.js' )
// DragNDrop support
require('../assets/js/DragPorts.js')

// Inject bundled Elm app into div#main
var App = require( '../src/Main' );

window.addEventListener('load', _ => {

    // Local session
    // --
    // User session
    var uctx = JSON.parse(localStorage.getItem("user_ctx"));
    // Window pos
    var window_pos = JSON.parse(localStorage.getItem("window_pos"));
    // Menu
    var orga_menu = JSON.parse(localStorage.getItem("orga_menu"));
    var tree_menu = JSON.parse(localStorage.getItem("tree_menu"));
    // Theme
    var theme = localStorage.getItem("theme");
    if (theme) document.documentElement.className = theme;
    else document.documentElement.className = "dark";
    // Lang
    var lang = localStorage.getItem("lang");
    if (uctx && uctx.lang) {
       lang = uctx.lang;
    } else if (!lang) {
        lang = DEFAULT_LANG;
    }

    // Init Elm
    // --
    window.ports.init(
        App.Elm.Main.init({
            node: document.getElementById('main'),
            flags: {
                uctx: uctx,
                lang: lang,
                window_pos: window_pos,
                orga_menu: orga_menu,
                tree_menu: tree_menu,
                apis: {
                    auth: AUTH_API,
                    gql: GRAPHQL_API,
                    rest: REST_API,
                    assets: ASSETS_API,
                    version: VERSION,
                },
                screen: { w: window.innerWidth, h: window.innerHeight },
            }
        })
    );

    // Setup Actions
    // --
    // Setup the layout
    if (orga_menu == true) {
        setTimeout(Ports.actions["OPEN_ORGA_MENU"], 333);
    }
    if (tree_menu == true) {
        setTimeout(Ports.actions["OPEN_TREE_MENU"], 333);
    }

});
