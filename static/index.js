// pull in desired CSS/SASS files
require( '../assets/sass/dark.scss' );
//require( '../assets/sass/light.scss' );

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
    var uctx = JSON.parse(localStorage.getItem("user_ctx"));
    window.ports.init(App.Elm.Main.init({
        node: document.getElementById('main'),
        flags: {
            uctx: uctx,
            apis: {
                gql: GRAPHQL_API,
                rest: REST_API,
                auth: AUTH_API
            }
        }
    }));
});
