// pull in desired CSS/SASS files
require( '../assets/sass/dark.scss' );

// pull in desired JS files
require( '../node_modules/d3/dist/d3.min.js' ); // d3.js
require( '../node_modules/minisearch/dist/umd/index.js' ); // minisearch.js
// Custom JS api
require( '../assets/js/bulma_drivers.js' )
require( '../assets/js/graphpack_d3.js' )
require( '../assets/js/ports.js' )

// inject bundled Elm app into div#main
//var Elm = require( '../elm/Main.elm' );
//Elm.Main.embed( document.getElementById( 'main' ) );
var App = require( '../src/Main' );
//Elm.Elm.Main.init({
//      node: document.getElementById("main")
//});
window.addEventListener('load', _ => {
    var uctx = JSON.parse(localStorage.getItem(localStorage.key(0)));
    window.ports.init(App.Elm.Main.init({
        node: document.getElementById('main'),
        flags: uctx
    }));
});
