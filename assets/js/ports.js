//import {BulmaDriver} from  './my.js';
function BulmaDriver() {

  // Get all "navbar-burger" elements
  const $navbarBurgers = Array.prototype.slice.call(document.querySelectorAll('.navbar-burger'), 0);

  // Check if there are any navbar burgers
  if ($navbarBurgers.length > 0) {

    // Add a click event on each of them
    $navbarBurgers.forEach( el => {
      el.addEventListener('click', () => {

        // Get the target from the "data-target" attribute
        const target = el.dataset.target;
        const $target = document.getElementById(target);

        // Toggle the "is-active" class on both the "navbar-burger" and the "navbar-menu"
        el.classList.toggle('is-active');
        $target.classList.toggle('is-active');

      });
    });
  }

}

// On load, listen to Elm!
window.addEventListener('load', _ => {
  window.ports = {
    init: (app) =>
      app.ports.outgoing.subscribe(({ action, data }) =>
        actions[action]
          ? actions[action](data)
          : console.warn(`I didn't recognize action "${action}".`)
      )
  }
})

// maps actions to functions!
const actions = {
  'LOG': (message) =>
    console.log(`From Elm:`, message),
  'BULMA': (message) => {
    console.log(`Activate Bulma driver:`);
    BulmaDriver();
  },
}
