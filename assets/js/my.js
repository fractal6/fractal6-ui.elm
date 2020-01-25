//export default BulmaDriver;

/*
 *
 * Bulma javascript helpers
 *
 */

//document.addEventListener('DOMContentLoaded', () => {
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


/*
 *
 * Utils
 *
 */

function toggleTheme() {
    var is_dark = document
        .getElementById('cssTheme')
        .classList[0] === 'dark_theme';

    var element = document.getElementById('cssTheme');

    if (is_dark) {
        element.href = '/light.css';
        element.classList.remove('dark_theme')
        element.classList.add('light_theme');
    } else {
        element.href = '/dark.css';
        element.classList.remove('light_theme')
        element.classList.add('dark_theme');
    }
}

