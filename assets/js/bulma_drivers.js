
/*
 *
 * Bulma javascript helpers
 *
 */

//document.addEventListener('DOMContentLoaded', () => {
const BulmaDriver = () => {

  /*
   * Burger open/close rationale
   */

	// Close all burger by removing `is-active` class.
	function closeBurgers(objs) {
		objs.forEach(function(el) {
      const $target = document.getElementById(el.dataset.target)
			el.classList.remove('is-active');
			$target.classList.remove('is-active');
		});
	}

	// Toggle is-active on click event for each {navbar-burger}
	const $navbarBurgers = document.querySelectorAll('.navbar-burger');
	// Check if there is any target
	if ($navbarBurgers.length > 0) {
		// For each dropdown, add event handler to open on click.
		$navbarBurgers.forEach( el => {
			el.addEventListener('click', () => {
				// Get the target from the "data-target" attribute
				const $target = document.getElementById(el.dataset.target);
				// Toggle the "is-active" class on both the "navbar-burger" and the "navbar-menu"
				el.classList.toggle('is-active');
				$target.classList.toggle('is-active');
			});
		});

		// Close burger menu if ESC pressed
		document.addEventListener('keydown', function (event) {
			let e = event || window.event;
			if (e.key === 'Esc' || e.key === 'Escape') {
				closeBurgers($navbarBurgers);
			}
		});
	}

  /*
   * Dropdown open/close rationale
   */

	// Close all dorpdown by removing `is-active` class.
	function closeDropdowns(objs) {
		objs.forEach(function(el) {
			el.classList.remove('is-active');
		});
	}

	// Get all dropdowns on the page that aren't hoverable.
	const $dropdowns = document.querySelectorAll('.has-dropdown:not(.is-hoverable)');
	// Check if there is any target
	if ($dropdowns.length > 0) {
		// For each dropdown, add event handler to open on click.
		$dropdowns.forEach(function(el) {
			el.addEventListener('click', function(e) {
				e.stopPropagation();
				el.classList.toggle('is-active');
			});
		});

		// Close dropdowns if ESC pressed
		document.addEventListener('keydown', function (event) {
			let e = event || window.event;
			if (e.key === 'Esc' || e.key === 'Escape') {
				closeDropdowns($dropdowns);
			}
		});

		// If user clicks outside dropdown, close it.
		document.addEventListener('click', function(e) {
			closeDropdowns($dropdowns);
		});
	}

    /*
     * Tooltip effect rational
     */
    // Todo...

}


