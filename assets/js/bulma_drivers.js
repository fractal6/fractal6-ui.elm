
/*
 *
 * Bulma javascript helpers
 *
 */

//document.addEventListener('DOMContentLoaded', () => {
const BulmaDriver = () => {

    var eltThatCloseOnEsc = [];

    /*
     * Burger open/close rationale
     */

    // Close all burger by removing `is-active` class.
    const closeBurgers = objs => {
        objs.forEach(function(el) {
            const $target = document.getElementById(el.dataset.target);
            el.classList.remove('is-active');
            $target.classList.remove('is-active');
        });
    }

    // Toggle is-active on click event for each {navbar-burger}
    const $navbarBurgers = document.querySelectorAll('.navbar-burger');
    //
    // * toggle active state on click
    // * close on escape
    //
    // Toggle navbars when clicking on burgers
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
        eltThatCloseOnEsc.push([closeBurgers, $navbarBurgers]);
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
    //
    // * toggle dropdown state on click
    // * close on Escape
    // * stopeventpropgation (difference witn preventdefault ?)
    //
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
     * Button & Tooltip effect rational
     */
    var $btns = document.querySelectorAll('.btnToggle');
    //
    // * toggle active state on click
    // * preventdefault
    //
    if ($btns.length > 0) {
        $btns.forEach(function(el) {
            el.addEventListener('mousedown', function(e) {
                el.classList.toggle('is-active');
                e.preventDefault() // important: don't let html handle the button element state
            });
        });
    }

    /*
     * Modal logics
     */

    // Close all modals if ESC pressed
    const closeModals = objs => {
        objs.forEach(function(el) {
            el.classList.remove('is-active');
            // Fix block scrolling
            document.documentElement.classList.remove('has-modal-active');
        });
    }

    const $modals = document.querySelectorAll('.modal');
    const $modal_closes = document.querySelectorAll('.modal-close');
    const $modal_triggers = document.querySelectorAll('.modalTrigger'); // app specific
    //
    // * toggle active modal when clicking *trigger elements.
    // * close when clicking *close-modal.
    //
    // Check if there is any target
    if ($modal_closes.length > 0) {
        // For each dropdown, add event handler to open on click.
        $modal_closes.forEach( el => {
            el.addEventListener('mousedown', function(e) {
                closeModals($modals);
            });
        });

        eltThatCloseOnEsc.push([closeModals, $modals]);

    }
    // Check if there is any target
    if ($modal_triggers.length > 0) {
        $modal_triggers.forEach( el => {
            el.addEventListener('mousedown', function(e) {
                var $target = document.getElementById(el.dataset.modal);
                $target.classList.add("is-active");
            });
        });
    }


    //
    //
    // Close on Esc Logics
    //
    //
    //

    document.addEventListener('keydown', function (event) {
        let e = event || window.event;
        if (e.key === 'Esc' || e.key === 'Escape') {

            for (var i=0; i < eltThatCloseOnEsc.length; i++) {
                var func = eltThatCloseOnEsc[i][0];
                var objs = eltThatCloseOnEsc[i][1];
                func(objs);
            }
        }
    });

}


