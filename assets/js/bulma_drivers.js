
/*
 *
 * Bulma javascript helpers
 *
 */

//document.addEventListener('DOMContentLoaded', () => {
const BulmaDriver = (target) => {

    var $target;
    if (!target) {
        $target = document;
    } else {
        // @DEBUG: document handler may be added several times here...
        // --
        // Use parentNode to be sure to not miss the target in the case
        // where the eltId is defined at the same level of the wanted selector.
        $target = document.getElementById(target).parentNode;
    }

    var eltThatCloseOnEsc = [];
    var addedHandlers = [];

    /*
     * Activate autofocus
     */
    const $autofocuses = $target.querySelectorAll('.autofocus');
    // * listen for enter to advanced the focus on textarea
    if ($autofocuses.length > 0) {
        $autofocuses.forEach( el => {
            el.focus();
        });
    }

    /*
     * Follow focus
     */

    function advanceFocus(e, el) {
        if (e.key == "Enter") {
            $target = document.getElementById(el.dataset.nextfocus);
            $target.focus();
        }
    }

    // Toggle is-active on click event for each {navbar-burger}
    const $followFocuses = $target.querySelectorAll('.followFocus');
    // * listen for enter to advanced the focus on textarea
    if ($followFocuses.length > 0) {
        // For each dropdown, add event handler to open on click.
        $followFocuses.forEach( el => {
            var evt = "keypress";
            var h = e => advanceFocus(e, el);
            el.addEventListener(evt, h);
            addedHandlers.push([evt, h, el]);
        });
    }


    /*
     * Burger open/close rationale
     */

    // Close all burger by removing `is-active` class.
    const closeBurgers = objs => {
        objs.forEach(function(el) {
            const $target_ = document.getElementById(el.dataset.target);
            el.classList.remove('is-active');
            $target_.classList.remove('is-active');
        });
    }

    // Toggle is-active on click event for each {navbar-burger}
    const $navbarBurgers = $target.querySelectorAll('.navbar-burger');
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
                const $target_ = document.getElementById(el.dataset.target);
                // Toggle the "is-active" class on both the "navbar-burger" and the "navbar-menu"
                el.classList.toggle('is-active');
                $target_.classList.toggle('is-active');
            });
        });

        // Close burger menu if ESC pressed
        eltThatCloseOnEsc.push([closeBurgers, $navbarBurgers]);
    }

    /*
     * Dropdown open/close rationale
     */

    function buttonDropdownHandler(e, btn) {
        e.stopPropagation();
        btn.classList.toggle('is-active');
    }

    // Close all dorpdown by removing `is-active` class.
    function closeDropdowns(objs) {
        objs.forEach(function(el) {
            el.classList.remove('is-active');
        });
    }

    // Get all dropdowns on the page that aren't hoverable.
    const $dropdowns = $target.querySelectorAll('.has-dropdown:not(.is-hoverable)');
    //
    // * toggle dropdown state on click
    // * close on Escape
    // * stopeventpropgation (difference witn preventdefault ?)
    //
    if ($dropdowns.length > 0) {
        // For each dropdown, add event handler to open on click.
        $dropdowns.forEach(function(el) {
            var evt = "click";
            var h = e => buttonDropdownHandler(e, el);
            el.addEventListener(evt, h);
            addedHandlers.push([evt, h, el]);
        });

        // If user clicks outside dropdown, close it.
        document.addEventListener('click', function(e) {
            closeDropdowns($dropdowns);
        });

        // Close burger menu if ESC pressed
        eltThatCloseOnEsc.push([closeDropdowns, $dropdowns]);

    }

    /*
     * Button **Toggle** effect rational
     */

    function buttonToggleHandler(e, btn) {
        btn.classList.toggle('is-active');
        e.preventDefault() // important: don't let html handle the button element state
    }

    var $btns = $target.querySelectorAll('.buttonToggle');
    //
    // * toggle active state on click
    // * preventdefault
    //
    if ($btns.length > 0) {
        $btns.forEach(function(el) {
            var evt = "mousedown";
            var h = e => buttonToggleHandler(e, el);
            el.addEventListener(evt, h);
            addedHandlers.push([evt, h, el]);
        });
    }

    /*
     * Button **Radio** effect rational
     */

    function buttonRadioHandler(e, btn, btns) {
        btns.forEach( o => {
            if (o === btn) {
                o.classList.add('is-active');
            } else {
                o.classList.remove('is-active');
            }
        });
        e.preventDefault() // important: don't let html handle the button element state
    }

    var $btns = $target.querySelectorAll('.buttonRadio');
    //
    // * switch active state on click on each button child
    // * preventdefault
    //
    if ($btns.length > 0) {
        $btns.forEach(function(el) {
            $subBtns = el.querySelectorAll('.button');
            // button click logics
            $subBtns.forEach( btn => {
                var evt = "mousedown";
                var h = e => buttonRadioHandler(e, btn, $subBtns);
                btn.addEventListener(evt, h);
                addedHandlers.push([evt, h, btn]);
            });
        });
    }

    /*
     * Modal logics
     */

    // Close all modals if ESC pressed
    const closeModals = objs => {
        objs.forEach(function(el) {
            // deactivate all buttons that are below that modal
            el.querySelectorAll('.button').forEach(btn => {
                btn.classList.remove('is-active');
            });

            // Remove added handlers below that modal
            for (var i=0; i < addedHandlers.length; i++) {
                evt = addedHandlers[i][0];
                func = addedHandlers[i][1];
                obj = addedHandlers[i][2];
                // check if it belongs to modal
                if (el.contains(obj)) obj.removeEventListener(evt, func);
            }

            // Fix block scrolling
            document.documentElement.classList.remove('has-modal-active');
            // deactivate modal
            el.classList.remove('is-active');

        });
    }

    const $modals = $target.querySelectorAll('.modal');
    const $modal_closes = $target.querySelectorAll('.modal-close');
    const $modal_background = $target.querySelectorAll('.modal-background');
    const $modal_triggers = $target.querySelectorAll('.modalTrigger'); // app specific
    //
    // * toggle active modal when clicking *trigger elements.
    // * fix scroll blocking by activating/deactivating has-modal-active on <html>.
    // * close when clicking on background
    // * close when clicking *close-modal.
    // * /!\ When closing deactive all buttons.
    //
    if ($modal_triggers.length > 0) {
        $modal_triggers.forEach( el => {
            el.addEventListener('mousedown', function(e) {
                var $target_ = document.getElementById(el.dataset.modal);
                // Activate modal
                $target_.classList.add("is-active");
            });
        });
    }
    if ($modal_background.length > 0) {
        $modal_background.forEach( el => {
            el.addEventListener('mousedown', function(e) {
                closeModals($modals);
            });
        });

        eltThatCloseOnEsc.push([closeModals, $modals]);

    }
    if ($modal_closes.length > 0) {
        $modal_closes.forEach( el => {
            el.addEventListener('mousedown', function(e) {
                closeModals($modals);
            });
        });
    }


    //
    //
    // Close on Esc Logics
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


