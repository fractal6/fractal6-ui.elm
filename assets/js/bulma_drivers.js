/*
 *
 * Bulma javascript helpers
 *
 */

export function InitBulma(app, session, eltId) {
    console.log(`Activate Bulma driver...`);
    // This timeout is needed when bulma driver is called by elm Cmd,
    // to wait foe the Html Msg to be updated by elm in order
    // to have new node accessible by Javascript.
    //document.addEventListener('DOMContentLoaded', () => {
    var handlers = session.bulmaHandlers;
    setTimeout(BulmaDriver, 300, app, eltId, handlers);
}

function catchEsc(e, fun) {
    let evt = event || window.event;
    if (evt.key === 'Esc' || evt.key === 'Escape') {
        fun();
    }
}


export function BulmaDriver(app, target, handlers) {
    //
    // Setup
    //

    var $doc;
    if (!target) {
        $doc = document;
    } else {
        // @DEBUG: document handler may be added several times here...
        // --
        // Use parentNode to be sure to not miss the target in the case
        // where the eltId is defined at the same level of the wanted selector.
        $doc = document.getElementById(target).parentNode;
    }

    //
    // Object Behaviours
    //

    function setupHandler(evt, hdl, elt) {
        if (!hasHandler(evt, hdl, elt)) {
            elt.addEventListener(evt, hdl);
            handlers.push([evt, hdl, elt]);
        } else {
            // pass
        }
    }
    // @DEBUG: use a HashMap instead!
    function hasHandler(evt, hdl, elt) {
        // Check if the object is already in the list of handler return true
        for (var i=0; i < handlers.length; i++) {
            // check if it belongs to modal
            if (evt === handlers[i][0] && hdl.name === handlers[i][1].name && elt === handlers[i][2] ) {
                return true
            }
        }
        return false
    }

    // Remove document handlers
    var idxToRemove = [];
    for (var i=0; i < handlers.length; i++) {
        var evt = handlers[i][0];
        var func = handlers[i][1];
        var obj = handlers[i][2];
        // check if it belongs to modal
        if (obj == document) {
            obj.removeEventListener(evt, func)
            idxToRemove.push(i)
        }
    }
    for (var i = idxToRemove.length -1; i >= 0; i--)
        handlers.splice(idxToRemove[i],1);

    ////////////////////////////////////////Bulma Behaviours

    //
    // Activate autofocus
    //

    const $autofocuses = $doc.querySelectorAll('.autofocus');
    // * listen for enter to advanced the focus on textarea
    //
    if ($autofocuses.length > 0) {
        $autofocuses.forEach( el => {
            el.focus();
            return true
        });
    }

    //
    // Follow focus
    //

    function advanceFocus(e, el) {
        if (e.key == "Enter") {
            var $target = document.getElementById(el.dataset.nextfocus);
            if ($target) {
                $target.focus();
                return true
            }
        }
    }

    // Toggle is-active on click event for each {navbar-burger}
    const $followFocuses = $doc.querySelectorAll('.followFocus');
    // * listen for enter to advanced the focus on textarea
    //
    if ($followFocuses.length > 0) {
        // For each dropdown, add event handler to open on click.
        $followFocuses.forEach( el => {
            var h = e => advanceFocus(e, el);
            setupHandler("keypress", h, el);
        });
    }

    //
    // Burger open/close rationale
    //

    function burgerToggleHandler(e, el) {
        // Get the target from the "data-target" attribute
        const $target_ = document.getElementById(el.dataset.target);
        // Toggle the "is-active" class on both the "navbar-burger" and the "navbar-menu"
        el.classList.toggle('is-active');
        $target_.classList.toggle('is-active');
    }

    // Close all burger by removing `is-active` class.
    function closeBurgers(e, objs) {
        objs.forEach(function(el) {
            const $target_ = document.getElementById(el.dataset.target);
            el.classList.remove('is-active');
            $target_.classList.remove('is-active');
        });
    }

    // Toggle is-active on click event for each {navbar-burger}
    const $navbarBurgers = $doc.querySelectorAll('.navbar-burger');
    // * toggle active state on click
    // * close on escape
    //
    // Toggle navbars when clicking on burgers
    if ($navbarBurgers.length > 0) {
        $navbarBurgers.forEach( el => {
            // For each dropdown, add event handler to open on click.
            var h1 = e => burgerToggleHandler(e, el);
            setupHandler("click", h1, el);
        });

        // For each dropdown, add event handler to close on esc
        var h2 = e => catchEsc(e, () => closeBurgers(e, $navbarBurgers));
        setupHandler("keydown", h2, document);
    }

    //
    // Dropdown open/close rationale
    //

    function buttonDropdownHandler(e, btn, all) {
        e.stopPropagation();
        all.forEach(function(el) {
            if (el !== btn) {
                el.classList.remove('is-active');
            }
        });
        btn.classList.toggle('is-active');
    }

    // Close all dropdown by removing `is-active` class.
    function closeDropdowns(e, objs) {
        objs.forEach(function(el) {
            el.classList.remove('is-active');
        });
    }

    // Get all dropdowns on the page that aren't hoverable.
    const $dropdowns = $doc.querySelectorAll('.has-dropdown:not(.is-hoverable)');
    // * toggle dropdown state on click
    // * close on Escape
    // * stopeventpropgation (difference witn preventdefault ?)
    //
    if ($dropdowns.length > 0) {
        $dropdowns.forEach(function(el) {
            // For each dropdown, add event handler to open on click.
            var h3 = e => buttonDropdownHandler(e, el, $dropdowns);
            setupHandler("click", h3, el);
        });

        // For each dropdown, add event handler to close on Esc.
        var h4 = e => catchEsc(e, () => closeDropdowns(e, $dropdowns));
        setupHandler("keydown", h4, document);

        // For each dropdown, add event handler to close if a click occurs outside.
        var h5 = e => closeDropdowns(e, $dropdowns);
        setupHandler("click", h5, document);
    }

    //
    // Button **Toggle** effect rational
    //
    function buttonToggleHandler(e, btn) {
        btn.classList.toggle('is-active');
        e.preventDefault(); // important: don't let html handle the button element state
    }

    var $btns = $doc.querySelectorAll('.buttonToggle');
    // * toggle active state on click
    // * preventdefault
    //
    if ($btns.length > 0) {
        $btns.forEach(function(el) {
            var h6 = e => buttonToggleHandler(e, el);
            setupHandler("mousedown", h6, el);
        });
    }

    //
    // Button **Radio** effect rational
    //

    //function buttonRadioHandler(e, btn, btns) {
    //    btns.forEach( o => {
    //        if (o === btn) {
    //            o.classList.add('is-active');
    //        } else {
    //            o.classList.remove('is-active');
    //        }
    //    });
    //    e.preventDefault() // important: don't let html handle the button element state
    //}

    //var $btns = $doc.querySelectorAll('.buttonRadio');
    ////
    //// * switch active state on click on each button child
    //// * preventdefault
    ////
    //if ($btns.length > 0) {
    //    $btns.forEach(function(el) {
    //        $subBtns = el.querySelectorAll('.button');
    //        // button click logics
    //        $subBtns.forEach( btn => {
    //            var h = e => buttonRadioHandler(e, btn, $subBtns);
    //            setupHandler("mousedown", h, el);
    //        });
    //    });
    //}

    //
    // Modal logics
    //

    // Close all modals if ESC pressed
    function closeModal(e, modal) {
        if (document.documentElement.classList.contains("has-modal-active2" )) {
            return
        }
        // deactivate all buttons that are below that modal
        modal.querySelectorAll('.button').forEach(btn => {
            btn.classList.remove('is-active');
        });

        // Elm compatibility
        if (modal.classList.contains("elmModal" )) {
            // Close modal with elm
            app.ports.closeModalFromJs.send("")
        } else {
            modal.classList.remove('is-active');
            // Fix block scrolling
            document.documentElement.classList.remove('has-modal-active');
            document.getElementById("navbarTop").classList.remove('has-modal-active');
        }
    }

    // Activate modal
    function triggerModal(e, el) {
        var $target_ = document.getElementById(el.dataset.modal);
        document.documentElement.classList.add('has-modal-active');
        document.getElementById("navbarTop").classList.add('has-modal-active');
        $target_.classList.add("is-active");
    }

    const $modal_esc = $doc.querySelectorAll('.modal-escape');
    const $modal_triggers = $doc.querySelectorAll('.modalTrigger'); // app specific
    // * toggle active modal when clicking *trigger* elements.
    // * fix scroll blocking by activating/deactivating has-modal-active on <html>.
    // * close when pressing ESC
    //
    if ($modal_triggers.length > 0) {
        $modal_triggers.forEach( el => {
            var h7 = e => triggerModal(e, el);
            setupHandler("mousedown", h7, el);
        });
    }
    if ($modal_esc.length > 0) {
        $modal_esc.forEach( el => {
            var $modal = document.getElementById(el.dataset.modal);
            var h8 = e => catchEsc(e, () => closeModal(e, $modal));
            setupHandler("keydown", h8, document);
        });
    }

}


