/*
 *
 * Bulma javascript helpers
 *
 */

// The container where the burgers should close when a clicl "anywhere" occurs
const closeOnClickBurger = ['userMenu']; // data-target of burger


export function InitBulma(app, session, eltId) {
    var handlers = session.bulmaHandlers;
    if (!eltId) console.log(`Activate Bulma driver (%d)...`, handlers.length);

    // This timeout is needed when bulma driver is called by elm Cmd,
    // to wait foe the Html Msg to be updated by elm in order
    // to have new node accessible by Javascript.
    //document.addEventListener('DOMContentLoaded', () => {
        setTimeout(BulmaDriver, 300, app, eltId, handlers);
    //});
}

export function catchEsc(e, fun) {
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
    // Handlers Logics
    //
    // Remove document handlers

    function setupHandler(evt, hdl, elt) {
        if (!hasHandler(evt, hdl, elt)) {
            // Dbug what handler is happened
            //for (var i=0; i < handlers.length; i++) {
            //    if (evt === handlers[i][0] && hdl.name === handlers[i][1].name ) {
            //        console.log(evt === handlers[i][0], hdl.name === handlers[i][1].name, elt === handlers[i][2])
            //        console.log([evt, hdl, elt,handlers[i][2] ])
            //    }
            //}
            elt.addEventListener(evt, hdl);
            handlers.push([evt, hdl, elt]);
        } else {
            //console.log("handler already exits, passing");
        }
    }

    // @DEBUG: use a HashMap instead!
    function hasHandler(evt, hdl, elt) {
        // Check if the object is already in the list of handler return true
        for (var i=0; i < handlers.length; i++) {
            if (evt === handlers[i][0] && hdl.name === handlers[i][1].name && elt === handlers[i][2]) {
                return true
            }
        }
        return false
    }

    // Remove handlers of document and not connected elements.
    var idxToRemove = [];
    for (var i=0; i < handlers.length; i++) {
        var evt = handlers[i][0];
        var func = handlers[i][1];
        var obj = handlers[i][2];
        if (obj == document || !obj.isConnected) {
            obj.removeEventListener(evt, func)
            idxToRemove.push(i)
        }
    }
    for (var i = idxToRemove.length -1; i >= 0; i--)
        handlers.splice(idxToRemove[i],1);


    //////////////////// Setup Bulma Components ////////////////////

    //
    // Activate autofocus
    //
    // * focus on the element automatically
    //
    const $autofocuses = $doc.querySelectorAll('.autofocus');
    if ($autofocuses.length > 0) {
        $autofocuses.forEach( el => {
            el.focus();
            return true
        });
    }

    //
    // Follow focus
    //
    // * listen for enter to advanced the focus on textarea
    //
    const $followFocuses = $doc.querySelectorAll('.followFocus');
    if ($followFocuses.length > 0) {
        $followFocuses.forEach( el => {
            var h = e => moveFocus(e, el);
            setupHandler("keypress", h, el);
        });
    }

    //
    // Burger open/close rationale
    //
    // Toggle is-active on click event for each {burger}
    // * toggle active state on click
    // * close on escape
    // * close on click outside
    //
    const $burgers = $doc.querySelectorAll('.burger');
    if ($burgers.length > 0) {
        // For each burger, add event handler to toggle on click.
        $burgers.forEach( el => {
            var h1 = e => burgerToggleHandler(e, el);
            setupHandler("click", h1, el);
        });

        // For each burger, add event handler to close on Esc
        var h2 = e => catchEsc(e, () => closeBurgers(e, $burgers));
        setupHandler("keydown", h2, document);

        // For each burger, add event handler to close if a click occurs outside.
        var h3 = e => closeBurgersClick(e, $burgers);
        setupHandler("click", h3, document);
    }

    //
    // Dropdown open/close rationale
    //
    // * toggle dropdown state on click
    // * close on Escape
    // * stopeventpropgation (difference witn preventdefault ?)
    //
    // Get all dropdowns on the page that aren't hoverable.
    const $dropdowns = $doc.querySelectorAll('.dropdown:not(.is-hoverable), .has-dropdown:not(.is-hoverable)');
    if ($dropdowns.length > 0) {

        // Toggle on click
        $dropdowns.forEach(function(el) {
            // For each dropdown, add event handler to toggle on click.
            var h3 = e => buttonGroupedToggleHandler(e, el, $dropdowns);
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
    // * toggle active state on click
    // * preventdefault
    //
    var $btns = $doc.querySelectorAll('.buttonToggle');
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
            var h8 = e => catchEsc(e, () => closeModal(e, $modal, app));
            setupHandler("keydown", h8, document);
        });
    }

}

//
// Focus methods
//

function moveFocus(e, el) {
    if (e.key == "Enter") {
        var $target = document.getElementById(el.dataset.nextfocus);
        if ($target) {
            $target.focus();
            return true
        }
    }
}


//
// Burgers methods
//

function burgerToggleHandler(e, el) {
    e.stopPropagation();
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

// Close all burger by removing `is-active` class.
function closeBurgersClick(e, objs) {
    objs.forEach(function(el) {
        if (closeOnClickBurger.includes(el.dataset.target)) {
            const $target_ = document.getElementById(el.dataset.target);
            el.classList.remove('is-active');
            $target_.classList.remove('is-active');
        }
    });
}

//
// Dropdown metdods
//

function buttonGroupedToggleHandler(e, btn, all) {
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

//
// Button methods
//
function buttonToggleHandler(e, btn) {
    btn.classList.toggle('is-active');
    e.preventDefault(); // important: don't let html handle the button element state
}

//
// Modal methods
//
// Activate modal
function triggerModal(e, el) {
    var $target_ = document.getElementById(el.dataset.modal);
    document.documentElement.classList.add('has-modal-active');
    document.getElementById("navbarTop").classList.add('has-modal-active');
    $target_.classList.add("is-active");
}

// Close all modals if ESC pressed
function closeModal(e, modal, app) {
    // except this one
    if (document.documentElement.classList.contains("has-modal-active2")) {
        return
    }

    if (!modal.classList.contains("is-active")) {
        return
    }

    // deactivate all buttons that are below that modal
    modal.querySelectorAll('.button').forEach(btn => {
        btn.classList.remove('is-active');
    });

    // Elm compatibility
    if (modal.classList.contains("elmModal")) {
        // Close modal with elm
        app.ports.closeModalFromJs.send("")
    } else {
        modal.classList.remove('is-active');
        // Fix block scrolling
        document.documentElement.classList.remove('has-modal-active');
        document.getElementById("navbarTop").classList.remove('has-modal-active');
    }
}
