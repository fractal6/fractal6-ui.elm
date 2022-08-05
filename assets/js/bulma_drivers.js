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
    // to wait for the Html Msg to be updated by elm in order
    // to have new node accessible by Javascript.
    //document.addEventListener('DOMContentLoaded', () => {
        setTimeout(BulmaDriver, 333, app, eltId, handlers);
    //});
}

export function catchEsc(e, fun, ...args) {
    let evt = event || window.event;
    if (evt.key === 'Esc' || evt.key === 'Escape') {
        fun(e, ...args);
    }
}

export function catchEnter(e, fun, ...args) {
    let evt = event || window.event;
    if (evt.key === 'Enter') {
        fun(e, ...args);
    }
}

export function updateLang(app, lang) {
    localStorage.setItem('lang', lang);
    app.ports.updateLangFromJs.send(lang);
    setTimeout(() => {
        var loc = window.location;
        window.location.replace(
            loc.protocol + '//' + loc.host + "/"+lang.toLowerCase() + loc.pathname + loc.search
        );
		// May try this to force reoload ?
        // https://itecnote.com/tecnote/javascript-force-a-reload-of-page-in-chrome-using-javascript-no-cache/
		//$.ajax({
		//	url: window.location.href,
		//	headers: {
		//		"Pragma": "no-cache",
		//		"Expires": -1,
		//		"Cache-Control": "no-cache"
		//	}
		//}).done(function () {
		//	window.location.reload(true);
		//});
    }, 333);
}



export function BulmaDriver(app, target, handlers) {
    //
    // Setup
    //

    // Get the target node
    var $doc;
    if (!target) {
        $doc = document;
        // Eventually clean active modal style
        document.documentElement.classList.remove('has-modal-active');
        document.getElementById("navbarTop").classList.remove('has-modal-active');
        document.documentElement.classList.remove('has-modal-active2');
        document.getElementById("navbarTop").classList.remove('has-modal-active2');
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

    // evt: the event type
    // hdl: the event handler
    // elt: the element where the event apply
    // objs: the arguments of the handlers
    function setupHandler(evt, hdl, elt, ...objs) {
        if (!hasHandler(evt, hdl, elt)) {
            //// Debug what handler is happened
            //console.log(evt, hdl.name, elt)
            //for (var i=0; i < handlers.length; i++) {
            //    if (evt === handlers[i][0] && hdl.name === handlers[i][1].name ) {
            //        console.log(evt === handlers[i][0], hdl.name === handlers[i][1].name, elt === handlers[i][2])
            //        console.log([evt, hdl, elt,handlers[i][2] ])
            //    }
            //}

            // Apply the handler to the first given object
            var _hdl_;
            if (evt === "esc") {
                evt = "keydown" ;
                _hdl_ = e => catchEsc(e, hdl, ...objs);
            } else if (evt === "enter") {
                evt = "keydown" ;
                _hdl_ = e => catchEnter(e, hdl, ...objs);
            } else {
                _hdl_ = e => hdl(e, ...objs);
            }
            elt.addEventListener(evt, _hdl_);

            // Memorize active handlers
            handlers.push([evt, hdl, elt, _hdl_]);
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
        var elt = handlers[i][2];
        var func = handlers[i][3];
        if (elt == document || !elt.isConnected) {
            elt.removeEventListener(evt, func)
            idxToRemove.push(i)
        }
    }
    for (var i = idxToRemove.length -1; i >= 0; i--)
        handlers.splice(idxToRemove[i],1);


    //////////////////// Setup Bulma Components ////////////////////

    // Special Elm function

    const $themeTrigger = $doc.querySelectorAll('#themeTrigger');
    if ($themeTrigger.length > 0) {
        $themeTrigger.forEach( el => {
            setupHandler("click", triggerTheme, el, el, app);
        });
    }

    const $langTrigger = $doc.querySelectorAll('.langTrigger');
    if ($langTrigger.length > 0) {
        $langTrigger.forEach( el => {
            setupHandler("click", triggerLang, el, el, app);
        });
    }

    const $notifTrigger = $doc.querySelectorAll('.notifTrigger');
    if ($notifTrigger.length > 0) {
        $notifTrigger.forEach( el => {
            setupHandler("click", triggerNotif, el, el, app);
        });
    }

    const $helpTrigger = $doc.querySelectorAll('.helpTrigger');
    if ($helpTrigger.length > 0) {
        $helpTrigger.forEach( el => {
            setupHandler("click", triggerHelp, el, el, app);
        });
    }

    const $joinTrigger = $doc.querySelectorAll('.joinTrigger');
    if ($joinTrigger.length > 0) {
        $joinTrigger.forEach( el => {
            setupHandler("click", triggerJoin, el, el, app);
        });
    }

    const $joinTrigger2 = $doc.querySelectorAll('.joinPendingTrigger');
    if ($joinTrigger2.length > 0) {
        $joinTrigger2.forEach( el => {
            setupHandler("click", triggerJoin2, el, el, app);
        });
    }

    const $inviteTrigger = $doc.querySelectorAll('.inviteTrigger');
    if ($inviteTrigger.length > 0) {
        $inviteTrigger.forEach( el => {
            setupHandler("click", triggerInvite, el, el, app);
        });
    }

    const $menuOrgaTrigger = $doc.querySelectorAll('.menuOrgaTrigger');
    if ($menuOrgaTrigger.length > 0) {
        $menuOrgaTrigger.forEach( el => {
            setupHandler("click", triggerMenuOrga, el, el, app);
        });
    }

    const $menuTreeTrigger = $doc.querySelectorAll('.menuTreeTrigger');
    if ($menuTreeTrigger.length > 0) {
        $menuTreeTrigger.forEach( el => {
            setupHandler("click", triggerMenuTree, el, el, app);
        });
    }

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
    // Submit data
    //
    // * listen for ctrl+enter to submit data
    //
    const $submitFocuses = $doc.querySelectorAll('.submitFocus');
    if ($submitFocuses.length > 0) {
        $submitFocuses.forEach( el => {
            // /!\ keypress won't capture TAB and some other keys.
            setupHandler("keydown", submitFocus, el, el);
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
            setupHandler("keydown", moveFocus, el, el);
        });
    }

    //
    // "Rich Text" on textarea
    //
    // * Capture TAB to insert space
    //
    const $textareas = $doc.querySelectorAll('.textarea');
    if ($textareas.length > 0) {
        $textareas.forEach( el => {
            setupHandler("keydown", richText, el, el);
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
            setupHandler("click", burgerToggleHandler, el, el);
        });

        // For each burger, add event handler to close on Esc
        setupHandler("esc", closeBurgers, document, $burgers);

        // For each burger, add event handler to close if a click occurs outside.
        // Note:  Only for those define in closeOnClickBurger
        setupHandler("click", closeBurgersClick, document, $burgers);

        // Close dropdown on init (navbar dropdown don't close on click !)
        closeBurgersClick(document, $burgers);
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
            setupHandler("click", dropdownToggleHandler, el, el, $dropdowns);
        });

        // For each dropdown, add event handler to close on Esc.
        setupHandler("esc", closeDropdowns, document, $dropdowns);

        // For each dropdown, add event handler to close if a click occurs outside.
        setupHandler("click", closeDropdowns, document, $dropdowns);
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
            setupHandler("mousedown", buttonToggleHandler, el, el);
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
    //            setupHandler("mousedown", buttonRadioHandler, btn, btn, $subBtns);
    //        });
    //    });
    //}

    //
    // Modal logics
    //
    const $modal_esc = $doc.querySelectorAll('.modal-escape');
    const $modal_triggers = $doc.querySelectorAll('.modal-trigger'); // app specific
    // * toggle active modal when clicking *trigger* elements.
    // * fix scroll blocking by activating/deactivating has-modal-active on <html>.
    // * close when pressing ESC
    //
    if ($modal_triggers.length > 0) {
        $modal_triggers.forEach( el => {
            setupHandler("mousedown", triggerModal, el, el);
        });
    }
    if ($modal_esc.length > 0) {
        $modal_esc.forEach( el => {
            var $modal = document.getElementById(el.dataset.modal);
            setupHandler("esc", closeModal, document, $modal, app);
        });
    }

}

//
// Focus methods
//

function moveFocus(e, el) {
    if ((e.key == "Enter" || e.key == "Tab") && !e.ctrlKey) {
        var $t = document.getElementById(el.dataset.nextfocus);
        if ($t) {
            e.preventDefault(); // prevent a line break on the next text area.
            $t.focus();
            return true
        }
    }
}

//
// Submit methods
//

function submitFocus(e, el) {
    if (e.key == "Enter" && e.ctrlKey) {
        // submit focus.
        var s = el.querySelector(".defaultSubmit")
        if (s) {
            e.preventDefault(); // prevent a line break on the text area
            s.click();
        }
    }
}

//
// """Rich Text"""
//

function richText(e, el) {
    if (e.key == "Tab" && !e.ctrlKey && !e.shiftKey) {
        // Ignore if Tab occurs at the beginning ot not after a new line or tab
        console.log(el.selectionStart)
        if (el.selectionStart == 0 || ! ["\n", "\t"].includes(el.value[el.selectionStart-1])) return

		e.preventDefault();
		var start = el.selectionStart;
		var end = el.selectionEnd;

        if (el.value[el.selectionStart-2] == "\n") {
            // 4 space (Tab) for **code** indentation
            var replacer = "\t";

        } else {
            // Assumes we are in a **list content**
            // 2 space for sublist indentation
            var replacer = "  ";
        }

		// set textarea value to: text before caret + tab + text after caret
		el.value = el.value.substring(0, start) +
			replacer + el.value.substring(end);   // Insert double space
			//"\t" + el.value.substring(end); // Insert Tab

		// put caret at right position again
		el.selectionStart =
			el.selectionEnd = start + replacer.length;
    }
    // Breaking space or not ???
    //else if (e.key ==  '\xa0') { // Non-breakable space is char 0xa0 (160 dec)
    //    el.value = "x";
    //}
}

//
// Burgers methods
//

function burgerToggleHandler(e, el) {
    e.stopPropagation();
    // Toggle the "is-active" class on both the "navbar-burger" and the "navbar-menu"
    el.classList.toggle('is-active');
    // Get the target from the "data-target" attribute
    var $t = document.getElementById(el.dataset.target);
    if ($t) {
        $t.classList.toggle('is-active');
    }
}

// Close all burger by removing `is-active` class.
function closeBurgers(e, objs) {
    objs.forEach(function(el) {
        el.classList.remove('is-active');
        var $t = document.getElementById(el.dataset.target);
        if ($t) {
            $t.classList.remove('is-active');
        }
    });
}

// Close all burger by removing `is-active` class.
function closeBurgersClick(e, objs) {
    objs.forEach(function(el) {
        if (closeOnClickBurger.includes(el.dataset.target)) {
            el.classList.remove('is-active');
            var $t = document.getElementById(el.dataset.target);
            if ($t) {
                $t.classList.remove('is-active');
            }
        }
    });
}

//
// Dropdown metdods
//

function dropdownToggleHandler(e, btn, all) {
    // @debug: if bulma is not reset the $all object will changed !
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
    document.documentElement.classList.add('has-modal-active');
    document.getElementById("navbarTop").classList.add('has-modal-active');
    var $t = document.getElementById(el.dataset.modal);
    if ($t) {
        $t.classList.add("is-active");
    }
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

    // Defined like so: `attribute "data-modal-close" "closeMyModalFromJs"`
    var closeMsg = modal.dataset.modalClose;
    // Elm compatibility
    if (closeMsg) {
        // Close modal with elm
        if (app.ports[closeMsg] === undefined) {
            return
        }
        // Do not reset modal when is quitted with ESC.
        app.ports[closeMsg].send({reset:false, link:""});
    } else {
        modal.classList.remove('is-active');
        // Fix block scrolling
        document.documentElement.classList.remove('has-modal-active');
        document.getElementById("navbarTop").classList.remove('has-modal-active');
    }
}

//
// Other Triggers
//

function triggerHelp(e, el, app) {
    app.ports.triggerHelpFromJs.send(null)
}

function triggerJoin(e, el, app) {
    app.ports.triggerJoinFromJs.send(null)
}

function triggerJoin2(e, el, app) {
    app.ports.triggerJoinPendingFromJs.send(null)
}

function triggerInvite(e, el, app) {
    app.ports.triggerInviteFromJs.send(null)
}

function triggerNotif(e, el, app) {
    //app.ports.triggerNotifFromJs.send(null)
}

function triggerMenuOrga(e, el, app) {
    app.ports.triggerMenuOrgaFromJs.send(null)
}

function triggerMenuTree(e, el, app) {
    app.ports.triggerMenuTreeFromJs.send(null)
}

function triggerTheme(e, el, app) {
    // Toggle theme color
    var theme;
    if (document.documentElement.classList.contains("dark")) {
        theme = "light";
    } else if (document.documentElement.classList.contains("light")) {
        theme = "dark";
    } else {
        // Assume default is dark
        theme = "light"
    }
    document.documentElement.className = theme;
    localStorage.setItem('theme', theme);
}

function triggerLang(e, el, app) {
    // Toggle theme color
    var lang = el.dataset.lang;
    if (!lang) return
    updateLang(app, lang);
}

