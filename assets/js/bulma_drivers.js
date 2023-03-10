/*
 * Fractale - Self-organisation for humans.
 * Copyright (C) 2023 Fractale Co
 *
 * This file is part of Fractale.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
 */

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
		// Maybe try this to force reoload ?
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
        var nvt = document.getElementById("navbarTop");
        if (nvt) {
            nvt.classList.remove('has-modal-active');
            nvt.classList.remove('has-modal-active2');
        }
    } else if (document.getElementById(target)) {
        // @DEBUG: document handler may be added several times here...
        // --
        // Use parentNode to be sure to not miss the target in the case
        // where the eltId is defined at the same level of the wanted selector.
        $doc = document.getElementById(target).parentNode;
    } else {
        console.log("Bulma init target not found");
        return
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
    // Copy to clipboard
    //
    // * Copy data to clibpard
    //
    const $clips = $doc.querySelectorAll('[data-clipboard]');
    if ($clips.length > 0) {
        $clips.forEach( el => {
            setupHandler("click", copyToClipboard, el, el);
        });
    }

    //
    // "Rich Text" make checkbox readonly
    //
    //
    const $checkboxes = $doc.querySelectorAll('.checkbox_readonly');
    if ($checkboxes.length > 0) {
        $checkboxes.forEach( el => {
            setupHandler("click", (a,b) => {a.preventDefault(); return false} , el, el);
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
            setupHandler("keydown", markupRichText, el, el, app);
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
// Copy to clipboard
//

function copyToClipboard(e, el) {
    var text = el.dataset.clipboard;
    navigator.clipboard.writeText(text);
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
// """ Markup Rich Text"""
//

function markupRichText(e, el, app) {

    /*
     * User search input
     * tooltip helper.
     */

    const userTooltip = document.getElementById(el.id + "searchInput");

    // Handle backspace/removing charater
    if (!isHidden(userTooltip)) {

        // Handle toggle down tooltip
        if (e.key == " " || e.key == "\n" || e.key == "Enter") {
            hideSearchInput(userTooltip, app);
            return
        }

        var start = el.selectionStart;

        // Handle update patter/input
        //var m = el.value.slice(Math.max(0, start-50), start).search(/(^|\n| )@[\w-\.]*$/)
        if (e.key === "Backspace" && el.value[start-1] == "@") { // Check if @ keyword has been deleted
            // Hide tooltip
            hideSearchInput(userTooltip, app);
        } else {
            // update pattern
            var pattern = "";
            var extra = "";
            var m = null;
            if (e.key === "Backspace") {
                m = el.value.slice(Math.max(0, start-50), start-1).match(/@[\w-\.]*$/);
            } else if (e.key.match(/[\w-\.]/)) {
                m = el.value.slice(Math.max(0, start-50), start).match(/@[\w-\.]*$/);
                extra = e.key;
            }

            if (m) {
                pattern = m[m.length -1] + extra;
                pattern = pattern.slice(1);
                app.ports.changePatternFromJs.send(pattern);
            }
        }
    }

    // Handle toggle up tooltip
    if (e.key == "@" &&
        (el.selectionStart == 0 || [" ", "\n"].includes(el.value[el.selectionStart-1]))) {
        // Show user search input
        showSearchInput(el, userTooltip, app);
    }


    /*
     * Tabulations and
     * List completion on newline
     *
     */

    if (e.key == "Tab" && !e.ctrlKey && !e.shiftKey) {
        // Allow indentions (as space) - usefull to enter list level
        // or indent for code - after two line break
        var start = el.selectionStart;
        var end = el.selectionEnd;
        var replacer;

        if (el.value.length < 3 || start == 0 || !["\n", " "].includes(el.value[start-1])) return

        // /[^\S\r\n]/ -> all whitespace but without newline
        var isLastLineList = (el.value.slice(Math.max(0, start-500), start).search(/(^|\n)[^\S\r\n]*[0-9]+\. [^\n]*\n[^\S\r\n]*$|(^|\n)[^\S\r\n]*[\-\+\*] [^\n]*\n[^\S\r\n]*$/) >= 0)

        if (isLastLineList) {
            // Assumes we are in a **list content**
            // 2 space for sublist indentation
            replacer = "  ";
        //} else if (el.value.slice(el.selectionStart-2, el.selectionStart) == "\n\n") {
        //    // Tab (4 space) for **code** indentation
        //    replacer = "\t";
        } else {
            return
        }

		e.preventDefault();

		// set textarea value to: text before caret + tab + text after caret
		el.value = el.value.substring(0, start) +
			replacer + el.value.substring(end);

		// put caret at right position again
		el.selectionStart =
			el.selectionEnd = start + replacer.length;
    } else if (e.key == "Enter" && !e.ctrlKey && !e.shiftKey) {
        // Insert list if inside list
        var start = el.selectionStart;
        var end = el.selectionEnd;

        if (el.value.length < 3 || start == 0) return

        // /[^\S\r\n]/ -> all whitespace but without newline
        var currentLineList = el.value.slice(Math.max(0, start-500), start).search(/(^|\n)[^\S\r\n]*?[0-9]+\. [^\n]*?$|(^|\n)[^\S\r\n]*?[\-\+\*] [^\n]*?$/)
        var replacer;

        if (currentLineList >= 0) {
            var s = el.value.slice(currentLineList, currentLineList+10).trimLeft().slice(0, 3)
            if (s == "- [") {
                replacer = "\n" + "- [ ] ";
            } else if (parseInt(s)) {
                var i = parseInt(s)
                replacer = "\n" + (i+1) + ". ";
            } else {
                replacer = "\n" + s[0] + " ";
            }
        //} else if (el.value.slice(el.selectionStart-2, el.selectionStart) == "\n\n") {
        //    // Tab (4 space) for **code** indentation
        //    var replacer = "\t";
        } else {
            return
        }

		e.preventDefault();

		// set textarea value to: text before caret + tab + text after caret
		el.value = el.value.substring(0, start) +
			replacer + el.value.substring(end);

		// put caret at right position again
		el.selectionStart =
			el.selectionEnd = start + replacer.length;
    }
    // Breaking space or not ???
    //else if (e.key ==  '\xa0') { // Non-breakable space is char 0xa0 (160 dec)
    //    el.value = "x";
    //}
}

/**
 * returns x, y coordinates for absolute positioning of a span within a given text input
 * at a given selection point
 * @param {object} input - the input element to obtain coordinates for
 * @param {number} selectionPoint - the selection point for the input
 * https://gist.github.com/jh3y/6c066cea00216e3ac860d905733e65c7#file-getcursorxy-js
 */
export function getCaretCoordinates(content, selectionPoint) {
  const {
    offsetLeft: inputX,
    offsetTop: inputY,
  } = content
  // create a dummy element that will be a clone of our input
  const div = document.createElement('div')
  // get the computed style of the input and clone it onto the dummy element
  const copyStyle = getComputedStyle(content)
  for (const prop of copyStyle) {
    div.style[prop] = copyStyle[prop]
  }
  // we need a character that will replace whitespace when filling our dummy element if it's a single line <input/>
  const swap = '.'
  const inputValue = content.tagName === 'INPUT' ? content.value.replace(/ /g, swap) : content.value
  // set the div content to that of the textarea up until selection
  const textContent = inputValue.substr(0, selectionPoint)
  // set the text content of the dummy element div
  div.textContent = textContent
  if (content.tagName === 'TEXTAREA') div.style.height = 'auto'
  // if a single line input then the div needs to be single line and not break out like a text area
  if (content.tagName === 'INPUT') div.style.width = 'auto'
  // create a marker element to obtain caret position
  const span = document.createElement('span')
  // give the span the textContent of remaining content so that the recreated dummy element is as close as possible
  span.textContent = inputValue.substr(selectionPoint) || '.'
  // append the span marker to the div
  div.appendChild(span)
  // append the dummy element to the body
  document.body.appendChild(div)
  // get the marker position, this is the caret position top and left relative to the input
  const { offsetLeft: spanX, offsetTop: spanY } = span
  // lastly, remove that dummy element
  // NOTE:: can comment this out for debugging purposes if you want to see where that span is rendered
  document.body.removeChild(div)
  // return an object with the x and y of the caret. account for input positioning so that you don't need to wrap the input
  return {
    x: inputX + spanX,
    y: inputY + spanY,
  }
}

export function showSearchInput(content, input, app) {
    if (!input) return
    const { x, y } = getCaretCoordinates(content, content.selectionStart);
    input.setAttribute("aria-hidden", "false");
    input.setAttribute( "style", `display: inline-block; left: ${x}px; top: ${y + 30}px`);

    app.ports.openMembersFromJs.send(null);
}

export function hideSearchInput(input, app) {
    if (!input) return
    input.setAttribute("aria-hidden", "true");
    input.setAttribute("style", "display: none;");

    app.ports.closeMembersFromJs.send(null);
}

// Where el is the DOM element you'd like to test for visibility.
// Shouldn't work for position:fixed element.
export function isHidden(el) {
    if (!el) return true
    return (el.offsetParent === null)
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
    var nvt = document.getElementById("navbarTop");
    if (nvt) {
        nvt.classList.add('has-modal-active');
    }
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
        var nvt = document.getElementById("navbarTop");
        if (nvt) {
            nvt.classList.remove('has-modal-active');
        }
    }
}

//
// Other Triggers
//

function triggerHelp(e, el, app) {
    var v = el.dataset.help;
    if (!v) v = "";
    app.ports.triggerHelpFromJs.send(v)
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
    app.ports.flushGraphPackFromJs.send(null)
}

function triggerLang(e, el, app) {
    // Toggle theme color
    var lang = el.dataset.lang;
    if (!lang) return
    updateLang(app, lang);
}

