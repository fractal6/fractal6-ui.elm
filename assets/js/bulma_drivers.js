/*
 * Fractale - Self-organisation for humans.
 * Copyright (C) 2026 Fractale Co
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

import { replaceRange, getCaretCoordinates } from './textutils'

// The container where the burgers should close when a clicl "anywhere" occurs
const closeOnClickBurger = ['userMenu']; // data-target of burger

// Dropdowns driven by this file (`.elm` ones are controlled by Elm).
const DROPDOWN_SELECTOR = '.dropdown:not(.is-hoverable):not(.elm), .has-dropdown:not(.is-hoverable)';

export function InitBulma(app, session, eltId) {
    var handlers = session.bulmaHandlers;
    if (!eltId)
        console.log(`Activate Bulma driver (%d)...`, handlers.length);

    // This timeout is needed when bulma driver is called by elm Cmd,
    // to wait for the Html Msg to be updated by elm in order
    // to have new node accessible by Javascript.
    //document.addEventListener('DOMContentLoaded', () => {
    setTimeout(BulmaDriver, 333, app, eltId, handlers);
    //});
}

export function catchEsc(e, fun, ...args) {
    if (e.key === 'Esc' || e.key === 'Escape') {
        fun(e, ...args);
    }
}

export function catchEnter(e, fun, ...args) {
    if (e.key === 'Enter') {
        fun(e, ...args);
    }
}

// Theme preference: "system" (default) | "light" | "dark".
// "system" sets no attribute, so the prefers-color-scheme rules in the stylesheet apply.
export function getThemePref() {
    var pref = localStorage.getItem('theme');
    return (pref === 'light' || pref === 'dark') ? pref : 'system';
}

// Mirrored by the inline boot script in public/index.html (runs before first paint).
export function applyTheme(pref) {
    var root = document.documentElement;
    if (pref === 'light' || pref === 'dark') {
        root.setAttribute('data-theme', pref);
    } else {
        root.removeAttribute('data-theme');
    }
}

export function updateLang(app, lang) {
    localStorage.setItem('lang', lang);
    app.ports.updateLangFromJs.send(lang);
    setTimeout(() => {
        var loc = window.location;
        window.location.replace(
            loc.protocol + '//' + loc.host + "/" + lang.toLowerCase() + loc.pathname + loc.search
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
    } else {
        var targetEl = document.getElementById(target);
        if (!targetEl) {
            console.log("Bulma init target not found");
            return
        }
        // @DEBUG: document handler may be added several times here...
        // --
        // Use parentNode to be sure to not miss the target in the case
        // where the eltId is defined at the same level of the wanted selector.
        $doc = targetEl.parentNode;
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
                evt = "keydown";
                _hdl_ = e => catchEsc(e, hdl, ...objs);
            } else if (evt === "enter") {
                evt = "keydown";
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
        for (var i = 0; i < handlers.length; i++) {
            if (evt === handlers[i][0] && hdl.name === handlers[i][1].name && elt === handlers[i][2]) {
                return true
            }
        }
        return false
    }

    // Remove handlers of document and not connected elements.
    var idxToRemove = [];
    for (var i = 0; i < handlers.length; i++) {
        var evt = handlers[i][0];
        var elt = handlers[i][2];
        var func = handlers[i][3];
        if (elt == document || !elt.isConnected) {
            elt.removeEventListener(evt, func)
            idxToRemove.push(i)
        }
    }
    for (var i = idxToRemove.length - 1; i >= 0; i--)
        handlers.splice(idxToRemove[i], 1);


    //////////////////// Setup Bulma Components ////////////////////

    // Special Elm function

    const $themeTrigger = $doc.querySelectorAll('.themeTrigger');
    if ($themeTrigger.length > 0) {
        $themeTrigger.forEach(el => {
            setupHandler("click", triggerTheme, el, el, app);
        });
    }

    const $passwordViz = $doc.querySelectorAll('.passwordVisibilityTrigger');
    if ($passwordViz.length > 0) {
        $passwordViz.forEach(el => {
            setupHandler("click", triggerPasswerodViz, el, el, app);
        });
    }

    const $langTrigger = $doc.querySelectorAll('.langTrigger');
    if ($langTrigger.length > 0) {
        $langTrigger.forEach(el => {
            setupHandler("click", triggerLang, el, el, app);
        });
    }

    const $helpTrigger = $doc.querySelectorAll('.helpTrigger');
    if ($helpTrigger.length > 0) {
        $helpTrigger.forEach(el => {
            setupHandler("click", triggerHelp, el, el, app);
        });
    }

    const $joinTrigger = $doc.querySelectorAll('.joinTrigger');
    if ($joinTrigger.length > 0) {
        $joinTrigger.forEach(el => {
            setupHandler("click", triggerJoin, el, el, app);
        });
    }

    const $joinTrigger2 = $doc.querySelectorAll('.joinPendingTrigger');
    if ($joinTrigger2.length > 0) {
        $joinTrigger2.forEach(el => {
            setupHandler("click", triggerJoin2, el, el, app);
        });
    }

    const $inviteTrigger = $doc.querySelectorAll('.inviteTrigger');
    if ($inviteTrigger.length > 0) {
        $inviteTrigger.forEach(el => {
            setupHandler("click", triggerInvite, el, el, app);
        });
    }

    const $menuOrgaTrigger = $doc.querySelectorAll('.menuOrgaTrigger');
    if ($menuOrgaTrigger.length > 0) {
        $menuOrgaTrigger.forEach(el => {
            setupHandler("click", triggerMenuOrga, el, el, app);
        });
    }

    const $menuTreeTrigger = $doc.querySelectorAll('.menuTreeTrigger');
    if ($menuTreeTrigger.length > 0) {
        $menuTreeTrigger.forEach(el => {
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
    $autofocuses.forEach(el => {
        el.focus();
        el.classList.remove('autofocus'); // Remove the autofocus class after focusing to allow focus to the next one...
        return true;
    });
}

    //
    // Submit data
    //
    // * listen for ctrl+enter to submit data
    //
    const $submitFocuses = $doc.querySelectorAll('.submitFocus');
    if ($submitFocuses.length > 0) {
        $submitFocuses.forEach(el => {
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
        $followFocuses.forEach(el => {
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
        $clips.forEach(el => {
            setupHandler("click", copyToClipboard, el, el);
        });
    }

    //
    // "Rich Text" make checkbox readonly
    //
    //
    const $checkboxes = $doc.querySelectorAll('.checkbox_readonly');
    if ($checkboxes.length > 0) {
        $checkboxes.forEach(el => {
            setupHandler("click", (e, el, app) => {

                // Find the first parent with the class "message"
                let parentMessage = el.closest('.message');
                if (parentMessage) {
                    // Extract the ID of the parent element
                    let cid = parentMessage.id;

                    // Get all checkboxes within the parent element
                    let checkboxes = parentMessage.querySelectorAll('input[type="checkbox"]');

                    // Find the position of the clicked checkbox
                    let position = Array.from(checkboxes).indexOf(el);
                    let isChecked = el.checked;

                    app.ports.checkboxFromJs.send({ isChecked: isChecked, position: position, cid: cid });
                }

                e.preventDefault();
                return
            }, el, el, app);
        });
    }

    //
    // "Rich Text" on textarea
    //
    // * Capture TAB to insert space
    // * List continuation
    // * Search box interactions
    //
    const $textareas = $doc.querySelectorAll('.textarea');
    if ($textareas.length > 0) {
        $textareas.forEach(el => {
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
        $burgers.forEach(el => {
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
    // * close the others, and close all on a click outside
    // * close on Escape
    //
    // Delegated on document: dropdowns are resolved at click time, so handlers
    // survive any re-render (elements rendered after this driver run included).
    setupHandler("click", dropdownDelegate, document);
    setupHandler("esc", closeDropdowns, document);

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
        $modal_triggers.forEach(el => {
            setupHandler("mousedown", triggerModal, el, el);
        });
    }
    if ($modal_esc.length > 0) {
        $modal_esc.forEach(el => {
            var modalId = el.dataset.modal;
            if (!modalId) {
                console.error("modal-escape element missing data-modal attribute:", el);
                return
            }
            var $modal = document.getElementById(modalId);
            if (!$modal) {
                console.error("modal-escape data-modal references unknown element:", modalId);
                return
            }
            setupHandler("esc", closeModal, document, $modal, app);
        });
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
    } else if (e.key == "Tab" && !e.shiftKey && !e.ctrlKey) {
        var s = el.querySelector(".defaultSubmit")
        if (s) {
            var $active = document.activeElement;
            if ($active && $active.dataset.nextfocus) { return true }
            e.preventDefault();
            s.focus();
        }
    };
}

//
// Focus methods
//
//

function moveFocus(e, el) {
    if ((e.key == "Enter" || e.key == "Tab") && !e.ctrlKey) {
        var $t = document.getElementById(el.dataset.nextfocus);
        if ($t) {
            e.preventDefault(); // prevent a line break on the next text area.
            e.stopPropagation(); // stop furher event execution.
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
// """ Markup Rich Text (On Input)"""
//

export function markupRichText(e, el, app) {

    // Skip during IME composition: the Enter that commits a Japanese/Chinese/
    // Korean candidate also fires keydown, and would wrongly trigger list
    // continuation mid-composition. keyCode 229 is the legacy fallback.
    if (e.isComposing || e.keyCode === 229) return;

    /*
     * User search input
     * tooltip helper.
     */

    const userTooltip = document.getElementById(el.id + "searchInput");
    const emojiTooltip = document.getElementById(el.id + "emojiInput");

    if (!isHidden(userTooltip) && !e.ctrlKey && !e.shiftKey && !e.altKey) {
        handlePickerKey(e, el, userTooltip, {
            prefix: "@",
            patternRegex: /@[\w-\.]*$/,
            patternChars: /[\w-\.]/,
            patternPort: app.ports.changePatternFromJs,
            arrowPort: app.ports.arrowFromJs,
            selectPort: app.ports.selectActiveItemFromJs,
            hide: () => hideSearchInput(userTooltip, app),
        });
    }
    else if (!isHidden(emojiTooltip) && !e.shiftKey && !e.altKey) {
        handlePickerKey(e, el, emojiTooltip, {
            prefix: ":",
            patternRegex: /:[\w-]*$/,
            patternChars: /[\w-]/,
            patternPort: app.ports.changeEmojiPatternFromJs,
            arrowPort: app.ports.arrowFromJs,
            selectPort: app.ports.selectActiveItemFromJs,
            hide: () => hideEmojiInput(emojiTooltip, app),
        });
    }

    // Handle toggle up tooltip
    if (e.key == "@" &&
        (el.selectionStart == 0 || [" ", "\n", "\t"].includes(el.value[el.selectionStart - 1]))) {
        // Show user search input
        showSearchInput(el, userTooltip, app);
    }

    // Handle toggle up emoji tooltip
    if (e.key == ":" &&
        (el.selectionStart == 0 || [" ", "\n", "\t"].includes(el.value[el.selectionStart - 1]))) {
        showEmojiInput(el, emojiTooltip, app);
    }

    /*
     * List Tabulations and
     * List completion on newline
     *
     */

    if (e.key == "Tab" && !e.ctrlKey && !e.shiftKey) {
        var start = el.selectionStart;
        var end = el.selectionEnd;

        if (start != end) {
            // Selection exists: indent all selected lines by adding 2 spaces at line starts
            e.preventDefault();
            e.stopPropagation();

            // Extend selection to the beginning of the first selected line
            var lineStart = el.value.substring(0, start).lastIndexOf("\n") + 1;
            var selected = el.value.substring(lineStart, end);
            var indented = selected.replace(/^/gm, "  ");

            replaceRange(el, lineStart, end, indented, lineStart, lineStart + indented.length);
        } else {
            // No selection: allow indentation in list/blockquote context
            var replacer;
            var offset = 0; // bacward index to insert the replacer text

            if (el.value.length < 3 || start == 0 || !["\n", " "].includes(el.value[start - 1])) return

            // Try to see if we are at the beginning of list/blockquote pattern
            var backward = el.value.slice(Math.max(0, start - 12), start);
            var isLineList = backward.search(/\n\s*[-\*\+] $|\n\s*- \[[x ]\] $|\n\s*[0-9]+\. $|\n\s*> $/) >= 0
            if (isLineList) {
                // Assumes we are in a **list content** or blockquote
                // 2 space for sublist indentation
                var lastIndex = backward.lastIndexOf("\n");
                var offset = backward.length - lastIndex - 1;
                replacer = "  ";
            } else {
                // /[^\S\r\n]/ -> all whitespace but without newline
                var isLastLineList = el.value.slice(Math.max(0, start - 500), start).search(/(^|\n)[^\S\r\n]*[0-9]+\. [^\n]*\n[^\S\r\n]*$|(^|\n)[^\S\r\n]*[\-\+\*] [^\n]*\n[^\S\r\n]*$|(^|\n)[^\S\r\n]*- \[[x ]\] [^\n]*\n[^\S\r\n]*$|(^|\n)[^\S\r\n]*> [^\n]*\n[^\S\r\n]*$/) >= 0
                if (isLastLineList) {
                    replacer = "  ";
                    //} else if (el.value.slice(el.selectionStart-2, el.selectionStart) == "\n\n") {
                    //    // Tab (4 space) for **code** indentation
                    //    replacer = "\t";
                } else {
                    return
                }
            }

            e.preventDefault();
            e.stopPropagation();

            replaceRange(el, start - offset, end - offset, replacer, start + replacer.length);
        }
    } else if (e.key == "Tab" && !e.ctrlKey && e.shiftKey) {
        // Shift+Tab: dedent selected lines by removing up to 2 leading spaces
        var start = el.selectionStart;
        var end = el.selectionEnd;
        if (start == end) return; // no selection, let default behavior handle it

        // Extend selection to the beginning of the first selected line
        var lineStart = el.value.substring(0, start).lastIndexOf("\n") + 1;
        var selected = el.value.substring(lineStart, end);

        e.preventDefault();
        e.stopPropagation();

        var dedented = selected.replace(/^( {1,2})/gm, "");
        replaceRange(el, lineStart, end, dedented, lineStart, lineStart + dedented.length);
    } else if (e.key == "Backspace" && !e.ctrlKey && !e.shiftKey) {
        // Remove indentation level on empty indented list items
        var start = el.selectionStart;
        var end = el.selectionEnd;
        if (start != end) return; // selection exists, let default behavior handle it

        var subvalue = el.value.slice(Math.max(0, start - 100), start);

        // Match empty list item: "\n- ", "\n  - ", "\n    1. ", "\n> ", etc.
        var indentedListMatch = subvalue.match(/\n([ \t]*)([-\*\+] |\d+\. |> |- \[ \] )$/);

        if (indentedListMatch) {
            var indent = indentedListMatch[1];
            var marker = indentedListMatch[2];
            var afterNlPos = start - indent.length - marker.length;

            e.preventDefault();
            if (indent.length >= 2) {
                // Dedent: drop 2 spaces of indentation, keep marker
                var newIndent = indent.slice(2);
                replaceRange(el, afterNlPos, end, newIndent + marker, start - 2);
            } else {
                // No indent left: remove the marker entirely
                replaceRange(el, afterNlPos, end, "", afterNlPos);
            }
        }
    } else if (e.key == "Enter" && !e.ctrlKey && !e.shiftKey) {
        // Insert list if inside list, or continue blockquote
        var start = el.selectionStart;
        var end = el.selectionEnd;

        if (el.value.length < 3 || start == 0) return

        // /[^\S\r\n]/ -> all whitespace but without newline
        var subvalue = el.value.slice(Math.max(0, start - 500), start);

        // Extract the cursor line - includes ordered lists, unordered lists, checkboxes, and blockquotes
        var currentLineStart = subvalue.search(/(^|\n)[^\S\r\n]*[0-9]+\. [^\n]*$|(^|\n)[^\S\r\n]*[\-\+\*] [^\n]*$|(^|\n)[^\S\r\n]*> [^\n]*$/)
        var replacer;

        if (currentLineStart >= 0) {
            // list/blockquote begins template
            var currentLine = subvalue.slice(currentLineStart)
            // Capture leading whitespace for indentation preservation
            var leadingWhitespace = currentLine.match(/^[\n]?([ \t]*)/)[1] || "";
            var trimmedLine = currentLine.trimStart();
            var s = trimmedLine.slice(0, 3)

            // Build current marker for empty line comparison
            var currentMarker;
            if (s.length >= 3 && s[2] == "[" && s[1] == " " && /[\-\*\+]/.test(s[0])) {
                currentMarker = s[0] + " [ ]";
                replacer = "\n" + leadingWhitespace + s[0] + " [ ] ";
            } else if (s.slice(0, 2) == "> ") {
                currentMarker = ">";
                replacer = "\n" + leadingWhitespace + "> ";
            } else if (/^\d+\. /.test(trimmedLine)) {
                var i = parseInt(s)
                currentMarker = i + ".";
                replacer = "\n" + leadingWhitespace + (i + 1) + ". ";
            } else {
                currentMarker = s[0];
                replacer = "\n" + leadingWhitespace + s[0] + " ";
            }

            // remove if empty (line contains only the list marker): exit list,
            // leaving a blank line between the last item and the cursor
            if ((trimmedLine.trim() == currentMarker) && start == end) {
                start -= currentLine.length;
                replacer = "\n\n";
            }
        } else {
            return
        }

        e.preventDefault();

        replaceRange(el, start, end, replacer, start + replacer.length);
    }
    // Do not allow non-breaking space
    else if (e.key == " ") {
        var start = el.selectionStart;
        var end = el.selectionEnd;
        if (start == end) {
            e.preventDefault();
            replaceRange(el, start, end, " ", start + 1);
        }
    }
}

export function showSearchInput(content, input, app) {
    if (!input) return
    const { x, y } = getCaretCoordinates(content, content.selectionStart);
    input.setAttribute("aria-hidden", "false");
    input.setAttribute("style", `display: inline-block; left: ${x}px; top: ${y + 30}px`);
    input.dataset.arrowMode = "false";
    installClickOutside(input, () => hideSearchInput(input, app));

    app.ports.openMembersFromJs.send(null);
}

export function hideSearchInput(input, app) {
    if (!input) return
    input.setAttribute("aria-hidden", "true");
    input.setAttribute("style", "display: none;");
    input.dataset.arrowMode = "false";
    removeClickOutside(input);

    app.ports.closeMembersFromJs.send(null);
}

export function showEmojiInput(content, input, app) {
    if (!input) return
    const { x, y } = getCaretCoordinates(content, content.selectionStart);
    input.setAttribute("aria-hidden", "false");
    input.setAttribute("style", `display: inline-block; left: ${x}px; top: ${y + 30}px`);
    input.dataset.arrowMode = "false";
    installClickOutside(input, () => hideEmojiInput(input, app));

    app.ports.openEmojiPickerFromJs.send(null);
}

export function hideEmojiInput(input, app) {
    if (!input) return
    input.setAttribute("aria-hidden", "true");
    input.setAttribute("style", "display: none;");
    input.dataset.arrowMode = "false";
    removeClickOutside(input);

    app.ports.closeEmojiPickerFromJs.send(null);
}

// Shared keydown handler for the @-mention and :emoji pickers.
// `opts` differs per picker: { prefix, patternRegex, patternChars, patternPort, arrowPort, selectPort, hide }
function handlePickerKey(e, el, tooltip, opts) {
    var start = el.selectionStart;
    var inArrowMode = tooltip.dataset.arrowMode === "true";

    if (inArrowMode) {
        if (e.key === "ArrowUp" || e.key === "ArrowDown" ||
            e.key === "ArrowLeft" || e.key === "ArrowRight") {
            e.preventDefault();
            e.stopPropagation();
            opts.arrowPort.send(e.key.replace("Arrow", "").toLowerCase());
            return
        }
        if (e.key === "Enter" || e.key === "Return" || e.key === "Tab") {
            e.preventDefault();
            e.stopPropagation();
            opts.selectPort.send(null);
            return
        }
        if (e.key === "Escape") e.stopPropagation();
        opts.hide();
        return
    }

    if (e.key === "ArrowDown" || e.key === "Tab") {
        e.preventDefault();
        e.stopPropagation();
        tooltip.dataset.arrowMode = "true";
        opts.arrowPort.send("down");
        return
    }

    if (e.key == " " ||
        e.key == "Enter" ||
        e.key == "Return" ||
        e.key == "Escape" ||
        e.key == "ArrowUp" ||
        e.key == "ArrowLeft" ||
        e.key == "ArrowRight" ||
        // Trigger char was just backspaced — exit
        (e.key == "Backspace" && el.value[start - 1] == opts.prefix)
    ) {
        if (e.key == "Escape") e.stopPropagation();
        opts.hide();
        return
    }

    var m = null;
    var extra = "";
    if (e.key === "Backspace") {
        m = el.value.slice(Math.max(0, start - 50), start - 1).match(opts.patternRegex);
    } else if (e.key.match(opts.patternChars)) {
        m = el.value.slice(Math.max(0, start - 50), start).match(opts.patternRegex);
        extra = e.key;
    }
    if (m) {
        opts.patternPort.send((m[m.length - 1] + extra).slice(1));
    }
}

// Close `el` when a mousedown happens outside of it.
function installClickOutside(el, onClose) {
    if (el._clickOutside) return
    const handler = (ev) => {
        if (!el.contains(ev.target)) onClose();
    };
    el._clickOutside = handler;
    document.addEventListener("mousedown", handler, true);
}

function removeClickOutside(el) {
    if (el && el._clickOutside) {
        document.removeEventListener("mousedown", el._clickOutside, true);
        el._clickOutside = null;
    }
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

// Toggle the clicked dropdown, close the others (a click outside closes all).
function dropdownDelegate(e) {
    var btn = e.target.closest(DROPDOWN_SELECTOR);
    document.querySelectorAll(DROPDOWN_SELECTOR).forEach(function(el) {
        // keep ancestors of the clicked dropdown open (nested dropdowns)
        if (el !== btn && !el.contains(btn)) {
            el.classList.remove('is-active');
        }
    });
    if (btn) {
        btn.classList.toggle('is-active');
    }
}

// Close all dropdown by removing `is-active` class.
function closeDropdowns(e) {
    document.querySelectorAll(DROPDOWN_SELECTOR).forEach(function(el) {
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

    if (!modal) {
        console.error("closeModal: modal element is null (missing data-modal attribute?)");
        return
    }

    if (!modal.classList.contains("is-active")) {
        return
    }

    // Skip closing the modal if a search panel (labels, users, emoji, etc.) is open inside it.
    // The panel's own Escape handler will close just the panel.
    var panels = modal.querySelectorAll(".panel.dropList");
    var hasOpenPanel = Array.from(panels).some(el => !isHidden(el));
    if (hasOpenPanel) {
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
        app.ports[closeMsg].send({ reset: false, link: "" });
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
    var theme = el.dataset.themePref;
    if (!theme) return
    localStorage.setItem('theme', theme);
    applyTheme(theme);
    app.ports.flushGraphPackFromJs.send(null)
    app.ports.updateThemeFromJs.send(theme);
}

function triggerPasswerodViz(e, el, app) {
    // Find the password input by looking at siblings within the parent container
    const inputContainer = el.parentNode;
    const passwordInput = inputContainer.querySelector('input[type="password"], input[type="text"]');

    if (passwordInput) {
        // Toggle between password and text type
        passwordInput.type = passwordInput.type === "password" ? "text" : "password";

        // Optionally update the icon if needed
        const iconElement = el.querySelector('i');
        if (iconElement) {
            if (passwordInput.type === 'text') {
                iconElement.classList.remove('icon-eye');
                iconElement.classList.add('icon-eye-off');
            } else {
                iconElement.classList.remove('icon-eye-off');
                iconElement.classList.add('icon-eye');
            }
        }
    }
}

function triggerLang(e, el, app) {
    // Toggle system language
    var lang = el.dataset.lang;
    if (!lang) return
    updateLang(app, lang);
}

