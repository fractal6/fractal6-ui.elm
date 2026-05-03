// Textarea / input utilities shared across the UI layer.
//
// Kept in a dedicated module so the bulma_drivers and ports files don't have
// to grow their own copies. Only generic <textarea>/<input> helpers belong
// here — anything tied to a specific picker or feature stays in its own file.

// Replace a range in a textarea while preserving native undo when possible.
//
// Tries `document.execCommand('insertText', ...)` first: it is deprecated but
// remains the only cross-browser way to mutate a <textarea> without wiping
// the Ctrl+Z stack, and every shipping browser still implements it (no spec'd
// replacement exists). If it returns false or throws — future removal,
// hardened context, headless quirks — we fall back to direct `el.value`
// assignment, which works but loses the native undo history for that edit.
//
// Args:
//   start, end   range in the current `el.value` to replace
//   text         string to insert in place of [start, end)
//   caretStart   optional final selectionStart (defaults: start + text.length)
//   caretEnd     optional final selectionEnd   (defaults: caretStart)
export function replaceRange(el, start, end, text, caretStart, caretEnd) {
    var ok = false;
    try {
        el.focus();
        el.setSelectionRange(start, end);
        ok = document.execCommand('insertText', false, text);
    } catch (_) {
        ok = false;
    }
    if (!ok) {
        el.value = el.value.substring(0, start) + text + el.value.substring(end);
        el.dispatchEvent(new Event('input', { bubbles: true, cancelable: true }));
    }
    if (caretStart != null) {
        el.selectionStart = caretStart;
        el.selectionEnd = caretEnd != null ? caretEnd : caretStart;
    }
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
        if (prop == "width" || prop == "maxwidth") continue
        div.style[prop] = copyStyle[prop]
    }
    div.style.removeProperty('min-inline-size');
    div.style.removeProperty('min-width');
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
    span.style.maxwidth = "100px";
    span.style.width = "100px";
    // append the span marker to the div
    div.appendChild(span)
    // append the dummy element to the body
    document.body.appendChild(div)
    // get the marker position, this is the caret position top and left relative to the input
    var { offsetLeft: spanX, offsetTop: spanY } = span
    // Adjust the offset for overlaping lines
    if (span.offsetLeft > content.offsetWidth) {
        // This is obsolete once "min-width" property has been removed
        spanX = span.offsetLeft % content.offsetWidth;
    }

    // lastly, remove that dummy element
    // NOTE:: can comment this out for debugging purposes if you want to see where that span is rendered
    document.body.removeChild(div)
    // return an object with the x and y of the caret. account for input positioning so that you don't need to wrap the input
    return {
        x: inputX + spanX - content.scrollLeft,
        y: inputY + spanY - content.scrollTop,
    }
}
