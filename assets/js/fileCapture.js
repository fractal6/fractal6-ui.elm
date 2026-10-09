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

// Delegated paste/drop capture on [data-paste-capture] elements -> pastedFilesFromJs.
export function initFileCapture(app) {
    // Natural size of a pasted image, as "WxH" ("" for anything else).
    // Elm carries it in the `![|WxH](name)` placeholder so the browser can
    // reserve the layout box before the uploaded image is downloaded.
    function imageDims(url, type) {
        if (!url || !type || type.indexOf('image/') !== 0) return Promise.resolve('');
        return new Promise(function (resolve) {
            var img = new Image();
            img.onload = function () {
                resolve(img.naturalWidth && img.naturalHeight ? img.naturalWidth + 'x' + img.naturalHeight : '');
            };
            img.onerror = function () { resolve(''); };
            img.src = url;
        });
    }

    // Paste/drop-capture: any element with [data-paste-capture] forwards
    // clipboard or dropped files to Elm. The element id (if any)
    // is sent so the receiver can disambiguate multiple editors.
    function sendCapturedFiles(e, target, dataTransfer) {
        var items = dataTransfer.items || [];
        var files = [];
        var objectUrls = [];
        // Stamp is shared within a single event; the per-file
        // index keeps names unique. Date.now() is live (unlike a stale
        // model.session.now relayed through Elm), so two events
        // ≥1ms apart never collide.
        var stamp = Date.now();
        for (var i = 0; i < items.length; i++) {
            if (items[i].kind === 'file') {
                var f = items[i].getAsFile();
                if (f) {
                    // Pick an extension from the original name first,
                    // then fall back to the MIME subtype. The backend
                    // sniffs the actual MIME server-side; we just need
                    // a stable suffix.
                    var ext = '';
                    var origName = f.name || '';
                    var dot = origName.lastIndexOf('.');
                    if (dot > -1) {
                        ext = origName.substring(dot);
                    } else if (f.type && f.type.indexOf('/') > -1) {
                        ext = '.' + f.type.split('/')[1].toLowerCase();
                    }
                    var fname = 'paste-' + stamp + '-' + i + ext;
                    // Rebuild the File so the multipart upload sends
                    // OUR filename — the backend's rewrite logic looks
                    // up `![](<filename>)` placeholders by it.
                    var renamed = new File([f], fname, {
                        type: f.type,
                        lastModified: f.lastModified || stamp,
                    });
                    files.push(renamed);
                    objectUrls.push(URL.createObjectURL(renamed));
                }
            }
        }
        if (files.length === 0) return;
        e.preventDefault();
        Promise.all(files.map(function (f, j) { return imageDims(objectUrls[j], f.type); })).then(function (dims) {
            app.ports.pastedFilesFromJs.send({
                targetId: target.id || '',
                files: files,
                objectUrls: objectUrls,
                dims: dims,
                isPaste: true,
            });
        });
    }

    function captureTarget(e) {
        var t = e.target;
        if (!t || typeof t.matches !== 'function') return null;
        if (!t.matches('[data-paste-capture]')) return null;
        return t;
    }

    document.addEventListener('paste', function (e) {
        var t = captureTarget(e);
        if (t && e.clipboardData) sendCapturedFiles(e, t, e.clipboardData);
    });

    // dragover must be prevented for the drop event to fire at all.
    document.addEventListener('dragover', function (e) {
        var t = captureTarget(e);
        // Text/link drags carry no files: leave them to the browser's native handling.
        if (!t || !e.dataTransfer || !Array.prototype.includes.call(e.dataTransfer.types || [], 'Files')) return;
        e.preventDefault();
        e.dataTransfer.dropEffect = 'copy';
        t.classList.add('is-dragover');
    });

    document.addEventListener('dragleave', function (e) {
        var t = captureTarget(e);
        if (t) t.classList.remove('is-dragover');
    });

    // Dropped files are staged as attachments (chips), not inline placeholders:
    // their own filename is kept, so no renaming and no blob preview.
    document.addEventListener('drop', function (e) {
        var t = captureTarget(e);
        if (!t) return;
        t.classList.remove('is-dragover');
        var files = e.dataTransfer ? Array.prototype.slice.call(e.dataTransfer.files || []) : [];
        if (files.length === 0) return;
        e.preventDefault();
        app.ports.pastedFilesFromJs.send({
            targetId: t.id || '',
            files: files,
            objectUrls: [],
            dims: [],
            isPaste: false,
        });
    });
}
