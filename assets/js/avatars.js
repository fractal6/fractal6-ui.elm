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

//
// User avatars: upload re-encoding and the shared user hover card (see docs/avatars.md).
//

export const AVATAR_SIZE = 256;
const SHOW_DELAY = 500; // same as the comment reaction hover
const HIDE_DELAY = 200; // time to reach the card from its anchor
const GAP = 8;

// Port of dividat/elm-identicon (One-at-a-Time hash, quirks included), so the canvas draws the
// same identicons as the HTML views.
export function identiconHash(s) {
    let h = 0;
    const chars = Array.from(s);
    for (let i = chars.length - 1; i >= 0; i--) { // List.foldr
        let x = chars[i].codePointAt(0) + h;
        x = x + (10 << x); // Bitwise.shiftLeftBy x 10
        h = x ^ (6 >> x);
    }
    h = h + (3 << h);
    h = h ^ (11 >> h);
    return h + (15 << h);
}

// LRU of the last identicons: the graphpack redraws them on every frame.
const IDENTICON_CACHE_SIZE = 50;
const identiconCache = new Map();

export function identicon(username) {
    let ic = identiconCache.get(username);
    if (ic) {
        identiconCache.delete(username); // re-inserted below as the most recent
    } else {
        ic = computeIdenticon(username);
        if (identiconCache.size >= IDENTICON_CACHE_SIZE) identiconCache.delete(identiconCache.keys().next().value);
    }
    identiconCache.set(username, ic);
    return ic;
}

// 5x5 grid, left half mirrored: cells [x, y] filled with `color`.
function computeIdenticon(username) {
    const hash = identiconHash(username);
    const cells = [];
    for (let i = 0; i < 15; i++) {
        if ((hash >> i) % 2 === 0) {
            const x = Math.floor(i / 5), y = i % 5;
            cells.push([x, y], [4 - x, y]);
        }
    }
    return { color: `hsl(${((hash % 360) + 360) % 360}, 50%, 70%)`, cells };
}

function toBlob(canvas, type, quality) {
    return new Promise(resolve => canvas.toBlob(resolve, type, quality));
}

// Center-crop to a square, scale to `size`, re-encode as WebP (JPEG where unsupported).
// Applies the EXIF orientation and drops the metadata. Rejects when the image can't be decoded.
export async function reencodeAvatar(file, size = AVATAR_SIZE) {
    const bmp = await createImageBitmap(file, { imageOrientation: 'from-image' });
    const s = Math.min(bmp.width, bmp.height);
    const canvas = document.createElement('canvas');
    canvas.width = canvas.height = size;
    const ctx = canvas.getContext('2d');
    ctx.drawImage(bmp, (bmp.width - s) / 2, (bmp.height - s) / 2, s, s, 0, 0, size, size);
    if (bmp.close) bmp.close();

    let blob = await toBlob(canvas, 'image/webp', 0.88);
    // Safari silently returns PNG for WebP.
    if (!blob || blob.type !== 'image/webp') {
        // JPEG has no alpha: fill behind, transparent pixels turn black otherwise.
        ctx.globalCompositeOperation = 'destination-over';
        ctx.fillStyle = '#fff';
        ctx.fillRect(0, 0, size, size);
        blob = await toBlob(canvas, 'image/jpeg', 0.88);
    }
    if (!blob) throw new Error('avatar encoding failed');
    const ext = blob.type === 'image/webp' ? 'webp' : 'jpg';
    return new File([blob], 'avatar.' + ext, { type: blob.type });
}

// Settings file input ([data-avatar-input]) -> re-encode -> avatarFileFromJs ({file} or {error}).
export function initAvatarCapture(app) {
    document.addEventListener('change', e => {
        const t = e.target;
        if (!t || typeof t.matches !== 'function' || !t.matches('[data-avatar-input]')) return;
        const f = t.files && t.files[0];
        t.value = ''; // so picking the same file again fires
        if (!f) return;
        reencodeAvatar(f).then(
            file => app.ports.avatarFileFromJs.send({ file }),
            err => app.ports.avatarFileFromJs.send({ error: String(err) }),
        );
    });
}

//
// User hover card: one instance rendered by Global.elm (#userCard), shown/placed from here.
//

const card = {
    app: null,
    showTimer: null,
    hideTimer: null,
    pressed: false, // press started on the card (e.g. text selection): keep it open
    username: null, // shown card
    rect: null, // anchor rect, viewport coordinates
    ro: null,
    observed: null,
};

// Below the anchor, flipped above when it doesn't fit, clamped to the viewport.
export function cardPosition(rect, w, h, vw, vh) {
    const left = Math.max(GAP, Math.min(rect.left, vw - w - GAP));
    let top = rect.bottom + GAP;
    if (top + h > vh - GAP && rect.top - GAP - h >= GAP) top = rect.top - GAP - h;
    return { left, top };
}

function placeCard() {
    const $card = document.getElementById('userCard');
    if (!$card || !card.rect || $card.dataset.username !== card.username) return false;
    const p = cardPosition(card.rect, $card.offsetWidth, $card.offsetHeight, window.innerWidth, window.innerHeight);
    $card.style.left = p.left + 'px';
    $card.style.top = p.top + 'px';
    $card.classList.add('is-placed');
    // Re-place when the profile lands and the card grows.
    if (card.ro && card.observed !== $card) {
        card.ro.disconnect();
        card.ro.observe($card);
        card.observed = $card;
    }
    return true;
}

// Wait for Elm to render the card for this user.
function placeWhenRendered(tries = 20) {
    requestAnimationFrame(() => {
        if (card.username && !placeCard() && tries > 0) placeWhenRendered(tries - 1);
    });
}

// `rect`: anchor bounding rect in viewport coordinates (DOM avatar or graphpack disc).
export function showUserCard(username, rect) {
    clearTimeout(card.hideTimer);
    card.hideTimer = null;
    if (username === card.username) return;
    clearTimeout(card.showTimer);
    card.showTimer = setTimeout(() => {
        card.showTimer = null;
        card.username = username;
        card.rect = rect;
        card.app.ports.userCardFromJs.send(username);
        placeWhenRendered();
    }, SHOW_DELAY);
}

export function hideUserCard(delay = HIDE_DELAY) {
    clearTimeout(card.showTimer);
    card.showTimer = null;
    if (!card.username || card.hideTimer) return;
    card.hideTimer = setTimeout(() => {
        card.hideTimer = null;
        card.username = null;
        card.app.ports.userCardFromJs.send(null);
    }, delay);
}

const ANCHOR = '[data-user-card], #userCard';

// Delegated hover on [data-user-card] elements. The card stays open while the pointer is on it.
export function initUserCard(app) {
    card.app = app;
    if (typeof ResizeObserver !== 'undefined') card.ro = new ResizeObserver(() => placeCard());

    document.addEventListener('pointerover', e => {
        if (e.pointerType === 'touch') return; // no hover: avatars keep linking to the profile
        const t = e.target;
        if (!t || typeof t.closest !== 'function') return;
        const el = t.closest(ANCHOR);
        if (!el) return;
        if (el.id === 'userCard' || el.closest('#userCard')) {
            clearTimeout(card.hideTimer);
            card.hideTimer = null;
            return;
        }
        showUserCard(el.dataset.userCard, el.getBoundingClientRect());
    });

    document.addEventListener('pointerout', e => {
        if (e.pointerType === 'touch' || card.pressed) return;
        const t = e.target;
        if (!t || typeof t.closest !== 'function') return;
        const from = t.closest(ANCHOR);
        if (!from) return;
        const to = e.relatedTarget;
        if (to && typeof to.closest === 'function' && to.closest(ANCHOR) === from) return;
        hideUserCard();
    });

    // Interacting with the card keeps it; a press elsewhere closes it.
    document.addEventListener('pointerdown', e => {
        card.pressed = !!(e.target && typeof e.target.closest === 'function' && e.target.closest('#userCard'));
        if (!card.pressed) hideUserCard(0);
    });
    document.addEventListener('pointerup', () => { card.pressed = false; });

    // The card is anchored in fixed coordinates.
    window.addEventListener('scroll', () => hideUserCard(0), { passive: true, capture: true });
}
