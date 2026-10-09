/*
 * Avatar upload re-encoding and the user hover card (assets/js/avatars.js).
 */

import { reencodeAvatar, cardPosition, initUserCard, identicon, identiconHash } from '../../assets/js/avatars.js';

describe('identicon (port of dividat/elm-identicon)', () => {
    test('same hash as Identicon.defaultHash (values from elm repl)', () => {
        expect(['bob', 'alice-02', 'dtrckd', '\u00e9lodie'].map(identiconHash))
            .toEqual([4093641051, -1454895947, -903019588, 2786398]);
    });

    test('mirrored 5x5 grid and hsl color from the hash', () => {
        const ic = identicon('bob');
        expect(ic.color).toBe(`hsl(${4093641051 % 360}, 50%, 70%)`);
        ic.cells.forEach(([x, y]) => expect(ic.cells).toContainEqual([4 - x, y]));
    });

    test('cached, the 50 most recent only', () => {
        const ic0 = identicon('lru-0');
        const ic1 = identicon('lru-1');
        expect(identicon('lru-0')).toBe(ic0); // hit, 'lru-1' is now the oldest
        for (let i = 2; i <= 50; i++) identicon('lru-' + i); // the 51st entry evicts 'lru-1'
        expect(identicon('lru-0')).toBe(ic0);
        expect(identicon('lru-1')).not.toBe(ic1);
        expect(identicon('lru-1')).toEqual(ic1);
    });
});

describe('reencodeAvatar', () => {
    let ctx, supported;

    beforeEach(() => {
        ctx = { drawImage: jest.fn(), fillRect: jest.fn(), globalCompositeOperation: 'source-over', fillStyle: '' };
        jest.spyOn(HTMLCanvasElement.prototype, 'getContext').mockReturnValue(ctx);
        // Encodes `type` only when supported, PNG otherwise (Safari's WebP behaviour).
        jest.spyOn(HTMLCanvasElement.prototype, 'toBlob').mockImplementation(function (cb, type) {
            cb(new Blob(['x'], { type: supported.includes(type) ? type : 'image/png' }));
        });
        globalThis.createImageBitmap = jest.fn().mockResolvedValue({ width: 400, height: 300, close: jest.fn() });
    });
    afterEach(() => jest.restoreAllMocks());

    test('center-crops to 256x256 WebP', async () => {
        supported = ['image/webp', 'image/jpeg'];
        const f = await reencodeAvatar(new Blob(['raw']));
        expect(f.type).toBe('image/webp');
        expect(f.name).toBe('avatar.webp');
        expect(createImageBitmap).toHaveBeenCalledWith(expect.anything(), { imageOrientation: 'from-image' });
        expect(ctx.drawImage).toHaveBeenCalledWith(expect.anything(), 50, 0, 300, 300, 0, 0, 256, 256);
        expect(ctx.fillRect).not.toHaveBeenCalled();
    });

    test('falls back to JPEG on a filled canvas when WebP is ignored', async () => {
        supported = ['image/jpeg'];
        const f = await reencodeAvatar(new Blob(['raw']));
        expect(f.type).toBe('image/jpeg');
        expect(f.name).toBe('avatar.jpg');
        expect(ctx.globalCompositeOperation).toBe('destination-over');
        expect(ctx.fillRect).toHaveBeenCalledWith(0, 0, 256, 256);
    });

    test('rejects undecodable input', async () => {
        supported = ['image/webp'];
        createImageBitmap.mockRejectedValue(new Error('InvalidStateError'));
        await expect(reencodeAvatar(new Blob(['heic']))).rejects.toThrow();
    });
});

describe('cardPosition', () => {
    const rect = { left: 100, top: 100, right: 130, bottom: 130 };

    test('below the anchor', () => {
        expect(cardPosition(rect, 300, 100, 1000, 800)).toEqual({ left: 100, top: 138 });
    });

    test('flips above at the bottom edge and clamps to the right edge', () => {
        const r = { left: 900, top: 700, right: 930, bottom: 730 };
        expect(cardPosition(r, 300, 100, 1000, 800)).toEqual({ left: 692, top: 592 });
    });
});

describe('user card hover listener', () => {
    const send = jest.fn();
    let avatar, other;

    const fire = (type, target, relatedTarget = null) =>
        target.dispatchEvent(new MouseEvent(type, { bubbles: true, relatedTarget }));

    beforeAll(() => {
        jest.useFakeTimers();
        initUserCard({ ports: { userCardFromJs: { send } } });
    });
    afterAll(() => jest.useRealTimers());

    beforeEach(() => {
        document.body.innerHTML = '<span data-user-card="bob"><i></i></span><p id="other"></p>';
        avatar = document.querySelector('[data-user-card]');
        other = document.getElementById('other');
        send.mockClear();
    });

    // Leave the card hidden for the next test.
    afterEach(() => {
        fire('pointerout', avatar, other);
        jest.advanceTimersByTime(1000);
    });

    test('shows after the delay, hides on leave', () => {
        fire('pointerover', avatar.firstChild);
        jest.advanceTimersByTime(499);
        expect(send).not.toHaveBeenCalled();
        jest.advanceTimersByTime(1);
        expect(send).toHaveBeenLastCalledWith('bob');

        fire('pointerout', avatar, other);
        jest.advanceTimersByTime(200);
        expect(send).toHaveBeenLastCalledWith(null);
    });

    test('leaving before the delay never shows', () => {
        fire('pointerover', avatar);
        jest.advanceTimersByTime(300);
        fire('pointerout', avatar, other);
        jest.advanceTimersByTime(1000);
        expect(send).not.toHaveBeenCalled();
    });

    test('moving inside the avatar keeps it', () => {
        fire('pointerover', avatar);
        jest.advanceTimersByTime(500);
        fire('pointerout', avatar, avatar.firstChild);
        jest.advanceTimersByTime(1000);
        expect(send).toHaveBeenCalledTimes(1);
    });

    test('stays open while the pointer is on the card', () => {
        fire('pointerover', avatar);
        jest.advanceTimersByTime(500);
        const $card = document.createElement('div');
        $card.id = 'userCard';
        $card.dataset.username = 'bob';
        document.body.appendChild($card);

        fire('pointerout', avatar, $card);
        fire('pointerover', $card, avatar);
        jest.advanceTimersByTime(1000);
        expect(send).toHaveBeenCalledTimes(1);
        expect($card.classList.contains('is-placed')).toBe(true);

        fire('pointerout', $card, other);
        jest.advanceTimersByTime(200);
        expect(send).toHaveBeenLastCalledWith(null);
    });

    test('a press on the card (text selection) keeps it, a press elsewhere closes it', () => {
        fire('pointerover', avatar);
        jest.advanceTimersByTime(500);
        const $card = document.createElement('div');
        $card.id = 'userCard';
        document.body.appendChild($card);

        fire('pointerover', $card, avatar);
        fire('pointerdown', $card);
        fire('pointerout', $card, other); // drag-select past the edge
        fire('pointerup', other);
        fire('click', $card);
        jest.advanceTimersByTime(1000);
        expect(send).toHaveBeenCalledTimes(1);

        fire('pointerdown', other);
        jest.advanceTimersByTime(0);
        expect(send).toHaveBeenLastCalledWith(null);
    });
});
