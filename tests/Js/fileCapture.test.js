// Files dropped on a [data-paste-capture] textarea reach Elm through the same
// pastedFilesFromJs port as a paste, but with isPaste=false and their own name:
// Elm stages them as attachment chips instead of inline `![](name)` placeholders.
import { initFileCapture } from '../../assets/js/ports.js';

const app = { ports: { pastedFilesFromJs: { send: jest.fn() } } };

const fire = (el, type, transfer) => {
    const e = new Event(type, { bubbles: true, cancelable: true });
    e[type === 'paste' ? 'clipboardData' : 'dataTransfer'] = transfer;
    el.dispatchEvent(e);
    return e;
};

beforeAll(() => {
    global.URL.createObjectURL = jest.fn(f => 'blob:' + f.name);
    initFileCapture(app);
});

beforeEach(() => {
    app.ports.pastedFilesFromJs.send.mockClear();
    document.body.innerHTML = `
        <textarea id="commentInput" data-paste-capture="true"></textarea>
        <textarea id="plainInput"></textarea>`;
});

test('drop forwards the files under their own name, as attachments', () => {
    const ta = document.getElementById('commentInput');
    const file = new File(['x'], 'report.pdf', { type: 'application/pdf' });
    const dt = { files: [file], types: ['Files'], dropEffect: 'none' };

    // Without preventDefault on dragover the browser never fires drop.
    expect(fire(ta, 'dragover', dt).defaultPrevented).toBe(true);
    expect(dt.dropEffect).toBe('copy');
    expect(ta.classList.contains('is-dragover')).toBe(true);

    expect(fire(ta, 'drop', dt).defaultPrevented).toBe(true);
    expect(ta.classList.contains('is-dragover')).toBe(false);
    expect(app.ports.pastedFilesFromJs.send).toHaveBeenCalledWith({
        targetId: 'commentInput',
        files: [file],
        objectUrls: [],
        isPaste: false,
    });
});

test('dragleave drops the highlight', () => {
    const ta = document.getElementById('commentInput');
    fire(ta, 'dragover', { files: [], types: ['Files'] });
    fire(ta, 'dragleave', {});
    expect(ta.classList.contains('is-dragover')).toBe(false);
});

test('a fileless drag (text, link) is left to the browser', () => {
    const ta = document.getElementById('commentInput');
    const dt = { files: [], types: ['text/plain'], dropEffect: 'move' };

    expect(fire(ta, 'dragover', dt).defaultPrevented).toBe(false);
    expect(dt.dropEffect).toBe('move');
    expect(ta.classList.contains('is-dragover')).toBe(false);
});

test('a textarea without the capture attribute is ignored', () => {
    const ta = document.getElementById('plainInput');
    expect(fire(ta, 'dragover', { files: [], types: ['Files'] }).defaultPrevented).toBe(false);
    fire(ta, 'drop', { files: [new File(['x'], 'a.png')] });
    expect(app.ports.pastedFilesFromJs.send).not.toHaveBeenCalled();
});

test('paste still renames and previews, as an inline placeholder', () => {
    const ta = document.getElementById('commentInput');
    const file = new File(['x'], 'image.png', { type: 'image/png' });
    const cd = { items: [{ kind: 'file', getAsFile: () => file }] };

    expect(fire(ta, 'paste', cd).defaultPrevented).toBe(true);
    const payload = app.ports.pastedFilesFromJs.send.mock.calls[0][0];
    expect(payload.isPaste).toBe(true);
    expect(payload.files[0].name).toMatch(/^paste-\d+-0\.png$/);
    expect(payload.objectUrls).toEqual(['blob:' + payload.files[0].name]);
});
