import { findAnchorPos } from '../../assets/js/textutils.js';

const SOURCE = [
    '# Title of the thing',
    '',
    'A **bold** paragraph with a [link](http://x.y) inside.',
    'A second line of the same paragraph that is long enough.',
    '',
    '- first item',
    '- second item',
].join('\n');

const eolOf = (needle) => SOURCE.indexOf('\n', SOURCE.indexOf(needle));

test('matches the paragraph block, markdown markup ignored', () => {
    // rendered text of the block: markup stripped, source lines joined
    const anchor = 'A bold paragraph with a link inside. A second line of the same paragraph that is long enough.';
    expect(findAnchorPos(SOURCE, anchor)).toBe(eolOf('A **bold**'));
});

test('matches a heading and a list item', () => {
    expect(findAnchorPos(SOURCE, 'Title of the thing')).toBe(eolOf('# Title'));
    expect(findAnchorPos(SOURCE, 'second item')).toBe(SOURCE.length);
});

test('falls back to the end of the text when nothing matches', () => {
    expect(findAnchorPos(SOURCE, 'nowhere to be found')).toBe(SOURCE.length);
    expect(findAnchorPos(SOURCE, '')).toBe(SOURCE.length);
});
