#!/usr/bin/env python

"""Build a WOFF2 subset of the IcoMoon icon font with only the glyphs used by the app.

assets/icons/ is IcoMoon output and is never modified: the derived CSS (style.css with its
@font-face pointing to the subset) is written to style.subset.css. Expects single-color icons.
Subsetting recomputes glyph bboxes, which silences the "Glyph bbox was incorrect" warnings.
"""

import re
from pathlib import Path

from fontTools import subset
from fontTools.ttLib import TTFont

ICONS = Path("assets/icons")
CSS = ICONS / "style.css"
OUT_CSS = ICONS / "style.subset.css"
SRC_FONT = ICONS / "fonts/fractaleicon.ttf"
OUT_FONT = ICONS / "fonts/fractaleicon.subset.woff2"
SCAN_DIRS = ["src", "assets/js", "assets/sass", "public", "i18n"]
SCAN_EXT = {".elm", ".js", ".scss", ".css", ".html", ".toml"}

FONT_FACE = """@font-face {
  font-family: 'fractaleicon';
  src: url('fonts/fractaleicon.subset.woff2') format('woff2');
  font-weight: normal;
  font-style: normal;
  font-display: block;
}"""


def main():
    css = CSS.read_text()
    glyphs = {
        name: int(code, 16)
        for name, code in re.findall(r'\.(icon-[\w-]+):before\s*\{\s*content:\s*"\\([0-9a-f]+)"', css)
    }
    assert glyphs, f"no .icon-*:before rule parsed in {CSS}: IcoMoon format changed?"
    out_css, n = re.subn(r"@font-face\s*\{[^}]*\}", FONT_FACE, css, count=1)
    assert n == 1, f"no @font-face found in {CSS}: IcoMoon format changed?"

    used = set()
    for d in SCAN_DIRS:
        for f in Path(d).rglob("*"):
            if f.suffix not in SCAN_EXT or not f.is_file():
                continue
            text = f.read_text(errors="ignore")
            used |= {glyphs[n] for n in re.findall(r"icon-[\w-]+", text) if n in glyphs}
            # Raw codepoints: JS "\ueXXX" (canvas in graphpack_d3.js) and CSS "\eXXX" (custom icon rules in sass)
            used |= {int(c, 16) for c in re.findall(r"\\u?(e[0-9a-f]{3})", text, re.I)}

    font = TTFont(SRC_FONT, recalcTimestamp=False)  # deterministic output: stable hashed URL across deploys
    options = subset.Options()
    options.flavor = "woff2"
    options.layout_features = []
    options.notdef_outline = True
    sub = subset.Subsetter(options)
    sub.populate(unicodes=used)
    sub.subset(font)
    # IcoMoon writes xMin=lsb=0; bboxes get recomputed on save, so sync lsb or glyphs shift by xMin
    glyf, hmtx = font["glyf"], font["hmtx"]
    for name in font.getGlyphOrder():
        g = glyf[name]
        g.recalcBounds(glyf)
        hmtx[name] = (hmtx[name][0], getattr(g, "xMin", 0))
    subset.save_font(font, OUT_FONT, options)

    missing = used - set(TTFont(OUT_FONT).getBestCmap())
    assert not missing, f"codepoints missing from subset: {sorted(map(hex, missing))}"

    OUT_CSS.write_text(out_css)
    print(f"{OUT_FONT}: {len(used)}/{len(glyphs)} glyphs, {OUT_FONT.stat().st_size} bytes")


if __name__ == "__main__":
    main()
