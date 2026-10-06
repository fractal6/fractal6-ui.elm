# Fonts

All fonts are self-hosted (no third-party request). `@font-face` rules live in `assets/sass/_fonts.scss`,
files in `assets/fonts/` (text) and `assets/icons/` (icons, see `docs/icons.md`). Webpack emits them hashed in `static/fonts/`.

| Font | Used by | `font-display` | Preloaded |
|---|---|---|---|
| Quicksand (latin, weights 500-700) | default text, titles, headings, graph canvas (`defaultFontFace`) | `optional` | yes |
| Figtree (latin, variable 300-900, normal + italic) | `.is-human`: comment and mandate content, headings excluded (`humanFontFace`) | `block` (default) | no |
| fractaleicon (subset) | icons | `block` | yes |
| System monospace (no download) | `code`, `pre`, mandate diff (`monoFontFace`) | - | - |

- `optional` never switches font mid-page, but if the font misses its ~100ms window the fallback is kept for the
  whole document (the whole SPA session). It is only safe because Quicksand is preloaded; without the preload, use `block`.
- Figtree is variable (one file per style, real bold and italic; the italic file only loads when italic text shows).
  It is not preloaded: it is not used on every page, and an unused preload triggers a browser warning.
- The graph canvas (`graphpack_d3.js`) doesn't wait for fonts: it redraws once Quicksand and the icon font are loaded.
  Its font list (`fontstyleCircle`) mirrors `defaultFontFace`.
- Preload tags are injected by the `PreloadFonts` plugin in `webpack.config.js`.
- Built CSS references fonts relatively (`MiniCssExtractPlugin.loader` `publicPath: '../../'`), so they follow the
  `/<lang>/` prefix of published builds and match the preload URLs.

To update a text font, download the latin WOFF2 from the Google Fonts CSS API
(`https://fonts.googleapis.com/css2?family=...`) into `assets/fonts/`.
