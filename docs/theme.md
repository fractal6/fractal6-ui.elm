# Theme

Three preferences: `system` (default), `light`, `dark`. Stored in `localStorage.theme`,
outside `VOLATILE_SESSION_ITEMS`, so it survives sign-out.

`applyTheme` in `assets/js/bulma_drivers.js` is the single writer of the theme on `<html>`:
`light`/`dark` set `data-theme`, `system` removes it so the `@media (prefers-color-scheme)`
rules of `assets/sass/fractal6-variables.scss` take over. Each theme block also emits
`color-scheme`, so native browser UI follows. An inline script in `public/index.html`
duplicates those two lines to apply the theme before first paint — keep them in sync.

`viewThemeSwitch` in `src/Components/Navbar.elm` renders the control; buttons carry
`.themeTrigger` and `data-theme-pref`, and JS notifies Elm back through `updateThemeFromJs`
plus `flushGraphPackFromJs` (the d3 canvas samples css variables at draw time, so it must
redraw — also on OS changes via `matchMedia` in `ports.js`).

One surface opts out: the logged out navbar is `is-primary`, a fixed bright color, so
`Navbar.elm` pins `data-theme="light"` on it.
