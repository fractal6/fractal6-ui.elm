# Testing

Three layers:

- **Elm unit tests** ([elm-test](https://github.com/rtfeldman/node-test-runner)) for pure Elm logic — `tests/Elm/`
- **JS unit tests** (Jest + jsdom) for the `assets/js/` helpers Elm can't reach — `tests/Js/`
- **E2E** ([Robot Framework](https://robotframework.org/), Chrome headless) for browser-level behavior — `tests/robot/`

`make test` runs Elm + JS. `make test_elm`, `make test_js`, `make test_e2e` for targeted runs.

## Elm

```bash
elm-test tests/Elm/                  # all
elm-test tests/Elm/MarkdownTest.elm  # one file
```

New test: add a `.elm` file in `tests/Elm/`, import `Test` and `Expect` from
`elm-explorations/test`, follow `MarkdownTest.elm`.

## JS

```bash
make test_js                       # all
npm run test:js -- replaceRange    # one file (Jest filters by name)
```

New test: add `tests/Js/*.test.js`, import the helper directly from `assets/js/`, use plain
DOM APIs (`jsdom` environment is set in `jest.config.js`).

Two things to know:

- `assets/js/` stays ESM for webpack; Jest transforms it through `babel-jest`
  (`babel.config.cjs`). Native ESM was tried and hits Jest's "module is already linked" bug,
  because `ports.js` re-exports through `bulma_drivers.js`.
- jsdom has no `document.execCommand`. `tests/Js/setup.js` polyfills `insertText`; tests can
  force the fallback path with `globalThis.__failExecCommand = true` (or `'throw'`). Real undo
  behavior is covered by `tests/robot/markup_undo.robot` instead.

## E2E

Needs Chrome + matching ChromeDriver, `pip install -r tests/robot/requirements.txt`, and the
app running (`npm start`, http://localhost:8080).

```bash
robot tests/robot/                            # all
robot tests/robot/scroll_position.robot       # one suite
robot --test "*Scroll*" tests/robot/          # by name
robot --outputdir tests/robot/robotresults tests/robot/
```

New test: add a `.robot` file in `tests/robot/` following `scroll_position.robot`, fixtures in
`tests/robot/data/`. See the [Robot Framework user guide](https://robotframework.org/robotframework/#user-guide)
and [Browser keywords](https://marketsquare.github.io/robotframework-browser/Browser.html) for the rest.
