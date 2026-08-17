# Testing

This project uses three layers of testing:

- **Elm unit tests** with [elm-test](https://github.com/rtfeldman/node-test-runner) for pure Elm logic
- **JS unit tests** with [Jest](https://jestjs.io/) + jsdom for `assets/js/` helpers
- **E2E tests** with [Robot Framework](https://robotframework.org/) for browser-level integration

`make test` runs Elm + JS; `make test_elm` / `make test_js` for targeted runs; `make test_e2e` for Robot.

## Directory structure

```
tests/
├── Elm/                         # Elm unit tests (elm-test)
│   ├── GovernanceStateTest.elm
│   └── MarkdownTest.elm
├── Js/                           # JS unit tests (Jest + jsdom)
│   ├── setup.js                  # execCommand polyfill
│   ├── replaceRange.test.js
│   ├── markupRichText.test.js
│   └── markdownFormatters.test.js
├── robot/                        # Robot Framework e2e tests
│   ├── data/                     # test fixtures
│   ├── robotresults/             # output dir (gitignored)
│   ├── requirements.txt          # Python deps
│   ├── scroll_position.robot     # e2e test
│   └── markup_undo.robot         # real-browser undo smoke test
└── lazy.md                       # dev notes
```

---

## Elm Unit Tests (elm-test)

### Prerequisites

- [elm-test](https://www.npmjs.com/package/elm-test) (install with `npm install -g elm-test`)

### Running

```bash
# Run all elm tests
elm-test tests/Elm/

# Run a specific file
elm-test tests/Elm/MarkdownTest.elm
```

### Writing new tests

1. Create a `.elm` file in `tests/Elm/`
2. Import `Test` and `Expect` from `elm-explorations/test`
3. Follow the existing pattern in `MarkdownTest.elm`

---

## JS Unit Tests (Jest)

Covers the `assets/js/` helpers — `replaceRange` and the markup keyboard handlers
in `bulma_drivers.js` / `ports.js` — which Elm tests can't reach.

### Prerequisites

```bash
npm install   # pulls jest + jest-environment-jsdom from devDependencies
```

### Running

```bash
make test_js                  # all JS tests
npm run test:js -- replaceRange   # one file (Jest filters by name)
```

Jest transforms the `assets/js/` ESM modules through `babel-jest` +
`@babel/preset-env` (`babel.config.cjs`, targeting current Node) so the
test VM can `require()` them. The source files stay ESM for webpack.

### Writing new tests

1. Create a `*.test.js` file in `tests/Js/`
2. Import the helper directly: `import { foo } from '../../assets/js/foo.js';`
3. Use plain DOM APIs — `testEnvironment: 'jsdom'` is configured in `jest.config.js`

### `execCommand` polyfill caveat

jsdom doesn't implement `document.execCommand`. `tests/Js/setup.js` installs a
minimal `insertText` polyfill so `replaceRange`'s happy path runs. Tests can opt
into the fallback path with `globalThis.__failExecCommand = true` (returns false)
or `'throw'`. Real-browser undo behavior is validated by the Robot
`markup_undo.robot` smoke test, not Jest.

### Why Babel and not native ESM?

We tried Node's `--experimental-vm-modules` first. It works for individual
files, but the second test file that transitively imports a module the first
already pulled in trips Jest's "module is already linked" bug — which is
exactly our shape (`ports.js` re-exports through `bulma_drivers.js`).
Babel-jest sidesteps the issue by transforming everything to CJS for the test VM.

---

## E2E Tests (Robot Framework)

### Prerequisites

- Python 3.8+
- Chrome browser (for headless testing)
- ChromeDriver (must match Chrome version)

### Install dependencies

```bash
pip install -r tests/robot/requirements.txt
```

### Start dev server

Tests require the application to be running:

```bash
npm start
# App runs on http://localhost:8080
```

### Running

```bash
# Run all e2e tests
robot tests/robot/

# Run a specific test file
robot tests/robot/scroll_position.robot

# Run a specific test case by name
robot --test "Page Starts At Top Position" tests/robot/

# Pattern matching (wildcards)
robot --test "*Scroll*" tests/robot/

# Dry run (validate syntax only)
robot --dryrun tests/robot/
```

### Run tests by tag

If tests have tags (e.g., `[Tags]  smoke  ui`):

```bash
robot --include smoke tests/robot/
robot --exclude slow tests/robot/
robot --include smokeANDui tests/robot/
```

### Output and reports

By default, Robot Framework generates `output.xml`, `log.html`, and `report.html`.

```bash
# Custom output directory
robot --outputdir tests/robot/robotresults tests/robot/

# Rerun failed tests
robot --rerunfailed output.xml tests/robot/
```

### Writing new tests

1. Create a `.robot` file in `tests/robot/`
2. Follow existing patterns (see `scroll_position.robot`)
3. Place test data/fixtures in `tests/robot/data/`

### Useful options

| Option | Short | Description |
|--------|-------|-------------|
| `--test NAME` | `-t` | Run specific test case |
| `--suite NAME` | `-s` | Run specific suite |
| `--include TAG` | `-i` | Run tests with tag |
| `--exclude TAG` | `-e` | Skip tests with tag |
| `--outputdir DIR` | `-d` | Set output directory |
| `--rerunfailed FILE` | `-R` | Rerun failed tests |
| `--dryrun` | | Syntax check only |
| `--loglevel LEVEL` | `-L` | Set log level (DEBUG, INFO, WARN) |

## References

- [elm-test documentation](https://github.com/rtfeldman/node-test-runner)
- [Robot Framework User Guide](https://robotframework.org/robotframework/#user-guide)
- [Robot Framework Browser](https://robotframework-browser.org/)
- [Robot Framework Browser Keywords](https://marketsquare.github.io/robotframework-browser/Browser.html)
