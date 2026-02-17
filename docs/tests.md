# Testing

This project uses two layers of testing:

- **Unit tests** with [elm-test](https://github.com/rtfeldman/node-test-runner) for pure Elm logic
- **E2E tests** with [Robot Framework](https://robotframework.org/) for browser-level integration

## Directory structure

```
tests/
├── Unit/                         # Elm unit tests (elm-test)
│   └── MarkdownTest.elm
├── robot/                        # Robot Framework e2e tests
│   ├── data/                     # test fixtures
│   ├── robotresults/             # output dir (gitignored)
│   ├── requirements.txt          # Python deps
│   └── scroll_position.robot     # e2e test
└── lazy.md                       # dev notes
```

---

## Unit Tests (elm-test)

### Prerequisites

- [elm-test](https://www.npmjs.com/package/elm-test) (install with `npm install -g elm-test`)

### Running

```bash
# Run all unit tests
elm-test tests/Unit/

# Run a specific file
elm-test tests/Unit/MarkdownTest.elm
```

### Writing new tests

1. Create a `.elm` file in `tests/Unit/`
2. Import `Test` and `Expect` from `elm-explorations/test`
3. Follow the existing pattern in `MarkdownTest.elm`

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
