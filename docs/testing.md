# Testing

This project uses [Robot Framework](https://robotframework.org/) for end-to-end browser testing with SeleniumLibrary.

## Setup

### Prerequisites

- Python 3.8+
- Chrome browser (for headless testing)
- ChromeDriver (must match Chrome version)

### Install dependencies

```bash
pip install -r tests/requirements.txt
```

### Start dev server

Tests require the application to be running:

```bash
npm start
# App runs on http://localhost:8080
```

## Running Tests

### Run all tests

Execute all `.robot` files in the tests directory:

```bash
robot tests/
```

### Run a specific test file

```bash
robot tests/scroll_position.robot
```

### Run a specific test case by name

Use `--test` or `-t` option:

```bash
robot --test "Page Starts At Top Position" tests/
```

With pattern matching (wildcards):

```bash
robot --test "*Scroll*" tests/
```

### Run tests by tag

If tests have tags (e.g., `[Tags]  smoke  ui`):

```bash
# Include tag
robot --include smoke tests/

# Exclude tag
robot --exclude slow tests/

# Combine tags (AND/OR)
robot --include smokeANDui tests/
robot --include smokeORregression tests/
```

### Run a test suite

In Robot Framework, folders and `.robot` files are suites:

```bash
robot --suite scroll_position tests/
```

## Output and Reports

By default, Robot Framework generates:
- `output.xml` - Machine-readable results
- `log.html` - Detailed execution log
- `report.html` - Summary report

### Custom output directory

```bash
robot --outputdir results tests/
```

### Rerun failed tests

After a test run, rerun only failures:

```bash
robot --rerunfailed output.xml tests/
```

## Dry Run

Validate test syntax without execution:

```bash
robot --dryrun tests/
```

## Useful Options Summary

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

## Writing New Tests

Test files should:
1. Be placed in `tests/` directory
2. Use `.robot` extension
3. Follow existing patterns (see `tests/scroll_position.robot`)

Basic structure and keywords

See context7 library for robotframework and robotframework-browser


## References

- [Robot Framework User Guide](https://robotframework.org/robotframework/#user-guide)
- [Robot Framework Browser](https://robotframework-browser.org/)
- [Robot Framework Browser Keywords](https://marketsquare.github.io/robotframework-browser/Browser.html)

