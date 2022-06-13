
## Coding

The frontend is written in [Elm](https://elm-lang.org/).
Elm is delightful language, purely functional, typed, with great performance, small assets and that compile to Javascript.

### File structure

Some important source folder and files (`src/`):

- Components/: all the reusable elm components with their own state.
- Form/: Like components but for special modal form in use.
- ModelSchema: GQL Data structure .
- Query/: GQL queries.
- ModelCommon: Data structure for input form plus helpers.
- ModelCommon/: Common data structure, views, and helpers.
    - Request/: Rest queries.
- Session: session data structure and utilities.
- Codecs: Json encoder/decoder.
- Ports: Ports bridges.

### Msg

- Message should start with a verb, that characterized the action that it convey.


## Style

Sass + Bulma.

## Issues

...

## Branches

...
