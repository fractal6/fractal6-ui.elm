All contribution, through issues, tensions and merge request are welcome.
Please read this file before contributing.

## File structure

The frontend is written in [Elm](https://elm-lang.org/).
Elm is delightful language, purely functional, typed, with great performance, small assets and that compile to Javascript.


Elm code is structured inside the `src/` folder:

- `Form/`: Components used to create new tensions.
- `Components/`: All other reusable elm components with their own state.
- `ModelCommon/`: Common data structure, functions, views, and helpers.
- `Query/`: GQL queries.
- `ModelSchema`: GQL client.
- `Request`: REST queries.
- `Session`: Session data structure and utilities.
- `Codecs`: Json encoder/decoder.
- `Ports`: Ports bridges. Javascript code is located in `assets/js/`
- `Main`: Entry point.


## Style

Bulma + custom Sass files. See `assets/sass/`.


## Typo rules and naming conventions

### Elm

- Elm Messages should start with a verb that characterize the action it convey.
- ...


### Git Branches

- `master`: Tag tracking (see also [CHANGELOG.md](CHANGELOG.md)).
- `dev`: The current development branch. Should only be merged via merge requests.
- `release-*`: A Production release (no more features added).
- `hotfix/*`: A bug fix for production release.
- `fix/*`: A fix an identified bug or issue.
- `feat/*`: A new feature.
- `codefactor/*`: Refactoring/Improvements of existing features.


### Git commits

The commit name should starts with a name that identify the **type** of modifications done (e.g. fix, feat, codefactor, optimization etc), then a **context** that help to determine the scope of the changes (e.g. a file name file modified or a thematic) and finally a short comment that explain, as explicitly as possible, not what modification has been done, but what behaviour has been fixed, added or removed for example.

example: `fix/AuthModal: prevent the auth modal to be closed when a click in the background occur.`


## Reporting issues, Questions, Feedback

- Issue on the versioning system platform for bug and low-level issues (technical).

- Tension on [Fractale](https://fractale.co/o/f6) for all the rest.

- Chat on Matrix: https://matrix.to/#/#fractal6:matrix.org