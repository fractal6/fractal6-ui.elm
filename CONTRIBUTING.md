All contribution, through issues, tensions and merge request are welcome.
Please read this file before contributing.

### Updating Schema

If you need to alter the schema, please refers to https://github.com/fractal6/fractal6.go/blob/master/CONTRIBUTING

To re-generate the GraphQl parser code for Elm:

    make gen

### File structure

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

Javascript ports are located in `assets/js`

### Style

Bulma + custom Sass files. See `assets/sass/`.

## Routing

We use [elm-spa](https://www.elm-spa.dev/) as the spa framework. It defines the routing and the Main input logic. Basycally, files located in `Pages` will be an accessible route in the browser. 
For exemple: `Pages/o/Dynamiyc.elm` makes the route at `https://[domain_name]/o/my_org`

Note: For the moment we lock the `elm-spa` version to the v4 version as it is working well, and upgrading to v6 will be time consuming while the benefits of it is not guaranteed.

## Creating new components

A components is a reusable piece of code use in a application to implement a given functionality.
By components, we mean here a state-full Elm module, that is a module which have its own state, Model & Msg managed by itself. It allows to limit the complexity of the elm files and prevents file from being too big.
For simple components that don't have complex states (i.e. have a few or no Msg), they can be implemented by getting the Msg from the main page, as it is done
in `Components/ColorPicker.elm` for example.
For more complex components, a lot of boilerplates are involved when creating a component with Elm (for the best!) which maintain their own state.
To help building new components quickly without having to repeat the same code again and again, and ensure code api consistency,
we provide a script to generate template code when creating new component:  `melm.py`.

Let say that you need a new component that implements a dropdown menu, and put the file in the `src/Components/` folder.
You will create the template for you dropdown like this

    melm.py add -w Components.MyDropdown

The file `MyDropdown.elm` will be created for you.

If your component is a modal, then you will need to change the default template as follows

    melm.py add -w -t modal Components.MyModal


Finally, when you want to use your component inside a file, let's say in a page located at `src/Page/Welcome.com`, you will need to write some boilerplate code to use your component. The following command will help you by adding in your file the necessery boilerplate code to use the component

    melm.py push -w Components.MyDropdown Page.Exemple

Note: some manually edit can be necessayry anyway, but following the elm compiler should guide you to light.

You can obtain the full script documentation by typing

    melm.py --help



### Typo rules and naming conventions

- Elm Messages should be prefix with `On` then the verb that characterize the action it convey (i.e. `OnSubmitClick`)
- Elm Share/Global message should be prefixed by `Do` then the verb that characterize the action it convey (i.e. `DoUpdateSession`)


### Updating schema

To propagate a schema update, run (\*):

    make gen

(\*) Assuming that your schema is located in `../fractal6-schema/gen/schema.graphql`

### Git Branches

- `prod`: Tag tracking release, main branch (see also [CHANGELOG.md](CHANGELOG.md)).
- `dev`: The current development branch.
- `hotfix/*`: A bug fix for production release.
- `fix/*`: A fix an identified bug or issue.
- `feat/*`: A new feature.
- `refactor/*`: Refactoring/Improvements of existing features.


### Git commits

The commit name should starts with a name that identify the **type** of modifications done (e.g. fix, feat, refactor, perf etc), then a **context** that help to determine the scope of the changes (e.g. a file name file modified or a thematic) and finally a short comment that explain, as explicitly as possible, not what modification has been done, but what behaviour has been fixed, added or removed for example.

example: `fix/AuthModal: prevent the auth modal to be closed when a click in the background occur.`

Here are some common used for so called semantic commit message:

- feat: A new feature
- fix: A bug fix
- perf: A code change that improves performance
- refactor: A code change that neither fixes a bug nor adds a feature
- typo: Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc)
- build: Changes that affect the build system or external dependencies (example scopes: gulp, broccoli, npm)
- test: Adding missing tests or correcting existing tests
- docs: Documentation only changes

Extra : 
- ui: A code change that affects visual components (size or position of button, with of something etc)
- ux: A code change that affects interaction, usability or ergonomics. 



### Reporting issues, Questions, Feedback

- Create an issue on the versioning system platform for bug and low-level issues (technical).

- Create a tension on [Fractale](https://fractale.co/o/f6) for all the rest.

- Chat on Matrix: https://matrix.to/#/#fractal6:matrix.org for support and talking.
