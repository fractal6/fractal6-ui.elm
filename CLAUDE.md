# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This codebase is a webapp written in elm (elm-lang). Elm is delightful language, purely functional, typed, with great performance, small assets and that transpile to Javascript.

It implements the user interface (frontend) for projects and organization management platform called Fractale (fractale.co). Note that the backed is managed by another repo we control. The webapp is implemented as a Single-Page-Application, from the elm-spa@v4.1.0 lib for basic routing structure and page templating.

**Fractale** is a platform for self-organisation. It is designed around the concept that an organisation can be represented as a tree and should follow principles of transparency, governance decentralization and authority distribution. A tree divides in branches and form leaves, likewise an organisation divides in **Circles** that can have **Roles**. Both, circles and roles have an associated descriptive document, called **Mandate**, intended to define its purpose and operating rules. Finally, the communication inside the organisation is done through **Tensions**, and make the link between users and organisations. You can think of a tension as an email, but more structured and more powerful.

Using Fractale for your organisation offers the following capabilities and features:
* Interactive tree and graph packing organisation chart
* Organisation visibility defined at circles level (authorization managed by the backend)
* ACL based on member roles and circle governance rules (authorization managed by the backend)
* Ticketing management through Tensions
* Discussion thread and subscription by tension
* Email notifications broadcast and email reply (managed by the backend)
* Labels system
* Role templates system
* Journal history of events (including mandate updates!)
* GraphQL API (the schema is managed by the backend. The src/Fractal is auto-generated from the schema)

The schema of the Fractale API is defined in the file @../fractal6.go/schema/graphql/fractal6.graphql


## Elm code Structure

The elm entry point is defined in the ./public/index.html and ./public/index.js files.

Here is the root structure:

- assets/ - all the code not elm code: JS (ports), SASS/CSS, and static files (images, icons fonts etc)
- dist/ - built code - ignore that folder
- docs/ - feature and architecture documentations
- i18n/ - the i18n translation files. Data is encoded in toml format
- public/ - entry point for html code and Elm
- releases/ - built code - ignore that folder
- review/ - ignore that folder
- snippets/ - contains file that with exemple of elm implementation pattern for this codebase
- src/ - tha actual Elm code (see below)
- elm.json - the elm requirements and dependencies file
- i18n.py - is a python script to generate the elm text in `src/Text.elm` from the traductions in `i18n/i18n.toml`
- melm.py - an utility script to generate elm template components
- package.json - the node requirements and dependencies file
- webpack.config.js - the webpack configuration used to build the project

The Elm code is structured as follows (below `src/` then):

- Assets - Static assets (images, fonts, etc.)
- Bulk/ - Common data structures and helpers
- Components/ and src/Form - Reusable components with their own state
- Extra/ - Lib extension utilities.
- Fractal/ - The auto-generated type from the Graphql schema of the Fractale API. Do not read this folder !
- Generated/ - this is the routing logics generated file (by elm-spa)
- Org/ - This the logics of the "organization" related views. It corresponds to the different tab navigation of an organization view.
- Pages/ - the route entrypoint
- Query/ - GraphQL queries
- Schema/ - For extension data schema, not managed by the Graphql schema
- User/ - This the logics of the "user" related views
- Assets.elm- the elm assets entry point
- Auth.elm - Handle http and Graphql requests and errors.
- Bulk.elm - Bulk folder entry points, lib utilities.
- Codecs.elm - JSON encoders/decoders
- Dom.elm - Extension of the Dom libraries
- Extra.elm - Extra tool utilities
- Footbar.elm - Footbar view
- Global.elm - The SPA entrypoint logics
- GqlClient.elm - The entrypoint for managing Graphql request
- Loading.elm - Data structure for handling remote requests (Graphql and HTTP)
- Main.elm - Elm Entry point
- Mardown.elm - Markdown parser extension and utilities
- ModelSchema.elm - GQL client
- Page.elm - The page entrypoint
- Ports.elm: Ports bridges. Javascript code is located in `assets/js/`
- QuickSearch.elm: quicksearch extension utilities
- Request.elm - REST queries
- Scroll.elm - Scroll helpers utilities
- Session.elm - Session data structure and utilities
- Spa.elm - the Spa utility entry point
- Text.elm - Auto-generated hardcoded texts and templates from i18n translation

Javascript ports are located in `assets/js` and are linked to elm via `src/Ports.elm`

The elm page structure are built in Global.elm, which defines how the navbar, footbar and body are assembled.

## Routing

We use [elm-spa](https://www.elm-spa.dev/) as the spa framework. It defines the routing and the Main input logic. Basically, files located in `Pages` will be an accessible route in the browser. 
For example: `Pages/o/Dynamiyc.elm` makes the route at `https://[domain_name]/o/my_org`

use `node_modules/.bin/elm-spa --help` to see the command to add new route and page.

Note: For the moment we lock the `elm-spa` version to the v4 version as it is working well, and upgrading to v6 will be time consuming while the benefits of it is not guaranteed.


## Styling

The elm code use scss/sass and the Bulma framework for the html design. 
The sass code is in `assets/sass/`.


## Rules & Guidelines

- The code is build with webpack and `webpack.config.js` file.
- Follow the Elm Architecture (Model, View, Update) pattern.
- Elm Messages are prefixed with `On` + verb (e.g. `OnSubmitClick`).
- Global messages are prefixed with `Do` + verb (e.g. `DoUpdateSession`).
- Component are focused on a single responsibility.
- When creating a new component, generate a template that follows Fractale standard with `melm.py add -w Components.MyComponent [-t modal]` to generate templates. The file will be generated in `src/Components/MyComponent.elm`. Add the option `-t modal` if the component live in a modal window.
- When the component should be used inside another elm file, let's say in a page located at `src/Page/Welcome.com`, you will need to write some boilerplate code to use your component. The following command will help you by adding in your file the necessary boilerplate code to use the component: `melm.py push -w Components.MyDropdown Page.Exemple`. In this case a component will be added under `src/Components/MyDropdown.elm` and boilerplate coded to use it injected in `src/Page/Exemple`.
- When using `melm.py`to generate file, keep in mind that their are template and might need updated. Let you drive by the elm compiler.

- Git commits: Use semantic prefixes (feat:, fix:, refactor:, etc) followed by context and description.

### Before Reading and Editing files

Try to minimize the number of ELM file you read as they can be long. Use extensively find and rg (ripgrep) when you search something before reading it to reduce the cost.

When you read Elm file, away filter the update function to minimize its content with the command: `awk '/^update[ :]/{skip=1; next} skip && /^[^ ]/{skip=0} !skip' file.elm`
If you need the read a update code of msg grep the code for the message/msg specifically.


### After adding a feature or a refactor

- Alway run elm after you make modification to elm file to ensure it compiles with : `elm make src/Main.elm`.
- If the Elm compiler files, read carefully the helpful error messages and fix the errors until it compiles.
- Update documentation by either add a new file for a new feature or update existing one if needed in `docs/` folder.
