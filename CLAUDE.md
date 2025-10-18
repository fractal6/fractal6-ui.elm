# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This codebase is a webapp written in elm (elm-lang). It implements the user interface (frontend) of a project and organization management platform called Fractale (fractale.co). 
It takes inspiration from self-organisation and Agile approaches.


## Elm code Structure

The elm entry point is defined in the ./public/index.html and ./public/index.js files.

The elm code is structured as follows:

- `src/` - the Elm code
- `src/Fractal` - The auto-generated type from the graphql schema of the fractale API
- `src/Components/` and `src/Form` - Reusable components with their own state
- `src/Bulk/` - Common data structures and helpers
- `src/Query/` - GraphQL queries
- `src/Request` - REST queries
- `src/Codecs` - JSON encoders/decoders
- `src/Ports` - JavaScript interop


## Styling

The elm code use scss/sass and the Bulma framework for the html design. The code is in `assets/sass/`.


## Code Style Guidelines

- Elm Messages are prefixed with `On` + verb (e.g. `OnSubmitClick`).
- Global messages are prefixed with `Do` + verb (e.g. `DoUpdateSession`).
- Component are focused on a single responsibility.
- When creating a new component, generate a template that follows Fractale standard with `melm.py add -w Components.MyComponent [-t modal]` to generate templates. Add the option `-t modal` if the component leave in a modal window.
- Git commits: Use semantic prefixes (feat, fix, refactor, etc) followed by context and description.
- Follow the Elm Architecture (Model, View, Update) pattern.

