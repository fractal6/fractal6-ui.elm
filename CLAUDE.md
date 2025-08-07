# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This codebase is in a web app written in elm (elm-lang). It implements the frontend of a project management platform called Fractale (fractale.co).

## Code Style Guidelines
- Format Elm code with `elm-format`
- Elm Messages should be prefixed with `On` + verb (e.g., `OnSubmitClick`)
- Global messages should be prefixed with `Do` + verb (e.g., `DoUpdateSession`)
- Component naming: Use `melm.py add -w Components.MyComponent` to generate templates
- Git commits: Use semantic prefixes (feat, fix, refactor, etc.) followed by context and description
- Follow the Elm Architecture (Model, View, Update) pattern
- Keep modules small and focused on a single responsibility
- Use types everywhere possible
- Error handling: Use Maybe/Result types for potential failures
- Imports should be organized alphabetically with qualified imports last

## Project Structure
- `src/`  - the Elm code
- `src/Form/` - Components for creating new tensions
- `src/Components/` - Reusable components with their own state
- `src/Bulk/` - Common data structures and helpers
- `src/Query/` - GraphQL queries
- `src/Request` - REST queries
- `src/Codecs` - JSON encoders/decoders
- `src/Ports` - JavaScript interop
- `assets/sass`  - the css/sass code
