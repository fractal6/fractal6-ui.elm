# Install

    npm install


# Run the test server

Build the code and run a webpack dev server

    make run

or alternatively (manual build)

    make build
    npm run webdev

# Build the production code

The code will be generated in the `dist` folder.

    make prod


# Regenerate files with external dependencies

Generate the GraphQL parser code for Elm:

    make gen

Generate the favicon:

    make icon


