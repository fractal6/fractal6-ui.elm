# Install

    npm install


# Creating new components

A components is reusable piece of code use in [page](see elm-spa page) to implement a given functionality. 
A lot of boilerplate is involved when creating component with elm (for the best!) that maintain their own state.
To help building new components that are relevant with the framework developed the repo, and without coding again and again the same thing,
we provide a script to generate template code for your new component called `melm.py`.

Let say that you need a new component that implement a dropdown menu, and put the file in the `src/Components/` folder.
You will create the template for you dropdown like this

    melm.py add -w Components.MyDropdown

The file `MyDropdown.elm` will be created for you.

If your component is a modal, then you will need to change the default template as follows

    melm.py add -w -t modal Components.MyDropdown


Finally, when you want to use your component inside a file, let's say in a page located at `src/Page/Welcome.com`, you will need to write some boilerplate code to use your component. The following command will help you by adding in your file the necessery boilerplate code to use the component 

    melm.py push -w Components.MyDropdown Page.Exemple

Note: some manually edit can be necessayry anyway, but following the elm compiler should guide you to light.

You can obtain the full script documentation by typing

    melm.py --help

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


