## Install

    npm install [--legacy-peer-deps]


NOTE: some packake needs `--legacy-peer-deps` options to be installed without npm trowing an error.
      It seems to be caused by elm-hot-webpack-loader@1.1.8 and webpack@5...

    npm install --save-dev --legacy-peer-deps elm-hot-webpack-loader
    npm install --save-dev --legacy-peer-deps node-sass


## Launch

**Run the test server**

Build the code and run a webpack dev server (hot reload)

    make run

To run webpack with the production code (i.e with optimization)

    make run_prod


## Build

**Build the production code**

The code will be generated in the `dist` folder.

    make prod


**Re-generate the GraphQL parser code for Elm**

    make gen


## Creating new components

A components is a reusable piece of code use in a application to implement a given functionality.
By components, we mean here a state-full Elm module, that is a module which have its own state, model & Msg managed by itself. It allows to limit the complexity of the elm files and prevents file from being too big.
For simple components that don't have states (i.e. have a few or no Msg), they can be implemented by getting the msg from the main page, as it is done
in `Components/ColorPicker.elm` for example.
For more complex components, a lot of boilerplate is involved when creating a component with Elm (for the best!) that maintain their own state.
To help building new components without coding again and again the same thing,
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


## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).
