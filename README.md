# Fractal6-ui.elm

Single-page application frontend for [Fractale](https://fractale.co).


## Install

    npm install


## Launch

First, generate the elm i18n langage source code (it will generate the `src/Text.elm` file): 

    ./i18n.py gen -w -l

**Run the test server**

Build the code and run a webpack dev server (hot reload)

    make run

To run webpack with the production code (i.e with optimization)

    make run_prod


## Build for production

The code will be generated in the `dist` folder.

    make prod


## Contributing

Fractale is free, open-source software licensed under AGPLv3.

You can open issues for bugs you've found or features you think are missing. You can also submit pull requests to this repository. To get started, take a look at [CONTRIBUTING.md](CONTRIBUTING.md).

You can follow Fractale organisation and roadmap at [o/f6](https://fractale.co/o/f6).

IRC channel: #fractal6 on matrix.org
