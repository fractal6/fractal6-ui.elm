# Fractale - self-organisation for humans

Elm frontend frontend for [Fractale](https://fractale.co).

**Fractale** is a platform for self-organisation. It is designed around the concept that an organisation can be represented as a tree and should follow principles of transparency, governance decentralization and authority distribution. A tree divides in branches and form leaves, likewise an organisation divides in **Circles** that can have **Roles**. Both, circles and roles have an associated descriptive document, called **Mandate**, intended to define its purpose and operating rules. Finally, the communication inside the organisation is done through **Tensions**, and make the link between users and organisations. You can think of a tension as an email, but more structured and more powerful.

Using Fractale for your organisation offers the following capabilities and features:
* Interactive tree and graph packing organisation chart
* Organisation visibility defined at circles level
* ACL based on member roles and circle governance rules
* Ticketing management through Tensions
* Discussion thread and subscription by tension
* Email notifications broadcast and email reply
* Labels system
* Role templates system
* Journal history of events (including mandate updates!)
* GraphQL API


## Install

    npm install


## Launch

First, generate the elm i18n langage source code (it will generate the `src/Text.elm` file): 

    ./i18n.py gen -w -l [LANG]

**Run the test server**

Build the code and run a webpack dev server (hot reload)

    make run

To run webpack with the production code (i.e with optimization)

    make run_prod


## Build for production

The code will be generated in the `dist` folder:

> you will need to edit the API_URL in `webpack.config.js` to match your server domain and CORS requirements Otherwise, the front will query the api.fractale.co domain.

    make prod


## Contributing

You can open issues for bugs you've found or features you think are missing. You can also submit pull requests to this repository. To get started, take a look at [CONTRIBUTING.md](CONTRIBUTING.md).

You can follow Fractale organisation and roadmap at [o/f6](https://fractale.co/o/f6) and espacially [t/f6/ui-ux](https://fractale.co/t/f6/ui-ux).

IRC channel: #fractal6 on matrix.org


## License

Fractale is free, open-source software licensed under AGPLv3.
