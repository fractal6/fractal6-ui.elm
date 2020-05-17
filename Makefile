default: build

build:
	# webkit !
	npm run js_build
	npm run css_build

run:
	npm run start

gen:
	npm run graphql_build

generate_starwars:
	./node_modules/.bin/elm-graphql https://elm-graphql.herokuapp.com/ --base StarWars --output ./src

elm-spa-org-alias:
	rm -rf src/Pages/O/
	cp -r src/Pages/Org src/Pages/O
	find src/Pages/O -type f | xargs sed -i "s/Org/O/g"


_setup-css:
	npm install node-sass --save-dev
	npm install bulma --save-dev

