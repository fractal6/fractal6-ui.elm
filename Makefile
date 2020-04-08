default: build_root

build_root:
	# webkit !
	@cp -v assets/js/*.js public/
	npm run css_build

setup-css:
	npm install node-sass --save-dev
	npm install bulma --save-dev

gen:
	npm run graphql_build

generate_starwars:
	./node_modules/.bin/elm-graphql https://elm-graphql.herokuapp.com/ --base StarWars --output ./src

