default: build_root

build_root:
	# weblit !
	@cp -v assets/js/*.js public/
	npm run css_build

setup-css:
	npm install node-sass --save-dev
	npm install bulma --save-dev

setup-elm-graphql:
	elm install dillonkearns/elm-graphql
	elm install elm/json
	npm install --save-dev @dillonkearns/elm-graphql
