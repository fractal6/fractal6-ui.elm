.ONESHELL:
SHELL := /bin/bash
OUTPUT := public/dist
elm-js := elm.js
elm-min-js := elm.min.js
uglifyjs := node_modules/uglify-js/bin/uglifyjs

default: run

run:
	# Run dev
	npm run webdev

run_localprod:
	npm run localprod

run_prod:
	npm run webprod

run_old:
	npm run start

build: assets
	npm run build

prod:
	npm run prod

deploy: prod
	cd ../build
	rm -r static
	cp -r ../fractal6-ui.elm/dist/* .
	git add *
	git commit -m all
	git push origin master
	cd -

gen:
	# Remove all directive due to a big of elm-graphql who
	# does not support multiple directive on the same field.
	sed -E  "s/^directive .*$$//g;  s/@[[:alnum:]_]+\([^\)]+\)//g; s/@[[:alnum:]_]+//g;"  ../schema/gen/schema.graphql  > schema.gql
	npm run graphql_build
	rm schema.gql


# =================================

assets: icon css js

elm-spa:
	elm-spa build .

elm:
	elm make src/Main.elm --optimize --output=$(elm-js)
	$(uglifyjs) elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | $(uglifyjs) --mangle --output=$(OUTPUT)/$(elm-min-js)
	#@echo Compiled size:$(shelllll cat elm.js | du -k) Kb
	#@echo Minified size:$(shelllll cat public/dist/elm.min.js | du -k) Kb
	#@echo Gzipped size: $(shelllll cat public/dist/elm.min.js) | gzip -c | du -k) Kb

css:
	npm run css_build

js:
	npm run js_build

icon:
	cp ../fractal6-logo/img/v1/favicon.ico assets/images/logo/favicon.ico
	cp ../fractal6-logo/img/v1/favicon*.png assets/images/logo/
	cp assets/images/logo/favicon.ico public/

generate_starwars:
	./node_modules/.bin/elm-graphql https://elm-graphql.herokuapp.com/ --base StarWars --output ./src

_elm-spa-org-alias:
	rm -rf src/Pages/O/
	cp -r src/Pages/Org src/Pages/O
	find src/Pages/O -type f | xargs sed -i "s/Org/O/g"

clean:
	rm -rf dist/

clean_npm:
	#rm ./package-lock.json
	npm cache clear --force
	npm prune

