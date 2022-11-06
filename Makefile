.ONESHELL:
SHELL := /bin/bash
OUTPUT := dist
LANGS := en fr
elm-js := elm.js
elm-min-js := elm.min.js
uglifyjs := node_modules/uglify-js/bin/uglifyjs


default: run

run:
	npm run webdev

run_prod:
	npm run webprod

run_old:
	npm run start

build_: assets
	npm run build

prod:
	npm run prod

dev:
	npm run dev

# Deploy on Netlyfy/Fleek
deploy_netlify: prod
	$(eval BRANCH_NAME=$(shell git rev-parse --abbrev-ref HEAD))
	$(eval COMMIT_NAME=$(shell git rev-parse --short HEAD))
	cd ../build && \
		rm static -rf && \
		cp ../fractal6-ui.elm/dist/* . -r && \
		cp ../fractal6-ui.elm/netlify.toml . && \
		git add * && \
		git commit -m "$(BRANCH_NAME) - $(COMMIT_NAME)" && \
		git push origin master && \
		cd -

# Publish builds for all LANGS
BUILD_DIRS := $(addprefix public/, $(LANGS))
#.PHONY: $(BUILD_DIRS)
publish: $(BUILD_DIRS)
	$(eval BRANCH_NAME=$(shell git rev-parse --abbrev-ref HEAD))
	$(eval COMMIT_NAME=$(shell git rev-parse --short HEAD))
	git rev-parse --short HEAD > ../public-build/client_version && \
		cd ../public-build/ && \
		git add * && \
		git commit -m "$(BRANCH_NAME) - $(COMMIT_NAME)" && \
		git push origin main && \
		cd - && \
		echo "-- $@ done"
# --
publish_test: $(BUILD_DIRS)
	git rev-parse --short HEAD > ../public-build/client_version
	echo "-- $@ done"
# --
$(BUILD_DIRS): public/%:
	# @DEBUG: -jX option won't work as Text.elm will be overwritten...(copy in a sperate environement?)
	# Build frontend and Replace "/static/" link in the index.html entry point
	./i18n.py gen -w -l $* && \
		if [ $(MAKECMDGOALS) == publish_test ]; then \
			npm run prod -- --env lang=$* --env debug=test; \
		else \
			npm run prod -- --env lang=$*; \
		fi && \
		rm -rf ../public-build/$* && \
		mkdir ../public-build/$* && \
		cp -r dist/* ../public-build/$* && \
		sed -i "s/\/static\//\/$*\/static\//g" ../public-build/$*/index.html && \
		echo "buid $* lang to $@"

gen:
	# Remove all directive due to a bug of elm-graphql who
	# does not support multiple directive on the same field.
	sed -E  "s/^directive .*$$//g;  s/@[[:alnum:]_]+\([^\)]+\)//g; s/@[[:alnum:]_]+//g;"  ../fractal6-schema/gen/schema.graphql  > schema.gql
	npm run graphql_build
	rm schema.gql

review:
	# TODO: try elm-review
	# INstall:
	# npx elm-review init --template jfmengels/elm-review-config/application


# =================================

assets: icon css js

install:
	# Node.js
	npm install
	# Python
	pip install docopt
	pip install loguru


elm-spa:
	npm run build:elm-spa


elm:
	elm make src/Main.elm --optimize --output=$(elm-js)
	$(uglifyjs) elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | $(uglifyjs) --mangle --output=$(OUTPUT)/$(elm-min-js)
	#@echo Compiled size:$(shelllll cat elm.js | du -k) Kb
	#@echo Minified size:$(shelllll cat dist/elm.min.js | du -k) Kb
	#@echo Gzipped size: $(shelllll cat dist/elm.min.js) | gzip -c | du -k) Kb

css:
	npm run css_build

js:
	npm run js_build

icon:
	cp ../fractal6-logo/img/v1/favicon.ico assets/images/logo/favicon.ico
	cp ../fractal6-logo/img/v1/favicon*.png assets/images/logo/
	cp ../fractal6-logo/img/v1/fractale-inv.svg assets/images/

_generate_starwars:
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

