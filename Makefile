.ONESHELL:
SHELL := /bin/bash
OUTPUT := dist
LANGS := en fr
elm-js := elm.js
elm-min-js := elm.min.js
uglifyjs := node_modules/uglify-js/bin/uglifyjs
$(eval BRANCH_NAME=$(shell git rev-parse --abbrev-ref HEAD))
$(eval COMMIT_NAME=$(shell git rev-parse --short HEAD))
$(eval RELEASE_VERSION=$(shell git tag -l --sort=-creatordate | head -n 1))
NAME := fractal6-ui.elm
RELEASE_NAME := fractal6-ui
RELEASE_DIR := releases/$(BRANCH_NAME)/$(RELEASE_VERSION)
BUILD_DIRS := $(addprefix public-build/, $(LANGS))
RELEASE_BUILD_DIRS := $(addprefix releases/, $(LANGS))

# To publish {op|prod}
# - generate release: git cliff --unreleased --tag XXX --topo-order
# - git tag XXX
# - git checkout op && git merge op
#   - eventually patch op/**
# - make publish_op
# - Annoucements !

#.PHONY: $(BUILD_DIRS)
.PHONY: review

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

gen:
	# Remove all directive due to a bug of elm-graphql who
	# does not support multiple directive on the same field.
	sed -E  "s/^directive .*$$//g;  s/@[[:alnum:]_]+\([^\)]+\)//g; s/@[[:alnum:]_]+//g;" ../fractal6.go/schema/schema.graphql > schema.gql
	npm run graphql_build
	rm schema.gql

review:
	# Setup: elm-review init --template jfmengels/elm-review-config/application
	mkdir -p review
	elm-review --ignore-dirs src/Fractal/ --compiler node_modules/.bin/elm > review/reviews.json

#
# Publish builds in public folder for all LANGS
#

# Deploy on Netlyfy/Fleek
publish_build: prod
	cd ../build && \
		rm static -rf && \
		cp ../fractal6-ui.elm/dist/* . -r && \
		cp ../fractal6-ui.elm/netlify.toml . && \
		git add * && \
		git commit -m "$(BRANCH_NAME) - $(COMMIT_NAME)" && \
		git push origin master && \
		cd -


# Build and publish in public-build repo.
publish: $(BUILD_DIRS)
	@echo $(COMMIT_NAME) > ../public-build/client_version && \
		cd ../public-build/ && \
		git add * && \
		git commit -m "$(BRANCH_NAME) - $(COMMIT_NAME)" && \
		git push origin main && \
		cd - && \
		echo "-- $@ in ../public-build/ done"

# Build in public-build but don't push.
publish_test: $(BUILD_DIRS)
	@git rev-parse --short HEAD > ../public-build/client_version
	echo "-- $@ in ../public-build/ done"

$(BUILD_DIRS): public-build/%:
	@# @DEBUG: -jX option won't work as Text.elm will be overwritten...(copy in a sperate environement?)
	# Build frontend and Replace "/static/" link in the index.html entry point
	./i18n.py gen -w -l $* && \
		if [ $(MAKECMDGOALS) == publish_test ]; then \
			npm run prod -- --env lang=$* --env debug=test; \
		elif [ $(MAKECMDGOALS) == publish_op ]; then \
			npm run prod -- --env lang=$* --env theme=light; \
		else \
			npm run prod -- --env lang=$*; \
		fi && \
		rm -rf ../public-build/$* && \
		mkdir ../public-build/$* && \
		cp -r dist/* ../public-build/$* && \
		sed -i "s/\/static\//\/$*\/static\//g" ../public-build/$*/index.html && \
		echo "buid $* for $@"

#
# Publish builds in prod releases
#

publish_prod: pre_build_prod build_release_prod
	@git push origin prod
	@echo "-- Please upload your release to github: $(RELEASE_DIR)/$(RELEASE_NAME)"

pre_build_prod:
	@if [ "$(BRANCH_NAME)" != "prod" ]; then
		echo "You should be on the 'prod' branch to use this rule."
		exit 1
	fi
	@if [ -d "$(RELEASE_DIR)" ]; then
		echo "$(RELEASE_DIR) does exist, please remove it manually to rebuild this release."
		exit 1
	fi
	echo "Building (or Re-building) release: $(RELEASE_NAME)"
	mkdir -p $(RELEASE_DIR)/$(RELEASE_NAME)

build_release_prod: $(RELEASE_BUILD_DIRS)
	@echo $(COMMIT_NAME) > $(RELEASE_DIR)/$(RELEASE_NAME)/client_version && \
		(cd $(RELEASE_DIR) && zip -q -r - $(RELEASE_NAME)) > $(RELEASE_NAME).zip && \
		mv $(RELEASE_NAME).zip $(RELEASE_DIR)

#
# Publish builds in op releases
#

publish_op: pre_build_op build_release_op upload_release_op
	@git push f6 op
	@echo "-- op release published"

pre_build_op:
	@if [ "$(BRANCH_NAME)" != "op" ]; then
		@echo "You should be on the 'op' branch to use this rule."
		exit 1
	fi
	@if [ -d "$(RELEASE_DIR)" ]; then
		@echo "$(RELEASE_DIR) does exist, please remove it manually to rebuild this release."
		exit 1
	fi
	@if [ -z "$(F6_TOKEN)" ]; then
		@echo "F6_TOKEN is not defined. Set your token to upload a release."
		exit 1
	fi
	echo "Building (or Re-building) release: $(RELEASE_NAME)"
	mkdir -p $(RELEASE_DIR)/$(RELEASE_NAME)

build_release_op: $(RELEASE_BUILD_DIRS)
	@echo $(COMMIT_NAME) > $(RELEASE_DIR)/$(RELEASE_NAME)/client_version && \
		(cd $(RELEASE_DIR) && zip -q -r - $(RELEASE_NAME)) > $(RELEASE_NAME).zip && \
		mv $(RELEASE_NAME).zip $(RELEASE_DIR)

upload_release_op:
	@echo "Uploading to " "https://code.fractale.co/api/packages/fractale/generic/$(NAME)/$(RELEASE_VERSION)/$(RELEASE_NAME).zip"
	@curl -f -k -H "Authorization: token $(F6_TOKEN)" --progress-bar \
		--upload-file $(RELEASE_DIR)/$(RELEASE_NAME).zip \
		https://code.fractale.co/api/packages/fractale/generic/$(NAME)/$(RELEASE_VERSION)/$(RELEASE_NAME).zip

delete_release_op:
	@echo "Deleteing " "https://code.fractale.co/api/packages/fractale/generic/$(NAME)/$(RELEASE_VERSION)/$(RELEASE_NAME).zip"
	curl -k -H "Authorization: token $(F6_TOKEN)" -X DELETE \
		https://code.fractale.co/api/packages/fractale/generic/$(NAME)/$(RELEASE_VERSION)/$(RELEASE_NAME).zip

#
# Share release build rules
#

$(RELEASE_BUILD_DIRS): releases/%:
	@# @DEBUG: -jX option won't work as Text.elm will be overwritten...(copy in a sperate environement?)
	# Build frontend and Replace "/static/" link in the index.html entry point
	./i18n.py gen -w -l $* && \
		if [ $(MAKECMDGOALS) == publish_test ]; then \
			npm run prod -- --env lang=$* --env debug=test; \
        elif [ $(MAKECMDGOALS) == publish_op ]; then \
			npm run prod -- --env lang=$* --env theme=light; \
		else \
			npm run prod -- --env lang=$*; \
		fi && \
		rm -rf $(RELEASE_DIR)/$(RELEASE_NAME)$* && \
		mkdir $(RELEASE_DIR)/$(RELEASE_NAME)/$* && \
		cp -r dist/* $(RELEASE_DIR)/$(RELEASE_NAME)/$* && \
		sed -i "s/\/static\//\/$*\/static\//g" $(RELEASE_DIR)/$(RELEASE_NAME)/$*/index.html && \
		echo "buid $@ for $(RELEASE_DIR)/$(RELEASE_NAME)"


# =============================================================================

assets: icon css js

install:
	# Node.js
	npm install
	# Python
	pip install docopt-ng


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

