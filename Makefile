OUTPUT := public/dist
elm-js := elm.js
elm-min-js := elm.min.js
uglifyjs := node_modules/uglify-js/bin/uglifyjs

default: run

run:
	npm run start

gen:
	npm run graphql_build

build: assets elm-spa elm
	# webkit !
	npm run js_build
	npm run css_build

###

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
	convert -resize x16 -gravity center -crop 16x16+0+0 ../logo/img/fractal-rotated-circle2.svg.png  -flatten -colors 256 assets/images/favicon-16.png
	convert -resize x32 -gravity center -crop 32x32+0+0 ../logo/img/fractal-rotated-circle2.svg.png  -flatten -colors 256 assets/images/favicon-32.png
	convert -resize x64 -gravity center -crop 64x64+0+0 ../logo/img/fractal-rotated-circle2.svg.png  -flatten -colors 256 assets/images/favicon-64.png
	icotool -c -o assets/images/favicon.ico assets/images/favicon-16.png assets/images/favicon-32.png assets/images/favicon-64.png
	cp assets/images/favicon.ico public/

generate_starwars:
	./node_modules/.bin/elm-graphql https://elm-graphql.herokuapp.com/ --base StarWars --output ./src

elm-spa-org-alias:
	rm -rf src/Pages/O/
	cp -r src/Pages/Org src/Pages/O
	find src/Pages/O -type f | xargs sed -i "s/Org/O/g"



clean:
	#rm ./package-lock.json
	npm cache clear --force

