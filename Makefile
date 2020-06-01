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

icon:
	convert -resize x16 -gravity center -crop 16x16+0+0 ../logo/img/fractal-rotated-circle2.svg.png  -flatten -colors 256 assets/images/favicon-16.png
	convert -resize x32 -gravity center -crop 32x32+0+0 ../logo/img/fractal-rotated-circle2.svg.png  -flatten -colors 256 assets/images/favicon-32.png
	convert -resize x64 -gravity center -crop 64x64+0+0 ../logo/img/fractal-rotated-circle2.svg.png  -flatten -colors 256 assets/images/favicon-64.png
	icotool -c -o assets/images/favicon.ico assets/images/favicon-16.png assets/images/favicon-32.png assets/images/favicon-64.png
	cp assets/images/favicon.ico public/


_setup-css:
	npm install node-sass --save-dev
	npm install bulma --save-dev

