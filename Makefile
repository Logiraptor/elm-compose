
all: output/styles.css output/main.js output/index.html output/config.js

output/index.html: src/Build/index.html
	cp src/Build/index.html output/index.html

output/config.js: src/Build/config.js
	cp src/Build/config.js output/config.js

output/styles.css: src/Style.elm src/Build/Stylesheets.elm output
	elm-css src/Build/Stylesheets.elm --output output

output/main.js: src/* output
	elm-make src/budget.elm --output output/main.js

output:
	mkdir output

clean:
	rm -r output