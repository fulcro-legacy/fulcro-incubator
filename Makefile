test:
	clj -A:dev:ci-test unit
	npm install
	npx shadow-cljs compile ci
	npx karma start --single-run

docs/routing.html: dynamic-routing.adoc
	asciidoctor -o docs/routing.html -b html5 -r asciidoctor-diagram dynamic-routing.adoc

docs/state-machines.html: state-machine-docs.adoc
	asciidoctor -o docs/state-machines.html -b html5 -r asciidoctor-diagram state-machine-docs.adoc

all: docs/routing.html docs/state-machines.html

