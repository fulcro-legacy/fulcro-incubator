test:
	clj -A:dev:provided:test:ci-test unit
	npm install
	npx shadow-cljs compile ci
	npx karma start --single-run
