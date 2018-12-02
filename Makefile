test:
	clj -A:dev:ci-test unit
	npm install
	npx shadow-cljs compile ci
	npx karma start --single-run
