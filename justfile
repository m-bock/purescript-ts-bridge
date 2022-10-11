build:
    spago build


dist: build
    rm -rf dist
    mkdir dist
    cp -r output assets package.json bin -t dist

run: build
    cd ../purescript-typescript-bridge.sample-project; \
    node ../purescript-typescript-bridge/bin/index.js

test:
    spago test

format:
    purs-tidy format-in-place 'src/**/*.purs'

docs:
    spago docs --no-search

install:
    yarn install

ci: install test end2end docs dist

ci-local: install test end2end-local docs dist

end2end:
    bash end-to-end.sh

end2end-local:
    export TARGET_DIR="e2e"; \
    export RM_GLOB="!(output|node_modules)"; \
    bash end-to-end.sh

