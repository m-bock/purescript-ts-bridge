build:
    spago build


dist: build
    rm -rf dist
    mkdir dist
    cp -r output assets package.json bin -t dist

run args='':
    mkdir -p tmp
    export ASSETS_DIR=assets; \
    spago --quiet --no-psa run --main TsBridgeGen.Main --node-args \
    "{{args}} --modules-file tmp/MyTsBridgeModules.purs --class-file tmp/MyTsBridgeClass.purs"

test:
    spago test

format:
    purs-tidy format-in-place 'src/**/*.purs'

docs:
    spago docs --no-search

install:
    yarn install

ci: install test end2end docs dist

end2end:
    bash end-to-end.sh

end2end-local:
    export TARGET_DIR="e2e"; \
    export RM_GLOB="!(output|node_modules)"; \
    bash end-to-end.sh

