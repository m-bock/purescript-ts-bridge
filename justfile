build:
    spago build

dist: build
    rm -rd dist
    mkdir dist
    cp -r output assets package.json bin -t dist

run:
    mkdir -p tmp
    export ASSETS_DIR=assets; \
    spago --quiet --no-psa run --main TsBridgeGen.Main --node-args \
    "--modules-file tmp/MyTsBridgeModules.purs --class-file tmp/MyTsBridgeClass.purs"
test:
    spago test

format:
    purs-tidy format-in-place 'src/**/*.purs'

docs:
    spago docs --no-search

install:
    yarn install