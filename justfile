build:
    spago build

dist: build
    rm -rd dist
    mkdir dist
    cp -r output assets package.json bin -t dist

run:
    export ASSETS_DIR=assets; \
    spago run --main TsBridgeGen.Cli

test:
    spago test

format:
    purs-tidy format-in-place 'src/**/*.purs'

docs:
    spago docs --no-search