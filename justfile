build:
    spago build

run:
    export ASSETS_DIR=assets; \
    spago run --main TsBridgeGen.Cli

test:
    spago test

format:
    purs-tidy format-in-place 'src/**/*.purs'

docs:
    spago docs --no-search