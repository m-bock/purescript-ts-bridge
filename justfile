build:
    spago build

format:
    purs-tidy format-in-place 'src/**/*.purs'
    purs-tidy format-in-place 'test/**/*.purs'

test:
    spago test

gen-readme:
    node gen-md.js