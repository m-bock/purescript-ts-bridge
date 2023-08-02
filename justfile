set shell := ["bash", "-c"]

allowed_warnings := "ImplicitQualifiedImport"

build:
    spago build --purs-args '--stash --censor-lib --censor-codes={{allowed_warnings}}'

build-strict:
    spago build --purs-args '--stash --censor-lib --strict --censor-codes={{allowed_warnings}}'

clean:
    rm -rf .spago output .psa-stash

format:
    purs-tidy format-in-place 'src/**/*.purs'
    purs-tidy format-in-place 'test/**/*.purs'

check-format:
    purs-tidy check 'src/**/*.purs'
    purs-tidy check 'test/**/*.purs'    

test:
    spago --config test.dhall test

gen-docs:
    node gen-docs/gen-md.js

check-spell:
    yarn run cspell "test/**/*.purs" || true
    yarn run cspell "src/**/*.purs" || true
    yarn run cspell "docs/**/*.md" || true
    yarn run cspell "README.md" || true

ci: clean check-format build-strict test gen-docs check-git-clean

check-git-clean:
    [ -z "$(git status --porcelain)" ]