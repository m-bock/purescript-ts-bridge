set shell := ["bash", "-c"]

allowed_warnings := "ImplicitQualifiedImport"

build:
    spago build --purs-args '--stash --censor-lib --censor-codes={{allowed_warnings}}'

build-strict:
    spago build --purs-args '--stash --censor-lib --strict --censor-codes={{allowed_warnings}}'

clean:
    rm -rf output

format:
    purs-tidy format-in-place 'src/**/*.purs'
    purs-tidy format-in-place 'test/**/*.purs'

check-format:
    purs-tidy check 'src/**/*.purs'
    purs-tidy check 'test/**/*.purs'    

test:
    spago test

gen-docs:
    node gen-docs/gen-md.js

check-spell:
    yarn run cspell "test/**/*.purs" || true
    yarn run cspell "src/**/*.purs" || true
    yarn run cspell "docs/**/*.md" || true
    yarn run cspell "README.md" || true

ci: check-format gen-docs check-git-clean build-strict test

check-git-clean:
    [ -z "$(git status --porcelain)" ]