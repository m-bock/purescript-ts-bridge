export PATH := "node_modules/.bin:" + env_var('PATH')
set shell := ["bash", "-c"]

build:
    spago build

build-strict:
    spago build

clean:
    rm -rf .spago output .psa-stash

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

install-git-hooks:
    rm -rf .git/hooks
    ln -s ../git-hooks .git/hooks

# CI

ci_: install check-format build-strict test gen-docs check-git-clean

ci:
    DIR=`pwd`; \
    TMP_DIR=`mktemp -d`; \
    git clone . ${TMP_DIR}; \
    cd ${TMP_DIR}; \
    just ci_

install:
    yarn install

check-git-clean:
    [ -z "$(git status --porcelain)" ]