#!/usr/bin/env bash

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

cd $ROOT/glue-core
cabal sdist
cabal upload --publish dist/glue-core-*.tar.gz

cd $ROOT/glue-common
cabal sdist
cabal upload --publish dist/glue-common-*.tar.gz

cd $ROOT/glue-ekg
cabal sdist
cabal upload --publish dist/glue-ekg-*.tar.gz

cd $ROOT/glue-example
cabal sdist
cabal upload --publish dist/glue-example-*.tar.gz

echo $ROOT
