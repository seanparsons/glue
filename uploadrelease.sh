#!/usr/bin/env bash

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

cd $ROOT/glue-core
rm dist/glue-core-*.tar.gz
cabal sdist
cabal upload --publish dist/glue-core-*.tar.gz

cd $ROOT/glue-common
rm dist/glue-common-*.tar.gz
cabal sdist
cabal upload --publish dist/glue-common-*.tar.gz

cd $ROOT/glue-ekg
rm dist/glue-ekg-*.tar.gz
cabal sdist
cabal upload --publish dist/glue-ekg-*.tar.gz

cd $ROOT/glue-example
rm dist/glue-example-*.tar.gz
cabal sdist
cabal upload --publish dist/glue-example-*.tar.gz

echo $ROOT
