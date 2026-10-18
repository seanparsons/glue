#!/usr/bin/env bash
set -euo pipefail

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$ROOT"

rm -rf dist-newstyle/sdist
cabal sdist all

for pkg in glue-common glue-core glue-ekg glue-example; do
  cabal upload --publish "dist-newstyle/sdist/${pkg}-"*.tar.gz
done
