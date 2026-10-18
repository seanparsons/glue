#!/usr/bin/env bash
set -euo pipefail

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$ROOT"

rm -rf dist-newstyle/sdist
cabal sdist all

for pkg in glue-common glue-core glue-ekg glue-example; do
  tarball=$(echo "dist-newstyle/sdist/${pkg}-"*.tar.gz)
  pkgid=$(basename "$tarball" .tar.gz)
  status=$(curl -s -o /dev/null -w '%{http_code}' "https://hackage.haskell.org/package/${pkgid}")
  case "$status" in
    200) echo "${pkgid} is already on Hackage, skipping." ;;
    404) cabal upload --publish "$tarball" ;;
    *) echo "Unexpected HTTP ${status} from Hackage for ${pkgid}." >&2; exit 1 ;;
  esac
done
