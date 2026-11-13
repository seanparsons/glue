#!/usr/bin/env bash
# Release tooling for the glue packages.
#
#   ./uploadrelease.sh bump <version>
#       Sets every package to <version>, updates the bounds between the glue packages, adds a
#       changelog section to fill in for each package, and commits the result.
#
#   ./uploadrelease.sh [--dry-run]
#       Releases the current version from an up-to-date master: checks the working tree, versions,
#       changelogs and tag, runs `cabal check`, builds and tests every package on every supported
#       GHC through the Nix flake, uploads each package not yet on Hackage, then creates and pushes
#       the v<version> tag. --dry-run does everything except uploading and tagging.
#
# Set SKIP_TESTS=1 to skip the Nix build and test run.
set -euo pipefail

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$ROOT"

PACKAGES=(glue-common glue-core glue-ekg glue-example glue-prometheus)
TODO_MARKER="TODO: describe the changes in this release."

die() { echo "error: $*" >&2; exit 1; }
step() { echo; echo "==> $*"; }

usage() {
  sed -n '2,15p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
}

package_version() {
  sed -n 's/^version:[[:space:]]*//p' "$1/$1.cabal"
}

# The version shared by every package, failing if they disagree.
current_version() {
  local version="" pkg v
  for pkg in "${PACKAGES[@]}"; do
    v=$(package_version "$pkg")
    [ -n "$v" ] || die "no version found in $pkg/$pkg.cabal"
    if [ -z "$version" ]; then
      version=$v
    elif [ "$v" != "$version" ]; then
      die "package versions disagree: $pkg is $v but ${PACKAGES[0]} is $version"
    fi
  done
  echo "$version"
}

# The changelog section for a version, without its heading.
changelog_section() {
  awk -v heading="## $2" '
    $0 == heading { inside = 1; next }
    inside && /^## / { exit }
    inside { print }
  ' "$1/CHANGELOG.md"
}

require_clean_tree() {
  [ -z "$(git status --porcelain)" ] || die "the working tree has uncommitted changes"
}

# Prints 200 if the package version is on Hackage, 404 if not; fails on anything else.
hackage_status() {
  local status
  status=$(curl -s -o /dev/null -w '%{http_code}' "https://hackage.haskell.org/package/$1-$2")
  case "$status" in
    200|404) echo "$status" ;;
    *) die "unexpected HTTP $status from Hackage for $1-$2" ;;
  esac
}

bump() {
  [ $# -eq 1 ] || die "usage: ./uploadrelease.sh bump <version>"
  local new=$1 major pkg file
  [[ "$new" =~ ^[0-9]+(\.[0-9]+){1,3}$ ]] || die "'$new' is not a version number"
  require_clean_tree

  # Bounds between the glue packages follow the first three components, e.g. ^>= 0.8.0.
  IFS=. read -r a b c _ <<< "$new"
  major="$a.$b.${c:-0}"

  step "Setting every package to $new (glue bounds ^>= $major)"
  for pkg in "${PACKAGES[@]}"; do
    file="$pkg/$pkg.cabal"
    sed -i -E "s/^(version:[[:space:]]*).*/\1$new/" "$file"
    sed -i -E "s/^([[:space:]]*, glue-[a-z]+[[:space:]]+\^>= )[0-9.]+/\1$major/" "$file"
  done

  step "Adding changelog sections"
  for pkg in "${PACKAGES[@]}"; do
    file="$pkg/CHANGELOG.md"
    [ -f "$file" ] || die "$file is missing"
    if grep -qx "## $new" "$file"; then
      echo "$file already has a section for $new"
    else
      awk -v version="$new" -v marker="$TODO_MARKER" '
        !added && /^## / { print "## " version; print ""; print "- " marker; print ""; added = 1 }
        { print }
        END { if (!added) { print ""; print "## " version; print ""; print "- " marker } }
      ' "$file" > "$file.new"
      mv "$file.new" "$file"
      echo "$file: added a section for $new"
    fi
  done

  git commit -q -am "Bump version to $new"
  step "Committed the bump to $new"
  echo "Next: replace the TODO line in each CHANGELOG.md, get the change onto master, then run ./uploadrelease.sh."
}

release() {
  local dry_run=false
  if [ "${1:-}" = "--dry-run" ]; then dry_run=true; fi

  step "Checking the working tree is a clean, up-to-date master"
  require_clean_tree
  git fetch --quiet --tags origin
  [ "$(git rev-parse HEAD)" = "$(git rev-parse origin/master)" ] \
    || die "HEAD is not origin/master; check out an up-to-date master to release"

  local version tag head pkg section
  version=$(current_version)
  tag="v$version"
  head=$(git rev-parse HEAD)
  echo "Releasing $version from $(git rev-parse --short HEAD)"

  step "Checking changelogs"
  for pkg in "${PACKAGES[@]}"; do
    [ -f "$pkg/CHANGELOG.md" ] || die "$pkg/CHANGELOG.md is missing"
    grep -qx "## $version" "$pkg/CHANGELOG.md" || die "$pkg/CHANGELOG.md has no '## $version' section"
    section=$(changelog_section "$pkg" "$version")
    [ -n "$(echo "$section" | tr -d '[:space:]')" ] || die "$pkg/CHANGELOG.md has an empty section for $version"
    if echo "$section" | grep -qF "$TODO_MARKER"; then
      die "$pkg/CHANGELOG.md still has the TODO placeholder for $version"
    fi
  done

  step "Checking Hackage and the release tag"
  local unpublished=() status
  for pkg in "${PACKAGES[@]}"; do
    status=$(hackage_status "$pkg" "$version")
    if [ "$status" = 200 ]; then
      echo "$pkg-$version is already on Hackage"
    else
      unpublished+=("$pkg")
    fi
  done
  if git rev-parse -q --verify "refs/tags/$tag" >/dev/null; then
    local tagged
    tagged=$(git rev-parse "$tag^{commit}")
    if [ ${#unpublished[@]} -eq 0 ]; then
      echo "$version is fully released and tagged as $tag; nothing to do."
      return 0
    fi
    [ "$tagged" = "$head" ] || die "$tag already exists at $(git rev-parse --short "$tagged"), not at HEAD"
  fi

  step "Running cabal check"
  for pkg in "${PACKAGES[@]}"; do
    (cd "$pkg" && cabal check) || die "cabal check failed for $pkg"
  done

  if [ "${SKIP_TESTS:-0}" = 1 ]; then
    step "Skipping the build and tests (SKIP_TESTS=1)"
  else
    step "Building and testing every package on every supported GHC"
    nix build -L --no-link .#all-ghcs
  fi

  step "Building source tarballs"
  rm -rf dist-newstyle/sdist
  cabal sdist all

  step "Uploading to Hackage"
  local tarball
  for pkg in "${unpublished[@]}"; do
    tarball="dist-newstyle/sdist/$pkg-$version.tar.gz"
    [ -f "$tarball" ] || die "expected $tarball"
    if $dry_run; then
      echo "Would upload $tarball"
    else
      cabal upload --publish "$tarball"
    fi
  done
  [ ${#unpublished[@]} -gt 0 ] || echo "Every package is already on Hackage."

  step "Tagging $tag"
  if git rev-parse -q --verify "refs/tags/$tag" >/dev/null; then
    echo "$tag already points at HEAD."
  elif $dry_run; then
    echo "Would create and push $tag at $(git rev-parse --short HEAD)"
  else
    git tag -a "$tag" -m "glue $version"
    git push origin "$tag"
  fi

  step "Done"
  if $dry_run; then
    echo "Dry run of $version complete; nothing was uploaded or tagged."
  else
    echo "Released $version."
  fi
}

case "${1:-}" in
  bump)           shift; bump "$@" ;;
  ""|--dry-run)   release "${1:-}" ;;
  -h|--help)      usage ;;
  *)              usage; exit 1 ;;
esac
