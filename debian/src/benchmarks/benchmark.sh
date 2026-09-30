#!/bin/bash
# Build the package twice, with and without LTO, and time both flavours
# of each.  Run from the top of an unpacked source tree:
#
#   debian/src/benchmarks/benchmark.sh [workdir]
#
# Takes a couple of hours: four builds' worth of Scheme compilation, or
# two if your machine has the cores to run them side by side, which this
# does.  Writes <workdir>/results.txt and prints a table.
set -eu
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../../.." && pwd)
W=${1:-$(mktemp -d)}
mkdir -p "$W"
VERSION=$(dpkg-parsechangelog -l "$SRC/debian/changelog" --show-field Version)
ARCH=$(dpkg-architecture -qDEB_HOST_ARCH)

for variant in nolto lto; do
  mkdir -p "$W/$variant"
  [ -d "$W/$variant/t" ] || cp -a "$SRC" "$W/$variant/t"
done

# Say -lto explicitly rather than merely omitting +lto: on a distribution
# where LTO is the default -- Ubuntu now, conceivably Debian later -- the
# two arms would otherwise be the same build.
opts_for () {
  if [ "$1" = lto ]; then echo "optimize=+lto parallel=8"
  else                    echo "optimize=-lto parallel=8"
  fi
}

for variant in nolto lto; do
  ( cd "$W/$variant/t" \
    && DEB_BUILD_OPTIONS="$(opts_for $variant)" dpkg-buildpackage -B -us -uc \
         > "$W/$variant/build.log" 2>&1
    echo $? > "$W/$variant/rc" ) &
done
wait

for variant in nolto lto; do
  rc=$(cat "$W/$variant/rc")
  [ "$rc" = 0 ] || { echo "$variant build failed (rc=$rc); see $W/$variant/build.log" >&2; exit 1; }
done

: > "$W/results.txt"
for variant in nolto lto; do
  for flavour in native svm; do
    pkg=$([ "$flavour" = native ] && echo mit-scheme || echo mit-scheme-svm)
    root=$W/x/$variant-$flavour
    rm -rf "$root"; mkdir -p "$root"
    dpkg-deb -x "$W/$variant/${pkg}_${VERSION}_${ARCH}.deb" "$root"
    "$HERE/measure.sh" "$root" "$flavour" "$variant $flavour" >> "$W/results.txt"
  done
done

"$HERE/tabulate.py" "$W/results.txt"
