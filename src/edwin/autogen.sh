#!/bin/sh

set -e
./sources.sh deps >source-dependencies.am
autoreconf --force --install
