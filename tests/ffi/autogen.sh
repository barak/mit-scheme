#!/bin/sh

set -e
rm -rf m4
mkdir m4
AUTOMAKE="automake --foreign"
export AUTOMAKE
autoreconf --force --install -I m4
