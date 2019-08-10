#!/bin/sh

set -e
./sources.sh
autoreconf --force --install >/dev/null 2>&1
