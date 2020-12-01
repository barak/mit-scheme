#!/bin/sh

set -e
./sources.sh
autoreconf --force --install
