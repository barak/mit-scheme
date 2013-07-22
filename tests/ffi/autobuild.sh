#!/bin/sh

set -e
autoreconf --force --install
./configure
make all
