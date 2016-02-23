#!/bin/sh

set -e
./autogen.sh
./configure
make all
