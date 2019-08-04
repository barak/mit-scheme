#!/bin/bash

set -e
rm -rf m4
mkdir m4
autoreconf --force --install -I m4 &>/dev/null
