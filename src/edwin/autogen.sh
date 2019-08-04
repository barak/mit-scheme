#!/bin/bash

set -e
./sources.sh
autoreconf --force --install &>/dev/null
