#!/bin/sh

set -e

if [ ${#} -ne 1 ]; then
    echo "usage: ${0} <command>"
    exit 1
fi
COMMAND=${1}

TOPDIR=../src ../src/etc/Clean.sh ${COMMAND}

for SUBDIR in ffi microcode runtime star-parser xml; do
    ( cd $SUBDIR; TOPDIR=../../src ../../src/etc/Clean.sh ${COMMAND} )
done
