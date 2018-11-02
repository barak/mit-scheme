#!/bin/sh

set -e

if [ ${#} -ne 1 ]; then
    echo "usage: ${0} <command>"
    exit 1
fi
COMMAND=${1}

TOPDIR=../src ../src/etc/Clean.sh ${COMMAND}

for SUBDIR in microcode runtime sos star-parser xml; do
    echo "Cleaning in ${SUBDIR}"
    (cd ${SUBDIR} && TOPDIR=../../src ../../src/etc/Clean.sh ${COMMAND})
done
    echo "Cleaning in ffi"
(cd ffi && make ${COMMAND})
