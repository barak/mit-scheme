#!/bin/sh

set -e

if [ ${#} -ne 1 ]; then
    echo "usage: ${0} <command>"
    exit 1
fi
COMMAND=${1}

TOPDIR=../src ../src/etc/Clean.sh ${COMMAND}

for SUBDIR in compiler libraries microcode runtime sos star-parser xml; do
    echo "Cleaning in ${SUBDIR}"
    TOPDIR=../../src
    (cd ${SUBDIR} && TOPDIR=${TOPDIR} ${TOPDIR}/etc/Clean.sh ${COMMAND})
done
for SUBDIR in runtime/test-continuation-parser-data; do
    echo "Cleaning in ${SUBDIR}"
    TOPDIR=../../../src
    (cd ${SUBDIR} && TOPDIR=${TOPDIR} ${TOPDIR}/etc/Clean.sh ${COMMAND})
done
echo "Cleaning in ffi"
(cd ffi && make ${COMMAND} || true)
