#!/bin/sh

set -e

if [ ${#} -ne 1 ]; then
    echo "usage: ${0} <command>"
    exit 1
fi

../etc/Clean.sh "${1}"
. ../etc/functions.sh

maybe_rm prhello-const prhello-const.scm
