#!/bin/sh
#
# $Id: Clean.sh,v 1.4 2000/12/08 18:20:24 cph Exp $
#
# Copyright (c) 2000 Massachusetts Institute of Technology
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# Utility for cleaning up MIT Scheme build directories.
# The working directory must be the top-level source directory.

if [ $# -le 1 ]; then
    echo "usage: $0 <command> <directory> ..."
    exit 1
fi

COMMAND=$1
shift

FULL=no
DIST=no
MAINTAINER=no
case "${COMMAND}" in
mostlyclean)
    ;;
clean)
    FULL=yes
    ;;
distclean)
    FULL=yes
    DIST=yes
    ;;
maintainer-clean)
    FULL=yes
    DIST=yes
    MAINTAINER=yes
    ;;
*)
    echo "$0: Unknown command ${COMMAND}"
    exit 1
    ;;
esac

if [ ${FULL} = yes ]; then
    echo "rm -f lib/*.com"
    rm -f lib/*.com
fi

if [ ${DIST} = yes ]; then
    echo "rm -f Makefile config.cache config.log config.status"
    rm -f Makefile config.cache config.log config.status
fi

if [ ${MAINTAINER} = yes ]; then
    echo "rm -rf configure lib"
    rm -rf configure lib
fi

for SUBDIR; do
    if test -x ${SUBDIR}/Clean.sh; then
	echo "making ${COMMAND} in ${SUBDIR}"
	( cd ${SUBDIR} && ./Clean.sh ${COMMAND} ) || exit 1
    fi
done
