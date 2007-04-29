#!/bin/sh
#
# $Id: Clean.sh,v 1.12 2007/04/29 18:01:33 cph Exp $
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007 Massachusetts Institute of Technology
#
# This file is part of MIT/GNU Scheme.
#
# MIT/GNU Scheme is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# MIT/GNU Scheme is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with MIT/GNU Scheme; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
# 02110-1301, USA.

# Utility for cleaning up MIT/GNU Scheme build directories.
# The working directory must be the top-level source directory.

if [ $# -le 1 ]; then
    echo "usage: $0 <command> <directory> ..."
    exit 1
fi

COMMAND=$1
shift
SUBDIRS="$@"

FULL=no
DIST=no
MAINTAINER=no
case "${COMMAND}" in
mostlyclean)
    ;;
clean)
    FULL=yes
    ;;
distclean | c-clean)
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

if [ "${COMMAND}" != c-clean ]; then
    echo "rm -f liarc.stamp"
    rm -f liarc.stamp
fi

if [ ${FULL} = yes ]; then
    echo "rm -f lib/*.com"
    rm -f lib/*.com
fi

if [ ${DIST} = yes ]; then
    echo "rm -f Makefile config.cache config.log config.status"
    rm -f Makefile config.cache config.log config.status
fi

if [ ${MAINTAINER} = yes ]; then
    echo "rm -rf configure lib autom4te.cache"
    rm -rf configure lib autom4te.cache
fi

for SUBDIR in ${SUBDIRS}; do
    if test -x ${SUBDIR}/Clean.sh; then
	echo "making ${COMMAND} in ${SUBDIR}"
	( cd ${SUBDIR} && ./Clean.sh ${COMMAND} ) || exit 1
    fi
done
