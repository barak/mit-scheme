#!/bin/sh
#
# $Id$
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008 Massachusetts Institute of Technology
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

set -e

if [ ${#} -le 1 ]; then
    echo "usage: ${0} <command> <directory> ..."
    exit 1
fi

COMMAND=${1}
shift
SUBDIRS=${@}

FULL=no
DIST=no
MAINTAINER=no
case ${COMMAND} in
mostlyclean)
    ;;
clean | c-clean)
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
    echo "${0}: Unknown command ${COMMAND}"
    exit 1
    ;;
esac

. etc/functions.sh

maybe_rm c-boot-compiler.com native-boot-compiler.com

if [ ${FULL} = yes ]; then
    maybe_rm lib/*.com
fi

if [ ${DIST} = yes ]; then
    maybe_rm Makefile boot-lib config.cache config.log config.status
fi

if [ ${MAINTAINER} = yes ]; then
    maybe_rm autom4te.cache configure lib stamp_* boot-root makefiles_created
fi

for SUBDIR in ${SUBDIRS}; do
    if test -x ${SUBDIR}/Clean.sh; then
	echo "making ${COMMAND} in ${SUBDIR}"
	( cd ${SUBDIR} && ./Clean.sh ${COMMAND} )
    fi
done
