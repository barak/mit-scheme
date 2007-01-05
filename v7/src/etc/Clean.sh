#!/bin/sh
#
# $Id: Clean.sh,v 1.15 2007/01/05 15:33:06 cph Exp $
#
# Copyright 2000,2001,2002,2006 Massachusetts Institute of Technology
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

# Utility for cleaning up an MIT/GNU Scheme build directory.
# The working directory must be the build directory.

if [ $# -eq 1 ]; then
    COMMAND="${1}"
    KEYWORDS="rm-bin rm-com rm-pkg"
elif [ $# -ge 2 ]; then
    COMMAND="${1}"
    shift
    KEYWORDS="$*"
else
    echo "usage: $0 <command> <keyword> ..."
    exit 1
fi

FULL="no"
DIST="no"
MAINTAINER="no"
case "${COMMAND}" in
mostlyclean)
    ;;
clean)
    FULL="yes"
    ;;
distclean)
    FULL="yes"
    DIST="yes"
    ;;
maintainer-clean)
    FULL="yes"
    DIST="yes"
    MAINTAINER="yes"
    ;;
*)
    echo "$0: Unknown command ${COMMAND}"
    exit 1
    ;;
esac

TOPDIR="${TOPDIR:-..}"
. "${TOPDIR}/etc/functions.sh"

if [ "${DIST}" = "yes" ]; then
    if [ -f Makefile.in ] && [ -f Makefile ]; then
	echo "rm Makefile"
	rm Makefile
    fi
fi

if [ "${MAINTAINER}" = "yes" ]; then
    maybe_unlink Makefile "${TOPDIR}/Makefile.std"
    maybe_unlink .edwin-ffi ed-ffi.scm
    for FN in Clean.sh Setup.sh Stage.sh Tags.sh; do
	maybe_unlink "${FN}" "${TOPDIR}/etc/${FN}"
    done
fi

for KEYWORD in ${KEYWORDS}; do
    case "${KEYWORD}" in
    rm-bin)
	echo "rm -f *.bin *.ext"
	rm -f *.bin *.ext
	;;
    rm-com)
	echo "rm -f *.com *.bci *.c *.o *.so *.sl *.dylib"
	rm -f *.com *.bci *.c *.o *.so *.sl *.dylib
	;;
    rm-old-pkg)
	echo "rm -f *.bco *.bld *.glo *.con *.ldr"
	rm -f *.bco *.bld *.glo *.con *.ldr
	;;
    rm-pkg)
	echo "rm -f *-unx.* *-w32.* *-os2.*"
	rm -f *-unx.* *-w32.* *-os2.*
	;;
    esac
done

exit 0
