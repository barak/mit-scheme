#!/bin/sh
#
# $Id: Clean.sh,v 1.4 2000/12/08 06:07:23 cph Exp $
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

# Utility for cleaning up an MIT Scheme build directory.
# The working directory must be the build directory.

if [ $# -eq 1 ]; then
    COMMAND="$1"
    KEYWORDS="rm-bin rm-com rm-pkg-src rm-pkg-bin"
elif [ $# -ge 2 ]; then
    COMMAND="$1"
    shift
    KEYWORDS="$*"
else
    echo "usage: $0 <command> <keyword> ..."
    exit 1
fi

case "${COMMAND}" in
mostlyclean | clean | distclean)
    ;;
maintainer-clean)
    for FN in .edwin-ffi Clean.sh Makefile Setup.sh Stage.sh Tags.sh; do
	if [ -L ${FN} ]; then
	    echo "rm ${FN}"
	    rm ${FN}
	fi
    done
    ;;
*)
    echo "$0: Unknown command ${1}"
    exit 1
    ;;
esac

for KEYWORD in ${KEYWORDS}; do
    case ${KEYWORD} in
    rm-bin)
	echo "rm -f *.bin *.ext"
	rm -f *.bin *.ext
	;;
    rm-com)
	echo "rm -f *.com *.bci"
	rm -f *.com *.bci
	;;
    rm-pkg-src)
	echo "rm -f *.con *.ldr"
	rm -f *.con *.ldr
	;;
    rm-pkg-bin)
	echo "rm -f *.bco *.bld *.crf *.fre *.glo"
	rm -f *.bco *.bld *.crf *.fre *.glo
	;;
    esac
done

exit 0
