#!/bin/sh
#
# $Id: Clean.sh,v 1.1 2000/12/08 04:49:54 cph Exp $
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
    RECURSIVE=no
elif [ $# -eq 2 ] && [ "${2}" = "recursive" ]; then
    RECURSIVE=yes
else
    echo "usage: $0 <command> [recursive]"
    exit 1
fi

case "${1}" in
mostlyclean | clean | distclean)
    ;;
maintainer-clean)
    for FN in .edwin-ffi Clean.sh Makefile Setup.sh Stage.sh; do
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

echo "rm -f *.bin *.ext *.com *.bci *.bco *.bld *.crf *.fre *.glo"
rm -f *.bin *.ext *.com *.bci *.bco *.bld *.crf *.fre *.glo

if [ ${RECURSIVE} = no ]; then
    echo "rm -f *.con *.ldr"
    rm -f *.con *.ldr
fi

exit 0
