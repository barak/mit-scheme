#!/bin/sh
#
# $Id: Clean.sh,v 1.7 2002/11/20 19:46:05 cph Exp $
#
# Copyright (c) 2000, 2001 Massachusetts Institute of Technology
#
# This file is part of MIT Scheme.
#
# MIT Scheme is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# MIT Scheme is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with MIT Scheme; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# Utility for cleaning up an MIT Scheme build directory.
# The working directory must be the build directory.

if [ $# -eq 1 ]; then
    COMMAND="$1"
    KEYWORDS="rm-bin rm-com rm-old-pkg rm-pkg"
elif [ $# -ge 2 ]; then
    COMMAND="$1"
    shift
    KEYWORDS="$*"
else
    echo "usage: $0 <command> <keyword> ..."
    exit 1
fi

case "${COMMAND}" in
mostlyclean | clean)
    ;;
distclean)
    if [ -f Makefile.in ] && [ -f Makefile ]; then
	echo "rm Makefile"
	rm Makefile
    fi
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
    rm-old-pkg)
	echo "rm -f *.bco *.bld *.glo *.con *.ldr"
	rm -f *.bco *.bld *.glo *.con *.ldr
	;;
    rm-pkg)
	echo "rm -f *.crf *.fre *.pkd"
	rm -f *.crf *.fre *.pkd
	;;
    esac
done

exit 0
