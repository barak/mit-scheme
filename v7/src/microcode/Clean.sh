#!/bin/sh
#
# $Id: Clean.sh,v 1.2 2002/11/20 19:46:06 cph Exp $
#
# Copyright (c) 2000 Massachusetts Institute of Technology
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

# Utility for cleaning up the MIT Scheme microcode directory.
# The working directory must be the microcode directory.

if [ $# -ne 1 ]; then
    echo "usage: $0 <command>"
    exit 1
fi

case "${1}" in
mostlyclean | clean | distclean)
    ;;
maintainer-clean)
    if [ ! -f Makefile ] && [ -f configure ]; then
	./configure
    fi
    ;;
*)
    echo "$0: Unknown command ${1}"
    exit 1
    ;;
esac

if [ -f Makefile ]; then
    make ${1}
fi

exit 0
