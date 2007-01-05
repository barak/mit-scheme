#!/bin/sh
#
# $Id: Clean.sh,v 1.5 2007/01/05 15:33:06 cph Exp $
#
# Copyright 2000 Massachusetts Institute of Technology
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

# Utility for cleaning up the MIT/GNU Scheme microcode directory.
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
