#!/bin/sh
#
# $Id: Setup.sh,v 1.7 2002/11/20 19:46:05 cph Exp $
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

# Utility to set up an MIT Scheme build directory.
# The working directory must be the build directory.

if [ $# -ne 0 ]; then
    echo "usage: $0"
    exit 1
fi

. ../etc/functions.sh

if [ ! -f Makefile.in ]; then
    maybe_link Makefile ../Makefile.std
fi
for FN in Clean.sh Stage.sh Tags.sh; do
    maybe_link ${FN} ../etc/${FN}
done

[ -e ed-ffi.scm ] && maybe_link .edwin-ffi ed-ffi.scm

exit 0
