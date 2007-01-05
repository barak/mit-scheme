#!/bin/sh
#
# $Id: Setup.sh,v 1.11 2007/01/05 15:33:06 cph Exp $
#
# Copyright 2000,2006 Massachusetts Institute of Technology
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

# Utility to set up an MIT/GNU Scheme build directory.
# The working directory must be the build directory.

. ../etc/functions.sh

if [ ! -f Makefile.in ]; then
    maybe_link Makefile ../Makefile.std
fi
for FN in Clean.sh Stage.sh Tags.sh; do
    maybe_link ${FN} ../etc/${FN}
done

[ -e ed-ffi.scm ] && maybe_link .edwin-ffi ed-ffi.scm

exit 0
