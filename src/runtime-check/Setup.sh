#!/bin/sh
#
# $Id: Setup.sh,v 1.5 2001/12/17 17:40:59 cph Exp $
#
# Copyright (c) 2000, 2001 Massachusetts Institute of Technology
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
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# Utility to set up an MIT Scheme build directory.
# The working directory must be the build directory.

if [ $# -ne 0 ]; then
    echo "usage: $0"
    exit 1
fi

. ../etc/functions.sh

../etc/Setup.sh

for FNS in `cd ../runtime; ls *.scm`; do
    FN="`basename ${FNS} .scm`.bin"
    maybe_link ${FN} ../runtime/${FN}
done

maybe_link runtime-unx.pkd ../runtime/runtime-unx.pkd

exit 0
