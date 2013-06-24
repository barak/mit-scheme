#!/bin/sh
#
# $Id: Setup.sh,v 1.3 2000/12/08 18:04:12 cph Exp $
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

# Utility to set up the MIT Scheme compiler directory.
# The working directory must be the compiler directory.

if [ $# -ne 0 ]; then
    echo "usage: $0"
    exit 1
fi

. ../etc/functions.sh

../etc/Setup.sh

for N in 1 2 3; do
    maybe_link machines/vax/dinstr${N}.scm instr${N}.scm
done

exit 0
