#!/bin/sh
#
# $Id: Setup.sh,v 1.5 2003/02/14 18:28:00 cph Exp $
#
# Copyright (c) 2000 Massachusetts Institute of Technology
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
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

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
