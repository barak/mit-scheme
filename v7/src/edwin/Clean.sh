#!/bin/sh
#
# $Id: Clean.sh,v 1.5 2001/12/17 17:40:58 cph Exp $
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

# Utility for cleaning up the MIT Scheme edwin directory.
# The working directory must be the edwin directory.

if [ $# -ne 1 ]; then
    echo "usage: $0 <command>"
    exit 1
fi

../etc/Clean.sh "${1}" rm-bin rm-com rm-pkg

echo "rm -f edwin-unx.* edwin-w32.* edwin-os2.* edwin.bld"
rm -f edwin-unx.* edwin-w32.* edwin-os2.* edwin.bld

exit 0
