#!/bin/sh

# $Id: Setup.sh,v 1.3 2000/12/08 17:53:58 cph Exp $
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

# Program to set up MIT Scheme microcode from CVS for distribution.
# Requires `gcc' and `scheme'.

if [ ! -f config.h.in ]; then
    echo "autoheader"
    autoheader
fi
if [ ! -x configure ]; then
    echo "autoconf"
    autoconf
fi
makegen/makeinit.sh
make setup
make distclean
