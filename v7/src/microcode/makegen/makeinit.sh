#!/bin/sh

# $Id: makeinit.sh,v 1.4 2000/12/05 23:16:51 cph Exp $
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

autoheader
autoconf
[ -f Makefile.in ] || touch Makefile.in
./configure
scheme -heap 2000 <<EOF
(load "makegen/makegen.scm")
(generate-makefile "makegen/Makefile.in.in"
		   "Makefile.deps"
		   "Makefile.in")
EOF
rm -f Makefile config.h config.cache config.log config.status
rm -f cmpauxmd.m4 cmpintmd.h
