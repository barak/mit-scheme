#!/bin/sh
#
# Copyright (C) 2010 Massachusetts Institute of Technology
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

# Utility to stage everything used by the compiler.  The working
# directory should be the top-level source directory.

set -e

. etc/functions.sh

for SUBDIR in `MAKEFLAGS= make -s subdir-list | sort | uniq`; do
    if [ -x $SUBDIR/Stage.sh ]; then
	# Try to avoid a subdir that was not compiled (else
	# $SUBDIR/Stage.sh will abort).
	if [ "`cd $SUBDIR && echo *.com`" = "*.com" \
	     -a "`cd $SUBDIR && echo *.moc`" = "*.moc" ]; then continue; fi
	run_cmd_in_dir $SUBDIR ./Stage.sh "$@"
    fi
done
