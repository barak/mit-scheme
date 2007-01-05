#!/bin/sh
#
# $Id: Tags.sh,v 1.5 2007/01/05 15:33:03 cph Exp $
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

# Utility to make TAGS files for MIT/GNU Scheme build directories.
# The working directory must be the top-level source directory.

for SUBDIR; do
    echo "making TAGS in ${SUBDIR}"
    ( cd ${SUBDIR} && ./Tags.sh ) || exit 1
done
