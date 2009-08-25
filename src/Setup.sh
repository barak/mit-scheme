#!/bin/sh
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009 Massachusetts Institute of
#     Technology
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

# Utility to set up the MIT/GNU Scheme build directories.
# The working directory must be the top-level source directory.

set -e

if [ ! -x configure ]; then
    echo "autoconf"
    autoconf
fi

. etc/functions.sh

INSTALLED_SUBDIRS="cref edwin imail sf sos ssp star-parser xml"
OTHER_SUBDIRS="6001 compiler rcs runtime win32 xdoc microcode"

# lib
maybe_mkdir lib
maybe_mkdir lib/lib
maybe_link lib/edwin ../edwin
maybe_link lib/include ../microcode
maybe_link lib/optiondb.scm ../etc/optiondb.scm
maybe_link lib/runtime ../runtime
maybe_link lib/utabmd.bin ../microcode/utabmd.bin

for SUBDIR in ${INSTALLED_SUBDIRS} ${OTHER_SUBDIRS}; do
    echo "setting up ${SUBDIR}"
    maybe_link ${SUBDIR}/Setup.sh ../etc/Setup.sh
    (cd ${SUBDIR} && ./Setup.sh "$@")
done
