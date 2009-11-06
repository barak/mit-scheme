#!/bin/sh

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

set -e

if test ${#} -ne 1; then
    echo "usage: ${0} TARGET_ARCH"
    exit 1
fi
HERE=`dirname "${0}"`
TARGET_ARCH=${1}

if test x"${TARGET_ARCH}" = xc; then
    echo C
    exit 0
fi

if test -d "${HERE}/machines/${TARGET_ARCH}"; then
    echo "${TARGET_ARCH}"
    exit 0
fi

echo "Unknown target architecture: ${TARGET_ARCH}" 1>&2
exit 1
