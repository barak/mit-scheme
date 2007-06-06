#!/bin/sh

# $Id: choose-machine.sh,v 1.1 2007/06/06 19:42:38 cph Exp $
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007 Massachusetts Institute of Technology
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

if [ ${#} -eq 0 ]; then
    MACHINE=
elif [ ${#} -eq 1 ]; then
    MACHINE=${1}
else
    echo "usage: ${0} [NATIVE-CODE-TYPE]"
    exit 1
fi

DIR=`dirname ${0}`

chosen ()
{
    if [ -d "${DIR}/machines/${1}" ]; then
	echo "${1}"
	exit 0
    else
	echo "Unknown machine type: ${1}" 1>&2
	exit 1
    fi
}

case "${MACHINE}" in
"" | yes)
    ;;
c)
    chosen C
    ;;
no)
    chosen none
    ;;
*)
    chosen "${MACHINE}"
esac

[ -f ../liarc.stamp ] && chosen C

case `${DIR}/config.guess` in
alpha-* | alphaev[56]-* | alphaev56-* | alphapca56-*)
    chosen alpha
    ;;
m68k-*)
    chosen bobcat
    ;;
i[3456]86-*)
    chosen i386
    ;;
mips-* | mipsel-*)
    chosen mips
    ;;
sparc-*)
    chosen sparc
    ;;
hppa-* | hppa1.[01]-* | hppa2.?-*)
    chosen spectrum
    ;;
vax-*)
    chosen vax
    ;;
*)
    chosen none
    ;;
esac
