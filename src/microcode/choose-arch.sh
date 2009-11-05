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

set -e

if test ${#} != 2; then
    echo "usage: ${0} ARCH_SPEC HOST_CPU"
    exit 1
fi

HERE=`dirname "${0}"`
ARCH_SPEC=${1}
HOST_CPU=${2}

case ${ARCH_SPEC} in
yes|y)
    case ${HOST_CPU} in
    alpha*)
	echo alpha
	;;
    hppa*)
	echo hppa
	;;
    i?86)
	echo i386
	;;
    m68k|m680?0)
	echo mc68k
	;;
    mips*)
	echo mips
	;;
    vax)
	echo vax
	;;
    x86_64)
	echo x86-64
	;;
    *)
	echo unknown_host
	;;
    esac
    ;;
c|C)
    echo c
    exit
    ;;
svm|svm1)
    echo svm1
    exit
    ;;
no|none|n)
    echo none
    exit
    ;;
*)
    # This is not quite right, because the compiler and microcode
    # disagree about what some architectures should be called, such as
    # bobcat vs mc68k or spectrum versus hppa.  I don't know what the
    # state of Scheme on these architectures is, however, so at least
    # this will flag an error if you try to use them.
    if test -f "${HERE}/cmpauxmd/${ARCH_SPEC}.m4"; then
	echo ${ARCH_SPEC}
    else
	echo unknown_spec
    fi
    ;;
esac
