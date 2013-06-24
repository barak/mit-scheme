#!/bin/sh

# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute
#     of Technology
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

# Program to set up MIT/GNU Scheme microcode from CVS for distribution.
# Requires `gcc' and `scheme'.

set -eu

autoheader=done
autoconf=done
makeinit=done

clean ()
{
    if [ "x${autoheader}" != xdone ]; then
	rm -f config.h.in
    fi
    if [ "x${autoconf}" != xdone ]; then
	rm -f configure
    fi
    if [ "x${makeinit}" != xdone ]; then
	rm -f Makefile.deps Makefile.in
    fi
}

trap clean EXIT INT TERM

if [ ! -f config.h.in ]; then
    autoheader=clean
    echo "autoheader"
    autoheader
    autoheader=done
fi
if [ ! -x configure ]; then
    autoconf=clean
    echo "autoconf"
    autoconf
    autoconf=done
fi
( cd cmpauxmd && make ${@:+"${@}"} )
if [ ! -f Makefile.in ]; then
    makeinit=clean
    makegen/makeinit.sh ${@:+"${@}"}
    makeinit=done
fi
