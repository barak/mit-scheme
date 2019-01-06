#!/bin/sh
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
#     2015, 2016, 2017, 2018, 2019 Massachusetts Institute of
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

# Utility for MIT/GNU Scheme subsystem staging.

set -e

. ../etc/functions.sh

if [ $# -ne 2 ]; then
    echo "usage: $0 <command> <tag>"
    exit 1
fi

DIRNAME="STAGE${2}"

case "${1}" in
make)
    mkdir "${DIRNAME}"
    maybe_mv *.com "${DIRNAME}/."
    maybe_mv *.bci "${DIRNAME}/."
    maybe_mv *.lap "${DIRNAME}/."
    maybe_mv *.rtl "${DIRNAME}/."
    ;;
make-cross)
    mkdir "${DIRNAME}"
    maybe_mv *.com "${DIRNAME}"
    maybe_mv *.bci "${DIRNAME}"
    maybe_mv *.moc "${DIRNAME}"
    maybe_mv *.fni "${DIRNAME}"
    maybe_mv *.lap "${DIRNAME}/."
    maybe_mv *.rtl "${DIRNAME}/."
    ;;
make-clean)
    mkdir "${DIRNAME}"
    maybe_mv *.bin "${DIRNAME}/."
    maybe_mv *.ext "${DIRNAME}/."
    maybe_mv *.com "${DIRNAME}/."
    maybe_mv *.bci "${DIRNAME}/."
    maybe_mv *.moc "${DIRNAME}/."
    maybe_mv *.fni "${DIRNAME}/."
    maybe_mv *.lap "${DIRNAME}/."
    maybe_mv *.rtl "${DIRNAME}/."
    ;;
unmake)
    if [ -d "${DIRNAME}" ]; then
	maybe_mv "${DIRNAME}"/* .
	rmdir "${DIRNAME}"
    fi
    ;;
remove)
    rm -rf "${DIRNAME}"
    ;;
copy)
    cp "${DIRNAME}"/* .
    ;;
link)
    ln "${DIRNAME}"/* .
    ;;
*)
    echo "$0: Unknown command ${1}"
    exit 1
    ;;
esac
