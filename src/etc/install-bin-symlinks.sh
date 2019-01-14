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

# Utility to set up symbolic links at installation.

set -e

. `dirname "${0}"`/functions.sh

if [ ${#} -eq 2 ]; then
    DIR=${1}
    EXE=${2}
else
    echo "usage: ${0} <directory> <name>"
    exit 1
fi

case ${EXE} in
    mit-scheme-c|mit-scheme-native)
	if test ! -f "${DIR}"/mit-scheme; then
	    run_cmd rm -f "${DIR}"/mit-scheme
	    run_cmd ln -s "${EXE}" "${DIR}"/mit-scheme
	fi
	;;
    mit-scheme-*)
	if test ! -f "${DIR}"/mit-scheme-native; then
	    run_cmd rm -f "${DIR}"/mit-scheme-native
	    run_cmd ln -s "${EXE}" "${DIR}"/mit-scheme-native
	fi
	if test ! -f "${DIR}"/mit-scheme; then
	    run_cmd rm -f "${DIR}"/mit-scheme
	    run_cmd ln -s mit-scheme-native "${DIR}"/mit-scheme
	fi
	;;
esac

run_cmd rm -f "${DIR}"/scheme "${DIR}"/bchscheme
run_cmd ln -s mit-scheme "${DIR}"/scheme
run_cmd ln -s mit-scheme "${DIR}"/bchscheme
