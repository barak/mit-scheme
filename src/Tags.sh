#!/bin/bash
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
#     2015, 2016, 2017, 2018, 2019, 2020 Massachusetts Institute of
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

# Utility to make TAGS files for MIT/GNU Scheme build directories.
# The working directory must be the top-level source directory.

set -e

EXCLUDED_DIRS=(etc relnotes tools)

function excluded_dir ()
{
    local DIR=${1}
    local N=${#EXCLUDED_DIRS[@]}
    local I=0
    while (( I < N )); do
        if [[ ${DIR} = ${EXCLUDED_DIRS[${I}]} ]]; then
            return 0
        fi
        (( I++ ))
    done
    return 1
}

ALL_SUBDIRS=($(find * -type d -depth 0))
SUBDIRS=()

for SUBDIR in "${ALL_SUBDIRS[@]}"; do
    if ! excluded_dir "${SUBDIR}"; then
        SUBDIRS+=("${SUBDIR}")
    fi
done

for SUBDIR in "${SUBDIRS[@]}"; do
    echo "making TAGS in ${SUBDIR}"
    if [[ -x ${SUBDIR}/Tags.sh ]]; then
        SCRIPT=./Tags.sh
    else
        SCRIPT=../etc/Tags.sh
    fi
    ( cd ${SUBDIR} && ${SCRIPT} ) || exit 1
done

function write_entries ()
{
    for SUBDIR in "${SUBDIRS[@]}"; do
	echo -e "\f"
	echo "${SUBDIR}"/TAGS,include
    done
}
write_entries > TAGS
