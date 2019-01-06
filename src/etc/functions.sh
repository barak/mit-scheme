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

# Functions for shell scripts.

run_cmd ()
{
    echo "run_cmd:" "${@}"
    "${@}"
}

run_configure ()
{
    run_cmd ./configure ${1+"$@"}
}

run_make ()
{
    run_cmd make ${1+"$@"}
}

run_cmd_in_dir ()
(
    D="${1}"
    shift
    cd "${D}"
    echo "run_cmd in ${D}/:" "${@}"
    "${@}"
)

get_fasl_file ()
{
    if [ -f runtime/make.o ]; then
	FASL=http://www.gnu.org/software/mit-scheme/lib/runtime/make.so
	return 0
    elif [ -f runtime/make.com ]; then
	FASL=make.com
	return 0
    else
	echo "Can't find argument for --fasl." >&2
	return 1
    fi
}

maybe_mkdir ()
{
    if [ ! -d "${1}" ]; then
	run_cmd mkdir "${1}"
    fi
}

maybe_link ()
{
    if [ ! -f "${1}" ] && [ ! -L "${1}" ]; then
	run_cmd ln -s "${2}" "${1}"
    fi
}

maybe_unlink ()
{
    if maybe_unlink_p "${1}" "${2}"; then
	run_cmd rm "${1}"
    fi
}

maybe_unlink_p ()
{
    (
    cd `dirname "${1}"`
    BN=`basename "${1}"`
    # What a wretched hack this is!  I can find no standard way to
    # compare two pathnames for identity of the file they name.  There
    # is a non-standard `-ef' option to `test', `test f1 -ef f2', but
    # Solaris does not support this option.  Ugh!  --TRC
    [ -L "${BN}" ] && [ -f "${2}" ] &&	\
	(ls -l "${BN}" | grep -- " -> ${2}\$" >/dev/null)
    )
}

maybe_rm ()
{
    FILES=
    DIRS=
    for FN in ${1+"$@"}; do
	if [ -L "${FN}" ]; then
	    :
	elif [ -f "${FN}" ]; then
	    FILES="${FILES} ${FN}"
	elif [ -d "${FN}" ]; then
	    DIRS="${DIRS} ${FN}"
	fi
    done
    if [ "${FILES}" ]; then
	run_cmd rm -f ${FILES}
    fi
    if [ "${DIRS}" ]; then
	run_cmd rm -rf ${DIRS}
    fi
}

maybe_mv ()
{
    # When $1 is e.g. *.com, punt.
    if [ -e "$1" ]; then mv "${@}"; fi
}
