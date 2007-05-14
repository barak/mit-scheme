#!/bin/sh
#
# $Id: functions.sh,v 1.8 2007/05/14 16:50:46 cph Exp $
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

# Functions for shell scripts.

run_setup_cmd ()
{
    echo "${1}"
    eval "${1}"
}

maybe_mkdir ()
{
    if [ ! -e "${1}" ]; then
	run_setup_cmd "mkdir ${1}"
    fi
}

maybe_link ()
{
    if [ ! -e "${1}" ] && [ ! -L "${1}" ]; then
	run_setup_cmd "ln -s ${2} ${1}"
    fi
}

maybe_unlink ()
{
    if maybe_unlink_p "${1}" "${2}"; then
	run_setup_cmd "rm ${1}"
    fi
}

maybe_unlink_p ()
{
    (
    cd `dirname "${1}"`
    BN=`basename "${1}"`
    [ -L "${BN}" ] && [ "${BN}" -ef "${2}" ]
    )
}

maybe_rm ()
{
    FNS=
    DIRS=
    for FN in "${@}"; do
	if [ ! -L "${FN}" ]; then
	    if [ -f "${FN}" ]; then
		FNS="${FNS} ${FN}"
	    elif [ -d "${FN}" ]; then
		DIRS="${DIRS} ${FN}"
	    fi
	fi
    done
    if [ "${FNS}" ]; then
	run_setup_cmd "rm -f ${FNS}"
    fi
    if [ "${DIRS}" ]; then
	run_setup_cmd "rm -rf ${DIRS}"
    fi
}
