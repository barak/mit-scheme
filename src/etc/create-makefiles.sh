#!/bin/sh

# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts
#     Institute of Technology
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

. etc/functions.sh

if test ${#} -ne 2; then
    echo "usage: ${0} HOST_SCHEME_EXE TARGET_ARCH"
    exit 1
fi
HOST_SCHEME_EXE=${1}
TARGET_ARCH=${2}

MDIR=`compiler/choose-machine.sh "${TARGET_ARCH}"`

if test -f makefiles_created; then
    CODE_TYPE=`cat makefiles_created`
    if test "${CODE_TYPE}" = "${MDIR}"; then
	echo "Makefiles already created."
	exit 0
    fi
fi

run_cmd rm -f compiler/machine compiler/compiler.pkg
run_cmd ln -s machines/"${MDIR}" compiler/machine
run_cmd ln -s machine/compiler.pkg compiler/.

BUNDLES="6001 compiler cref edwin ffi imail sf sos ssp star-parser xdoc xml"

run_cmd ${HOST_SCHEME_EXE} --batch-mode --heap 4000 <<EOF
(begin
  (load "etc/utilities")
  (generate-c-bundles (quote (${BUNDLES})) "${MDIR}"))
EOF

run_cmd rm -f compiler/machine compiler/compiler.pkg

for SUBDIR in ${BUNDLES} runtime win32; do
    echo "creating ${SUBDIR}/Makefile.in"
    rm -f ${SUBDIR}/Makefile.in
    cat etc/std-makefile-prefix ${SUBDIR}/Makefile-fragment	\
	> ${SUBDIR}/Makefile.in
    if test -f ${SUBDIR}/Makefile-bundle; then
	cat ${SUBDIR}/Makefile-bundle >> ${SUBDIR}/Makefile.in
	rm -f ${SUBDIR}/Makefile-bundle
    fi
    cat etc/std-makefile-suffix >> ${SUBDIR}/Makefile.in
done

run_cmd rm -f makefiles_created
echo "echo ${MDIR} > makefiles_created"
echo "${MDIR}" > makefiles_created
