#!/bin/sh
#
# $Id: c-prepare.sh,v 1.6 2007/05/08 12:54:52 cph Exp $
#
# Copyright 2007 Massachusetts Institute of Technology
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
    SCHEME_COMPILER="mit-scheme-c --compiler"
else
    SCHEME_COMPILER=${1}
    shift
    while [ ${#} -gt 0 ]; do
	SCHEME_COMPILER="${SCHEME_COMPILER} ${1}"
	shift
    done
fi

SCHEME_COMPILER="${SCHEME_COMPILER} --heap 6000"

echo "${SCHEME_COMPILER}"
${SCHEME_COMPILER} <<EOF
(begin
  (load "etc/compile.scm")
  (c-prepare))
EOF
