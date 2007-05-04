#!/bin/sh
#
# $Id: c-prepare.sh,v 1.4 2007/05/04 19:35:37 cph Exp $
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

if [ ${#} -ge 1 ]; then
    cd "${1}"
elif [ -r etc/compile.scm ]; then
    :
else
    echo "usage: ${0} DIRECTORY"
    exit 1
fi

if [ -z "${SCHEME_LARGE}" ]; then
    SCHEME_LARGE="mit-scheme --heap 6000"
fi

${SCHEME_LARGE} --band boot-compiler.com <<EOF
(begin
  (load "etc/compile.scm")
  (c-prepare))
EOF
