#!/bin/sh
#
# $Id: c-boot-compiler.sh,v 1.6 2007/05/08 12:54:52 cph Exp $
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

if [ ${#} -eq 2 ]; then
    EXE=${1}
    OUT=${2}
else
    echo "usage: ${0} <executable> <output-file>"
    exit 1
fi
CMD="${EXE} --heap 6000"

# Step 1: Load CREF and SF, and syntax the compiler configured with
# the C back end.

echo "${CMD}"
${CMD} <<EOF
(begin
  (load "etc/compile.scm")
  (compile-bootstrap-1))
EOF

# Step 2: Now that the compiler with the C back end is syntaxed and
# packaged, use the native compiler to compile the bootstrap C
# compiler natively.

echo "${CMD} --compiler"
${CMD} --compiler <<EOF
(begin
  (load "etc/compile.scm")
  (compile-bootstrap-2))
EOF

# Step 3: Load up the natively compiled compiler with the C back end,
# and save a band.

echo "${CMD}"
${CMD} <<EOF
(begin
  (load "etc/compile.scm")
  (compile-bootstrap-3)
  (disk-save "${OUT}"))
EOF
