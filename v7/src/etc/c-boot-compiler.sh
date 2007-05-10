#!/bin/sh
#
# $Id: c-boot-compiler.sh,v 1.7 2007/05/10 16:44:11 cph Exp $
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
