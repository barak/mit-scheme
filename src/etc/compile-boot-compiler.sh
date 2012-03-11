#!/bin/sh
#
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

if [ ${#} -eq 1 ]; then
    EXE=${1}
else
    echo "usage: ${0} <executable>"
    exit 1
fi

run_cmd "${EXE}" --batch-mode <<EOF
(begin
  (load "etc/compile.scm")
  (compile-cref compile-dir)
  (for-each compile-dir '("runtime" "star-parser" "sf")))
EOF

get_fasl_file
run_cmd_in_dir runtime "${EXE}" --batch-mode --library ../lib \
    --fasl "${FASL}" <<EOF
(disk-save "../lib/x-runtime.com")
EOF
echo ""

run_cmd "${EXE}" --batch-mode --library lib --band x-runtime.com <<EOF
(begin
  (load-option 'SF)
  (with-working-directory-pathname "compiler"
    (lambda ()
      (load "compiler.sf"))))
EOF

run_cmd "${EXE}" --batch-mode <<EOF
(with-working-directory-pathname "compiler"
  (lambda ()
    (load "compiler.cbf")))
EOF

run_cmd "${EXE}" --batch-mode --library lib --band x-runtime.com <<EOF
(begin
  (load-option 'SF)
  (load-option 'CREF)
  (load-option '*PARSER)
  (load-option 'COMPILER)
  (disk-save "lib/x-compiler.com"))
EOF

# Remove host (native) code to STAGEX/ subdirs.
run_cmd ./Stage.sh make X
