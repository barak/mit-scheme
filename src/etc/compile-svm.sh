#!/bin/sh
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute
#     of Technology
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

# Build a cross-compiler targeting a new Scheme Virtual Machine.  Use
# it to cross-compile everything.  Use the new machine to finish the
# cross-compile, leaving the build tree ready for build-bands.sh.

set -e

. etc/functions.sh

echo "# Remove the cross-compiler's bands and stash its products."
run_cmd rm -f lib/x-runtime.com
run_cmd rm -f lib/x-compiler.com
run_cmd ./Stage.sh remove 0
run_cmd ./Stage.sh make-cross 0

echo "# Restore the cross-compiler's host-compiled .coms."
run_cmd ./Stage.sh unmake X

echo "# Re-compile the cross-compiler."

echo "#    Re-syntax prerequisites."
for DIR in runtime sf cref; do
    run_cmd_in_dir $DIR "${@}" --batch-mode --load $DIR.sf </dev/null
done

echo "#    Re-compile prerequisites."
for DIR in runtime sf cref; do
    run_cmd_in_dir $DIR "${@}" --batch-mode --load $DIR.cbf </dev/null
done
run_cmd_in_dir star-parser "${@}" --batch-mode --load compile.scm </dev/null
FASL=make.com

echo "#    Dump new runtime into x-runtime.com."
run_cmd_in_dir runtime \
    "${@}" --batch-mode --library ../lib --fasl $FASL <<EOF
(disk-save "../lib/x-runtime.com")
EOF

echo "#    Re-syntax cross-compiler using x-runtime.com."
run_cmd_in_dir compiler \
    "${@}" --batch-mode --library ../lib --band x-runtime.com <<EOF
(begin
  (load "compiler.sf")
  (sf "base/crsend"))
EOF

if [ -s compiler/compiler-unx.crf ]; then
    echo "compiler/compiler-unx.crf:0: error: not empty!"
    exit 1
fi

echo "#     Re-compile cross-compiler."
run_cmd_in_dir compiler "${@}" --batch-mode --load compiler.cbf </dev/null

echo "# Dump cross-compiler into x-compiler.com."
run_cmd "${@}" --batch-mode --library lib --band x-runtime.com <<EOF
(begin
  (load-option 'SF)
  (load-option 'CREF)
  (load-option '*PARSER)
  (load-option 'COMPILER)
  (disk-save "lib/x-compiler.com"))
EOF

echo "# Remove host code to STAGEX/ subdirs."
run_cmd ./Stage.sh make X

echo "# Restore previously cross-compiled code."
# (Replace "unmake" with "remove" to start from scratch with each
# rebuilt cross-compiler.)
run_cmd ./Stage.sh unmake 0

echo "# Re-cross-compile everything."
run_cmd "${@}" --batch-mode --library lib --band x-compiler.com <<EOF
(begin
  (load "etc/compile")
  (fluid-let ((compiler:generate-lap-files? #t)
	      (compiler:intersperse-rtl-in-lap? #t)
	      (compiler:cross-compiling? #t))
    (compile-everything)))
EOF

echo "# Finish the cross-compilation with the new machine."
run_cmd_in_dir runtime \
    ../microcode/scheme --library ../lib --fasl make.bin --batch-mode <<EOF
(begin
  (load "../compiler/base/crsend")
  (finish-cross-compilation:directory ".."))
EOF

echo "# Ready to build-bands.sh with the new machine."
