#!/bin/sh
#
# Copyright (C) 2010 Massachusetts Institute of Technology
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

# Remove the cross-compiler's bands and stash its products (if any).
run_cmd rm -f lib/x-runtime.com
run_cmd rm -f lib/x-compiler.com
run_cmd ./Stage.sh remove 0
run_cmd ./Stage.sh make-cross 0

# Restore its host-compiled .com's (if any).
run_cmd ./Stage.sh unmake X

# Compile the cross-compiler.

# Syntax prerequisites.
for DIR in runtime sf cref; do
    run_cmd_in_dir $DIR "${@}" --batch-mode --load $DIR.sf </dev/null
done

# Compile prerequisites.
for DIR in runtime sf cref; do
    run_cmd_in_dir $DIR "${@}" --batch-mode --load $DIR.cbf </dev/null
done
run_cmd_in_dir star-parser "${@}" --batch-mode --load compile.scm </dev/null
FASL=make.com

# Dump prerequisites into x-runtime.com.
run_cmd_in_dir runtime \
    "${@}" --batch-mode --library ../lib --fasl $FASL <<EOF
(disk-save "../lib/x-runtime.com")
EOF
echo ""

# Syntax compiler, using x-runtime.com.
run_cmd_in_dir compiler \
    "${@}" --batch-mode --library ../lib --band x-runtime.com <<EOF
(load "compiler.sf")
(sf "base/crsend")
EOF

if [ -s compiler/compiler-unx.crf ]; then
    echo "compiler/compiler-unx.crf:0: error: not empty!"
    exit 1
fi

# Optionally, compile cross-compiler.
run_cmd_in_dir compiler "${@}" --batch-mode --load compiler.cbf </dev/null

# Load up everything, because it is all about to go away!
run_cmd "${@}" --batch-mode --library lib --band x-runtime.com <<EOF
(load-option 'SF)
(load-option 'CREF)
(load-option '*PARSER)
;;(load-option 'COMPILER)
;; The above fails!  Unable to find package directory: "compiler"
(with-working-directory-pathname "compiler"
  (lambda () (load "machine/make")))
(disk-save "lib/x-compiler.com")
EOF

# Remove host code to STAGEX/ subdirs.
run_cmd ./Stage.sh make X
# Dodge incompatibility between 9.0.1 and master.
run_cmd_in_dir runtime mv os2winp.ext os2winp.bin STAGEX

# Restore previously cross-compiled code (if any).  (Replace "unmake"
# with "remove" to start from scratch with each rebuilt
# cross-compiler.)
run_cmd ./Stage.sh unmake 0

# Cross-compile everything, producing svm1 .moc's.
# edwin/snr.scm needs more than --heap 9000!
run_cmd "${@}" --batch-mode --heap 10000 --library lib \
	       --band x-compiler.com <<EOF
(begin
  (load "etc/compile")
  (fluid-let (;;(compiler:generate-lap-files? #t)
	      ;;(compiler:intersperse-rtl-in-lap? #t)
	      (compiler:cross-compiling? #t))
    ;; Syntax star-parser before runtime, so runtime.sf does not die.
    ;; Our --library does not already include a *PARSER option!
    (compile-cref compile-dir)
    (compile-dir "star-parser")
    (compile-everything)))
EOF

# Finish the cross-compilation with the new machine.
run_cmd_in_dir runtime \
    ../microcode/scheme --library ../lib --fasl make.bin <<EOF
(begin
  (load "../compiler/base/crsend")
  (finish-cross-compilation:directory ".."))
EOF
echo ""

# Ready to build-bands.sh with the new machine.
