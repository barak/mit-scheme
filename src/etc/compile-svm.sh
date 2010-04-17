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

# Build a cross-compiler targeting the Scheme Virtual Machine.  Use it
# to cross-compile everything.  Use the new machine to finish the
# cross-compile, leaving the build tree ready for build-bands.sh.

set -e

. etc/functions.sh

if [ -f lib/x-compiler.com ]; then
    rm -v lib/x-runtime.com
    rm -v lib/x-compiler.com
    run_cmd ./Stage.sh remove 0
    run_cmd ./Stage.sh make-cross 0
    run_cmd ./Stage.sh unmake X
fi

# Compile the cross-compiler.

# This script follows the example of LIARC's compile-boot-
# compiler.sh script, which takes pains to syntax the target
# compiler withOUT the host compiler present.

for DIR in runtime sf cref; do
    run_cmd_in_dir $DIR "${@}" --batch-mode --load $DIR.sf </dev/null
done
FASL=make.bin

# Comment out the next 5 lines for a fully-interpreted cross-compiler.
# This does not really work because runtime.sf will die during
# cross-compilation without option *parser in --library ../lib.
for DIR in runtime sf cref; do
    run_cmd_in_dir $DIR "${@}" --batch-mode --load $DIR.cbf </dev/null
done
run_cmd_in_dir star-parser "${@}" --batch-mode --load compile.scm </dev/null
FASL=make.com

run_cmd_in_dir runtime \
    "${@}" --batch-mode --library ../lib --fasl $FASL <<EOF
(disk-save "../lib/x-runtime.com")
EOF
echo ""

run_cmd_in_dir compiler \
    "${@}" --batch-mode --library ../lib --band x-runtime.com <<EOF
(load "compiler.sf")
EOF

if [ -s compiler/compiler-unx.crf ]; then
    echo "compiler/compiler-unx.crf:0: error: not empty!"
    exit 1
fi

run_cmd_in_dir compiler "${@}" --batch-mode --load compiler.cbf </dev/null

run_cmd "${@}" --batch-mode --library lib --band x-runtime.com <<EOF
;; Load up everything, because it is all about to go away.
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
# Dodge unfortunate incompatibility between 9.0.1 and master.
run_cmd_in_dir runtime mv os2winp.ext os2winp.bin STAGEX

# Restore previously cross-compiled code (if any).
# (Comment this out to start from scratch with each rebuilt cross-compiler.)
if [ -e sf/STAGE0 ]; then run_cmd ./Stage.sh unmake 0; fi

# Cross-compile everything, producing svm1 .moc's.
# edwin/snr.scm needs more than --heap 9000!
run_cmd "${@}" --batch-mode --heap 10000 --library lib \
	       --band x-compiler.com <<EOF
(begin
  (load "etc/compile")
  (fluid-let (;;(compiler:generate-lap-files? #t)
	      ;;(compiler:intersperse-rtl-in-lap? #t)
	      (compiler:cross-compiling? #t))

    ;; Compile star-parser before runtime, so runtime.sf does
    ;; not die.  Our --library does not include a *PARSER option!
    (compile-cref compile-dir)
    (compile-dir "star-parser")
    (compile-everything))
  (sf "compiler/base/crsend"))
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
