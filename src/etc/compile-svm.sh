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

# Cross-compilation process:
#
#     Using the host compiler, syntax everything used by the target
#     compiler: the target runtime, sf and cref.
#
#     In the host runtime, load the host-syntaxed target sf and cref,
#     and use them to syntax the target compiler.
#
#     Create x-compiler.com, a band containing the target runtime, sf,
#     cref and compiler.  This band should contain NONE of the old,
#     host compiler code, though the runtime and sf (and cref) were
#     syntaxed by it.  It will depend only on the host machine.
#
#     Remove the host-compiled runtime, sf and cref to STAGE1
#     subdirectories.  Use the target compiler, on the host machine,
#     to cross-compile everything.  At this point, everything has been
#     cross-compiled by the INTERPRETED target compiler.
#
#     Finish the cross-compilation and build-bands with the target
#     machine.

set -e

. etc/functions.sh

for SUBSYS in runtime sf cref; do
    if [ ! -f $SUBSYS/$SUBSYS-unx.pkd ]; then
	run_cmd_in_dir $SUBSYS \
	    "${@}" --batch-mode --compiler --load $SUBSYS.sf </dev/null
    fi
done
run_cmd_in_dir compiler \
    "${@}" --batch-mode --band runtime.com --load compiler.sf </dev/null

run_cmd_in_dir runtime \
    ../microcode/scheme --batch-mode --fasl make.bin --library ../lib <<EOF
(disk-save "../lib/x-runtime.com")
EOF

run_cmd microcode/scheme --batch-mode --library lib --band x-runtime.com <<EOF
(begin
  (load-option 'SF)
  (load-option 'CREF)
  ;;(load-option 'COMPILER)	This fails: compiler/ not found!
  (with-working-directory-pathname "compiler"
    (lambda () (load "machine/make")))
  (disk-save "lib/x-compiler.com"))
EOF

make_stage1 ()
{
    # Unfortunately `make stage1' does not (re)move .bin's.
    # Thus this function.

    if [ -d STAGE1 ]; then
	echo "runtime/STAGE1 files already exist."
	exit 1
    fi
    mkdir STAGE1
    mv -f *.bin *.ext *.crf *.fre *.pkd STAGE1/
}

#run_cmd_in_dir runtime make stage1	# This does not move the .bin's.
(cd runtime/ && make_stage1)
#run_cmd_in_dir sf make stage1
(cd sf/ && make_stage1)
#run_cmd_in_dir cref make stage1
(cd cref/ && make_stage1)

run_cmd microcode/scheme --batch-mode --library lib --band x-compiler.com <<EOF
(begin
  (load "etc/compile")
  (fluid-let ((compiler:cross-compiling? #t)
	      (compiler:generate-lap-files? #t)
	      (compiler:intersperse-rtl-in-lap? #t))
    (compile-everything)))
EOF

run_cmd microcode/scheme --batch-mode --library lib --band x-compiler.com <<EOF
(begin
  (load "compiler/base/crsend")
  (finish-cross-compilation:directory ".."))
EOF
