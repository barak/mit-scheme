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

FASTP=no
NATIVE_CODE=
for ARG in "${@}"; do
    case ${ARG} in
    --help|--help=*|--version)
	FASTP=yes
	;;
    --enable-native-code=*)
	NATIVE_CODE=${ARG}
	;;
    esac
done

if [ ${FASTP} = yes ]; then
    exec ./configure "${@}"
fi

: ${MIT_SCHEME_EXE:=mit-scheme-c}
export MIT_SCHEME_EXE

# Please keep the following messages synchronized with the messages in
# these files:
#
#   src/Setup.sh
#   src/configure.ac
#   src/etc/make-native.sh

script_name="${0}"
report_error ()
{
  local line
  cat <<EOF
*** Error in ${script_name}

This script builds MIT/GNU Scheme a native back end, starting from
MIT/GNU Scheme with the portable C back end.
EOF
  printf '\n'
  cat
  printf '\n'
  cat <<EOF
If you have installed MIT/GNU Scheme with the portable C back end in
an unusual location, set the environment variable MIT_SCHEME_EXE to
the name or pathname of the MIT/GNU Scheme executable, which is
usually \`mit-scheme-c' or \`/usr/local/bin/mit-scheme-c', and set the
environment variable MITSCHEME_LIBRARY_PATH to the pathname of the
MIT/GNU Scheme library directory, which is usually
\`/usr/local/lib/mit-scheme-c'.
EOF
  exit 1
}

test_exp='(begin (write microcode-id/compiled-code-type) (%exit 0))'
scheme_failure=
cc_type=`"${MIT_SCHEME_EXE}" --batch-mode --eval "${test_exp}" 2>/dev/null` \
  || scheme_failure=yes

if [ yes = "${scheme_failure}" ]; then
  report_error <<EOF >&2
This script needs an existing MIT/GNU Scheme installation with the
portable C back end, but the program \`${MIT_SCHEME_EXE}' does not
appear to run it.
EOF
elif [ c != "${cc_type}" ]; then
  report_error <<EOF >&2
This script needs an existing MIT/GNU Scheme installation with the
portable C back end, but the program \`${MIT_SCHEME_EXE}' uses native
${cc_type} code rather than portable C code.
EOF
fi

run_cmd ./Setup.sh
run_configure --prefix=`pwd`/boot-root ${NATIVE_CODE}
run_cmd etc/compile-boot-compiler.sh "${MIT_SCHEME_EXE}"
run_cmd_in_dir compiler run_make compile-liarc-bundle
run_cmd etc/native-prepare.sh "${MIT_SCHEME_EXE}"
run_make compile-microcode

run_cmd_in_dir runtime ../microcode/scheme --library ../lib \
    --fasl make.bin --heap 6000 --batch-mode <<EOF
(begin
  (load "../compiler/base/crsend")
  (finish-cross-compilation:directory ".."))
EOF

run_make stamp_install-native-boot-compiler c-clean distclean

run_configure "${@}"
run_make stamp_native-compile-scheme build-bands clean-boot-root
