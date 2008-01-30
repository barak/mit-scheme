#!/bin/sh
#
# $Id: make-native.sh,v 1.4 2008/01/30 20:02:08 cph Exp $
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008 Massachusetts Institute of Technology
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
for ARG in "${@}"; do
    case ${ARG} in
    --help|--help=*|--version)
	FASTP=yes
	;;
    esac
done

if [ ${FASTP} = yes ]; then
    exec ./configure "${@}"
fi

run_cmd ./Setup.sh
MIT_SCHEME_EXE=mit-scheme-c run_configure --prefix=`pwd`/boot-root
run_cmd etc/compile-boot-compiler.sh mit-scheme-c
run_cmd_in_dir compiler run_make compile-liarc-bundle
run_cmd etc/native-prepare.sh mit-scheme-c
run_make compile-microcode

run_cmd_in_dir runtime ../microcode/scheme --library ../lib \
    --fasl make.bin --heap 6000 <<EOF
(begin
  (load "../compiler/base/crsend")
  (finish-cross-compilation:directory ".."))
EOF

run_make stamp_install-native-boot-compiler c-clean distclean

run_configure "${@}"
run_make stamp_native-compile-scheme build-bands clean-boot-root
