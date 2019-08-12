#!/bin/sh
# -*-Scheme-*-
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
#     2015, 2016, 2017, 2018, 2019 Massachusetts Institute of
#     Technology
#
# This file is part of a gdbm plugin for MIT/GNU Scheme.
#
# This plugin is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This plugin is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this plugin; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
# 02110-1301, USA.

# Compile the GDBM option.

set -e
: ${MIT_SCHEME_EXE=mit-scheme}
: ${MIT_SCHEME_FLAGS=}
${MIT_SCHEME_EXE} --prepend-library . --batch-mode --no-init-file ${MIT_SCHEME_FLAGS} <<\EOF
(load "compile.scm")
EOF
suffix=`echo "(display (microcode-id/operating-system-suffix))" \
	| ${MIT_SCHEME_EXE} --batch-mode --no-init-file`
report=gdbm-$suffix.crf
if [ -s "$report" ]; then
    echo "$0: warning: $report is not empty"
    awk "{printf \"$report:%d: %s\\n\",NR,\$0}" $report
    exit 1
fi
