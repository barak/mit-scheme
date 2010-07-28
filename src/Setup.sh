#!/bin/sh
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010 Massachusetts Institute of
#     Technology
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

# Utility to set up the MIT/GNU Scheme build directories.
# The working directory must be the top-level source directory.

set -e

: ${MIT_SCHEME_EXE:=mit-scheme}

# Please keep the following messages synchronized with the messages in
# these files:
#
#   src/Setup.sh
#   src/configure.ac
#   src/etc/make-native.sh

if ! ${MIT_SCHEME_EXE} --batch-mode --eval '(%exit)' > /dev/null 2> /dev/null
then
    cat <<EOF >&2
*** Error in ${0}

This script needs an existing MIT/GNU Scheme installation to function,
but the program \`${MIT_SCHEME_EXE}' does not appear to run it.

If you have installed MIT/GNU Scheme in an unusual location, set the
environment variable MIT_SCHEME_EXE to the name or pathname of the
MIT/GNU Scheme executable, which is usually \`mit-scheme' or
\`/usr/local/bin/mit-scheme', and set the environment variable
MITSCHEME_LIBRARY_PATH to the pathname of the MIT/GNU Scheme library
directory, which is usually \`/usr/local/lib/mit-scheme-ARCH', where
ARCH is the compiled code architecture, such as \`c', \`i386', etc.
EOF
    exit 1
fi

if [ ! -x configure ]; then
    echo "autoconf"
    autoconf --include=microcode
fi

. etc/functions.sh

INSTALLED_SUBDIRS="cref edwin ffi imail sf sos ssp star-parser xml"
OTHER_SUBDIRS="6001 compiler rcs runtime win32 xdoc microcode"

# lib
maybe_mkdir lib
maybe_link lib/edwin ../edwin
maybe_link lib/include ../microcode
maybe_link lib/optiondb.scm ../etc/optiondb.scm
maybe_link lib/runtime ../runtime
maybe_link lib/mit-scheme.h ../microcode/pruxffi.h
maybe_link lib/ffi ../ffi

# This is a kludge so that liarc bundles can coexist with microcode
# modules in the same lib directory.  If you have a better way to make
# this work, please implement it.  Making lib/lib a symlink to
# ../microcode does not work -- configure.ac wants to make symlinks at
# lib/lib/sf.so to ../../sf/sf.so, &c.
maybe_mkdir lib/lib
for MODULE in prbfish prmd5 prmhash prmcrypt prgdbm prdb4 prpgsql prx11; do
  maybe_link "lib/lib/${MODULE}.so" "../../microcode/${MODULE}.so"
done

for SUBDIR in ${INSTALLED_SUBDIRS} ${OTHER_SUBDIRS}; do
    echo "setting up ${SUBDIR}"
    maybe_link ${SUBDIR}/Setup.sh ../etc/Setup.sh
    (cd ${SUBDIR} && ./Setup.sh "$@")
done
