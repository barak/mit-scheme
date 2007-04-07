#!/bin/sh
#
# $Id: c-boot-compiler.sh,v 1.2 2007/04/07 04:03:37 cph Exp $
#
# Copyright 2007 Massachusetts Institute of Technology
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
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

set -e

if [ -z "${SCHEME_LARGE}" ]; then
    SCHEME_LARGE="scheme --heap 3000"
fi

if [ -z "${SCHEME_COMPILER}" ]; then
    SCHEME_COMPILER="${SCHEME_LARGE} --compiler"
fi

# Step 1: Compile CREF and SF natively, so that we can load them
# independently of the compiler.  (There is no standard band that
# loads them independently.)

${SCHEME_COMPILER} <<EOF
(begin
  (with-working-directory-pathname "cref"
    (lambda ()
      (load "cref.sf")
      (load "cref.cbf")
      (if (not (name->package '(CROSS-REFERENCE)))
	  (load "make"))))
  (with-working-directory-pathname "sf"
    (lambda ()
      (load "sf.sf")
      (load "sf.cbf"))))
EOF

# Step 2: Load CREF and SF, and syntax the compiler configured with
# the C back end.

# (There *must* be a better way to write this...)

LOAD_SF_CREF='
(for-each (lambda (subdirectory)
            (with-working-directory-pathname subdirectory
              (lambda ()
                (load "make"))))
          (quote ("cref" "sf")))
'

${SCHEME_LARGE} <<EOF
(begin
  ${LOAD_SF_CREF}
  (with-working-directory-pathname "compiler"
    (lambda ()
      (load "compiler.sf"))))
EOF

# Step 3: Now that the compiler with the C back end is syntaxed and
# packaged, use the native compiler to compile the bootstrap C
# compiler natively.

${SCHEME_COMPILER} <<EOF
(with-working-directory-pathname "compiler"
  (lambda ()
    (load "compiler.cbf")))
EOF

# Step 4: Load up the natively compiled compiler with the C back end,
# and save a band.

${SCHEME_LARGE} <<EOF
(begin
  ${LOAD_SF_CREF}
  (with-working-directory-pathname "compiler"
    (lambda ()
      (load "machines/C/make")))
  (disk-save "boot-compiler.com"))
EOF
