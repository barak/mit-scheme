#!/bin/sh
#
# $Id: bootstrap.sh,v 1.2 2000/10/16 18:24:10 cph Exp $
#
# Copyright (c) 2000 Massachusetts Institute of Technology
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# This must be at least 2000.
MITSCHEME_LARGE_HEAP=4000
export MITSCHEME_LARGE_HEAP

if [ ! -d 6001 ]; then
  echo "This must be run from the top-level Scheme source directory."
  exit 1
fi

# If this tree is freshly checked out, do the post-checkout adjustments.
if [ ! -f 6001/Makefile ]; then
  etc/build-tree.sh
fi

# Build the lib directory.
rm -rf lib
mkdir lib
mkdir edwin
mkdir edwin/etc
mkdir edwin/info
cp -p etc/optiondb.scm lib/.
(
 cd lib
 ln -s .. SRC
 ln -s SRC/runtime options
 ln -s ../../edwin edwin/autoload
 ln -s ../cref .
)

# Set up the compiler directory.
(
 cd compiler
 ln -s machines/i386 machine
 ln -s machine/compiler.* .
 ln -s machine/make.com .
)

# Compile everything.
scheme -compiler < etc/bootstrap-compile.scm

cp -p microcode/utabmd.bin lib/.

(cd runtime; scheme -library ../lib -fasl make.com) < etc/build-runtime.scm
scheme -library lib -large < etc/build-compiler.scm
scheme -library lib -large < etc/build-edwin.scm
