#!/bin/sh
#
# $Id: bootstrap.sh,v 1.3 2000/10/16 18:43:42 cph Exp $
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

if [ $# -eq 0 ]; then
    case "`uname`" in
    Linux)
	cname=i386-linux
	;;
    FreeBSD)
	cname=i386-freebsd
	;;
    *)
	echo "Don't know what system you're using."
	echo "You must re-run this script with a configuration name argument."
	echo "Here are the allowed names:"
	microcode/unxutl/config
	exit 1
	;;
    esac
elif [ $# -eq 1 ]; then
    cname=$1
else
    echo "usage: $0 [config-name]"
    exit 1
fi

if [ ! -d 6001 ]; then
    echo "This must be run from the top-level Scheme source directory."
    exit 1
fi

# If this tree is freshly checked out, do the post-checkout adjustments.
if [ ! -f 6001/Makefile ]; then
    etc/build-tree.sh
fi

# Set up the compiler directory.
(
    cd compiler
    if [ ! -L machine ]; then
	ln -s machines/i386 machine
	ln -s machine/compiler.* .
	ln -s machine/make.com .
    fi
)

# Compile the C code.
(
    cd microcode
    unxutl/config "${cname}"
    make
)

# Compile the Scheme code.
./microcode/scheme -compiler < etc/bootstrap-compile.scm

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

cp -p microcode/utabmd.bin lib/.

(
    cd runtime
    ../microcode/scheme -library ../lib -fasl make.com
) < etc/build-runtime.scm

./microcode/scheme -library lib -large < etc/build-compiler.scm
./microcode/scheme -library lib -large < etc/build-edwin.scm
