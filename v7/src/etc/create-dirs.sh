#!/bin/sh
#
# $Id: create-dirs.sh,v 1.3 2000/12/07 22:19:28 cph Exp $
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

# Create build directories and links.

maybe_mkdir ()
{
    if test ! -e ${1}; then
	echo "mkdir ${1}"
	mkdir ${1}
    fi
}

maybe_link ()
{
    if test ! -e ${1}; then
	echo "ln -s ${2} ${1}"
	ln -s ${2} ${1}
    fi
}

# runtime-check
maybe_mkdir runtime-check
(
    cd runtime
    for FN in *.scm; do
	FN_BIN="`basename ${FN} .scm`.bin"
	maybe_link ../runtime-check/${FN_BIN} ../runtime/${FN_BIN}
    done
    for FN in runtime.bco runtime.bld; do
	maybe_link ../runtime-check/${FN} ../runtime/${FN}
    done
)

# lib
maybe_mkdir lib
maybe_link lib/SRC ..
maybe_link lib/optiondb.scm ../etc/optiondb.scm
maybe_link lib/options ../runtime
maybe_link lib/utabmd.bin ../microcode/utabmd.bin

# lib/edwin
maybe_mkdir lib/edwin
maybe_mkdir lib/edwin/etc
maybe_mkdir lib/edwin/info
maybe_link lib/edwin/autoload ../../edwin
