#!/bin/sh
#
# $Id: Setup.sh,v 1.5 2000/12/08 18:04:12 cph Exp $
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

# Utility to set up the MIT Scheme build directories.
# The working directory must be the top-level source directory.

if [ ! -x configure ]; then
    echo "autoconf"
    autoconf
fi

. etc/functions.sh

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

SUBDIRS=""

for SUBDIR in 6001 compiler cref edwin imail microcode rcs \
	runtime runtime-check sf sos win32; do
    echo "setting up ${SUBDIR}"
    maybe_link ${SUBDIR}/Setup.sh ../etc/Setup.sh
    ( cd ${SUBDIR} && ./Setup.sh ) || exit 1
done
