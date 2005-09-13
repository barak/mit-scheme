#!/bin/sh
#
# $Id: Setup.sh,v 1.12 2004/12/07 04:29:58 cph Exp $
#
# Copyright 2000,2001,2003,2004 Massachusetts Institute of Technology
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

# Utility to set up the MIT/GNU Scheme build directories.
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
maybe_link lib/edwin/etc/TUTORIAL ../../../etc/TUTORIAL
maybe_link lib/edwin/etc/mime.types ../../../etc/mime.types
maybe_link lib/edwin/autoload ../../edwin

for SUBDIR in 6001 compiler cref edwin imail microcode rcs \
	runtime runtime-check sf sos ssp star-parser win32 xdoc xml; do
    echo "setting up ${SUBDIR}"
    maybe_link ${SUBDIR}/Setup.sh ../etc/Setup.sh
    ( cd ${SUBDIR} && ./Setup.sh ) || exit 1
done
