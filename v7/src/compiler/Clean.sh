#!/bin/sh
#
# $Id: Clean.sh,v 1.3 2000/12/08 06:15:12 cph Exp $
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

# Utility for cleaning up the MIT Scheme runtime-check directory.
# The working directory must be the runtime-check directory.

if [ $# -ne 1 ]; then
    echo "usage: $0 <command>"
    exit 1
fi

../etc/Clean.sh "${1}" rm-pkg-src rm-pkg-bin

for SUBDIR in back base fggen fgopt machine rtlbase rtlgen rtlopt; do
    if [ -d ${SUBDIR} ]; then
	echo "making ${1} in ${SUBDIR}"
	(cd ${SUBDIR} && rm -f *.bin *.ext *.com *.bci)
    fi
done

case "${1}" in
distclean | maintainer-clean)
    rm -f machine compiler.cbf compiler.pkg compiler.sf make.com
    rm -f machines/vax/dinstr[123].scm
    ;;
esac

exit 0
