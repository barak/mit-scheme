#!/bin/sh
#
# $Id: Clean.sh,v 1.8 2003/02/14 18:48:11 cph Exp $
#
# Copyright 2000,2001,2003 Massachusetts Institute of Technology
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

# Utility for cleaning up the MIT/GNU Scheme compiler directory.
# The working directory must be the compiler directory.

if [ $# -ne 1 ]; then
    echo "usage: $0 <command>"
    exit 1
fi

../etc/Clean.sh "${1}" rm-pkg

for SUBDIR in back base fggen fgopt machine rtlbase rtlgen rtlopt; do
    if [ -d ${SUBDIR} ]; then
	echo "making ${1} in ${SUBDIR}"
	(cd ${SUBDIR} && rm -f *.bin *.ext *.com *.bci)
    fi
done

case "${1}" in
distclean | maintainer-clean)
    rm -f machine compiler.cbf compiler.pkg compiler.sf make.com
    ;;
esac

case "${1}" in
maintainer-clean)
    rm -f machines/vax/dinstr[123].scm
    ;;
esac

exit 0
