# $Id: Makefile,v 1.1 2000/12/05 22:45:50 cph Exp $
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

SHELL = /bin/sh

SCHEME_SUBDIRS = 6001 compiler cref edwin imail runtime sf sos
SUBDIRS = $(SCHEME_SUBDIRS) microcode

all:
	echo "No ALL action"

setup:
	@for SUBDIR in $(SCHEME_SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    test -f $${SUBDIR}/Makefile \
	    || ln -s ../Makefile.std $${SUBDIR}/Makefile; \
	    (cd $${SUBDIR} && $(MAKE) $@) || exit 1; \
	done
	(cd microcode && $(MAKE) $@)

tags TAGS mostlyclean clean distclean maintainer-clean:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && $(MAKE) $@) || exit 1; \
	done

.PHONY: all setup tags TAGS mostlyclean clean distclean maintainer-clean
