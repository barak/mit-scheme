# $Id: Makefile,v 1.15 2000/12/07 22:56:18 cph Exp $
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

SCHEME_SUBDIRS = 6001 compiler cref edwin imail rcs runtime sf sos win32
SUBDIRS = $(SCHEME_SUBDIRS) microcode

all:
	( cd microcode && $(MAKE) $@ )
	etc/create-dirs.sh
	scheme -compiler -heap 4000 < etc/compile.scm
	etc/build-bands.sh

setup:
	@for SUBDIR in $(SCHEME_SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    test -e $${SUBDIR}/Makefile \
	    || ln -s ../Makefile.std $${SUBDIR}/Makefile; \
	    ( cd $${SUBDIR} && $(MAKE) $@ ) || exit 1; \
	done
	( cd microcode && ./Setup.sh )

tags TAGS:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    ( cd $${SUBDIR} && $(MAKE) $@ ) || exit 1; \
	done

mostlyclean: mostlyclean-extra
	@for SUBDIR in $(SUBDIRS); do \
	    if test -e $${SUBDIR}/Makefile; then \
		echo "making $@ in $${SUBDIR}"; \
		( cd $${SUBDIR} && $(MAKE) $@ ); \
	    fi; \
	done

mostlyclean-extra:
	@if test -d runtime-check && test -e runtime-check/Makefile; then \
	    echo "making mostlyclean in runtime-check"; \
	    ( cd runtime-check && $(MAKE) mostlyclean ); \
	fi

clean: clean-extra
	@for SUBDIR in $(SUBDIRS); do \
	    if test -e $${SUBDIR}/Makefile; then \
		echo "making $@ in $${SUBDIR}"; \
		( cd $${SUBDIR} && $(MAKE) $@ ); \
	    fi; \
	done

clean-extra: mostlyclean-extra
	-rm -f lib/*.com

distclean: distclean-extra
	@for SUBDIR in $(SUBDIRS); do \
	    if test -e $${SUBDIR}/Makefile; then \
		echo "making $@ in $${SUBDIR}"; \
		( cd $${SUBDIR} && $(MAKE) $@ ); \
	    fi; \
	done

distclean-extra: clean-extra
	rm -rf runtime-check lib

maintainer-clean: maintainer-clean-extra
	@for SUBDIR in $(SCHEME_SUBDIRS); do \
	    if test -e $${SUBDIR}/Makefile; then \
		echo "making $@ in $${SUBDIR}"; \
		( cd $${SUBDIR} && $(MAKE) $@ ); \
	    fi; \
	done
	( cd microcode && ./Mclean.sh )

maintainer-clean-extra: distclean-extra

.PHONY: all setup tags TAGS
.PHONY: mostlyclean mostlyclean-extra clean clean-extra
.PHONY: distclean distclean-extra maintainer-clean maintainer-clean-extra
