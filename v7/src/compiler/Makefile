# $Id: Makefile,v 1.15 2000/12/06 02:58:24 cph Exp $
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

# Makefile for MIT Scheme compiler.
# This makefile supports some utilities for the compiler.
# The compiler provides its own tools for rebuilding itself,
# written in Scheme.

FIXED_SUBDIRS = back base fggen fgopt rtlbase rtlgen rtlopt
SUBDIRS = $(FIXED_SUBDIRS) machine

SHELL = /bin/sh

all:
	echo "No ALL action"

setup:
	(cd machines/vax; \
	for N in 1 2 3; do \
	    ln -s instr$${N}.scm dinstr$${N}.scm; \
	done)

tags TAGS:
	etags back/*.scm base/*.scm fggen/*.scm fgopt/*.scm \
	machines/*/*.scm rtlbase/*.scm rtlgen/*.scm rtlopt/*.scm

mostlyclean:
	@for SUBDIR in $(FIXED_SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && rm -f *.bin *.ext *.com *.bci); \
	done
	if test -d machine/.; then \
	    echo "making $@ in machine"; \
	    (cd machine && rm -f *.bin *.ext *.com *.bci); \
	fi
	rm -f compiler.bco compiler.bld compiler.con compiler.crf \
	      compiler.fre compiler.glo compiler.ldr

clean: mostlyclean

distclean: clean

maintainer-clean: distclean
	rm -f Makefile .edwin-ffi

stage1:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && mkdir STAGE1 && mv -f *.com *.bci STAGE1/.); \
	done

unstage1:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && mv -f STAGE1/* . && rmdir STAGE1); \
	done

rmstage1:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    rm -rf $${SUBDIR}/STAGE1; \
	done

cpstage1:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && cp -p STAGE1/* .); \
	done

lnstage1:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && ln -f STAGE1/* .); \
	done

stage2:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && mkdir STAGE2 && mv -f *.com *.bci STAGE2/.); \
	done

unstage2:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && mv -f STAGE2/* . && rmdir STAGE2); \
	done

rmstage2:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    rm -rf $${SUBDIR}/STAGE2; \
	done

cpstage2:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && cp -p STAGE2/* .); \
	done

lnstage2:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && ln -f STAGE2/* .); \
	done

stage3:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && mkdir STAGE3 && mv -f *.com *.bci STAGE3/.); \
	done

unstage3:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && mv -f STAGE3/* . && rmdir STAGE3); \
	done

rmstage3:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    rm -rf $${SUBDIR}/STAGE3; \
	done

cpstage3:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && cp -p STAGE3/* .); \
	done

lnstage3:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "making $@ in $${SUBDIR}"; \
	    (cd $${SUBDIR} && ln -f STAGE3/* .); \
	done

.PHONY: all setup tags TAGS mostlyclean clean distclean maintainer-clean
.PHONY: stage1 unstage1 rmstage1 cpstage1 lnstage1
.PHONY: stage2 unstage2 rmstage2 cpstage2 lnstage2
.PHONY: stage3 unstage3 rmstage3 cpstage3 lnstage3
