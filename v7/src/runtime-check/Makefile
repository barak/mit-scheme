# $Id: Makefile,v 1.2 2000/12/07 23:08:49 cph Exp $
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

# Standard Makefile for Scheme subsystem directories.
# This makefile supports some utilities for Scheme subsystems.
# Tools to rebuild these subsystems are written in Scheme and do not
# use `make'.

SHELL = /bin/sh

all:
	echo "No ALL action"

setup:
	(cd ../runtime; \
	 for FNS in *.scm; do \
	     FN="`basename $${FNS} .scm`.bin"; \
	     if test ! -e ../runtime-check/$${FN}; then \
		 echo "ln -s ../runtime/$${FN} ../runtime-check/$${FN}"; \
		 ln -s ../runtime/$${FN} ../runtime-check/$${FN}; \
	     fi; \
	 done; \
	 for FN in runtime.bco runtime.bld; do \
	     if test ! -e ../runtime-check/$${FN}; then \
		 echo "ln -s ../runtime/$${FN} ../runtime-check/$${FN}"; \
		 ln -s ../runtime/$${FN} ../runtime-check/$${FN}; \
	     fi; \
	 done)

tags TAGS:

mostlyclean:
	-rm -f *.com *.bci

clean: mostlyclean

distclean: clean

maintainer-clean: distclean
	-rm -f *.bin runtime.bco runtime.bld

stage1:
	mkdir STAGE1 && mv -f *.com *.bci STAGE1/.

unstage1:
	mv -f STAGE1/* . && rmdir STAGE1

rmstage1:
	rm -rf STAGE1

cpstage1:
	cp -p STAGE1/* .

lnstage1:
	ln -f STAGE1/* .

stage2:
	mkdir STAGE2 && mv -f *.com *.bci STAGE2/.

unstage2:
	mv -f STAGE2/* . && rmdir STAGE2

rmstage2:
	rm -rf STAGE2

cpstage2:
	cp -p STAGE2/* .

lnstage2:
	ln -f STAGE2/* .

stage3:
	mkdir STAGE3 && mv -f *.com *.bci STAGE3/.

unstage3:
	mv -f STAGE3/* . && rmdir STAGE3

rmstage3:
	rm -rf STAGE3

cpstage3:
	cp -p STAGE3/* .

lnstage3:
	ln -f STAGE3/* .

.PHONY: all setup tags TAGS mostlyclean clean distclean maintainer-clean
.PHONY: stage1 unstage1 rmstage1 cpstage1 lnstage1
.PHONY: stage2 unstage2 rmstage2 cpstage2 lnstage2
.PHONY: stage3 unstage3 rmstage3 cpstage3 lnstage3
