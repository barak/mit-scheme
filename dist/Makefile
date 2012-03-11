# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts
#     Institute of Technology
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
# USA.

SUBDIRS = doc src

all:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "Making $@ in $${SUBDIR}";\
	    ( cd $${SUBDIR}; $(MAKE) $@ ) || exit 1;\
	done

mostlyclean clean distclean maintainer-clean tags TAGS install:
	@for SUBDIR in $(SUBDIRS); do \
	    echo "Making $@ in $${SUBDIR}";\
	    ( cd $${SUBDIR}; $(MAKE) $@ ) || exit 1;\
	done

install-info-gz install-info install-html install-pdf install-ps:
	echo "making $@ in doc"
	( cd doc && $(MAKE) $@ )

.PHONY: all mostlyclean clean distclean maintainer-clean tags TAGS install
.PHONY: install-info-gz install-info install-html install-pdf install-ps
