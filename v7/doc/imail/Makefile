
# $Id: Makefile,v 1.8 2000/07/21 18:10:18 cph Exp $
#
# Copyright (c) 2000 Massachusetts Institute of Technology
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

all:	imail.ps imail.pdf imail.info html/imail.html

imail.ps: imail.dvi
	dvips -o $@ $^

imail.dvi: imail.texinfo gfdl.texinfo
	texi2dvi $^

imail.pdf: imail.texinfo gfdl.texinfo
	texi2pdf $^

imail.info: imail.texinfo gfdl.texinfo
	makeinfo $^
	rm -f /usr/local/lib/mit-scheme/edwin/info/imail.info*
	cpx imail.info* /usr/local/lib/mit-scheme/edwin/info/.
	gzip -v /usr/local/lib/mit-scheme/edwin/info/imail.info*

html/imail.html: imail.texinfo gfdl.texinfo
	(cd html;make)

AUX_SUFFIXES = aux log toc
INDEX_SUFFIXES = cp fn ky pg tp vr

clean:
	rm -f imail.ps imail.dvi imail.pdf imail.info*
	rm -f $(addprefix imail.,$(AUX_SUFFIXES))
	rm -f $(addprefix imail.,$(INDEX_SUFFIXES))
	rm -f $(addsuffix s,$(addprefix imail.,$(INDEX_SUFFIXES)))
	$(MAKE) -C html $@
