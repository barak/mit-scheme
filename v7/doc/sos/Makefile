
# $Id: Makefile,v 2.2 2000/07/10 22:18:04 cph Exp $
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

all:	sos.ps sos.pdf sos.info html/sos_toc.html

sos.ps: sos.dvi
	dvips -o $@ $^

sos.dvi: sos.texinfo
	texi2dvi $^

sos.pdf: sos.texinfo
	texi2pdf $^

sos.info: sos.texinfo
	makeinfo $^
	rm -f /usr/local/lib/mit-scheme/edwin/info/sos.info*
	cpx sos.info* /usr/local/lib/mit-scheme/edwin/info/.
	gzip -v /usr/local/lib/mit-scheme/edwin/info/sos.info*

html/sos_toc.html: sos.texinfo
	(cd html;make)

AUX_SUFFIXES = aux log toc
INDEX_SUFFIXES = cp fn ky pg tp vr

clean:
	rm -f scheme.ps scheme.dvi scheme.pdf scheme.info*
	rm -f $(addprefix scheme.,$(AUX_SUFFIXES))
	rm -f $(addprefix scheme.,$(INDEX_SUFFIXES))
	rm -f $(addsuffix s,$(addprefix scheme.,$(INDEX_SUFFIXES)))
	$(MAKE) -C html $@
