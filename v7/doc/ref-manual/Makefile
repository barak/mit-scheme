
# $Id: Makefile,v 1.2 2000/07/10 22:14:33 cph Exp $
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

all:	scheme.ps scheme.pdf scheme.info html/scheme_toc.html

scheme.ps: scheme.dvi
	dvips -o $@ $^

scheme.dvi: scheme.texinfo
	texi2dvi $^

scheme.pdf: scheme.texinfo
	texi2pdf $^

scheme.info: scheme.texinfo
	makeinfo $^
	rm -f /usr/local/lib/mit-scheme/edwin/info/scheme.info*
	cpx scheme.info* /usr/local/lib/mit-scheme/edwin/info/.
	gzip -v /usr/local/lib/mit-scheme/edwin/info/scheme.info*

html/scheme_toc.html: scheme.texinfo
	(cd html;make)

AUX_SUFFIXES = aux log toc
INDEX_SUFFIXES = cp fn ky pg tp vr

clean:
	rm -f scheme.ps scheme.dvi scheme.pdf scheme.info*
	rm -f $(addprefix scheme.,$(AUX_SUFFIXES))
	rm -f $(addprefix scheme.,$(INDEX_SUFFIXES))
	rm -f $(addsuffix s,$(addprefix scheme.,$(INDEX_SUFFIXES)))
	$(MAKE) -C html $@
