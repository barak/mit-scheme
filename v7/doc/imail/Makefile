
# $Id: Makefile,v 1.10 2000/12/08 20:14:24 cph Exp $
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

all: imail.info imail.html imail.pdf imail.ps

imail.info: imail.texinfo gfdl.texinfo
	rm -f imail.info*
	makeinfo imail.texinfo

imail.html: imail.texinfo gfdl.texinfo
	rm -f imail*.html
	texi2html -split_chapter imail.texinfo

imail.pdf: imail.texinfo gfdl.texinfo
	texi2pdf imail.texinfo

imail.ps: imail.dvi
	dvips -o $@ $^

imail.dvi: imail.texinfo gfdl.texinfo
	texi2dvi imail.texinfo

AUX_SUFFIXES = aux log toc
INDEX_SUFFIXES = cp fn ky pg tp vr

mostlyclean:
	@for S in $(AUX_SUFFIXES); do \
	    echo "rm -f imail.$${S}";\
	    rm -f imail.$${S};\
	done
	@for S in $(INDEX_SUFFIXES); do \
	    echo "rm -f imail.$${S}";\
	    rm -f imail.$${S};\
	    echo "rm -f imail.$${S}s";\
	    rm -f imail.$${S}s;\
	done
	rm -f imail.dvi

clean: mostlyclean
	rm -f imail.info* imail*.html imail.pdf imail.ps

distclean: clean

maintainer-clean: distclean

# The install rules depend on macro variables passwd down from the
# makefile in the top-level documentation directory.

install-info: imail.info
	$(INSTALL_DATA) imail.info* $(DESTDIR)$(infodir)/.

install-html: imail.html
	$(INSTALL_DATA) imail*.html $(DESTDIR)$(htmldir)/.

install-pdf: imail.pdf
	$(INSTALL_DATA) imail.pdf $(DESTDIR)$(pdfdir)/.

install-ps: imail.ps
	$(INSTALL_DATA) imail.ps $(DESTDIR)$(psdir)/.

.PHONY: all mostlyclean clean distclean maintainer-clean
.PHONY: install-info install-html install-pdf install-ps
