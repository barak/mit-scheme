
# $Id: Makefile,v 1.8 2000/12/08 20:14:24 cph Exp $
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

all: user.info user.html user.pdf user.ps

user.info: user.texinfo gfdl.texinfo
	rm -f user.info*
	makeinfo user.texinfo

user.html: user.texinfo gfdl.texinfo
	rm -f user*.html
	texi2html -split_chapter user.texinfo

user.pdf: user.texinfo gfdl.texinfo
	texi2pdf user.texinfo

user.ps: user.dvi
	dvips -o $@ $^

user.dvi: user.texinfo gfdl.texinfo
	texi2dvi user.texinfo

AUX_SUFFIXES = aux log toc
INDEX_SUFFIXES = cp fn ky pg tp vr

mostlyclean:
	@for S in $(AUX_SUFFIXES); do \
	    echo "rm -f user.$${S}";\
	    rm -f user.$${S};\
	done
	@for S in $(INDEX_SUFFIXES); do \
	    echo "rm -f user.$${S}";\
	    rm -f user.$${S};\
	    echo "rm -f user.$${S}s";\
	    rm -f user.$${S}s;\
	done
	rm -f user.dvi

clean: mostlyclean
	rm -f user.info* user*.html user.pdf user.ps

distclean: clean

maintainer-clean: distclean

# The install rules depend on macro variables passwd down from the
# makefile in the top-level documentation directory.

install-info: user.info
	$(INSTALL_DATA) user.info* $(DESTDIR)$(infodir)/.

install-html: user.html
	$(INSTALL_DATA) user*.html $(DESTDIR)$(htmldir)/.

install-pdf: user.pdf
	$(INSTALL_DATA) user.pdf $(DESTDIR)$(pdfdir)/.

install-ps: user.ps
	$(INSTALL_DATA) user.ps $(DESTDIR)$(psdir)/.

.PHONY: all mostlyclean clean distclean maintainer-clean
.PHONY: install-info install-html install-pdf install-ps
