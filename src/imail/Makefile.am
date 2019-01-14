## Process this file with automake to produce Makefile.in
##
## Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
##     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
##     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
##     2015, 2016, 2017, 2018, 2019 Massachusetts Institute of
##     Technology
##
## This file is part of MIT/GNU Scheme.
##
## MIT/GNU Scheme is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or (at
## your option) any later version.
##
## MIT/GNU Scheme is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with MIT/GNU Scheme; if not, write to the Free Software
## Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
## USA.

EXTRA_DIST = autogen.sh

MIT_SCHEME_EXE = @MIT_SCHEME_EXE@
scmlibdir = @MIT_SCHEME_LIBDIR@
scmlib_subdir = $(scmlibdir)imail
scmdocdir = $(datarootdir)/doc/@MIT_SCHEME_PROJECT@

sources = imail-browser.scm imail-core.scm imail-file.scm imail-imap.scm
sources += imail-mime.scm imail-rmail.scm imail-summary.scm imail-top.scm
sources += imail-umail.scm imail-util.scm imap-response.scm imap-syntax.scm

binaries = imail-browser.bci imail-browser.com
binaries += imail-core.bci imail-core.com
binaries += imail-file.bci imail-file.com
binaries += imail-imap.bci imail-imap.com
binaries += imail-mime.bci imail-mime.com
binaries += imail-rmail.bci imail-rmail.com
binaries += imail-summary.bci imail-summary.com
binaries += imail-top.bci imail-top.com
binaries += imail-umail.bci imail-umail.com
binaries += imail-util.bci imail-util.com
binaries += imap-response.bci imap-response.com
binaries += imap-syntax.bci imap-syntax.com

scmlib_sub_DATA = $(sources)
scmlib_sub_DATA += $(binaries)
scmlib_sub_DATA += make.scm imail.pkg imail-@MIT_SCHEME_OS_SUFFIX@.pkd

info_TEXINFOS = mit-scheme-imail.texi
AM_MAKEINFOHTMLFLAGS = --no-split

imail-browser.bci: stamp-scheme
imail-browser.com: stamp-scheme
imail-core.bci: stamp-scheme
imail-core.com: stamp-scheme
imail-file.bci: stamp-scheme
imail-file.com: stamp-scheme
imail-imap.bci: stamp-scheme
imail-imap.com: stamp-scheme
imail-mime.bci: stamp-scheme
imail-mime.com: stamp-scheme
imail-rmail.bci: stamp-scheme
imail-rmail.com: stamp-scheme
imail-summary.bci: stamp-scheme
imail-summary.com: stamp-scheme
imail-top.bci: stamp-scheme
imail-top.com: stamp-scheme
imail-umail.bci: stamp-scheme
imail-umail.com: stamp-scheme
imail-util.bci: stamp-scheme
imail-util.com: stamp-scheme
imap-response.bci: stamp-scheme
imap-response.com: stamp-scheme
imap-syntax.bci: stamp-scheme
imap-syntax.com: stamp-scheme
imail-@MIT_SCHEME_OS_SUFFIX@.pkd: stamp-scheme
stamp-scheme: $(sources) imail.pkg
	touch stamp-scheme
	if ! ./compile.sh; then rm stamp-scheme; exit 1; fi

CLEANFILES = *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd
CLEANFILES += stamp-scheme

#TESTS = imail-check.sh

ETAGS_ARGS = $(sources)
TAGS_DEPENDENCIES = $(sources)

EXTRA_DIST += $(sources) compile.sh make.scm compile.scm imail.pkg

install-data-hook:

install-html: install-html-am
	( echo '(add-plugin "imail" "@MIT_SCHEME_PROJECT@"'; \
	  echo '            ""'; \
	  echo '            ""'; \
	  echo '            "$(DESTDIR)$(scmdocdir)")' ) \
	| $(MIT_SCHEME_EXE) --batch-mode

uninstall-hook:
