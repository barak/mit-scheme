## Process this file with automake to produce Makefile.in
##
## Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
##     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
##     2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
##     Massachusetts Institute of Technology
##
## This file is part of an X11-screen plugin for MIT/GNU Scheme.
##
## This plugin is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published
## by the Free Software Foundation; either version 2 of the License,
## or (at your option) any later version.
##
## This plugin is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this plugin; if not, write to the Free Software
## Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
## 02110-1301, USA.

EXTRA_DIST = autogen.sh

MIT_SCHEME_EXE = @MIT_SCHEME_EXE@
scmlibdir = @MIT_SCHEME_LIBDIR@
scmlib_subdir = $(scmlibdir)x11-screen

sources = x11-screen.scm # x11-key.scm x11-command.scm
binaries = x11-screen.bci x11-screen.com
# binaries += x11-key.bci x11-key.com x11-command.scm.bci x11-command.scm.com

scmlib_sub_DATA = $(sources)
scmlib_sub_DATA += $(binaries)
scmlib_sub_DATA += make.scm x11-screen-@MIT_SCHEME_OS_SUFFIX@.pkd

# Set these to the defaults used by Scheme.
infodir = $(datarootdir)/info
htmldir = $(libdir)/mit-scheme/doc
dvidir = $(libdir)/mit-scheme/doc
pdfdir = $(libdir)/mit-scheme/doc

#x11-key.bci: stamp-scheme
#x11-key.com: stamp-scheme
#x11-command.scm.bci: stamp-scheme
#x11-command.scm.com: stamp-scheme
x11-screen.bci: stamp-scheme
x11-screen.com: stamp-scheme
x11-screen-@MIT_SCHEME_OS_SUFFIX@.pkd: stamp-scheme
stamp-scheme: $(sources) x11-screen.pkg
	touch stamp-scheme
	if ! ./compile.sh; then rm stamp-scheme; exit 1; fi

CLEANFILES = *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd
CLEANFILES += stamp-scheme

TESTS = x11-screen-check.sh

ETAGS_ARGS = $(sources)
TAGS_DEPENDENCIES = $(sources)

EXTRA_DIST += $(sources) compile.sh x11-screen.pkg
EXTRA_DIST += x11-screen-check.sh x11-screen-test.scm
EXTRA_DIST += make.scm optiondb.scm

install-data-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)")' \
	| $(MIT_SCHEME_EXE) --batch-mode

uninstall-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)")' \
	| $(MIT_SCHEME_EXE) --batch-mode
	[ -d "$(DESTDIR)$(scmlib_subdir)" ] \
	&& rmdir "$(DESTDIR)$(scmlib_subdir)"
