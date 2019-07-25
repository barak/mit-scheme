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

MIT_SCHEME_EXE = @MIT_SCHEME_TOOLCHAIN_EXE@
scmlibdir = @MIT_SCHEME_LIBDIR@
scmlib_subdir = $(scmlibdir)x11-screen
scmdocdir = $(datarootdir)/doc/@MIT_SCHEME_PROJECT@

sources = @MIT_SCHEME_SCMs@

binaries = @MIT_SCHEME_BCIs@ @MIT_SCHEME_COMs@

scmlib_sub_DATA = $(sources) $(binaries)
scmlib_sub_DATA += make.scm @MIT_SCHEME_PKD@

#info_TEXINFOS = mit-scheme-x11-screen.texi
#AM_MAKEINFOHTMLFLAGS = --no-split

@MIT_SCHEME_DEPS@
stamp-scheme: $(sources) x11-screen.pkg
	MIT_SCHEME_EXE=$(MIT_SCHEME_EXE) ./compile.sh
	echo "done" > $@

CLEANFILES = *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd
CLEANFILES += stamp-scheme
CLEANFILES += @MIT_SCHEME_CLEAN@

TESTS = x11-screen-check.sh

TAGS_FILES = $(sources)

EXTRA_DIST += $(sources) compile.sh x11-screen.pkg
EXTRA_DIST += x11-screen-check.sh x11-screen-test.scm
EXTRA_DIST += make.scm optiondb.scm compile.scm
