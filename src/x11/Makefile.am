## Process this file with automake to produce Makefile.in
##
## Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
##     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
##     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
##     2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Massachusetts
##     Institute of Technology
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

ACLOCAL_AMFLAGS = -I m4
EXTRA_DIST = autogen.sh

MIT_SCHEME_EXE = @MIT_SCHEME_TOOLCHAIN_EXE@
scmlibdir = @MIT_SCHEME_LIBDIR@
scmlib_subdir = $(scmlibdir)x11
scmdocdir = $(datarootdir)/doc/@MIT_SCHEME_PROJECT@

scmlib_LTLIBRARIES = x11-shim.la
scmlib_DATA = x11-types.bin x11-const.bin

sources = @MIT_SCHEME_SCMs@
cdecls = x11.cdecl

binaries = @MIT_SCHEME_BCIs@ @MIT_SCHEME_COMs@

scmlib_sub_DATA = $(sources) $(binaries)
scmlib_sub_DATA += make.scm @MIT_SCHEME_PKD@

#info_TEXINFOS = mit-scheme-x11.texi
#AM_MAKEINFOHTMLFLAGS = --no-split

AM_CPPFLAGS = -I@MIT_SCHEME_INCLUDEDIR@
AM_CFLAGS = @MIT_CFLAGS@

x11_shim_la_SOURCES = x11base.c x11color.c x11graph.c x11term.c x11-shim.h x11.h
nodist_x11_shim_la_SOURCES = x11-shim.c
x11_shim_la_LDFLAGS = -module -avoid-version -shared
x11_shim_la_LIBADD = @MIT_LIBS@

noinst_PROGRAMS = x11-const$(EXEEXT)
x11_const_SOURCES = x11-shim.h x11.h
nodist_x11_const_SOURCES = x11-const.c

x11-shim.c: stamp-shim
x11-const.c: stamp-shim
x11-types.bin: stamp-shim
stamp-shim: x11-shim.h x11.h $(cdecls)
	echo '(generate-shim "x11" "#include \"x11-shim.h\"")' \
	    | $(MIT_SCHEME_EXE) --batch-mode
	echo "done" > $@

x11-const.bin: x11-const.scm
	echo '(sf "x11-const")' | $(MIT_SCHEME_EXE) --batch-mode

x11-const.scm: x11-const$(EXEEXT)
	./x11-const

@MIT_SCHEME_DEPS@
stamp-scheme: stamp-shim $(sources) x11-const.bin x11.pkg
	MIT_SCHEME_EXE=$(MIT_SCHEME_EXE) ./compile.sh
	echo "done" > $@

MOSTLYCLEANFILES = $(scmlib_LTLIBRARIES) x11-const$(EXEEXT)

mostlyclean-local:
	-rm -rf .libs

CLEANFILES = x11-const* x11-shim.c
CLEANFILES += *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd
CLEANFILES += stamp-shim stamp-scheme
CLEANFILES += @MIT_SCHEME_CLEAN@

TESTS = x11-check.sh

tags: tags-am $(sources) $(cdecls)
	$(ETAGS) -a $(sources) -r '/^([^iI].*/' $(cdecls)
	./tags-fix.sh

EXTRA_DIST += $(sources) $(cdecls)
EXTRA_DIST += compile.sh compile.scm x11.pkg
EXTRA_DIST += x11-check.sh x11-test.scm
EXTRA_DIST += make.scm optiondb.scm tags-fix.sh
