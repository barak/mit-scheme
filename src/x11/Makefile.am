## Process this file with automake to produce Makefile.in
##
## Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
##     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
##     2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
##     Massachusetts Institute of Technology
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

MIT_SCHEME_EXE = @MIT_SCHEME_EXE@
scmlibdir = @MIT_SCHEME_LIBDIR@
scmlib_subdir = $(scmlibdir)x11

scmlib_LTLIBRARIES = x11-shim.la
scmlib_DATA = x11-types.bin x11-const.bin

sources = x11base.scm x11color.scm x11graph.scm x11term.scm x11device.scm
binaries = x11base.bci x11base.com x11color.bci x11color.com
binaries += x11graph.bci x11graph.com x11term.bci x11term.com
binaries += x11device.bci x11device.com
cdecls = x11.cdecl

scmlib_sub_DATA = $(sources) $(binaries)
scmlib_sub_DATA += make.scm x11-@MIT_SCHEME_OS_SUFFIX@.pkd

#info_TEXINFOS = mit-scheme-x11.texi
AM_MAKEINFOHTMLFLAGS = --no-split

# Set these to the defaults used by Scheme.
infodir = $(datarootdir)/info
htmldir = $(libdir)/mit-scheme/doc
dvidir = $(libdir)/mit-scheme/doc
pdfdir = $(libdir)/mit-scheme/doc

AM_CPPFLAGS = -I@MIT_SCHEME_INCLUDEDIR@
AM_CFLAGS = `pkg-config --cflags x11`
LIBS = `pkg-config --libs x11`

x11_shim_la_LIBADD = x11base.lo x11color.lo x11graph.lo x11term.lo
c_sources = x11-shim.h x11.h x11base.c x11color.c x11graph.c x11term.c
x11_shim_la_LDFLAGS = -module -avoid-version -shared

noinst_PROGRAMS = x11-const
x11_const_SOURCES = x11-const.c x11-shim.h

x11-shim.c: stamp-shim
x11-const.c: stamp-shim
x11-types.bin: stamp-shim
stamp-shim: $(c_sources) $(cdecls)
	touch stamp-shim
	echo '(generate-shim "x11" "#include \"x11-shim.h\"")' \
	| $(MIT_SCHEME_EXE) --batch-mode \
	|| rm stamp-shim

x11-const.bin: x11-const.scm
	echo '(sf "x11-const")' | $(MIT_SCHEME_EXE) --batch-mode

x11-const.scm: x11-const
	./x11-const

x11base.bci: stamp-scheme
x11base.com: stamp-scheme
x11color.bci: stamp-scheme
x11color.com: stamp-scheme
x11graph.bci: stamp-scheme
x11graph.com: stamp-scheme
x11term.bci: stamp-scheme
x11term.com: stamp-scheme
x11device.bci: stamp-scheme
x11device.com: stamp-scheme
x11-@MIT_SCHEME_OS_SUFFIX@.pkg: stamp-scheme
stamp-scheme: stamp-shim $(sources) x11.pkg
	touch stamp-scheme
	if ! ./compile.sh; then rm stamp-scheme; exit 1; fi

CLEANFILES = x11-const* x11-shim.c
CLEANFILES += *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd

TESTS = x11-check.sh

tags: tags-am
	./tags-fix.sh x11

all_sources = $(sources) $(c_sources)
ETAGS_ARGS = $(all_sources) -r '/^([^iI].*/' $(cdecls)
TAGS_DEPENDENCIES = $(all_sources) $(cdecls)

EXTRA_DIST += $(all_sources) $(cdecls) compile.sh x11.pkg
EXTRA_DIST += x11-check.sh x11-test.scm
EXTRA_DIST += make.scm optiondb.scm tags-fix.sh

install-data-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)")' \
	| $(MIT_SCHEME_EXE) --batch-mode

uninstall-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)")' \
	| $(MIT_SCHEME_EXE) --batch-mode
	[ -d "$(DESTDIR)$(scmlib_subdir)" ] \
	&& rmdir "$(DESTDIR)$(scmlib_subdir)"
