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

ACLOCAL_AMFLAGS = -I m4
EXTRA_DIST = autogen.sh

MIT_SCHEME_EXE = @MIT_SCHEME_EXE@
scmlibdir = @MIT_SCHEME_LIBDIR@
scmlib_subdir = $(scmlibdir)gdbm
scmdocdir = $(datarootdir)/doc/@MIT_SCHEME_PROJECT@

scmlib_LTLIBRARIES = gdbm-shim.la
scmlib_DATA = gdbm-types.bin gdbm-const.bin

sources = @MIT_SCHEME_SCMs@
cdecls = gdbm.cdecl

binaries = @MIT_SCHEME_BCIs@ @MIT_SCHEME_COMs@

scmlib_sub_DATA = $(sources) $(binaries)
scmlib_sub_DATA += make.scm @MIT_SCHEME_PKD@

info_TEXINFOS = mit-scheme-gdbm.texi
AM_MAKEINFOHTMLFLAGS = --no-split

AM_CPPFLAGS = -I@MIT_SCHEME_INCLUDEDIR@
AM_CFLAGS = @MIT_CFLAGS@

gdbm_shim_la_LIBADD = gdbm-adapter.lo
gdbm_shim_la_LDFLAGS = -module -avoid-version -shared

noinst_PROGRAMS = gdbm-const
gdbm_const_SOURCES = gdbm-const.c gdbm-shim.h

gdbm-shim.c: stamp-shim
gdbm-const.c: stamp-shim
gdbm-types.bin: stamp-shim
stamp-shim: gdbm-shim.h $(cdecls)
	touch stamp-shim
	echo '(generate-shim "gdbm" "#include \"gdbm-shim.h\"")' \
	| $(MIT_SCHEME_EXE) --batch-mode \
	|| rm stamp-shim

gdbm-const.bin: gdbm-const.scm
	echo '(sf "gdbm-const")' | $(MIT_SCHEME_EXE) --batch-mode

gdbm-const.scm: gdbm-const
	./gdbm-const

@MIT_SCHEME_DEPS@
stamp-scheme: stamp-shim $(sources) gdbm.pkg
	touch stamp-scheme
	if ! ./compile.sh; then rm stamp-scheme; exit 1; fi

CLEANFILES = gdbm-const* gdbm-shim.c
CLEANFILES += *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd
CLEANFILES += stamp-shim stamp-scheme
CLEANFILES += @MIT_SCHEME_CLEAN@

TESTS = gdbm-check.sh
CLEANFILES += gdbm-check.db

tags: tags-am
	./tags-fix.sh gdbm

all_sources = $(sources) gdbm-adapter.c
ETAGS_ARGS = $(all_sources) -r '/^([^iI].*/' $(cdecls)
TAGS_DEPENDENCIES = $(all_sources) $(cdecls)

EXTRA_DIST += $(all_sources) $(cdecls) compile.scm gdbm.pkg
EXTRA_DIST += gdbm-check.scm gdbm-check.sh
EXTRA_DIST += make.scm optiondb.scm tags-fix.sh

install-data-hook:
	( echo '(add-plugin "gdbm" "@MIT_SCHEME_PROJECT@"'; \
	  echo '            ""'; \
	  echo '            "$(DESTDIR)$(scmlibdir)"'; \
	  echo '            "")' ) \
	| $(MIT_SCHEME_EXE) --batch-mode

install-html: install-html-am
	( echo '(add-plugin "gdbm" "@MIT_SCHEME_PROJECT@"'; \
	  echo '            ""'; \
	  echo '            ""'; \
	  echo '            "$(DESTDIR)$(scmdocdir)")' ) \
	| $(MIT_SCHEME_EXE) --batch-mode

uninstall-hook:
	( echo '(remove-plugin "gdbm" "@MIT_SCHEME_PROJECT@"'; \
	  echo '               ""'; \
	  echo '               "$(DESTDIR)$(scmlibdir)"'; \
	  echo '               "$(DESTDIR)$(scmdocdir)")' ) \
	| $(MIT_SCHEME_EXE) --batch-mode
	[ -d "$(DESTDIR)$(scmlib_subdir)" ] \
	&& rmdir "$(DESTDIR)$(scmlib_subdir)"
