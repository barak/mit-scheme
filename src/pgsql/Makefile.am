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

MIT_SCHEME_EXE = @MIT_SCHEME_TOOLCHAIN_EXE@
scmlibdir = @MIT_SCHEME_LIBDIR@
scmlib_subdir = $(scmlibdir)pgsql
scmdocdir = $(datarootdir)/doc/@MIT_SCHEME_PROJECT@

scmlib_LTLIBRARIES = pgsql-shim.la
scmlib_DATA = pgsql-types.bin pgsql-const.bin

sources = @MIT_SCHEME_SCMs@
cdecls = pgsql.cdecl

binaries = @MIT_SCHEME_BCIs@ @MIT_SCHEME_COMs@

scmlib_sub_DATA = $(sources) $(binaries)
scmlib_sub_DATA += make.scm @MIT_SCHEME_PKD@

#info_TEXINFOS = mit-scheme-pgsql.texi
#AM_MAKEINFOHTMLFLAGS = --no-split

AM_CPPFLAGS = -I@MIT_SCHEME_INCLUDEDIR@
AM_CFLAGS = @MIT_CFLAGS@ `pkg-config --cflags libpq`
AM_LIBS = `pkg-config --libs libpq`

pgsql_shim_la_SOURCES = pgsql-shim.h
nodist_pgsql_shim_la_SOURCES = pgsql-shim.c
pgsql_shim_la_LDFLAGS = -module -avoid-version -shared

noinst_PROGRAMS = pgsql-const$(EXEEXT)
pgsql_const_SOURCES = pgsql-shim.h
nodist_pgsql_const_SOURCES = pgsql-const.c

pgsql-shim.c: stamp-shim
pgsql-const.c: stamp-shim
pgsql-types.bin: stamp-shim
stamp-shim: pgsql-shim.h $(cdecls)
	echo '(generate-shim "pgsql" "#include \"pgsql-shim.h\"")' \
	    | $(MIT_SCHEME_EXE) --batch-mode
	echo "done" > $@

pgsql-const.bin: pgsql-const.scm
	echo '(sf "pgsql-const")' | $(MIT_SCHEME_EXE) --batch-mode

pgsql-const.scm: pgsql-const$(EXEEXT)
	./pgsql-const

@MIT_SCHEME_DEPS@
stamp-scheme: stamp-shim $(sources) pgsql-const.bin pgsql.pkg
	MIT_SCHEME_EXE=$(MIT_SCHEME_EXE) ./compile.sh
	echo "done" > $@

MOSTLYCLEANFILES = $(scmlib_LTLIBRARIES) pgsql-const$(EXEEXT)

mostlyclean-local:
	-rm -rf .libs

CLEANFILES = pgsql-const* pgsql-shim.c
CLEANFILES += *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd
CLEANFILES += stamp-shim stamp-scheme
CLEANFILES += @MIT_SCHEME_CLEAN@

TESTS = pgsql-check.sh
CLEANFILES += pgsql-check.db

tags: tags-am $(sources) $(cdecls)
	$(ETAGS) -a $(sources) -r '/^([^iI].*/' $(cdecls)
	./tags-fix.sh

EXTRA_DIST += $(sources) $(cdecls)
EXTRA_DIST += compile.sh compile.scm pgsql.pkg
EXTRA_DIST += pgsql-check.scm pgsql-check.sh
EXTRA_DIST += make.scm optiondb.scm tags-fix.sh

install-data-hook:
	( echo '(add-plugin "pgsql" "@MIT_SCHEME_PROJECT@"'; \
	  echo '            ""'; \
	  echo '            "$(DESTDIR)$(scmlibdir)"'; \
	  echo '            "")' ) \
	| $(MIT_SCHEME_EXE) --batch-mode

install-html: install-html-am
	( echo '(add-plugin "pgsql" "@MIT_SCHEME_PROJECT@"'; \
	  echo '            ""'; \
	  echo '            ""'; \
	  echo '            "$(DESTDIR)$(scmdocdir)")' ) \
	| $(MIT_SCHEME_EXE) --batch-mode

uninstall-hook:
	( echo '(remove-plugin "pgsql" "@MIT_SCHEME_PROJECT@"'; \
	  echo '               ""'; \
	  echo '               "$(DESTDIR)$(scmlibdir)"'; \
	  echo '               "$(DESTDIR)$(scmdocdir)")' ) \
	| $(MIT_SCHEME_EXE) --batch-mode
	[ -d "$(DESTDIR)$(scmlib_subdir)" ] \
	&& rmdir "$(DESTDIR)$(scmlib_subdir)"
