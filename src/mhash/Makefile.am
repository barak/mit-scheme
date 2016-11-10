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
scmlib_subdir = $(scmlibdir)mhash

scmlib_LTLIBRARIES = mhash-shim.la
scmlib_DATA = mhash-types.bin mhash-const.bin

sources = @MIT_SCHEME_SCMs@
cdecls = mhash.cdecl

binaries = @MIT_SCHEME_BINs@ @MIT_SCHEME_BCIs@ @MIT_SCHEME_COMs@

scmlib_sub_DATA = $(sources) $(binaries)
scmlib_sub_DATA += make.scm @MIT_SCHEME_PKD@

#info_TEXINFOS = mit-scheme-mhash.texi
AM_MAKEINFOHTMLFLAGS = --no-split

# Set these to the defaults used by Scheme.
infodir = $(datarootdir)/info
htmldir = $(libdir)/mit-scheme/doc
dvidir = $(libdir)/mit-scheme/doc
pdfdir = $(libdir)/mit-scheme/doc

AM_CPPFLAGS = -I@MIT_SCHEME_INCLUDEDIR@

mhash_shim_la_LIBADD = mhash-adapter.lo
mhash_shim_la_LDFLAGS = -module -avoid-version -shared

noinst_PROGRAMS = mhash-const
mhash_const_SOURCES = mhash-const.c mhash-shim.h

mhash-shim.c: stamp-shim
mhash-const.c: stamp-shim
mhash-types.bin: stamp-shim
stamp-shim: mhash-shim.h $(cdecls)
	touch stamp-shim
	echo '(generate-shim "mhash" "#include \"mhash-shim.h\"")' \
	| $(MIT_SCHEME_EXE) --batch-mode \
	|| rm stamp-shim

mhash-const.bin: mhash-const.scm
	echo '(sf "mhash-const")' | $(MIT_SCHEME_EXE) --batch-mode

mhash-const.scm: mhash-const
	./mhash-const

@MIT_SCHEME_DEPS@
stamp-scheme: stamp-shim $(sources) mhash.pkg
	touch stamp-scheme
	if ! echo '(load "compile.scm")' \
	     | $(MIT_SCHEME_EXE) --prepend-library . --batch-mode; then \
	    rm stamp-scheme; exit 1; fi

CLEANFILES = mhash-const* mhash-shim.c
CLEANFILES += *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd
CLEANFILES += stamp-shim stamp-scheme
CLEANFILES += @MIT_SCHEME_CLEAN@

TESTS = mhash-check.sh
CLEANFILES += sample

tags: tags-am
	./tags-fix.sh mhash

all_sources = $(sources) mhash-adapter.c
ETAGS_ARGS = $(all_sources) -r '/^([^iI].*/' $(cdecls)
TAGS_DEPENDENCIES = $(all_sources) $(cdecls)

EXTRA_DIST += $(all_sources) $(cdecls) compile.scm mhash.pkg
EXTRA_DIST += mhash-check.scm mhash-check.sh
EXTRA_DIST += make.scm optiondb.scm tags-fix.sh

install-data-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)")' \
	| $(MIT_SCHEME_EXE) --batch-mode

#install-html: install-html-am
#	echo '(update-html-index "$(DESTDIR)$(htmldir)/")' \
#	| $(MIT_SCHEME_EXE) --batch-mode

uninstall-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)")' \
	| $(MIT_SCHEME_EXE) --batch-mode
	[ -d "$(DESTDIR)$(scmlib_subdir)" ] \
	&& rmdir "$(DESTDIR)$(scmlib_subdir)"
#	[ -d "$(DESTDIR)$(htmldir)" ]					\
#	&& ( echo  '(update-html-index "$(DESTDIR)$(htmldir)/")'	\
#	     | $(MIT_SCHEME_EXE) --batch-mode )
