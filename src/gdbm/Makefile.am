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
scmlibdir = $(libdir)/mit-scheme-@MIT_SCHEME_ARCH@
scmlib_gdbmdir = $(scmlibdir)/gdbm

scmlib_LTLIBRARIES = gdbm-shim.la
scmlib_DATA = gdbm-types.bin gdbm-const.bin
scmlib_gdbm_DATA = make.scm
scmlib_gdbm_DATA += gdbm.scm gdbm.bin gdbm.bci gdbm.com
scmlib_gdbm_DATA += gdbm-@MIT_SCHEME_OS_SUFFIX@.pkd

#info_TEXINFOS = mit-scheme-gdbm.texi
AM_MAKEINFOHTMLFLAGS = --no-split

# Set this to the default used by Scheme.
htmldir = $(libdir)/mit-scheme/doc

AM_CPPFLAGS = -I$(scmlibdir)

gdbm_shim_la_LIBADD = gdbm-adapter.lo
gdbm_shim_la_LDFLAGS = -module -avoid-version -shared

noinst_PROGRAMS = gdbm-const
gdbm_const_SOURCES = gdbm-const.c gdbm-shim.h

gdbm-shim.c gdbm-const.c gdbm-types.bin: gdbm.cdecl gdbm-shim.h
	echo '(generate-shim "gdbm" "#include \"gdbm-shim.h\"")' \
	| $(MIT_SCHEME_EXE) --batch-mode

gdbm-const.bin: gdbm-const.scm
	echo '(sf "gdbm-const")' | $(MIT_SCHEME_EXE) --batch-mode

gdbm-const.scm: gdbm-const
	./gdbm-const

gdbm.bin gdbm.bci gdbm.com gdbm-@MIT_SCHEME_OS_SUFFIX@.pkd: \
  gdbm.scm gdbm-const.bin gdbm-types.bin
	echo '(load "compile.scm")' \
	| $(MIT_SCHEME_EXE) --prepend-library . --batch-mode

CLEANFILES = gdbm-const* gdbm-shim.c
CLEANFILES += *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd

TESTS = gdbm-check.sh
CLEANFILES += gdbm-check.db

install-data-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)/")' \
	| $(MIT_SCHEME_EXE) --batch-mode

install-html: install-html-am
	echo '(update-html-index "$(DESTDIR)$(htmldir)/")' \
	| $(MIT_SCHEME_EXE) --batch-mode

uninstall-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)/")' \
	| $(MIT_SCHEME_EXE) --batch-mode
	[ -d "$(DESTDIR)$(scmlib_gdbmdir)" ] \
	&& rmdir "$(DESTDIR)$(scmlib_gdbmdir)"
	[ -d "$(DESTDIR)$(htmldir)" ]					\
	&& ( echo  '(update-html-index "$(DESTDIR)$(htmldir)/")'	\
	     | $(MIT_SCHEME_EXE) --batch-mode )
