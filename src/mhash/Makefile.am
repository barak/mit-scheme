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
scmlib_mhashdir = $(scmlibdir)/mhash

scmlib_LTLIBRARIES = mhash-shim.la
scmlib_DATA = mhash-types.bin mhash-const.bin
scmlib_mhash_DATA = make.scm
scmlib_mhash_DATA += mhash.scm mhash.bin mhash.bci mhash.com
scmlib_mhash_DATA += mhash-@MIT_SCHEME_OS_SUFFIX@.pkd

#info_TEXINFOS = mit-scheme-mhash.texi
AM_MAKEINFOHTMLFLAGS = --no-split

# Set these to the defaults used by Scheme.
infodir = $(datarootdir)/info
htmldir = $(libdir)/mit-scheme/doc

AM_CPPFLAGS = -I$(scmlibdir)

mhash_shim_la_LIBADD = mhash-adapter.lo
mhash_shim_la_LDFLAGS = -module -avoid-version -shared

noinst_PROGRAMS = mhash-const
mhash_const_SOURCES = mhash-const.c mhash-shim.h

mhash-shim.c mhash-const.c mhash-types.bin: mhash.cdecl mhash-shim.h
	echo '(generate-shim "mhash" "#include \"mhash-shim.h\"")' \
	| $(MIT_SCHEME_EXE) --batch-mode

mhash-const.bin: mhash-const.scm
	echo '(sf "mhash-const")' | $(MIT_SCHEME_EXE) --batch-mode

mhash-const.scm: mhash-const
	./mhash-const

mhash.bin mhash.bci mhash.com mhash-@MIT_SCHEME_OS_SUFFIX@.pkd: \
  mhash.scm mhash-const.bin mhash-types.bin
	echo '(load "compile.scm")' \
	| $(MIT_SCHEME_EXE) --prepend-library . --batch-mode

CLEANFILES = mhash-const* mhash-shim.c
CLEANFILES += *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd

TESTS = mhash-check.sh
CLEANFILES += sample

install-data-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)/")' \
	| $(MIT_SCHEME_EXE) --batch-mode

install-html: install-html-am
	echo '(update-html-index "$(DESTDIR)$(htmldir)/")' \
	| $(MIT_SCHEME_EXE) --batch-mode

uninstall-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)/")' \
	| $(MIT_SCHEME_EXE) --batch-mode
	[ -d "$(DESTDIR)$(scmlib_mhashdir)" ] \
	&& rmdir "$(DESTDIR)$(scmlib_mhashdir)"
	[ -d "$(DESTDIR)$(htmldir)" ]					\
	&& ( echo  '(update-html-index "$(DESTDIR)$(htmldir)/")'	\
	     | $(MIT_SCHEME_EXE) --batch-mode )
