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
scmlib_blowfishdir = $(scmlibdir)/blowfish

scmlib_LTLIBRARIES = blowfish-shim.la
scmlib_DATA = blowfish-types.bin blowfish-const.bin
scmlib_blowfish_DATA = make.scm
scmlib_blowfish_DATA += blowfish.scm blowfish.bin blowfish.bci blowfish.com
scmlib_blowfish_DATA += blowfish-@MIT_SCHEME_OS_SUFFIX@.pkd

##info_TEXINFOS = blowfish.texi

AM_CPPFLAGS = -I$(scmlibdir)

blowfish_shim_la_LIBADD = blowfish-adapter.lo
blowfish_shim_la_LDFLAGS = -module -avoid-version -shared

noinst_PROGRAMS = blowfish-const
blowfish_const_SOURCES = blowfish-const.c blowfish-shim.h

blowfish-shim.c blowfish-const.c blowfish-types.bin: \
  blowfish.cdecl blowfish-shim.h
	echo '(generate-shim "blowfish" "#include \"blowfish-shim.h\"")' \
	| $(MIT_SCHEME_EXE) --batch-mode

blowfish-const.bin: blowfish-const.scm
	echo '(sf "blowfish-const")' | $(MIT_SCHEME_EXE) --batch-mode

blowfish-const.scm: blowfish-const
	./blowfish-const

blowfish.bin blowfish.bci blowfish.com blowfish-@MIT_SCHEME_OS_SUFFIX@.pkd: \
  blowfish.scm blowfish-const.bin blowfish-types.bin
	echo '(load "compile.scm")' | $(MIT_SCHEME_EXE) --batch-mode

CLEANFILES = blowfish-const* blowfish-shim.c
CLEANFILES += *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd
CLEANFILES += test

TESTS = check-scm

install-data-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)/")' \
	| $(MIT_SCHEME_EXE) --batch-mode

uninstall-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)/")' \
	| $(MIT_SCHEME_EXE) --batch-mode
