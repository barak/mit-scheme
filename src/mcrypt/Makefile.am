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
scmlib_mcryptdir = $(scmlibdir)/mcrypt

scmlib_LTLIBRARIES = mcrypt-shim.la
scmlib_DATA = mcrypt-types.bin mcrypt-const.bin
scmlib_mcrypt_DATA = make.scm
scmlib_mcrypt_DATA += mcrypt.scm mcrypt.bin mcrypt.bci mcrypt.com
scmlib_mcrypt_DATA += mcrypt-@MIT_SCHEME_OS_SUFFIX@.pkd

##info_TEXINFOS = mcrypt.texi

AM_CPPFLAGS = -I$(scmlibdir)

mcrypt_shim_la_LIBADD = mcrypt-adapter.lo
mcrypt_shim_la_LDFLAGS = -module -avoid-version -shared

noinst_PROGRAMS = mcrypt-const
mcrypt_const_SOURCES = mcrypt-const.c mcrypt-shim.h

mcrypt-shim.c mcrypt-const.c mcrypt-types.bin: mcrypt.cdecl mcrypt-shim.h
	echo '(generate-shim "mcrypt" "#include \"mcrypt-shim.h\"")' \
	| $(MIT_SCHEME_EXE) --batch-mode

mcrypt-const.bin: mcrypt-const.scm
	echo '(sf "mcrypt-const")' | $(MIT_SCHEME_EXE) --batch-mode

mcrypt-const.scm: mcrypt-const
	./mcrypt-const

mcrypt.bin mcrypt.bci mcrypt.com mcrypt-unx.pkd: \
    mcrypt.scm mcrypt-const.bin mcrypt-types.bin
	echo '(load "compile.scm")' | $(MIT_SCHEME_EXE) --batch-mode

CLEANFILES = mcrypt-const* mcrypt-shim.c
CLEANFILES += *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd
CLEANFILES += test

TESTS = check-scm

install-data-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)/")' \
	| $(MIT_SCHEME_EXE) --batch-mode

uninstall-hook:
	echo '(update-optiondb "$(DESTDIR)$(scmlibdir)/")' \
	| $(MIT_SCHEME_EXE) --batch-mode
