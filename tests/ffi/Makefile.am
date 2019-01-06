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

MIT_SCHEME_EXE = @MIT_SCHEME_EXE@
scmlibdir = $(libdir)/mit-scheme-@MIT_SCHEME_ARCH@

scmlib_LTLIBRARIES = ffi-test-shim.la
scmlib_DATA = ffi-test-types.bin ffi-test-const.bin

AM_CPPFLAGS = -I@MIT_SCHEME_INCLUDE_DIR@

ffi_test_shim_la_LIBADD = ffi-test.lo
ffi_test_shim_la_LDFLAGS = -module -avoid-version -shared

noinst_PROGRAMS = ffi-test-const
ffi_test_const_SOURCES = ffi-test-const.c ffi-test-shim.h

ffi-test-shim.c ffi-test-const.c ffi-test-types.bin: ffi-test.cdecl ffi-test.h
	echo '(generate-shim "ffi-test" "#include \"ffi-test.h\"")' \
	| $(MIT_SCHEME_EXE) --batch-mode

ffi-test-const.bin: ffi-test-const.scm
	echo '(sf "ffi-test-const")' | $(MIT_SCHEME_EXE) --batch-mode

ffi-test-const.scm: ffi-test-const
	./ffi-test-const

ffi-test.bin ffi-test.bci ffi-test.com ffi-test-unx.pkd: \
    ffi-test.scm ffi-test-const.bin ffi-test-types.bin
	echo '(load "compile.scm")' | $(MIT_SCHEME_EXE) --batch-mode

CLEANFILES = ffi-test-const* ffi-test-shim.c
CLEANFILES += *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd
MAINTAINERCLEANFILES = Makefile.in aclocal.m4 compile config.* configure
MAINTAINERCLEANFILES += depcomp install-sh ltmain.sh missing

maintainer-clean-local:
	-rm -rf m4
