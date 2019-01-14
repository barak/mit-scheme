dnl Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([MIT/GNU Scheme test-ffi plugin],
        [0.1],
        [bug-mit-scheme@gnu.org],
        [mit-scheme-test-ffi])
AC_CONFIG_SRCDIR([ffi-test.cdecl])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])

AC_COPYRIGHT(
[Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.
])

AH_TOP([/*

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/])

AM_INIT_AUTOMAKE

AC_PROG_LIBTOOL
AC_PROG_CC
AC_PROG_CPP

dnl Initialize libtool
LT_PREREQ([2.2.6])
LT_INIT([dlopen win32-dll])


: ${MIT_SCHEME_EXE="../../src/run-build"}
: ${MITSCHEME_LIBRARY_PATH="../../src/lib"}
: ${MIT_SCHEME_INCLUDE_DIR=${MITSCHEME_LIBRARY_PATH}}
MIT_SCHEME_ARCH=`echo "(display microcode-id/compiled-code-type)" \
		 | ${MIT_SCHEME_EXE} --batch-mode`

AC_SUBST([MIT_SCHEME_EXE])
AC_SUBST([MIT_SCHEME_INCLUDE_DIR])
AC_SUBST([MIT_SCHEME_ARCH])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
