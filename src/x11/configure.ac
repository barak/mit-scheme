dnl Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([MIT/GNU Scheme x11 plugin],
        [0.1],
        [bug-mit-scheme@gnu.org],
        [mit-scheme-x11])
AC_CONFIG_SRCDIR([x11.pkg])
AC_CONFIG_MACRO_DIR([m4])

AC_COPYRIGHT(
[Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

This file is part of an x11 plugin for MIT/GNU Scheme.

This plugin is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

This plugin is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this plugin; if not, write to the Free Software Foundation,
Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.
])

AM_INIT_AUTOMAKE

LT_PREREQ([2.2.6])
LT_INIT([dlopen])

AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL

AC_CHECK_PROG([PKG_CONFIG], [pkg-config], [yes])

if ! pkg-config --exists x11 2>/dev/null; then
    AC_MSG_ERROR([X11 not found.])
fi

: ${MIT_SCHEME_EXE=mit-scheme}
MIT_SCHEME_LIBDIR=`( echo "(display (->namestring" ;\
		     echo "          (system-library-directory-pathname)))" ) \
		   | ${MIT_SCHEME_EXE} --batch-mode`
MIT_SCHEME_INCLUDEDIR=`( echo "(display (->namestring" ;\
			 echo "          (directory-pathname" ;\
			 echo "           (system-library-pathname" ;\
			 echo '            "mit-scheme.h"))))' ) \
		       | ${MIT_SCHEME_EXE} --batch-mode`
MIT_SCHEME_OS_SUFFIX=`echo "(display (microcode-id/operating-system-suffix))" \
		      | ${MIT_SCHEME_EXE} --batch-mode`

AC_SUBST([MIT_SCHEME_EXE])
AC_SUBST([MIT_SCHEME_LIBDIR])
AC_SUBST([MIT_SCHEME_INCLUDEDIR])
AC_SUBST([MIT_SCHEME_OS_SUFFIX])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
