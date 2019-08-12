dnl Process this file with autoconf to produce a configure script.

AC_INIT([MIT/GNU Scheme documentation],
	[10.1.10],
	[bug-mit-scheme@gnu.org],
	[mit-scheme-doc])
AC_CONFIG_SRCDIR([ref-manual/scheme.texinfo])

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

AC_PROG_INSTALL
if test x"$(${INSTALL} --help 2> /dev/null | fgrep -e --preserve-timestamps)" != x
then
    INSTALL="${INSTALL} --preserve-timestamps"
fi
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_CHECK_PROG([INSTALL_INFO_PROG], [install-info], [install-info], [true])

PROJECT=mit-scheme
TARGETS=
INST_TARGETS=

AC_ARG_ENABLE([html],
    [AC_HELP_STRING([--enable-html],
        [generate HTML documentation [[yes]]])])
: ${enable_html=yes}
if test x"${enable_html}" = xno; then
    htmldir=
elif test x"${enable_html}" != xyes; then
    htmldir=${enable_html}
fi
test x"${htmldir}" = x || TARGETS="${TARGETS} \$(TARGET_ROOT)/index.html"
test x"${htmldir}" = x || INST_TARGETS="${INST_TARGETS} install-html"

AC_ARG_ENABLE([pdf],
    [AC_HELP_STRING([--enable-pdf],
	[generate PDF documentation [[yes]]])])
: ${enable_pdf=yes}
if test x"${enable_pdf}" = xno; then
    pdfdir=
elif test x"${enable_pdf}" != xyes; then
    pdfdir=${enable_pdf}
fi
test x"${pdfdir}" = x || TARGETS="${TARGETS} \$(TARGET_ROOT).pdf"
test x"${pdfdir}" = x || INST_TARGETS="${INST_TARGETS} install-pdf"

AC_ARG_ENABLE([ps],
    [AC_HELP_STRING([--enable-ps],
	[generate Postscript documentation [[no]]])])
: ${enable_pdf=no}
if test x"${enable_ps}" = xno; then
    psdir=
elif test x"${enable_ps}" != xyes; then
    psdir=${enable_ps}
fi
test x"${psdir}" = x || TARGETS="${TARGETS} \$(TARGET_ROOT).ps"
test x"${psdir}" = x || INST_TARGETS="${INST_TARGETS} install-ps"

AC_SUBST([PROJECT])
AC_SUBST([TARGETS])
AC_SUBST([INST_TARGETS])

AC_CONFIG_FILES([
	Makefile
	make-common
	ffi/Makefile
	ref-manual/Makefile
	sos/Makefile
	user-manual/Makefile])
AC_OUTPUT
