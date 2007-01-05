dnl Process this file with autoconf to produce a configure script.

AC_INIT([MIT/GNU Scheme], [7.7.91], [bug-mit-scheme@gnu.org], [mit-scheme])
AC_REVISION([$Id: configure.ac,v 1.4 2007/01/05 15:33:03 cph Exp $])
AC_CONFIG_SRCDIR([microcode/boot.c])
AC_PROG_MAKE_SET

AC_COPYRIGHT(
[Copyright 2000,2001,2004,2006 Massachusetts Institute of Technology

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

AC_ARG_ENABLE([c-back-end],
[[  --enable-c-back-end     Build portable system with C output [no]]])

AC_PROG_INSTALL
AC_PROG_LN_S
if test "$(${INSTALL} --help 2> /dev/null | fgrep -e --preserve-timestamps)"
then
    INSTALL="${INSTALL} --preserve-timestamps"
fi

AUXDIR=${libdir}/mit-scheme
AC_SUBST(AUXDIR)

AC_CONFIG_SUBDIRS([microcode compiler])
AC_CONFIG_FILES([
Makefile
cref/Makefile
edwin/Makefile
imail/Makefile
runtime/Makefile
sos/Makefile
ssp/Makefile
star-parser/Makefile
xdoc/Makefile
xml/Makefile
])
AC_OUTPUT
