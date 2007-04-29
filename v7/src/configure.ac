dnl Process this file with autoconf to produce a configure script.

AC_INIT([MIT/GNU Scheme], [7.7.91], [bug-mit-scheme@gnu.org], [mit-scheme])
AC_REVISION([$Id: configure.ac,v 1.8 2007/04/29 18:01:44 cph Exp $])
AC_CONFIG_SRCDIR([microcode/boot.c])
AC_PROG_MAKE_SET

AC_COPYRIGHT(
[Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

AC_ARG_ENABLE([native-code],
    AS_HELP_STRING([--enable-native-code],
	[Support native compiled code if available [[yes]]]))
if test -f liarc.stamp; then
    : ${enable_native_code='c'}
else
    : ${enable_native_code='yes'}
fi

case ${enable_native_code} in
c)
    COMPILE_SCRIPT=c-compile.sh
    BUILD_BANDS_SCRIPT=c-build-bands.sh
    INSTALL_COM=:
    ;;
*)
    COMPILE_SCRIPT=compile.sh
    BUILD_BANDS_SCRIPT=build-bands.sh
    INSTALL_COM='$(INSTALL_DATA)'
    ;;
esac

AC_SUBST(COMPILE_SCRIPT)
AC_SUBST(BUILD_BANDS_SCRIPT)
AC_SUBST(INSTALL_COM)

AC_PROG_INSTALL
AC_PROG_LN_S
if test "`${INSTALL} --help 2> /dev/null | fgrep -e --preserve-timestamps`"
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
