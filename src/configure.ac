dnl Process this file with autoconf to produce a configure script.

AC_INIT([MIT/GNU Scheme], [9.0.1], [bug-mit-scheme@gnu.org], [mit-scheme])
AC_CONFIG_SRCDIR([microcode/boot.c])
AC_CONFIG_AUX_DIR([microcode])
AC_PROG_MAKE_SET

AC_COPYRIGHT(
[Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

AC_ARG_ENABLE([debugging],
    AS_HELP_STRING([--enable-debugging],
	[Compile with debugging support [[no]]]))
: ${enable_debugging='no'}

AC_ARG_ENABLE([native-code],
    AS_HELP_STRING([--enable-native-code],
	[Support native compiled code if available [[yes]]]))
: ${enable_native_code=yes}

AC_ARG_ENABLE([host-scheme-test],
    AS_HELP_STRING([--enable-host-scheme-test],
	[Test for working scheme on build host [[no]]]))
: ${enable_host_scheme_test=no}

AC_CANONICAL_HOST

MIT_SCHEME_NATIVE_CODE([${enable_native_code}],[${host_cpu}])
case ${mit_scheme_native_code} in
c)
    ALL_TARGET=all-liarc
    INSTALL_COM=:
    INSTALL_LIARC_BUNDLES=install-liarc-bundles
    ;;
svm1)
    ALL_TARGET=all-svm
    INSTALL_COM='$(INSTALL_DATA)'
    INSTALL_LIARC_BUNDLES=
    ;;
*)
    ALL_TARGET=all-native
    INSTALL_COM='$(INSTALL_DATA)'
    INSTALL_LIARC_BUNDLES=
    ;;
esac
AUXDIR_NAME=mit-scheme-${mit_scheme_native_code}
AUXDIR='$(libdir)'/${AUXDIR_NAME}

# Please keep the following messages synchronized with the messages in
# these files:
#
#   src/Setup.sh
#   src/configure.ac
#   src/etc/make-native.sh

: ${MIT_SCHEME_EXE:=mit-scheme-${mit_scheme_native_code}}
if test x"${enable_host_scheme_test}" = xyes; then
    AC_MSG_CHECKING([for an existing MIT/GNU Scheme installation])
    if "${MIT_SCHEME_EXE}" --batch-mode --eval '(%exit)' >/dev/null 2>&1; then
	:
    elif mit-scheme --batch-mode --eval '(%exit)' >/dev/null 2>&1; then
	MIT_SCHEME_EXE=mit-scheme
    else
	AC_MSG_ERROR([
This script needs an existing MIT/GNU Scheme installation to function,
but neither the program \`${MIT_SCHEME_EXE}' nor the program
\`mit-scheme' appears to run it.

If you have installed MIT/GNU Scheme in an unusual location, set the
environment variable MIT_SCHEME_EXE to the name or pathname of the
MIT/GNU Scheme executable, which is usually \`mit-scheme' or
\`/usr/local/bin/mit-scheme', and set the environment variable
MITSCHEME_LIBRARY_PATH to the pathname of the MIT/GNU Scheme library
directory, which is usually \`/usr/local/lib/mit-scheme-${mit_scheme_native_code}'.
])
    fi
    AC_MSG_RESULT([yes])
fi

AC_SUBST([ALL_TARGET])
AC_SUBST([FFIS])
AC_SUBST([INSTALL_COM])
AC_SUBST([INSTALL_LIARC_BUNDLES])
AC_SUBST([MIT_SCHEME_EXE])
AC_SUBST([AUXDIR_NAME])
AC_SUBST([AUXDIR])

AC_PROG_INSTALL
AC_PROG_LN_S

echo etc/create-makefiles.sh "${MIT_SCHEME_EXE}" "${mit_scheme_native_code}"
etc/create-makefiles.sh "${MIT_SCHEME_EXE}" "${mit_scheme_native_code}"
compiler/configure "${mit_scheme_native_code}"

AC_CONFIG_SUBDIRS([microcode])

m4_include(microcode/achost.ac)

AC_SUBST([CCLD])
AC_SUBST([DEFS])
AC_SUBST([CFLAGS])
AC_SUBST([CPPFLAGS])
AC_SUBST([LDFLAGS])
AC_SUBST([SHIM_CFLAGS])
AC_SUBST([SHIM_LDFLAGS])

AC_CONFIG_FILES([
Makefile
6001/Makefile
compiler/Makefile
cref/Makefile
edwin/Makefile
ffi/Makefile
imail/Makefile
runtime/Makefile
sf/Makefile
sos/Makefile
ssp/Makefile
star-parser/Makefile
win32/Makefile
xdoc/Makefile
xml/Makefile
])
AC_OUTPUT

if test x"${mit_scheme_native_code}" = xc; then
    BN=liarc-cc
    (cd lib; rm -f ${BN}; ${LN_S} ../microcode/${BN} .)
    BN=liarc-ld
    (cd lib; rm -f ${BN}; ${LN_S} ../microcode/${BN} .)
    for BN in star-parser; do
        (cd lib; rm -f ${BN}; ${LN_S} ../${BN} .)
    done
    for BUNDLE in 6001 compiler cref edwin ffi imail sf sos ssp star-parser \
		  xdoc xml $FFIS; do
    	SO=${BUNDLE}.so
	(cd lib/lib; rm -f ${SO}; ${LN_S} ../../${BUNDLE}/${SO} .)
    done
fi
