dnl Process this file with autoconf to produce a configure script.

AC_INIT([MIT/GNU Scheme], [10.1.10], [bug-mit-scheme@gnu.org], [mit-scheme])
AC_CONFIG_SRCDIR([microcode/boot.c])
AC_CONFIG_AUX_DIR([microcode])
AC_PROG_MAKE_SET

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

if test -f .native-release-marker; then
    IS_NATIVE_RELEASE=`cat .native-release-marker`
else
    IS_NATIVE_RELEASE=
fi

AC_ARG_ENABLE([debugging],
    AS_HELP_STRING([--enable-debugging],
	[Compile with debugging support [[no]]]))
: ${enable_debugging=no}

AC_ARG_ENABLE([native-code],
    AS_HELP_STRING([--enable-native-code],
	[Support native compiled code if available [[yes]]]))
if test x"${IS_NATIVE_RELEASE}" != x; then
    : ${enable_native_code=${IS_NATIVE_RELEASE}}
else
    : ${enable_native_code=yes}
fi

AC_ARG_WITH([compiler-target],
    AS_HELP_STRING([--with-compiler-target],
	[Compiler target architecture [[same as native code]]]))
: ${with_compiler_target=yes}

AC_ARG_ENABLE([host-scheme-test],
    AS_HELP_STRING([--enable-host-scheme-test],
	[Test for working scheme on build host [[if necessary]]]))
if test x"${IS_NATIVE_RELEASE}" != x || test -f lib/all.com; then
    : ${enable_host_scheme_test=no}
else
    : ${enable_host_scheme_test=yes}
fi

AC_ARG_ENABLE([cross-compiling],
    AS_HELP_STRING([--enable-cross-compiling],
	[Cross-compile scheme, even if to the same target]))
: ${enable_cross_compiling=no}
if test "x${enable_cross_compiling}" = xyes; then
    IF_CROSS=
else
    IF_CROSS='#!cross-compiling: '
fi

AC_ARG_WITH([default-target],
    AS_HELP_STRING([--with-default-target],
	[Set the default make target [[all]]]))
if test x"${IS_NATIVE_RELEASE}" != x; then
    DEFAULT_DEFAULT_TARGET=compile-microcode
elif test "x${enable_cross_compiling}" = xyes; then
    DEFAULT_DEFAULT_TARGET=cross-host
else
    DEFAULT_DEFAULT_TARGET=all
fi
: ${with_default_target=${DEFAULT_DEFAULT_TARGET}}
DEFAULT_TARGET=${with_default_target}

AC_ARG_WITH([scheme-build],
    AS_HELP_STRING([--with-scheme-build],
	[Use this directory for the Scheme compiler]))
: ${with_scheme_build=default}

# optional plugins

AC_ARG_ENABLE([default-plugins],
    AS_HELP_STRING([--enable-default-plugins],
	[Enable default plugins [[yes]]]))
: ${enable_default_plugins=yes}

AC_ARG_ENABLE([x11],
    AS_HELP_STRING([--enable-x11],
	[Add support for x11 [[yes]]]))

AC_ARG_ENABLE([edwin],
    AS_HELP_STRING([--enable-edwin],
	[Add support for the Edwin editor [[yes]]]))
: ${enable_edwin=${enable_default_plugins}}

AC_ARG_ENABLE([imail],
    AS_HELP_STRING([--enable-imail],
	[Add support for the Imail mail reader (implies --enable-edwin) [[yes]]]))
: ${enable_imail=${enable_default_plugins}}
if test x"${enable_imail}" = xyes && test x"${enable_edwin}" = xno; then
    enable_edwin=${enable_default_plugins}
fi

AC_ARG_ENABLE([blowfish],
    AS_HELP_STRING([--enable-blowfish],
	[Add support for the Blowfish encryption [[no]]]))
: ${enable_blowfish=no}

AC_ARG_ENABLE([gdbm],
    AS_HELP_STRING([--enable-gdbm],
	[Add support for the gdbm support [[no]]]))
: ${enable_gdbm=no}

AC_ARG_ENABLE([pgsql],
    AS_HELP_STRING([--enable-pgsql],
	[Add support for the PostgreSQL support [[no]]]))
: ${enable_pgsql=no}

AC_ARG_ENABLE([mcrypt],
    AS_HELP_STRING([--enable-mcrypt],
	[Add support for the mcrypt support [[no]]]))
: ${enable_mcrypt=no}

AC_CANONICAL_HOST

ALL_TARGET=all-native
INSTALL_COM='$(INSTALL_DATA)'
INSTALL_LIARC_BUNDLES=
IF_LIARC='#!liarc: '
IF_SVM='#!svm: '

MIT_SCHEME_NATIVE_CODE([${enable_native_code}],[${host_cpu}])
case ${mit_scheme_native_code} in
c)
    INSTALL_COM=:
    INSTALL_LIARC_BUNDLES=install-liarc-bundles
    IF_LIARC=
    ;;
svm1)
    IF_SVM=
    ;;
esac

MIT_SCHEME_COMPILER_TARGET([${with_compiler_target}])
if test x"${mit_scheme_compiler_target}" = xsvm1; then
    IF_SVM_COMPILER=
else
    IF_SVM_COMPILER='#!svm compiler: '
fi

AUXDIR_NAME=mit-scheme-${mit_scheme_native_code}
AUXDIR='$(libdir)'/${AUXDIR_NAME}
MIT_SCHEME_LIBDIR=${AUXDIR}/
MIT_SCHEME_INCLUDEDIR=`pwd`/lib/
MIT_SCHEME_OS_SUFFIX=unx
MIT_SCHEME_CC_TYPE=${mit_scheme_native_code}

# Please keep the following messages synchronized with the messages in
# these files:
#
#   src/Setup.sh
#   src/configure.ac
#   src/etc/make-native.sh

if test x"${IS_NATIVE_RELEASE}" != x; then
    MIT_SCHEME_EXE=no-mit-scheme-needed
elif test x"${with_scheme_build}" != xdefault; then
    AC_MSG_CHECKING([for specified MIT/GNU Scheme build])
    HOST_BUILD_DIR=${with_scheme_build}
    case ${HOST_BUILD_DIR} in
    /*)
        ;;
    *)
        HOST_BUILD_DIR=`pwd`/${HOST_BUILD_DIR}
        ;;
    esac
    case ${HOST_BUILD_DIR} in
    */)
        ;;
    *)
        HOST_BUILD_DIR=${HOST_BUILD_DIR}/
        ;;
    esac
    MIT_SCHEME_EXE=${HOST_BUILD_DIR}run-build
    if "${MIT_SCHEME_EXE}" --batch-mode --no-init-file --eval '(%exit)' >/dev/null 2>&1; then
        AC_MSG_RESULT([yes, using ${MIT_SCHEME_EXE}])
    else
        AC_MSG_ERROR([no, unable to use ${MIT_SCHEME_EXE}])
    fi
    # To get some version numbers into the build log:
    ${MIT_SCHEME_EXE} --no-init-file --version
else
    AC_MSG_CHECKING([for an existing MIT/GNU Scheme installation])
    # Kinda hairy logic to detect this: (1) Use MIT_SCHEME_EXE, and
    # don't try to detect anything automagically, if it's set -- this
    # will make broken scripts that set MIT_SCHEME_EXE brokenly fail
    # early.  (2) Otherwise, use mit-scheme-ARCH if we can, because
    # it's guaranteed to have the right compiler.  (3) Try mit-scheme
    # if that fails.  (Will mit-scheme and not mit-scheme-ARCH ever be
    # the right thing?)
    native_exe=mit-scheme-${mit_scheme_native_code}
    found_p=no
    env_p=no
    if test x"${enable_host_scheme_test}" = xno; then
        : ${MIT_SCHEME_EXE=${native_exe}}
        found_p=yes
    elif test x"${MIT_SCHEME_EXE}" != x; then
        env_p=yes
        if "${MIT_SCHEME_EXE}" --batch-mode --no-init-file --eval '(%exit)' >/dev/null 2>&1;
        then
            found_p=yes
        fi
    elif "${native_exe}" --batch-mode --no-init-file --eval '(%exit)' >/dev/null 2>&1; then
        MIT_SCHEME_EXE=${native_exe}
        found_p=yes
    elif mit-scheme --batch-mode --no-init-file --eval '(%exit)' >/dev/null 2>&1; then
        MIT_SCHEME_EXE=mit-scheme
        found_p=yes
    fi
    if test x"${found_p}" = xyes; then
        AC_MSG_RESULT([yes, using ${MIT_SCHEME_EXE}])
        if test x"${enable_host_scheme_test}" != xno; then
            # To get some version numbers into the build log:
            ${MIT_SCHEME_EXE} --version --no-init-file
        fi
    else
        message=
        if test x"${env_p}" = xyes; then
            message="the MIT_SCHEME_EXE environment variable does not run it:

   MIT_SCHEME_EXE=${MIT_SCHEME_EXE}"
        else
            message="the following programs do not run it:

   ${native_exe}
   mit-scheme"
        fi
        AC_MSG_ERROR([
This script needs an existing MIT/GNU Scheme installation to function,
but ${message}

If you have installed MIT/GNU Scheme in an unusual location, set the
environment variable MIT_SCHEME_EXE to the name or pathname of the
MIT/GNU Scheme executable, which is usually \`mit-scheme' or
\`/usr/local/bin/mit-scheme', and set the environment variable
MITSCHEME_LIBRARY_PATH to the pathname of the MIT/GNU Scheme library
directory, which is usually \`/usr/local/lib/mit-scheme-${mit_scheme_native_code}'.
])
    fi
fi

# A 32bit host running LIAR/svm needs a large heap.
if test x"${mit_scheme_native_code}" = xsvm1; then
    AC_CHECK_SIZEOF([unsigned long])
    if test ${ac_cv_sizeof_unsigned_long} -eq 4; then
	HOST_COMPILER_HEAP="--heap 10000"
    fi
fi

if test x"${enable_x11}" = x && test x"${enable_default_plugins}" = xyes; then
    AC_PATH_X
    if test x"${no_x}" = xyes; then
	: ${enable_x11=no}
    else
	: ${enable_x11=yes}
    fi
fi

if test x"${enable_x11}" = xyes && test x"${enable_edwin}" = xyes; then
    enable_x11_screen=yes
else
    enable_x11_screen=no
fi

OPTION_SUBDIRS=
AC_MSG_CHECKING([configuring blowfish plugin])
if test x"${enable_blowfish}" = xyes; then
    OPTION_SUBDIRS="${OPTION_SUBDIRS} blowfish"
    IF_BLOWFISH=
    AC_MSG_RESULT([yes])
else
    IF_BLOWFISH="#!blowfish: "
    AC_MSG_RESULT([no])
fi
AC_MSG_CHECKING([configuring edwin plugin])
if test x"${enable_edwin}" = xyes; then
    OPTION_SUBDIRS="${OPTION_SUBDIRS} edwin"
    IF_EDWIN=
    AC_MSG_RESULT([yes])
else
    IF_EDWIN="#!edwin: "
    AC_MSG_RESULT([no])
fi
AC_MSG_CHECKING([configuring gdbm plugin])
if test x"${enable_gdbm}" = xyes; then
    OPTION_SUBDIRS="${OPTION_SUBDIRS} gdbm"
    IF_GDBM=
    AC_MSG_RESULT([yes])
else
    IF_GDBM="#!gdbm: "
    AC_MSG_RESULT([no])
fi
AC_MSG_CHECKING([configuring imail plugin])
if test x"${enable_imail}" = xyes; then
    OPTION_SUBDIRS="${OPTION_SUBDIRS} imail"
    IF_IMAIL=
    AC_MSG_RESULT([yes])
else
    IF_IMAIL="#!imail: "
    AC_MSG_RESULT([no])
fi
AC_MSG_CHECKING([configuring mcrypt plugin])
if test x"${enable_mcrypt}" = xyes; then
    OPTION_SUBDIRS="${OPTION_SUBDIRS} mcrypt"
    IF_MCRYPT=
    AC_MSG_RESULT([yes])
else
    IF_MCRYPT="#!mcrypt: "
    AC_MSG_RESULT([no])
fi
AC_MSG_CHECKING([configuring pgsql plugin])
if test x"${enable_pgsql}" = xyes; then
    OPTION_SUBDIRS="${OPTION_SUBDIRS} pgsql"
    IF_PGSQL=
    AC_MSG_RESULT([yes])
else
    IF_PGSQL="#!pgsql: "
    AC_MSG_RESULT([no])
fi
AC_MSG_CHECKING([configuring x11 plugin])
if test x"${enable_x11}" = xyes; then
    OPTION_SUBDIRS="${OPTION_SUBDIRS} x11"
    IF_X11=
    AC_MSG_RESULT([yes])
else
    IF_X11="#!x11: "
    AC_MSG_RESULT([no])
fi
AC_MSG_CHECKING([configuring x11-screen plugin])
if test x"${enable_x11_screen}" = xyes; then
    OPTION_SUBDIRS="${OPTION_SUBDIRS} x11-screen"
    IF_X11_SCREEN=
    AC_MSG_RESULT([yes])
else
    IF_X11_SCREEN="#!x11-screen: "
    AC_MSG_RESULT([no])
fi

AC_SUBST([ALL_TARGET])
AC_SUBST([AUXDIR])
AC_SUBST([AUXDIR_NAME])
AC_SUBST([DEFAULT_TARGET])
AC_SUBST([HOST_COMPILER_HEAP])
AC_SUBST([IF_BLOWFISH])
AC_SUBST([IF_CROSS])
AC_SUBST([IF_EDWIN])
AC_SUBST([IF_GDBM])
AC_SUBST([IF_IMAIL])
AC_SUBST([IF_LIARC])
AC_SUBST([IF_MCRYPT])
AC_SUBST([IF_PGSQL])
AC_SUBST([IF_SVM])
AC_SUBST([IF_SVM_COMPILER])
AC_SUBST([IF_X11])
AC_SUBST([IF_X11_SCREEN])
AC_SUBST([INSTALL_COM])
AC_SUBST([INSTALL_LIARC_BUNDLES])
AC_SUBST([MIT_SCHEME_EXE])
AC_SUBST([OPTION_SUBDIRS])

AC_PROG_INSTALL
AC_PROG_LN_S

echo etc/create-makefiles.sh "${MIT_SCHEME_EXE}" "${mit_scheme_compiler_target}"
etc/create-makefiles.sh "${MIT_SCHEME_EXE}" "${mit_scheme_compiler_target}" \
  || exit $?
compiler/configure "${mit_scheme_compiler_target}" || exit $?

# configure scripts for subdirs care about these
export IS_NATIVE_RELEASE
export MIT_SCHEME_EXE
export MIT_SCHEME_LIBDIR
export MIT_SCHEME_INCLUDEDIR
export MIT_SCHEME_OS_SUFFIX
export MIT_SCHEME_CC_TYPE

AC_CONFIG_SUBDIRS([microcode])
if test x"${enable_blowfish}" = xyes; then
    AC_CONFIG_SUBDIRS([blowfish])
fi
if test x"${enable_edwin}" = xyes; then
    AC_CONFIG_SUBDIRS([edwin])
fi
if test x"${enable_gdbm}" = xyes; then
    AC_CONFIG_SUBDIRS([gdbm])
fi
if test x"${enable_imail}" = xyes; then
    AC_CONFIG_SUBDIRS([imail])
fi
if test x"${enable_mcrypt}" = xyes; then
    AC_CONFIG_SUBDIRS([mcrypt])
fi
if test x"${enable_pgsql}" = xyes; then
    AC_CONFIG_SUBDIRS([pgsql])
fi
if test x"${enable_x11}" = xyes; then
    AC_CONFIG_SUBDIRS([x11])
fi
if test x"${enable_x11_screen}" = xyes; then
    AC_CONFIG_SUBDIRS([x11-screen])
fi

AC_CONFIG_FILES([
Makefile
6001/Makefile
compiler/Makefile
cref/Makefile
ffi/Makefile
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

if test x"${mit_scheme_compiler_target}" = xc; then
    BN=liarc-cc
    (cd lib; rm -f ${BN}; ${LN_S} ../microcode/${BN} .)
    BN=liarc-ld
    (cd lib; rm -f ${BN}; ${LN_S} ../microcode/${BN} .)
fi

if test x"${mit_scheme_native_code}" = xc; then
    for BN in star-parser; do
        (cd lib; rm -f ${BN}; ${LN_S} ../${BN} .)
    done
    mkdir lib/lib
    for BUNDLE in 6001 compiler cref ffi sf sos ssp star-parser \
		  xdoc xml; do
    	SO=${BUNDLE}.so
	(cd lib/lib; rm -f ${SO}; ${LN_S} ../../${BUNDLE}/${SO} .)
    done
fi
