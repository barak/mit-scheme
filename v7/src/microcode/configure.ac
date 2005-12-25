dnl Process this file with autoconf to produce a configure script.

AC_INIT([MIT/GNU Scheme], [14.16], [bug-mit-scheme@gnu.org], [mit-scheme])
AC_REVISION([$Id: configure.ac,v 1.7 2005/12/25 05:59:46 riastradh Exp $])
AC_CONFIG_SRCDIR([boot.c])
AC_CONFIG_HEADERS([config.h])
AC_PROG_MAKE_SET

AC_COPYRIGHT(
[Copyright 2000,2002,2003,2004,2005 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
])

AH_TOP([/*

Copyright 2000,2002,2003,2004,2005 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

#ifndef SCM_CONFIG_H
#define SCM_CONFIG_H])

AH_BOTTOM(
[#ifndef __unix__
#  define __unix__
#endif

#if defined(_IRIX) || defined(_IRIX4) || defined(_IRIX6)
#  define __IRIX__
#endif

#if defined(__hpux) || defined(hpux)
#  define __HPUX__
#endif

/* If we're running under GNU libc, turn on all the features.
   Otherwise this should be harmless.  */
#define _GNU_SOURCE

#include <sys/types.h>

#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    include <time.h>
#  endif
#endif

#ifdef HAVE_TERMIOS_H
#  include <termios.h>
#else
#  ifdef HAVE_TERMIO_H
#    include <termio.h>
#  endif
#endif

#ifdef HAVE_SYS_MMAN_H
#  include <sys/mman.h>
#endif

#ifdef HAVE_MMAP
#  if defined(MAP_ANON) && !defined(MAP_ANONYMOUS)
#    define MAP_ANONYMOUS MAP_ANON
#  endif
#  ifdef MAP_ANONYMOUS
#    define USE_MMAP_HEAP_MALLOC
#  endif
#endif

/* Include the shared configuration header.  */
#include "confshared.h"

#endif /* SCM_CONFIG_H */])

dnl Feature options.
AC_ARG_ENABLE([static-libs],
[[  --enable-static-libs    Link some libraries statically [no]]])
AC_ARG_ENABLE([dynamic-modules],
[[  --enable-dynamic-modules Link some primitives dynamically [yes]]])
AC_ARG_ENABLE([valgrind-mode],
[[  --enable-valgrind-mode  Support running under valgrind [no]]])
AC_ARG_WITH([openssl],
[[  --with-openssl          Use OpenSSL crypto library if available [yes]]])
AC_ARG_WITH([mhash],
[[  --with-mhash            Use mhash library if available [yes]]])
AC_ARG_WITH([mcrypt],
[[  --with-mcrypt           Use mcrypt library if available [yes]]])
AC_ARG_WITH([gdbm],
[[  --with-gdbm             Use gdbm library if available [yes]]])
AC_ARG_WITH([db4],
[[  --with-db4              Use Berkeley DB v4 library if available [yes]]])
AC_ARG_WITH([libpq],
[[  --with-libpq            Use PostgreSQL libpq library if available [yes]]])

dnl Substitution variables to be filled in below.
AS_FLAGS=
GC_HEAD_FILES=
LIB_X11=
M4_FLAGS=
OPTIONAL_BASES=
OPTIONAL_OBJECTS=
OPTIONAL_SOURCES=
STATIC_LIBS=
QUASI_STATIC_LIBS=
STATIC_PREFIX=
STATIC_SUFFIX=
PRBFISH_LIBS=
PRMD5_LIBS=
SCHEME_DEFS=
DLD_LDFLAGS=
MODULE_BASES=
MODULE_CFLAGS="-DCOMPILE_AS_MODULE"
MODULE_LDFLAGS=

dnl Checks for programs.
AC_PROG_CC
AC_PROG_GCC_TRADITIONAL
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
if test "${GCC}" = "yes"; then
    CFLAGS="${CFLAGS} -Wall"
fi
if test "$(${INSTALL} --help 2> /dev/null | fgrep -e --preserve-timestamps)"
then
    INSTALL="${INSTALL} --preserve-timestamps"
fi

dnl Checks for libraries.
AC_CHECK_LIB([dl], [dlopen])
AC_CHECK_LIB([m], [exp])

AC_CHECK_LIB([crypto], [BF_set_key],
    [scheme_cv_lib_crypto=yes],
    [scheme_cv_lib_crypto=no])
AC_CHECK_LIB([mhash], [mhash_count],
    [scheme_cv_lib_mhash=yes],
    [scheme_cv_lib_mhash=no])
AC_CHECK_LIB([mcrypt], [mcrypt_generic_init],
    [scheme_cv_lib_mcrypt=yes],
    [scheme_cv_lib_mcrypt=no],
    [-lltdl])
AC_CHECK_LIB([blowfish], [BF_set_key],
    [scheme_cv_lib_blowfish=yes],
    [scheme_cv_lib_blowfish=no])
AC_CHECK_LIB([md5], [MD5Init],
    [scheme_cv_lib_md5=yes],
    [scheme_cv_lib_md5=no])
AC_CHECK_LIB([gdbm], [gdbm_open],
    [scheme_cv_lib_gdbm=yes],
    [scheme_cv_lib_gdbm=no])
AC_CHECK_LIB([ncurses], [tparm],
    [scheme_cv_lib_ncurses=yes],
    [scheme_cv_lib_ncurses=no])
AC_CHECK_LIB([ncurses], [tparam],
    [scheme_cv_lib_ncurses_defines_tparam=yes],
    [scheme_cv_lib_ncurses_defines_tparam=no])
AC_CHECK_LIB([curses], [tparm],
    [scheme_cv_lib_curses=yes],
    [scheme_cv_lib_curses=no])
AC_CHECK_LIB([termcap], [tparam],
    [scheme_cv_lib_termcap=yes],
    [scheme_cv_lib_termcap=no])
AC_CHECK_LIB([pq], [PQconnectdb],
    [scheme_cv_lib_pq=yes],
    [scheme_cv_lib_pq=no])

AC_MSG_CHECKING([for db_create in -ldb-4])
save_LIBS="$LIBS"
LIBS="-ldb-4 $LIBS"
AC_TRY_LINK(
    [#include <db.h>],
    [DB * db; db_create ((&db), 0, 0);],
    [scheme_cv_lib_db4=yes],
    [scheme_cv_lib_db4=no])
LIBS="$save_LIBS"
AC_MSG_RESULT(${scheme_cv_lib_db4})

if test "${scheme_cv_lib_pq}" = "yes"; then
    PQ_INCLUDE=
    if test -x "/usr/bin/pg_config"; then
	PQ_INCLUDE="$(/usr/bin/pg_config --includedir 2> /dev/null)"
    elif test -d "/usr/include/postgresql"; then
	PQ_INCLUDE="/usr/include/postgresql"
    fi
    if test "${PQ_INCLUDE}"; then
	CPPFLAGS="${CPPFLAGS} -I${PQ_INCLUDE}"
    fi
fi

dnl Checks for header files.
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_HEADER_TIME
AC_CHECK_HEADERS([bsdtty.h fcntl.h limits.h malloc.h sgtty.h stropts.h time.h])
AC_CHECK_HEADERS([sys/file.h sys/ioctl.h sys/mount.h sys/param.h sys/poll.h])
AC_CHECK_HEADERS([sys/ptyio.h sys/socket.h sys/time.h sys/un.h sys/vfs.h])
AC_CHECK_HEADERS([stdbool.h termio.h termios.h unistd.h utime.h])
AC_CHECK_HEADERS([openssl/blowfish.h openssl/md5.h blowfish.h md5.h])
AC_CHECK_HEADERS([mhash.h mcrypt.h gdbm.h db.h curses.h term.h dlfcn.h])
AC_CHECK_HEADERS([sys/mman.h libpq-fe.h])

dnl Checks for typedefs
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIGNAL
AC_TYPE_SIZE_T
AC_TYPE_UID_T
AC_CHECK_TYPE([nlink_t], [short])

AC_MSG_CHECKING([for clock_t])
AC_TRY_COMPILE([
#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    include <time.h>
#  endif
#endif],
    [clock_t x;],
    [scheme_cv_type_clock_t=yes],
    [scheme_cv_type_clock_t=no])
AC_MSG_RESULT(${scheme_cv_type_clock_t})
if test "${scheme_cv_type_clock_t}" = "no"; then
    AC_DEFINE([clock_t], [unsigned long],
	[Define to `unsigned long' if <time.h> doesn't define.])
fi

AC_MSG_CHECKING([for time_t])
AC_TRY_COMPILE([
#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    include <time.h>
#  endif
#endif],
    [time_t x;],
    [scheme_cv_type_time_t=yes],
    [scheme_cv_type_time_t=no])
AC_MSG_RESULT(${scheme_cv_type_time_t})
if test "${scheme_cv_type_time_t}" = "no"; then
    AC_DEFINE([time_t], [long],
	[Define to `long' if <time.h> doesn't define.])
fi

if test "${ac_cv_header_sys_socket_h}" = "yes"; then
    AC_MSG_CHECKING([for socklen_t])
    AC_TRY_COMPILE([
#include <sys/types.h>
#include <sys/socket.h>],
	[socklen_t x;],
	[scheme_cv_type_socklen_t=yes],
	[scheme_cv_type_socklen_t=no])
    AC_MSG_RESULT(${scheme_cv_type_socklen_t})
    if test "${scheme_cv_type_socklen_t}" = "no"; then
	AC_DEFINE([socklen_t], [int],
	    [Define to `int' if <sys/socket.h> doesn't define.])
    fi
fi

AC_MSG_CHECKING([for cc_t])
AC_TRY_COMPILE([
#ifdef HAVE_TERMIOS_H
#  include <termios.h>
#else
#  ifdef HAVE_TERMIO_H
#    include <termio.h>
#  endif
#endif],
	[cc_t x;],
	[scheme_cv_type_cc_t=yes],
	[scheme_cv_type_cc_t=no])
AC_MSG_RESULT(${scheme_cv_type_cc_t})
if test "${scheme_cv_type_cc_t}" = "no"; then
    AC_DEFINE([cc_t], [unsigned char],
	[Define to `unsigned char' if <termios.h> doesn't define.])
fi

AC_MSG_CHECKING([for speed_t])
AC_TRY_COMPILE([
#ifdef HAVE_TERMIOS_H
#  include <termios.h>
#else
#  ifdef HAVE_TERMIO_H
#    include <termio.h>
#  endif
#endif],
	[speed_t x;],
	[scheme_cv_type_speed_t=yes],
	[scheme_cv_type_speed_t=no])
AC_MSG_RESULT(${scheme_cv_type_speed_t})
if test "${scheme_cv_type_speed_t}" = "no"; then
    AC_DEFINE([speed_t], [short],
	[Define to `short' if <termios.h> doesn't define.])
fi

dnl Checks for structures.
AC_STRUCT_TM
AC_STRUCT_TIMEZONE


dnl Timezone stuff is hairy; there's no standard.
have_timezone_info=no

if test "${have_timezone_info}" = "no"; then
    AC_MSG_CHECKING([for tm_gmtoff in struct tm])
    AC_TRY_LINK(
	[#include <time.h>],
	[struct tm t; t.tm_gmtoff],
	[scheme_cv_struct_tm_gmtoff=yes],
	[scheme_cv_struct_tm_gmtoff=no])
    AC_MSG_RESULT(${scheme_cv_struct_tm_gmtoff})
    if test "${scheme_cv_struct_tm_gmtoff}" = "yes"; then
	AC_DEFINE([HAVE_TM_GMTOFF], [1],
	    [Define if `struct tm' has the `tm_gmtoff' member.])
	AC_DEFINE([TM_GMTOFF], [tm_gmtoff],
	    [Define to name of `tm_gmtoff' member if available.])
	have_timezone_info=yes
    fi
fi

if test "${have_timezone_info}" = "no"; then
    AC_MSG_CHECKING([for __tm_gmtoff in struct tm])
    AC_TRY_LINK(
	[#include <time.h>],
	[struct tm t; t.__tm_gmtoff],
	[scheme_cv_struct___tm_gmtoff=yes],
	[scheme_cv_struct___tm_gmtoff=no])
    AC_MSG_RESULT(${scheme_cv_struct___tm_gmtoff})
    if test "${scheme_cv_struct___tm_gmtoff}" = "yes"; then
	AC_DEFINE([HAVE_TM_GMTOFF], [1],
	    [Define if `struct tm' has the `tm_gmtoff' member.])
	AC_DEFINE([TM_GMTOFF], [__tm_gmtoff],
	    [Define to name of `tm_gmtoff' member if available.])
	have_timezone_info=yes
    fi
fi

if test "${have_timezone_info}" = "no"; then
    AC_MSG_CHECKING([for __timezone])
    AC_TRY_LINK([
#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    include <time.h>
#  endif
#endif],
	[__timezone;],
	[scheme_cv_var___timezone=yes],
	[scheme_cv_var___timezone=no])
    AC_MSG_RESULT(${scheme_cv_var___timezone})
    if test "${scheme_cv_var___timezone}" = "yes"; then
	AC_DEFINE([HAVE_TIMEZONE], [1],
	    [Define if timezone variable is available.])
	AC_DEFINE([TIMEZONE], [__timezone],
	    [Define to name of timezone variable if available.])
	have_timezone_info=yes
    fi
fi

if test "${have_timezone_info}" = "no"; then
    AC_MSG_CHECKING([for _timezone])
    AC_TRY_LINK([
#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    include <time.h>
#  endif
#endif],
	[_timezone;],
	[scheme_cv_var__timezone=yes],
	[scheme_cv_var__timezone=no])
    AC_MSG_RESULT(${scheme_cv_var__timezone})
    if test "${scheme_cv_var__timezone}" = "yes"; then
	AC_DEFINE([HAVE_TIMEZONE], [1],
	    [Define if timezone variable is available.])
	AC_DEFINE([TIMEZONE], [_timezone],
	    [Define to name of timezone variable if available.])
	have_timezone_info=yes
    fi
fi

if test "${have_timezone_info}" = "no"; then
    AC_MSG_CHECKING([for timezone])
    AC_TRY_LINK([
#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    include <time.h>
#  endif
#endif],
	[timezone;],
	[scheme_cv_var_timezone=yes],
	[scheme_cv_var_timezone=no])
    AC_MSG_RESULT(${scheme_cv_var_timezone})
    if test "${scheme_cv_var_timezone}" = "yes"; then
	AC_DEFINE([HAVE_TIMEZONE], [1],
	    [Define if timezone variable is available.])
	AC_DEFINE([TIMEZONE], [timezone],
	    [Define to name of timezone variable if available.])
	have_timezone_info=yes
    fi
fi

if test "${ac_cv_header_bsdtty_h}" = "yes"; then
    AC_MSG_CHECKING([for struct ltchars])
    AC_TRY_COMPILE(
	[#include <bsdtty.h>],
	[struct ltchars x;],
	[scheme_cv_struct_ltchars=yes],
	[scheme_cv_struct_ltchars=no])
    AC_MSG_RESULT(${scheme_cv_struct_ltchars})
    if test "${scheme_cv_struct_ltchars}" = "yes"; then
	AC_DEFINE([HAVE_STRUCT_LTCHARS], [1],
	    [Define if `struct ltchars' is defined in <bsdtty.h>.])
    fi
fi

AC_MSG_CHECKING([for hostent h_addr_list])
AC_TRY_COMPILE(
    [#include <netdb.h>],
    [struct hostent x; x.h_addr_list;],
    [scheme_cv_struct_hostent_h_addr_list=yes],
    [scheme_cv_struct_hostent_h_addr_list=no])
AC_MSG_RESULT(${scheme_cv_struct_hostent_h_addr_list})
if test "${scheme_cv_struct_hostent_h_addr_list}" = "yes"; then
    AC_DEFINE([HAVE_HOSTENT_H_ADDR_LIST], [1],
	[Define if `struct hostent' has the `h_addr_list' member.])
fi

AC_MSG_CHECKING([for struct sigcontext])
AC_TRY_COMPILE(
    [#include <signal.h>],
    [struct sigcontext x;],
    [scheme_cv_struct_sigcontext=yes],
    [scheme_cv_struct_sigcontext=no])
AC_MSG_RESULT(${scheme_cv_struct_sigcontext})
if test "${scheme_cv_struct_sigcontext}" = "yes"; then
    AC_DEFINE([HAVE_STRUCT_SIGCONTEXT], [1],
	[Define if `struct sigcontext' is defined in <signal.h>.])
fi

dnl Checks for compiler characteristics.
AC_C_BIGENDIAN
AC_C_CHAR_UNSIGNED
AC_C_CONST
AC_CHECK_SIZEOF([unsigned long])

dnl Checks for C library functions.
AC_FUNC_GETPGRP
AC_FUNC_MEMCMP
AC_FUNC_MMAP
AC_FUNC_SETPGRP
AC_FUNC_SETVBUF_REVERSED
AC_FUNC_UTIME_NULL
AC_FUNC_VFORK
AC_FUNC_VPRINTF
AC_FUNC_WAIT3
AC_CHECK_FUNCS([ctermid])
AC_CHECK_FUNCS([dup2])
AC_CHECK_FUNCS([fcntl floor fpathconf frexp ftruncate])
AC_CHECK_FUNCS([getcwd gethostbyname gethostname getlogin getpagesize getpgrp])
AC_CHECK_FUNCS([getpt gettimeofday getwd grantpt])
AC_CHECK_FUNCS([kill])
AC_CHECK_FUNCS([lockf])
AC_CHECK_FUNCS([memcpy mkdir mktime modf])
AC_CHECK_FUNCS([nice])
AC_CHECK_FUNCS([poll prealloc])
AC_CHECK_FUNCS([rename rmdir])
AC_CHECK_FUNCS([select setitimer setpgrp setpgrp2 shmat sigaction])
AC_CHECK_FUNCS([sighold socket statfs strchr strerror strstr strtol])
AC_CHECK_FUNCS([strtoul symlink sysconf])
AC_CHECK_FUNCS([times truncate])
AC_CHECK_FUNCS([uname utime])
AC_CHECK_FUNCS([waitpid])

if test "${ac_cv_type_signal}" = "void"; then
    AC_DEFINE([VOID_SIGNAL_HANDLERS], [1],
	[Define if RETSIGTYPE is `void'.])
fi

dnl Checks for system characteristics.
AC_CANONICAL_HOST
AC_PATH_XTRA
AC_SYS_LONG_FILE_NAMES
AC_SYS_RESTARTABLE_SYSCALLS

if test "${ac_cv_lib_dl_dlopen}" = "yes"; then
    OPTIONAL_BASES="${OPTIONAL_BASES} pruxdld"
    enable_dynamic_modules="${enable_dynamic_modules:-yes}"
else
    enable_dynamic_modules="no"
fi

dnl Handle options to enable/disable use of some libraries.
test "${with_openssl:-yes}" = "yes" || scheme_cv_lib_crypto="no"
test "${with_mhash:-yes}" = "yes" || scheme_cv_lib_mhash="no"
test "${with_mcrypt:-yes}" = "yes" || scheme_cv_lib_mcrypt="no"
test "${with_gdbm:-yes}" = "yes" || scheme_cv_lib_gdbm="no"
test "${with_db4:-yes}" = "yes" || scheme_cv_lib_db4="no"
test "${with_libpq:-yes}" = "yes" || scheme_cv_lib_pq="no"

dnl OK, now some complex logic to handle the crypto stuff.

dnl Next, we decide which libraries to use.  We mostly want support
dnl for Blowfish and MD5, each of which can come from multiple libraries.
HAVE_BLOWFISH="no"
HAVE_MD5="no"

dnl The OpenSSL crypto library provides support for both, and is preferred.
if test "${scheme_cv_lib_crypto}" = "yes"; then
    AC_DEFINE([HAVE_LIBCRYPTO], [1],
	[Define if OpenSSL crypto library is present.])
    if test "${ac_cv_header_openssl_blowfish_h}" = "yes" \
	    || test "${ac_cv_header_blowfish_h}" = "yes"; then
	if test "${enable_dynamic_modules}" = "yes"; then
	    MODULE_BASES="${MODULE_BASES} prbfish"
	    PRBFISH_LIBS="-lcrypto"
	else
	    OPTIONAL_BASES="${OPTIONAL_BASES} prbfish"
	fi
	HAVE_BLOWFISH="yes"
    fi
    if test "${ac_cv_header_openssl_md5_h}" = "yes" \
	    || test "${ac_cv_header_md5_h}" = "yes"; then
	if test "${enable_dynamic_modules}" = "yes"; then
	    MODULE_BASES="${MODULE_BASES} prmd5"
	    PRMD5_LIBS="-lcrypto"
	else
	    OPTIONAL_BASES="${OPTIONAL_BASES} prmd5"
	fi
	HAVE_MD5="yes"
    fi
    if test "${enable_dynamic_modules}" != "yes"; then
	if test "${HAVE_BLOWFISH}" = "yes" || test "${HAVE_MD5}" = "yes"; then
	    QUASI_STATIC_LIBS="${QUASI_STATIC_LIBS} -lcrypto"
	fi
    fi
fi

dnl The mhash library provides md5 support.  It can be loaded in addition
dnl to other MD5 libraries and provides a rich set of hashes.
if test "${scheme_cv_lib_mhash}" = "yes"; then
    AC_DEFINE([HAVE_LIBMHASH], [1],
	[Define if mhash library is present.])
    if test "${ac_cv_header_mhash_h}" = "yes"; then
	if test "${enable_dynamic_modules}" = "yes"; then
	    MODULE_BASES="${MODULE_BASES} prmhash"
	else
	    QUASI_STATIC_LIBS="${QUASI_STATIC_LIBS} -lmhash"
	    OPTIONAL_BASES="${OPTIONAL_BASES} prmhash"
	fi
	HAVE_MD5="yes"
    fi
fi

dnl The mcrypt library provides blowfish, but its CFB mode is 8 bit.
dnl We have been using 64-bit CFB, so this isn't really compatible.
dnl But mcrypt provides many ciphers and can be loaded in addition.
if test "${scheme_cv_lib_mcrypt}" = "yes"; then
    AC_DEFINE([HAVE_LIBMCRYPT], [1],
	[Define if mcrypt library is present.])
    if test "${ac_cv_header_mcrypt_h}" = "yes"; then
	if test "${enable_dynamic_modules}" = "yes"; then
	    MODULE_BASES="${MODULE_BASES} prmcrypt"
	else
	    QUASI_STATIC_LIBS="${QUASI_STATIC_LIBS} -lmcrypt -lltdl"
	    OPTIONAL_BASES="${OPTIONAL_BASES} prmcrypt"
	fi
    fi
fi

dnl This is a stripped-down library that provides just Blowfish.  It
dnl seems to be code that was extracted from OpenSSL long ago.  We
dnl used this for a long time but no longer do.
if test "${HAVE_BLOWFISH}" = "no"; then
    if test "${scheme_cv_lib_blowfish}" = "yes"; then
	AC_DEFINE([HAVE_LIBBLOWFISH], [1],
	    [Define if blowfish library is present.])
	if test "${ac_cv_header_blowfish_h}" = "yes"; then
	    if test "${enable_dynamic_modules}" = "yes"; then
		MODULE_BASES="${MODULE_BASES} prbfish"
		PRBFISH_LIBS="-lblowfish"
	    else
		STATIC_LIBS="${STATIC_LIBS} -lblowfish"
		OPTIONAL_BASES="${OPTIONAL_BASES} prbfish"
	    fi
	    HAVE_BLOWFISH="yes"
	fi
    fi
fi

dnl This is Phil Karn's MD5 implementation, which seems to have nearly
dnl the same interface as that of OpenSSL.
if test "${HAVE_MD5}" = "no"; then
    if test "${scheme_cv_lib_md5}" = "yes"; then
	AC_DEFINE([HAVE_LIBMD5], [1],
	    [Define if md5 library is present.])
	if test "${ac_cv_header_md5_h}" = "yes"; then
	    if test "${enable_dynamic_modules}" = "yes"; then
		MODULE_BASES="${MODULE_BASES} prmd5"
		PRMD5_LIBS="-lmd5"
	    else
		STATIC_LIBS="${STATIC_LIBS} -lmd5"
		OPTIONAL_BASES="${OPTIONAL_BASES} prmd5"
	    fi
	    HAVE_MD5="yes"
	fi
    fi
fi

dnl gdbm support.
if test "${scheme_cv_lib_gdbm}" = "yes"; then
    AC_DEFINE([HAVE_LIBGDBM], [1],
	[Define if gdbm library is present.])
    if test "${ac_cv_header_gdbm_h}" = "yes"; then
	if test "${enable_dynamic_modules}" = "yes"; then
	    MODULE_BASES="${MODULE_BASES} prgdbm"
	else
	    QUASI_STATIC_LIBS="${QUASI_STATIC_LIBS} -lgdbm"
	    OPTIONAL_BASES="${OPTIONAL_BASES} prgdbm"
	fi
    fi
fi

dnl db4 support.
if test "${scheme_cv_lib_db4}" = "yes"; then
    AC_DEFINE([HAVE_LIBDB4], [1],
	[Define if db4 library is present.])
    if test "${ac_cv_header_db_h}" = "yes"; then
	if test "${enable_dynamic_modules}" = "yes"; then
	    MODULE_BASES="${MODULE_BASES} prdb4"
	else
	    QUASI_STATIC_LIBS="${QUASI_STATIC_LIBS} -ldb-4"
	    OPTIONAL_BASES="${OPTIONAL_BASES} prdb4"
	fi
    fi
fi

dnl curses/termcap/terminfo support.
if test "${scheme_cv_lib_ncurses}" = "yes"; then
    AC_DEFINE([HAVE_LIBNCURSES], [1],
	[Define if ncurses library is present.])
    if test "${ac_cv_header_curses_h}" = "yes" \
	&& test "${ac_cv_header_term_h}" = "yes"; then
	QUASI_STATIC_LIBS="${QUASI_STATIC_LIBS} -lncurses"
	if test "${scheme_cv_lib_ncurses_defines_tparam}" = "yes"; then
	    AC_DEFINE([LIBNCURSES_DEFINES_TPARAM], [1],
		[Define if ncurses library defines `tparam'.])
	else
	    OPTIONAL_BASES="${OPTIONAL_BASES} terminfo"
	fi
    fi
else
    if test "${scheme_cv_lib_curses}" = "yes"; then
	AC_DEFINE([HAVE_LIBCURSES], [1],
	    [Define if curses library is present.])
	QUASI_STATIC_LIBS="${QUASI_STATIC_LIBS} -lcurses"
	OPTIONAL_BASES="${OPTIONAL_BASES} terminfo"
    else
	if test "${scheme_cv_lib_termcap}" = "yes"; then
	    AC_DEFINE([HAVE_LIBTERMCAP], [1],
		[Define if termcap library is present.])
	    QUASI_STATIC_LIBS="${QUASI_STATIC_LIBS} -ltermcap"
	    OPTIONAL_BASES="${OPTIONAL_BASES} tparam"
	else
	    OPTIONAL_BASES="${OPTIONAL_BASES} termcap tparam"
	fi
    fi
fi

dnl PostgreSQL support.
if test "${scheme_cv_lib_pq}" = "yes"; then
    AC_DEFINE([HAVE_LIBPQ], [1],
	[Define if PostgreSQL libpq library is present.])
    if test "${ac_cv_header_libpq_fe_h}" = "yes"; then
	if test "${enable_dynamic_modules}" = "yes"; then
	    MODULE_BASES="${MODULE_BASES} prpgsql"
	else
	    QUASI_STATIC_LIBS="${QUASI_STATIC_LIBS} -lpq"
	    OPTIONAL_BASES="${OPTIONAL_BASES} prpgsql"
	fi
    fi
fi

dnl Decide whether we're using static or dynamic libraries.
if test "${enable_static_libs:-no}" = "no"; then
    LIBS="${LIBS}${QUASI_STATIC_LIBS}"
else
    STATIC_LIBS="${STATIC_LIBS}${QUASI_STATIC_LIBS}"
fi

if test "${enable_valgrind_mode:-no}" != "no"; then
    SCHEME_DEFS="${SCHEME_DEFS} -DVALGRIND_MODE"
    M4_FLAGS="${M4_FLAGS} -P VALGRIND_MODE,1"
fi

dnl Add support for X if present.
if test "${no_x}" = "yes"; then
    LIB_X11=
else
    LIB_X11=-lX11
    OPTIONAL_BASES="${OPTIONAL_BASES} x11base x11term x11graph x11color"
fi

dnl Add OS-dependent customizations.
DO_GCC_TESTS="no"
case "$host_os" in
linux-gnu)
    DO_GCC_TESTS="yes"
    ;;
freebsd*)
    M4_FLAGS="${M4_FLAGS} -P SUPPRESS_LEADING_UNDERSCORE,1"
    ;;
solaris*)
    if test "${GCC}" = "yes"; then
	DO_GCC_TESTS="yes"
	M4_FLAGS="${M4_FLAGS} -P SUPPRESS_LEADING_UNDERSCORE,1"
    fi
    ;;
esac

if test "${DO_GCC_TESTS}" = "yes"; then
    if test "${STATIC_LIBS}" != ""; then
	STATIC_PREFIX="-Xlinker -Bstatic"
	STATIC_SUFFIX="-Xlinker -Bdynamic"
    fi
    DLD_LDFLAGS="${DLD_LDFLAGS} -export-dynamic"
    MODULE_CFLAGS="${MODULE_CFLAGS} -fPIC"
    MODULE_LDFLAGS="${MODULE_LDFLAGS} -shared"
    AC_MSG_CHECKING([for ELF binaries])
    AC_TRY_RUN(
[int
main ()
{
#ifdef __ELF__
return 0;
#endif
return 1;
}],
	[scheme_cv_linux_elf=yes],
	[scheme_cv_linux_elf=no])
    AC_MSG_RESULT(${scheme_cv_linux_elf})
    if test "${scheme_cv_linux_elf}" = "yes"; then
	M4_FLAGS="${M4_FLAGS} -P SUPPRESS_LEADING_UNDERSCORE,1"
    fi
fi

dnl Add architecture-dependent customizations.
dnl This is mostly support for native-code compilation.
scheme_arch=
OPTIONAL_BASES="${OPTIONAL_BASES} cmpint"
GC_HEAD_FILES="gccode.h cmpgc.h"
case "$host_cpu" in
alpha*)
    scheme_arch=alpha
    ;;
hppa*)
    scheme_arch=hppa
    GC_HEAD_FILES="${GC_HEAD_FILES} hppacach.h"
    ;;
i?86)
    scheme_arch=i386
    ;;
m68k|m680?0)
    scheme_arch=mc68k
    ;;
mips*)
    scheme_arch=mips
    ;;
vax)
    scheme_arch=vax
    ;;
esac
if test "${scheme_arch}" != ""; then
    AC_DEFINE([HAS_COMPILER_SUPPORT], [1],
	[Define if architecture has native-code compiler support.])
    test -f cmpauxmd.m4 || ${LN_S} cmpauxmd/${scheme_arch}.m4 cmpauxmd.m4
    test -f cmpintmd.h || ${LN_S} cmpintmd/${scheme_arch}.h cmpintmd.h
    OPTIONAL_SOURCES="${OPTIONAL_SOURCES} cmpauxmd.m4"
    OPTIONAL_OBJECTS="${OPTIONAL_OBJECTS} cmpauxmd.o"
    GC_HEAD_FILES="${GC_HEAD_FILES} cmpintmd.h"
fi

for base in ${OPTIONAL_BASES}; do
    OPTIONAL_SOURCES="${OPTIONAL_SOURCES} ${base}.c"
    OPTIONAL_OBJECTS="${OPTIONAL_OBJECTS} ${base}.o"
done

MODULE_TARGETS=
if test "${MODULE_BASES}" = ""; then
    MODULE_RULES="/dev/null"
else
    MODULE_RULES="config.modrules"
    rm -f "${MODULE_RULES}"
    for BASE in ${MODULE_BASES}; do
	MODULE_TARGETS="${MODULE_TARGETS} ${BASE}.so"
	echo "" >> "${MODULE_RULES}"
	echo "${BASE}.o: ${BASE}.c" >> "${MODULE_RULES}"
	echo "	\$(COMPILE_MODULE) -c \$*.c" >> "${MODULE_RULES}"
    done
fi

AC_SUBST([AS_FLAGS])
AC_SUBST([GC_HEAD_FILES])
AC_SUBST([LIB_X11])
AC_SUBST([M4_FLAGS])
AC_SUBST([OPTIONAL_OBJECTS])
AC_SUBST([OPTIONAL_SOURCES])
AC_SUBST([STATIC_LIBS])
AC_SUBST([STATIC_PREFIX])
AC_SUBST([STATIC_SUFFIX])
AC_SUBST([PRBFISH_LIBS])
AC_SUBST([PRMD5_LIBS])
AC_SUBST([SCHEME_DEFS])
AC_SUBST([DLD_LDFLAGS])
AC_SUBST([MODULE_TARGETS])
AC_SUBST_FILE([MODULE_RULES])
AC_SUBST([MODULE_CFLAGS])
AC_SUBST([MODULE_LDFLAGS])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

# Make custom compilation program for "makegen.scm".
rm -f makegen-cc
echo "#!/bin/sh" > makegen-cc
echo "exec gcc -MM -DMIT_SCHEME ${CPPFLAGS} \"\${1}\"" >> makegen-cc
chmod +x makegen-cc

if test "${MODULE_RULES}" != "/dev/null"; then
    rm -f "${MODULE_RULES}"
fi
