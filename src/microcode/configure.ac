dnl Process this file with autoconf to produce a configure script.

AC_INIT([MIT/GNU Scheme microcode], [15.1], [bug-mit-scheme@gnu.org], [mit-scheme])
AC_REVISION([$Id: cde298a496b45b909d478d52614cdf4257817757 $])
AC_CONFIG_SRCDIR([boot.c])
AC_CONFIG_HEADERS([config.h])
AC_PROG_MAKE_SET

AC_COPYRIGHT(
[Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
    2006, 2007, 2008 Massachusetts Institute of Technology

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

dnl Feature options
AC_ARG_ENABLE([static-libs],
    AS_HELP_STRING([--enable-static-libs],
	[Link some libraries statically [[no]]]))
: ${enable_static_libs='no'}

AC_ARG_ENABLE([valgrind-mode],
    AS_HELP_STRING([--enable-valgrind-mode],
	[Support running under valgrind [[no]]]))
: ${enable_valgrind_mode='no'}

AC_ARG_ENABLE([debugging],
    AS_HELP_STRING([--enable-debugging],
	[Compile with debugging support [[no]]]))
: ${enable_debugging='no'}

AC_ARG_ENABLE([native-code],
    AS_HELP_STRING([--enable-native-code],
	[Support native compiled code if available [[yes]]]))
: ${enable_native_code='yes'}

AC_ARG_WITH([openssl],
    AS_HELP_STRING([--with-openssl],
	[Use OpenSSL crypto library if available [[yes]]]))
: ${with_openssl='yes'}

AC_ARG_WITH([mhash],
    AS_HELP_STRING([--with-mhash],
	[Use mhash library if available [[yes]]]))
: ${with_mhash='yes'}

AC_ARG_WITH([mcrypt],
    AS_HELP_STRING([--with-mcrypt],
	[Use mcrypt library if available [[yes]]]))
: ${with_mcrypt='yes'}

AC_ARG_WITH([gdbm],
    AS_HELP_STRING([--with-gdbm],
	[Use gdbm library if available [[yes]]]))
: ${with_gdbm='yes'}

AC_ARG_WITH([db-4],
    AS_HELP_STRING([--with-db-4],
	[Use Berkeley DB v4 library if available [[yes]]]))
: ${with_db_4='yes'}

AC_ARG_WITH([libpq],
    AS_HELP_STRING([--with-libpq],
	[Use PostgreSQL libpq library if available [[yes]]]))
: ${with_libpq='yes'}

if test ${enable_static_libs} != no && test ${enable_native_code} = c; then
    AC_MSG_ERROR(
	[--enable-native-code=c incompatible with --enable-static-libs])
fi

dnl Substitution variables to be filled in below.
GC_HEAD_FILES=
M4_FLAGS=
OPTIONAL_BASES=
OPTIONAL_SOURCES=
OPTIONAL_OBJECTS=
STATIC_LIBS=
STATIC_PREFIX=
STATIC_SUFFIX=
PRBFISH_LIBS=
PRMD5_LIBS=
SCHEME_DEFS=-DMIT_SCHEME
SCHEME_LDFLAGS=
MODULE_BASES=
MODULE_LIBS=
MODULE_TARGETS=
MODULE_RULES=/dev/null
MODULE_CFLAGS=
MODULE_LDFLAGS=
LIARC_VARS=/dev/null
LIARC_RULES=/dev/null
AUX_PROGRAMS=
AUX_DATA=utabmd.bin
AUXDIR_NAME=mit-scheme
EXE_NAME=mit-scheme-native
INSTALL_INCLUDE=

AC_CANONICAL_HOST

dnl Save these prior to running AC_PROG_CC.
SAVED_CFLAGS=${CFLAGS}
SAVED_LDFLAGS=${LDFLAGS}

dnl Checks for programs.
AC_PROG_CC
AC_C_BACKSLASH_A
AC_C_BIGENDIAN
AC_C_CONST
AC_C_RESTRICT
AC_C_VOLATILE
AC_C_INLINE
AC_C_STRINGIZE
AC_C_PROTOTYPES
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

if test ${GCC} = yes; then

    dnl Discard flags computed by AC_PROG_CC; we'll use our own.
    CFLAGS=${SAVED_CFLAGS}
    LDFLAGS=${SAVED_LDFLAGS}

    if test ${enable_debugging} = no; then
	CFLAGS="${CFLAGS} -O3"
    else
	CFLAGS="${CFLAGS} -O0 -g -DENABLE_DEBUGGING_TOOLS"
	LDFLAGS="${LDFLAGS} -g"
    fi
    CFLAGS="${CFLAGS} -Wall -Wundef -Wpointer-arith -Winline"
    CFLAGS="${CFLAGS} -Wstrict-prototypes -Wnested-externs -Wredundant-decls"

    AC_MSG_CHECKING([for GCC>=4])
    AC_COMPILE_IFELSE(
	[AC_LANG_PROGRAM(
	    [[
	    #if __GNUC__ >= 4
	    ;
	    #else
	    #error "gcc too old"
	    #endif
	    ]],
	    [[]]
	)],
	[
	AC_MSG_RESULT([yes])
        CFLAGS="${CFLAGS} -Wextra -Wno-sign-compare -Wno-unused-parameter"
        CFLAGS="${CFLAGS} -Wold-style-definition"
	],
	[AC_MSG_RESULT([no])])

    # other possibilities:
    # -Wmissing-prototypes -Wunreachable-code -Wwrite-strings
fi
FOO=`${INSTALL} --help 2> /dev/null | fgrep -e --preserve-timestamps`
if test "x${FOO}" != x; then
    INSTALL="${INSTALL} --preserve-timestamps"
fi
CCLD=${CC}

dnl Add OS-dependent customizations.  This must happen before checking
dnl any headers or library routines, because it may add CFLAGS or
dnl LDFLAGS that the subsequent checks require.

DO_GCC_TESTS=no
GNU_LD=no
case ${host_os} in
linux-gnu)
    DO_GCC_TESTS=yes
    GNU_LD=yes
    ;;
freebsd*)
    M4_FLAGS="${M4_FLAGS} -P SUPPRESS_LEADING_UNDERSCORE,1"
    DO_GCC_TESTS=yes
    GNU_LD=yes
    ;;
dragonfly*)
    M4_FLAGS="${M4_FLAGS} -P SUPPRESS_LEADING_UNDERSCORE,1"
    DO_GCC_TESTS=yes
    GNU_LD=yes
    ;;
darwin*)
    MACOSX=yes
    MACOSX_SYSROOT=/Developer/SDKs/MacOSX10.4u.sdk
    if test -d ${MACOSX_SYSROOT}; then
	MACOSX_CFLAGS="-mmacosx-version-min=10.4 -isysroot ${MACOSX_SYSROOT}"
	MACOSX_LDFLAGS="${MACOSX_CFLAGS} -Wl,-syslibroot,${MACOSX_SYSROOT}"
	CFLAGS="${CFLAGS} ${MACOSX_CFLAGS}"
	LDFLAGS="${LDFLAGS} ${MACOSX_LDFLAGS}"
    fi
    MODULE_LDFLAGS="${MODULE_LDFLAGS} -bundle -bundle_loader "'${SCHEME_EXE}'
    ;;
netbsd*)
    DO_GCC_TESTS=yes
    GNU_LD=yes
    dnl NetBSD, by default, has programs find non-base libraries via RPATH
    if test "x${x_libraries}" != x; then
	FOO=-Wl,-rpath,`echo ${x_libraries} | sed -e "s/:/ -Wl,-rpath,/g"`
	LDFLAGS="${LDFLAGS} ${FOO}"
    fi
    ;;
openbsd*)
    M4_FLAGS="${M4_FLAGS} -P SUPPRESS_LEADING_UNDERSCORE,1"
    DO_GCC_TESTS=yes
    GNU_LD=yes
    ;;
solaris*)
    # How do we tell whether we're using GNU ld or Solaris ld?
    if test ${GCC} = yes; then
	DO_GCC_TESTS=yes
	M4_FLAGS="${M4_FLAGS} -P SUPPRESS_LEADING_UNDERSCORE,1"
    fi
    LDFLAGS="${LDFLAGS} -lsocket -lnsl"
    ;;
esac

if test "${DO_GCC_TESTS}" = yes; then
    if test "x${STATIC_LIBS}" != x; then
	STATIC_PREFIX="-Xlinker -Bstatic"
	STATIC_SUFFIX="-Xlinker -Bdynamic"
    fi
    if test "${GNU_LD}" = yes; then
	SCHEME_LDFLAGS="${SCHEME_LDFLAGS} -export-dynamic"
    fi
    MODULE_CFLAGS="${MODULE_CFLAGS} -fPIC"
    MODULE_LDFLAGS="${MODULE_LDFLAGS} -shared -fPIC"
    AC_MSG_CHECKING([for ELF binaries])
    AC_RUN_IFELSE(
	[AC_LANG_PROGRAM(
	    [[]],
	    [[
	    #ifdef __ELF__
	    return 0;
	    #endif
	    return 1;
	    ]]
	)],
	[
	AC_MSG_RESULT([yes])
	M4_FLAGS="${M4_FLAGS} -P SUPPRESS_LEADING_UNDERSCORE,1"
	],
	[AC_MSG_RESULT([no])])
fi

dnl Checks for libraries.
AC_CHECK_LIB([m], [exp])

dnl Checks for header files.
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_HEADER_STDBOOL
AC_HEADER_SYS_WAIT
AC_HEADER_TIME
AC_CHECK_HEADERS([bsdtty.h fcntl.h fenv.h limits.h malloc.h sgtty.h stropts.h time.h])
AC_CHECK_HEADERS([sys/file.h sys/ioctl.h sys/mount.h sys/param.h sys/poll.h])
AC_CHECK_HEADERS([sys/ptyio.h sys/socket.h sys/time.h sys/un.h sys/vfs.h])
AC_CHECK_HEADERS([termio.h termios.h unistd.h utime.h])
AC_CHECK_HEADERS([dlfcn.h netdb.h signal.h])
AC_CHECK_HEADERS([sys/mman.h float.h assert.h stdint.h])

define([SCM_INC_TIME],
    [
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
    ])

define([SCM_INC_TERMIO],
    [
    #ifdef HAVE_TERMIOS_H
    #  include <termios.h>
    #else
    #  ifdef HAVE_TERMIO_H
    #    include <termio.h>
    #  endif
    #endif
    ])

define([SCM_INC_NETDB],
    [
    #ifdef HAVE_NETDB_H
    #  include <netdb.h>
    #endif
    ])

define([SCM_INC_SIGNAL],
    [
    #ifdef HAVE_SIGNAL_H
    #  include <signal.h>
    #endif
    ])

define([SCM_INC_SOCKET],
    [
    #include <sys/types.h>
    #ifdef HAVE_SYS_SOCKET_H
    #  include <sys/socket.h>
    #endif
    ])

define([SCM_INC_BSDTTY],
    [
    #ifdef HAVE_BSDTTY_H
    #  include <bsdtty.h>
    #endif
    ])

dnl Checks for types and structs
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIGNAL
AC_TYPE_SIZE_T
AC_TYPE_UID_T

dnl These macros are too new; we'll handle this by hand for now.

dnl AC_TYPE_INT8_T
dnl AC_TYPE_INT16_T
dnl AC_TYPE_INT32_T
dnl AC_TYPE_INTPTR_T
dnl AC_TYPE_UINT8_T
dnl AC_TYPE_UINT16_T
dnl AC_TYPE_UINT32_T
dnl AC_TYPE_UINTPTR_T

define([SCM_STDINT_MSG], [Define replacement if no <stdint.h>.])dnl

AC_CHECK_TYPE([int8_t],,
    [AC_DEFINE_UNQUOTED([int8_t], [signed char], SCM_STDINT_MSG)])

AC_CHECK_TYPE([int16_t],,
    [AC_DEFINE_UNQUOTED([int16_t], [short], SCM_STDINT_MSG)])

AC_CHECK_TYPE([int32_t],,
    [AC_DEFINE_UNQUOTED([int32_t], [int], SCM_STDINT_MSG)])

AC_CHECK_TYPE([uint8_t],,
    [AC_DEFINE_UNQUOTED([uint8_t], [unsigned char], SCM_STDINT_MSG)])

AC_CHECK_TYPE([uint16_t],,
    [AC_DEFINE_UNQUOTED([uint16_t], [unsigned short], SCM_STDINT_MSG)])

AC_CHECK_TYPE([uint32_t],,
    [AC_DEFINE_UNQUOTED([uint32_t], [unsigned int], SCM_STDINT_MSG)])

AC_CHECK_TYPE([intptr_t],,
    [AC_DEFINE_UNQUOTED([intptr_t], [long], SCM_STDINT_MSG)])

AC_CHECK_TYPE([uintptr_t],,
    [AC_DEFINE_UNQUOTED([uintptr_t], [unsigned long], SCM_STDINT_MSG)])

AC_CHECK_SIZEOF([char])
AC_CHECK_SIZEOF([short])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([unsigned char])
AC_CHECK_SIZEOF([unsigned short])
AC_CHECK_SIZEOF([unsigned int])
AC_CHECK_SIZEOF([unsigned long])
AC_CHECK_SIZEOF([void *])
AC_CHECK_SIZEOF([intptr_t])
AC_CHECK_SIZEOF([uintptr_t])

if test ${ac_cv_sizeof_char} -ne 1; then
    AC_MSG_ERROR([SIZEOF_CHAR is not 1])
fi

if test ${ac_cv_sizeof_unsigned_char} -ne 1; then
    AC_MSG_ERROR([SIZEOF_UNSIGNED_CHAR is not 1])
fi

AC_CHECK_TYPE([nlink_t],
    [],
    [AC_DEFINE([nlink_t], [short],
	[Define to 'short' if undefined.])])

AC_CHECK_TYPE([clock_t],
    [],
    [AC_DEFINE([clock_t], [unsigned long],
	[Define to `unsigned long' if <time.h> doesn't define.])],
    [SCM_INC_TIME])

AC_CHECK_TYPE([time_t], 
    [],
    [AC_DEFINE([time_t], [long],
	[Define to `long' if <time.h> doesn't define.])],
    [SCM_INC_TIME])

AC_CHECK_TYPE([socklen_t],
    [],
    [AC_DEFINE([socklen_t], [int],
	[Define to `int' if <sys/socket.h> doesn't define.])],
    [SCM_INC_SOCKET])

AC_CHECK_TYPE([cc_t],
    [],
    [AC_DEFINE([cc_t], [unsigned char],
	[Define to `unsigned char' if <termios.h> doesn't define.])],
    [SCM_INC_TERMIO])

AC_CHECK_TYPE([speed_t],
    [],
    [AC_DEFINE([speed_t], [short],
	[Define to `short' if <termios.h> doesn't define.])],
    [SCM_INC_TERMIO])

AC_STRUCT_TM
AC_STRUCT_TIMEZONE

dnl Timezone stuff is hairy; there's no standard.
have_timezone_info=no
val_tm_gmtoff=
val_timezone=

if test ${have_timezone_info} = no; then
    AC_CHECK_MEMBER([struct tm.tm_gmtoff],
	[
	have_timezone_info=yes
	val_tm_gmtoff=tm_gmtoff
	],
	[],
	[SCM_INC_TIME])
fi

if test ${have_timezone_info} = no; then
    AC_CHECK_MEMBER([struct tm.__tm_gmtoff],
	[
	have_timezone_info=yes
	val_tm_gmtoff=__tm_gmtoff
	],
	[],
	[SCM_INC_TIME])
fi

if test ${have_timezone_info} = no; then
    AC_CHECK_DECL([__timezone],
	[
	have_timezone_info=yes
	val_timezone=__timezone
	],
	[],
	[SCM_INC_TIME])
fi

if test ${have_timezone_info} = no; then
    AC_CHECK_DECL([_timezone],
	[
	have_timezone_info=yes
	val_timezone=_timezone
	],
	[],
	[SCM_INC_TIME])
fi

if test ${have_timezone_info} = no; then
    AC_CHECK_DECL([timezone],
	[
	have_timezone_info=yes
	val_timezone=timezone
	],
	[],
	[SCM_INC_TIME])
fi

if test -n "${val_tm_gmtoff}"; then
    AC_DEFINE([HAVE_TM_GMTOFF], [1],
	[Define if `struct tm' has the `tm_gmtoff' member.])
    AC_DEFINE_UNQUOTED([TM_GMTOFF], [${val_tm_gmtoff}],
	[Define to name of `tm_gmtoff' member if available.])
fi

if test -n "${val_timezone}"; then
    AC_DEFINE([HAVE_TIMEZONE], [1],
	[Define if timezone variable is available.])
    AC_DEFINE_UNQUOTED([TIMEZONE], [${val_timezone}],
	[Define to name of timezone variable if available.])
fi

AC_CHECK_TYPE([struct ltchars],
    [AC_DEFINE([HAVE_STRUCT_LTCHARS], [1],
	[Define if `struct ltchars' is defined in <bsdtty.h>.])],
    [],
    [SCM_INC_BSDTTY])

AC_CHECK_MEMBER([struct hostent.h_addr_list],
    [AC_DEFINE([HAVE_HOSTENT_H_ADDR_LIST], [1],
	[Define if `struct hostent' has the `h_addr_list' member.])],
    [],
    [SCM_INC_NETDB])

AC_CHECK_TYPE([struct sigcontext],
    [AC_DEFINE([HAVE_STRUCT_SIGCONTEXT], [1],
	[Define if `struct sigcontext' is defined in <signal.h>.])],
    [],
    [SCM_INC_SIGNAL])

dnl Checks for compiler characteristics.
AC_C_BIGENDIAN
AC_C_CHAR_UNSIGNED
AC_C_CONST

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
AC_CHECK_FUNCS([fcntl fdatasync fegetround fesetround floor fpathconf frexp])
AC_CHECK_FUNCS([fsync fsync_range ftruncate])
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
AC_CHECK_FUNCS([strtoul symlink sync_file_range sysconf])
AC_CHECK_FUNCS([times truncate])
AC_CHECK_FUNCS([uname utime])
AC_CHECK_FUNCS([waitpid])

if test ${ac_cv_type_signal} = void; then
    AC_DEFINE([VOID_SIGNAL_HANDLERS], [1],
	[Define if RETSIGTYPE is `void'.])
fi

dnl Checks for system characteristics.
AC_PATH_X
AC_SYS_LARGEFILE
AC_SYS_LONG_FILE_NAMES
AC_SYS_RESTARTABLE_SYSCALLS

dnl curses/termcap/terminfo support
AC_CHECK_LIB([ncurses], [tparm],
    [
    AC_DEFINE([HAVE_LIBNCURSES], [1],
	[Define to 1 if you have the `ncurses' library (-lncurses).])
    STATIC_LIBS="-lncurses ${STATIC_LIBS}"
    AC_CHECK_HEADER([curses.h],
	[
	AC_DEFINE([HAVE_CURSES_H], [1],
	    [Define to 1 if you have the <curses.h> header file.])
	AC_CHECK_HEADER([term.h],
	    [
	    AC_DEFINE([HAVE_TERM_H], [1],
		[Define to 1 if you have the <term.h> header file.])
	    AC_CHECK_LIB([ncurses], [tparam],
		[
		AC_DEFINE([LIBNCURSES_DEFINES_TPARAM], [1],
		    [Define if ncurses library defines `tparam'.])
		],
		[
		OPTIONAL_BASES="${OPTIONAL_BASES} terminfo"
		])
	    ])
	])
    ],
    [AC_CHECK_LIB([curses], [tparm],
	[
	AC_DEFINE([HAVE_LIBCURSES], [1],
	    [Define to 1 if you have the `curses' library (-lcurses).])
	STATIC_LIBS="-lcurses ${STATIC_LIBS}"
	OPTIONAL_BASES="${OPTIONAL_BASES} terminfo"
	],
	[
	AC_CHECK_LIB([termcap], [tparam],
	    [
	    AC_DEFINE([HAVE_LIBTERMCAP], [1],
		[Define to 1 if you have the `termcap' library (-ltermcap).])
	    STATIC_LIBS="-ltermcap ${STATIC_LIBS}"
	    ],
	    [
	    OPTIONAL_BASES="${OPTIONAL_BASES} termcap tparam"
	    ])
	])
    ])

dnl The OpenSSL crypto library provides support for blowfish and MD5.
if test "${with_openssl}" != no; then
    if test "${with_openssl}" != yes; then
	CPPFLAGS="${CPPFLAGS} -I${with_openssl}/include"
	LDFLAGS="${LDFLAGS} -L${with_openssl}/lib"
    fi
    FOUND=
    AC_CHECK_HEADERS([openssl/blowfish.h openssl/md5.h],
	[
	AC_CHECK_LIB([crypto], [BF_set_key],
	    [
	    AC_DEFINE([HAVE_LIBCRYPTO], [1],
		[Define to 1 if you have the `crypto' library (-lcrypto).])
	    FOUND=yes
	    ])
	])
    if test -n "${FOUND}"; then
	MODULE_LIBS="-lcrypto ${MODULE_LIBS}"
	MODULE_BASES="${MODULE_BASES} prbfish prmd5"
	PRBFISH_LIBS="-lcrypto"
	PRMD5_LIBS="-lcrypto"
    fi
fi

dnl These libraries might not be installed, so take care generating
dnl file dependencies using "makegen/makegen.scm" when called on
dnl "makegen/files-optional.scm".  To wit, "prmhash.c" & "prmcrypt.c"
dnl must conditionalize their dependencies on <mhash.h> & <mcrypt.h>,
dnl respectively, to avoid warnings in "Makefile.deps" and its embeds.
dnl Finally, note that "prmd5.c" is similarly conditionalized as well.

dnl The mhash library provides MD5 support.  It can be loaded in addition
dnl to other MD5 libraries and provides a rich set of hashes.
if test "${with_mhash}" != no; then
    if test "${with_mhash}" != yes; then
	CPPFLAGS="${CPPFLAGS} -I${with_mhash}/include"
	LDFLAGS="${LDFLAGS} -L${with_mhash}/lib"
    fi
    AC_CHECK_HEADER([mhash.h],
	[
	AC_DEFINE([HAVE_MHASH_H], [1],
	    [Define to 1 if you have the <mhash.h> header file.])
	AC_CHECK_LIB([mhash], [mhash_count],
	    [
	    AC_DEFINE([HAVE_LIBMHASH], [1],
		[Define to 1 if you have the `mhash' library (-lmhash).])
	    if test ${enable_debugging} != no; then
	       LIBS="-lmhash ${LIBS}"
	    fi
	    MODULE_LIBS="-lmhash ${MODULE_LIBS}"
	    MODULE_BASES="${MODULE_BASES} prmhash"
	    if test "x${PRMD5_LIBS}" = x; then
		PRMD5_LIBS="-lmhash"
	    fi
	    ])
	])
fi

dnl The mcrypt library provides blowfish, but its CFB mode is 8 bit.
dnl We have been using 64-bit CFB, so this isn't really compatible.
dnl But mcrypt provides many ciphers and can be loaded in addition.
if test "${with_mcrypt}" != no; then
    if test "${with_mcrypt}" != yes; then
	CPPFLAGS="${CPPFLAGS} -I${with_mcrypt}/include"
	LDFLAGS="${LDFLAGS} -L${with_mcrypt}/lib"
    fi
    AC_CHECK_HEADER([mcrypt.h],
	[
	AC_DEFINE([HAVE_MCRYPT_H], [1],
	    [Define to 1 if you have the <mcrypt.h> header file.])
	AC_CHECK_LIB([ltdl], [lt_dlinit], [mc_libs=-lltdl], [mc_libs=])
	AC_CHECK_LIB([mcrypt], [mcrypt_generic_init],
	    [
	    AC_DEFINE([HAVE_LIBMCRYPT], [1],
		[Define to 1 if you have the `mcrypt' library (-lmcrypt).])
	    MODULE_LIBS="-lmcrypt ${mc_libs} ${MODULE_LIBS}"
	    MODULE_BASES="${MODULE_BASES} prmcrypt"
	    ],
	    [],
	    [${mc_libs}])
	])
fi

dnl gdbm support
if test "${with_gdbm}" != no; then
    if test "${with_gdbm}" != yes; then
	CPPFLAGS="${CPPFLAGS} -I${with_gdbm}/include"
	LDFLAGS="${LDFLAGS} -L${with_gdbm}/lib"
    fi
    AC_CHECK_HEADER([gdbm.h],
	[
	AC_DEFINE([HAVE_GDBM_H], [1],
	    [Define to 1 if you have the <gdbm.h> header file.])
	AC_CHECK_LIB([gdbm], [gdbm_open],
	    [
	    AC_DEFINE([HAVE_LIBGDBM], [1],
		[Define to 1 if you have the `gdbm' library (-lgdbm).])
	    MODULE_LIBS="-lgdbm ${MODULE_LIBS}"
	    MODULE_BASES="${MODULE_BASES} prgdbm"
	    ])
	])
fi

dnl DB v4 support
if test "${with_db_4}" != no; then
    if test "${with_db_4}" != yes; then
	CPPFLAGS="${CPPFLAGS} -I${with_db_4}/include"
	LDFLAGS="${LDFLAGS} -L${with_db_4}/lib"
    fi
    AC_CHECK_HEADER([db.h],
	[
	AC_DEFINE([HAVE_DB_H], [1],
	    [Define to 1 if you have the <db.h> header file.])
	AC_MSG_CHECKING([for db_create in -ldb-4])
	save_LIBS=${LIBS}
	LIBS="${LIBS} -ldb-4"
	AC_LINK_IFELSE(
	    [AC_LANG_PROGRAM(
		[[#include <db.h>]],
		[[db_create (0, 0, 0)]])],
	    [
	    AC_MSG_RESULT([yes])
	    AC_DEFINE([HAVE_LIBDB_4], [1], 
		[Define to 1 if you have the `db-4' library (-ldb-4).])
	    MODULE_LIBS="-ldb-4 ${MODULE_LIBS}"
	    MODULE_BASES="${MODULE_BASES} prdb4"
	    ],
	    [
	    AC_MSG_RESULT([no])
	    ])
	LIBS=${save_LIBS}
	])
fi

dnl PostgreSQL support
if test "${with_libpq}" != no; then
    if test "${with_libpq}" != yes; then
	libpq_inc=${with_libpq}/include
	libpq_lib=${with_libpq}/lib
    else
	AC_PATH_PROG([PG_CONFIG], [pg_config])
	if test "x${PG_CONFIG}" != x; then
	    libpq_inc=`${PG_CONFIG} --includedir 2>/dev/null`
	    libpq_lib=`${PG_CONFIG} --libdir 2>/dev/null`
	else
	    if test -d /usr/include/postgresql; then
	       libpq_inc=/usr/include/postgresql
	    else
	       libpq_inc=/usr/include
	    fi
	    libpq_lib=/usr/lib
	fi
    fi
    if test "${libpq_inc}" != /usr/include; then
	CPPFLAGS="${CPPFLAGS} -I${libpq_inc}"
    fi
    if test "${libpq_lib}" != /usr/lib; then
	LDFLAGS="${LDFLAGS} -L${libpq_lib}"
    fi
    AC_CHECK_HEADER([libpq-fe.h],
	[
	AC_DEFINE([HAVE_LIBPQ_FE_H], [1],
	    [Define to 1 if you have the <libpq-fe.h> header file.])
	AC_CHECK_LIB([pq], [PQconnectdb],
	    [
	    AC_DEFINE([HAVE_LIBPQ], [1],
		[Define to 1 if you have the `pq' library (-lpq).])
	    MODULE_LIBS="-lpq ${MODULE_LIBS}"
	    MODULE_BASES="${MODULE_BASES} prpgsql"
	    ])
	])
fi

dnl Decide whether we're using static or dynamic libraries.
if test "${enable_static_libs}" = no; then
    AC_CHECK_FUNC([dlopen],
	[
	LIBS="${STATIC_LIBS} ${LIBS}"
	],
	[
	AC_CHECK_LIB([dl], [dlopen],
	    [
	    AC_DEFINE([HAVE_LIBDL], [1],
		[Define to 1 if you have the `dl' library (-ldl).])
	    LIBS="${STATIC_LIBS} -ldl ${LIBS}"
	    ],
	    [
	    if test ${enable_native_code} = c; then
		AC_MSG_ERROR(
		    [--enable-native-code=c requires dynamic loader support])
	    fi
	    enable_static_libs=yes
	    ])
	])
fi
if test ${enable_static_libs} != no; then
    STATIC_LIBS=${MODULE_LIBS}${STATIC_LIBS}
else
    STATIC_LIBS=
    OPTIONAL_BASES="${OPTIONAL_BASES} pruxdld"
    AC_DEFINE([UX_DLD_ENABLED], [1],
	[Define to 1 if unix dynamic loading support is enabled.])
fi

if test ${enable_valgrind_mode} != no; then
    SCHEME_DEFS="${SCHEME_DEFS} -DVALGRIND_MODE"
    M4_FLAGS="${M4_FLAGS} -P VALGRIND_MODE,1"
fi

dnl Add support for X if present.
if test "${no_x}" != yes; then
    if test "x${x_includes}" != x; then
	FOO=-I`echo ${x_includes} | sed -e "s/:/ -I/g"`
	CPPFLAGS="${CPPFLAGS} ${FOO}"
    fi
    if test "x${x_libraries}" != x; then
	FOO=-L`echo ${x_libraries} | sed -e "s/:/ -L/g"`
	LIBS="${LIBS} ${FOO}"
    fi
    LIBS="${LIBS} -lX11"
    OPTIONAL_BASES="${OPTIONAL_BASES} x11base x11term x11graph x11color"
fi

AC_MSG_CHECKING([for native-code support])
OPTIONAL_BASES="${OPTIONAL_BASES} cmpint cmpintmd comutl"
GC_HEAD_FILES="gccode.h cmpgc.h cmpintmd-config.h cmpintmd.h"
SCM_ARCH=none
ENC_WARNP=no

case ${enable_native_code} in
yes)
    case ${host_cpu} in
    alpha*)
	SCM_ARCH=alpha
	;;
    hppa*)
	SCM_ARCH=hppa
	GC_HEAD_FILES="${GC_HEAD_FILES} hppacach.h"
	;;
    i?86)
	SCM_ARCH=i386
	;;
    # x86_64)
    #     SCM_ARCH=i386
    #     CFLAGS="${CFLAGS} -m32"
    #     LDFLAGS="${LDFLAGS} -m32"
    #     ;;
    m68k|m680?0)
	SCM_ARCH=mc68k
	;;
    mips*)
	SCM_ARCH=mips
	;;
    vax)
	SCM_ARCH=vax
	;;
    esac
    ;;
c)
    SCM_ARCH=c
    ;;
svm)
    SCM_ARCH=svm1
    ;;
no|none)
    ;;
*)
    dnl This is not quite right, because the compiler and microcode
    dnl disagree abou what some architectures should be called, such as
    dnl bobcat vs mc68k or spectrum versus hppa.  I don't know what the
    dnl state of Scheme on these architectures is, however, so at least
    dnl this will flag an error if you try to use them.
    if test -f "cmpauxmd/${enable_native_code}.m4"; then
	SCM_ARCH="${enable_native_code}"
    else
	ENC_WARNP=yes
    fi
    ;;
esac

case ${SCM_ARCH} in
none)
    AC_MSG_RESULT([no])
    ;;
c)
    AC_MSG_RESULT([yes, using portable C code])
    AC_CONFIG_LINKS([cmpauxmd.c:cmpauxmd/c.c])
    AC_CONFIG_FILES([liarc-cc], [chmod +x liarc-cc])
    AC_CONFIG_FILES([liarc-ld], [chmod +x liarc-ld])
    OPTIONAL_BASES="${OPTIONAL_BASES} cmpauxmd unstackify compinit"
    LIARC_VARS=liarc-vars
    LIARC_RULES=liarc-rules
    AUX_PROGRAMS="gen-nonce extract-liarc-decls"
    AUX_DATA=
    AUXDIR_NAME=mit-scheme-c
    EXE_NAME=mit-scheme-c
    INSTALL_INCLUDE=install-include
    ;;
svm1)
    AC_MSG_RESULT([yes, using portable SVM code])
    OPTIONAL_BASES="${OPTIONAL_BASES} svm1-interp"
    AUXDIR_NAME=mit-scheme-svm
    EXE_NAME=mit-scheme-svm
    ;;
*)
    AC_MSG_RESULT([yes, for ${SCM_ARCH}])
    AC_CONFIG_LINKS([cmpauxmd.m4:cmpauxmd/${SCM_ARCH}.m4])
    OPTIONAL_SOURCES="${OPTIONAL_SOURCES} cmpauxmd.m4"
    OPTIONAL_OBJECTS="${OPTIONAL_OBJECTS} cmpauxmd.o"
    ;;
esac

if test ${ENC_WARNP} = yes; then
    AC_MSG_WARN([illegal --enable-native-code value: ${enable_native_code}])
fi

AC_CONFIG_LINKS([
    cmpintmd.h:cmpintmd/${SCM_ARCH}.h
    cmpintmd.c:cmpintmd/${SCM_ARCH}.c
    cmpintmd-config.h:cmpintmd/${SCM_ARCH}-config.h
    ])

for base in ${OPTIONAL_BASES}; do
    OPTIONAL_SOURCES="${OPTIONAL_SOURCES} ${base}.c"
    OPTIONAL_OBJECTS="${OPTIONAL_OBJECTS} ${base}.o"
done

if test "x${MODULE_BASES}" != x; then
    MODULE_RULES=config-rules.module
    rm -f ${MODULE_RULES}
    for BASE in ${MODULE_BASES}; do
	MODULE_TARGETS="${MODULE_TARGETS} ${BASE}.so"
	echo >> ${MODULE_RULES}
	echo ${BASE}.o: ${BASE}.c >> ${MODULE_RULES}
	echo "	" '$(COMPILE_MODULE)' -c '$*.c' >> ${MODULE_RULES}
    done
fi

if test x${MACOSX} = xyes && test ${SCM_ARCH} = i386; then
    SCHEME_LDFLAGS="${SCHEME_LDFLAGS} -Wl,-pagezero_size,04000000"
fi

AC_SUBST([AS_FLAGS])
AC_SUBST([GC_HEAD_FILES])
AC_SUBST([M4_FLAGS])
AC_SUBST([OPTIONAL_SOURCES])
AC_SUBST([OPTIONAL_OBJECTS])
AC_SUBST([STATIC_LIBS])
AC_SUBST([STATIC_PREFIX])
AC_SUBST([STATIC_SUFFIX])
AC_SUBST([PRBFISH_LIBS])
AC_SUBST([PRMD5_LIBS])
AC_SUBST([SCHEME_DEFS])
AC_SUBST([SCHEME_LDFLAGS])
AC_SUBST([MODULE_TARGETS])
AC_SUBST_FILE([MODULE_RULES])
AC_SUBST([MODULE_CFLAGS])
AC_SUBST([MODULE_LDFLAGS])
AC_SUBST_FILE([LIARC_VARS])
AC_SUBST_FILE([LIARC_RULES])
AC_SUBST([AUX_PROGRAMS])
AC_SUBST([AUX_DATA])
AC_SUBST([AUXDIR_NAME])
AC_SUBST([EXE_NAME])
AC_SUBST([INSTALL_INCLUDE])
AC_SUBST([CCLD])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

# Make custom compilation program for "makegen.scm".
rm -f makegen-cc
echo "#!/bin/sh" > makegen-cc
echo exec gcc -MM -MG -DMIT_SCHEME ${CPPFLAGS} '"${1}"' >> makegen-cc
chmod +x makegen-cc

if test ${MODULE_RULES} != /dev/null; then
    rm -f ${MODULE_RULES}
fi
