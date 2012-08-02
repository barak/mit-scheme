/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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
#define SCM_CONFIG_H

#ifndef __WIN32__
#  define __WIN32__
#endif

#if defined(_MSC_VER) && !defined(CL386)
#  define CL386
#endif

#if defined (__WATCOMC__) && (__WATCOMC__ >= 1240)
#  define __OPEN_WATCOM_14__
#endif

#ifdef CL386
typedef _off_t off_t;
#else
typedef short nlink_t;
#endif

#ifndef __OPEN_WATCOM_14__
  typedef unsigned short mode_t;
  typedef unsigned long pid_t;
  typedef short uid_t;
  typedef short gid_t;
  typedef long ssize_t;
#endif

typedef unsigned char cc_t;

/* Define to 1 if you have the <assert.h> header file. */
#define HAVE_ASSERT_H 1

/* Define if you have the <blowfish.h> header file.  */
#define HAVE_BLOWFISH_H 1

/* Define if you have the <fcntl.h> header file.  */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `feclearexcept' function. */
#define HAVE_FECLEAREXCEPT 1

/* Define to 1 if you have the `fedisableexcept' function. */
/* #undef HAVE_FEDISABLEEXCEPT */

/* Define to 1 if you have the `feenableexcept' function. */
/* #undef HAVE_FEENABLEEXCEPT */

/* Define to 1 if you have the `fegetenv' function. */
#define HAVE_FEGETENV 1

/* Define to 1 if you have the `fegetexcept' function. */
/* #undef HAVE_FEGETEXCEPT */

/* Define to 1 if you have the `fegetexceptflag' function. */
#define HAVE_FEGETEXCEPTFLAG 1

/* Define to 1 if you have the `fegetround' function. */
#define HAVE_FEGETROUND 1

/* Define to 1 if you have the `feholdexcept' function. */
#define HAVE_FEHOLDEXCEPT 1

/* Define to 1 if you have the <fenv.h> header file. */
#define HAVE_FENV_H 1

/* Define to 1 if the system has the type `fenv_t'. */
#define HAVE_FENV_T 1

/* Define to 1 if you have the `feraiseexcept' function. */
#define HAVE_FERAISEEXCEPT 1

/* Define to 1 if you have the `fesetenv' function. */
#define HAVE_FESETENV 1

/* Define to 1 if you have the `fesetexceptflag' function. */
#define HAVE_FESETEXCEPTFLAG 1

/* Define to 1 if you have the `fesetround' function. */
#define HAVE_FESETROUND 1

/* Define to 1 if you have the `fetestexcept' function. */
#define HAVE_FETESTEXCEPT 1

/* Define to 1 if you have the `feupdateenv' function. */
#define HAVE_FEUPDATEENV 1

/* Define to 1 if the system has the type `fexcept_t'. */
#define HAVE_FEXCEPT_T 1

/* Define to 1 if you have the <float.h> header file. */
#define HAVE_FLOAT_H 1

/* Define if you have the floor function.  */
#define HAVE_FLOOR 1

/* Define if you have the fmod function.  */
#define HAVE_FMOD 1

/* Define if you have the frexp function.  */
#define HAVE_FREXP 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Define to 1 if the system has the type `long long int'. */
#define HAVE_LONG_LONG_INT 1

/* Define if you have the <md5.h> header file.  */
#define HAVE_MD5_H 1

/* Define to 1 if you have the `memcpy' function. */
#define HAVE_MEMCPY 1

/* Define if you have the modf function.  */
#define HAVE_MODF 1

/* Define to 1 if stdbool.h conforms to C99. */
#define HAVE_STDBOOL_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strchr' function. */
#define HAVE_STRCHR 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if cpp supports the ANSI # stringizing operator. */
#define HAVE_STRINGIZE 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strstr' function. */
#define HAVE_STRSTR 1

/* Define to 1 if you have the `strtol' function. */
#define HAVE_STRTOL 1

/* Define to 1 if you have the `strtoul' function. */
#define HAVE_STRTOUL 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <time.h> header file. */
#define HAVE_TIME_H 1

/* Define to 1 if the system has the type `uintmax_t'. */
#define HAVE_UINTMAX_T 1

/* Define to 1 if the system has the type `uintptr_t'. */
#define HAVE_UINTPTR_T 1

/* Define if you have the <unistd.h> header file.  */
#undef HAVE_UNISTD_H

/* Define to 1 if the system has the type `unsigned long long int'. */
#define HAVE_UNSIGNED_LONG_LONG_INT 1

/* Define to 1 if the system has the type `_Bool'. */
#define HAVE__BOOL 1

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "bug-mit-scheme@gnu.org"

/* Define to the full name of this package. */
#define PACKAGE_NAME "MIT/GNU Scheme microcode"

/* Define to the version of this package. */
#define PACKAGE_VERSION "15.1"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING PACKAGE_NAME PACKAGE_VERSION

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "mit-scheme"

/* Define to 1 if the C compiler supports function prototypes. */
#define PROTOTYPES 1

/* The size of `char', as computed by sizeof. */
#define SIZEOF_CHAR 1

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `intmax_t', as computed by sizeof. */
#define SIZEOF_INTMAX_T 8

/* The size of `intptr_t', as computed by sizeof. */
#define SIZEOF_INTPTR_T 4

/* The size of `off_t', as computed by sizeof. */
#define SIZEOF_OFF_T 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 4

/* The size of `short', as computed by sizeof. */
#define SIZEOF_SHORT 2

/* The size of `time_t', as computed by sizeof. */
#define SIZEOF_TIME_T 4

/* The size of `uintmax_t', as computed by sizeof. */
#define SIZEOF_UINTMAX_T 8

/* The size of `uintptr_t', as computed by sizeof. */
#define SIZEOF_UINTPTR_T 4

/* The size of `unsigned char', as computed by sizeof. */
#define SIZEOF_UNSIGNED_CHAR 1

/* The size of `unsigned int', as computed by sizeof. */
#define SIZEOF_UNSIGNED_INT 4

/* The size of `unsigned long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG 4

/* The size of `unsigned short', as computed by sizeof. */
#define SIZEOF_UNSIGNED_SHORT 2

/* The size of `void *', as computed by sizeof. */
#define SIZEOF_VOID_P 4

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
/* #undef WORDS_BIGENDIAN */

#include <sys/types.h>
#include <time.h>

/* Include the shared configuration header.  */
#include "confshared.h"

#endif /* SCM_CONFIG_H */
