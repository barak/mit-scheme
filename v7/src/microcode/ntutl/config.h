/* -*-C-*-

$Id: config.h,v 1.13 2007/01/05 21:19:26 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
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

#include <sys/types.h>
#include <time.h>

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

/* The number of bytes in a unsigned long.  */
#define SIZEOF_UNSIGNED_LONG 4

/* Define if your processor stores words with the most significant
   byte first (like Motorola and SPARC, unlike Intel and VAX).  */
#undef WORDS_BIGENDIAN

/* Define if you have the floor function.  */
#define HAVE_FLOOR 1

/* Define if you have the frexp function.  */
#define HAVE_FREXP 1

/* Define if you have the modf function.  */
#define HAVE_MODF 1

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if you have the <unistd.h> header file.  */
#undef HAVE_UNISTD_H

/* Define if you have the <fcntl.h> header file.  */
#define HAVE_FCNTL_H 1

/* Define if architecture has native-code compiler support.  */
#define HAS_COMPILER_SUPPORT 1

/* Define if you have the <blowfish.h> header file.  */
#define HAVE_BLOWFISH_H 1

/* Define if you have the <md5.h> header file.  */
#define HAVE_MD5_H 1

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "bug-mit-scheme@gnu.org"

/* Define to the full name of this package. */
#define PACKAGE_NAME "MIT/GNU Scheme"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "MIT/GNU Scheme 14.18"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "mit-scheme"

/* Define to the version of this package. */
#define PACKAGE_VERSION "14.18"

/* Include the shared configuration header.  */
#include "confshared.h"

#endif /* SCM_CONFIG_H */
