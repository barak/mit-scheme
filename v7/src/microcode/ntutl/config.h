/* -*-C-*-

$Id: config.h,v 1.2 2000/12/05 21:23:50 cph Exp $

Copyright (c) 2000 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef SCM_CONFIG_H
#define SCM_CONFIG_H

#ifndef __WIN32__
#  define __WIN32__
#endif

#if defined(_MSC_VER) && !defined(CL386)
#  define CL386
#endif

#include <sys/types.h>
#include <time.h>

#ifdef CL386
typedef _off_t off_t;
#else
typedef short nlink_t;
#endif

typedef unsigned short mode_t;
typedef unsigned long pid_t;
typedef short uid_t;
typedef short gid_t;
typedef unsigned char cc_t;
typedef long ssize_t;

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

/* Define if architecture has native-code compiler support.  */
#define HAS_COMPILER_SUPPORT 1

/* Include the shared configuration header.  */
#include "confshared.h"

#endif /* SCM_CONFIG_H */
