/* -*-C-*-

$Id: config.h,v 1.5 2002/11/20 19:46:17 cph Exp $

Copyright (c) 2000-2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

#ifndef SCM_CONFIG_H
#define SCM_CONFIG_H

#ifndef __OS2__
#  define __OS2__
#endif

#include <sys/types.h>
#include <time.h>

#ifndef __GNUC__
  typedef unsigned short mode_t;
  typedef short nlink_t;
  typedef long pid_t;
  typedef short uid_t;
  typedef short gid_t;
#endif

typedef unsigned char cc_t;
#if (! ((defined (__IBMC__)) && (__IBMC__ >= 360)))
  typedef long ssize_t;
#endif

/* The number of bytes in a unsigned long.  */
#define SIZEOF_UNSIGNED_LONG 4

/* Define if your processor stores words with the most significant
   byte first (like Motorola and SPARC, unlike Intel and VAX).  */
/* #undef WORDS_BIGENDIAN */

/* Define if you have the floor function.  */
#define HAVE_FLOOR 1

/* Define if you have the frexp function.  */
#define HAVE_FREXP 1

/* Define if you have the modf function.  */
#define HAVE_MODF 1

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if you have the <unistd.h> header file.  */
/* #undef HAVE_UNISTD_H */

/* Define if you have the <fcntl.h> header file.  */
#define HAVE_FCNTL_H 1

/* Define if architecture has native-code compiler support.  */
#define HAS_COMPILER_SUPPORT 1

/* Define if you have the <blowfish.h> header file.  */
#define HAVE_BLOWFISH_H 1

/* Define if you have the <md5.h> header file.  */
#define HAVE_MD5_H 1

/* Include the shared configuration header.  */
#include "confshared.h"

#endif /* SCM_CONFIG_H */
