/* -*-C-*-

$Id: acconfig.h,v 11.6 2003/02/14 18:28:14 cph Exp $

Copyright (c) 2000-2001 Massachusetts Institute of Technology

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
#define SCM_CONFIG_H
@TOP@

/* Define if RETSIGTYPE is `void'.  */
#undef VOID_SIGNAL_HANDLERS

/* Define to `short' if <sys/types.h> doesn't define.  */
#undef nlink_t

/* Define to `unsigned long' if <time.h> doesn't define.  */
#undef clock_t

/* Define to `long' if <time.h> doesn't define.  */
#undef time_t

/* Define to `int' if <sys/socket.h> doesn't define.  */
#undef socklen_t

/* Define to `unsigned char' if <termios.h> doesn't define.  */
#undef cc_t

/* Define to `short' if <termios.h> doesn't define.  */
#undef speed_t

/* Define if `struct ltchars' is defined in <bsdtty.h>.  */
#undef HAVE_STRUCT_LTCHARS

/* Define if `struct sigcontext' is defined in <signal.h>.  */
#undef HAVE_STRUCT_SIGCONTEXT

/* Define if `struct hostent' has the `h_addr_list' member.  */
#undef HAVE_HOSTENT_H_ADDR_LIST

/* Define if `struct tm' has the `tm_gmtoff' member.  */
#undef HAVE_TM_GMTOFF

/* Define to name of `tm_gmtoff' member if HAVE_TM_GMTOFF defined.  */
#undef TM_GMTOFF

/* Define if global timezone variable is available.  */
#undef HAVE_TIMEZONE

/* Define to name of global timezone variable if HAVE_TIMEZONE defined.  */
#undef TIMEZONE

/* Define if architecture has native-code compiler support.  */
#undef HAS_COMPILER_SUPPORT

/* Define if blowfish library is present.  */
#undef HAVE_LIBBLOWFISH

/* Define if OpenSSL crypto library is present.  */
#undef HAVE_LIBCRYPTO

/* Define if curses library is present.  */
#undef HAVE_LIBCURSES

/* Define if dl library is present.  */
#undef HAVE_LIBDL

/* Define if gdbm library is present.  */
#undef HAVE_LIBGDBM

/* Define if md5 library is present.  */
#undef HAVE_LIBMD5

/* Define if mcrypt library is present.  */
#undef HAVE_LIBMCRYPT

/* Define if mhash library is present.  */
#undef HAVE_LIBMHASH

/* Define if ncurses library is present.  */
#undef HAVE_LIBNCURSES

/* Define if ncurses library defines `tparam'.  */
#undef LIBNCURSES_DEFINES_TPARAM

/* Define if termcap library is present.  */
#undef HAVE_LIBTERMCAP

@BOTTOM@

#ifndef __unix__
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

/* Include the shared configuration header.  */
#include "confshared.h"

#endif /* SCM_CONFIG_H */
