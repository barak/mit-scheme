/* -*-C-*-

$Id: posixtyp.h,v 1.16 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

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

#ifndef SCM_POSIXTYPE_H
#define SCM_POSIXTYPE_H

#ifdef _POSIX

#include <sys/types.h>
#include <sys/times.h>
#include <termios.h>

#else /* not _POSIX */

#ifdef _UNIX
#include <sys/types.h>

#if defined(_HPUX) && (_HPUX_VERSION == 65)
#define _MODE_T
#define _NLINK_T
#define _SIZE_T
#define _OFF_T
#endif

#ifdef _BSD
#define _UID_T
#define _SIZE_T
#define _OFF_T
#endif

#ifdef _BSD4_3
#ifndef _NEXTOS
#define _MODE_T
#define _NLINK_T
#define _PID_T
#define _CLOCK_T
#endif
#define _TIME_T
#endif

#if defined(_SUNOS4) && defined(__sys_stdtypes_h)
#define _MODE_T
#define _NLINK_T
#define _PID_T
#define _CLOCK_T
#define _TIME_T
#define _SIZE_T
#define _OFF_T
#define _CC_T
#endif

#ifdef apollo
#define _MODE_T
#define _NLINK_T
#define _PID_T
#define _OFF_T
#endif

#endif

#ifdef WINNT

#include <sys/types.h>
#include <time.h>

#ifdef CL386
/*#define _MODE_T*/
#define _NLINK_T
/* #define _PID_T */
#define _CLOCK_T
#define _TIME_T
#define _SIZE_T
#define _OFF_T
#define off_t _off_t
/*#define _CC_T*/
#endif

#ifdef __WATCOMC__
#define _TIME_T
#define _OFF_T
#define _SIZE_T
#define _CLOCK_T
#endif

#ifndef _PID_T
#define _PID_T
typedef unsigned long pid_t;
#endif

#endif

#ifdef _OS2
#if defined(__IBMC__) || defined(__WATCOMC__)

#include <sys/types.h>
#include <time.h>
#define _TIME_T
#define _OFF_T
#define _SIZE_T
#define _CLOCK_T

#else /* not __IBMC__ */
#ifdef __GNUC__

/* This is for GCC with the GNU C library.  */
#include <sys/types.h>
#include <time.h>
#define _TIME_T
#define _OFF_T
#define _SIZE_T
#define _CLOCK_T
#define _MODE_T
#define _NLINK_T
#define _PID_T
#define _UID_T
#define _GID_T

#endif /* __GNUC__ */
#endif /* not __IBMC__ */
#endif /* _OS2 */

#ifndef _MODE_T
#define _MODE_T
typedef unsigned short mode_t;
#endif

#ifndef _NLINK_T
#define _NLINK_T
#ifndef _ULTRIX
typedef short nlink_t;
#endif
#endif

#ifndef _PID_T
#define _PID_T
typedef long pid_t;
#endif

#ifndef _UID_T
#define _UID_T
#ifdef _SYSV
typedef unsigned short uid_t;
typedef unsigned short gid_t;
#else
typedef short uid_t;
typedef short gid_t;
#endif
#endif

#ifndef _CLOCK_T
#define _CLOCK_T
typedef unsigned long clock_t;
#endif

#ifndef _TIME_T
#define _TIME_T
typedef long time_t;
#endif

#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned int size_t;
#endif

#ifndef _OFF_T
#define _OFF_T
typedef unsigned int off_t;
#endif

#ifndef _CC_T
#define _CC_T
typedef unsigned char cc_t;
#endif

#endif /* not _POSIX */

#endif /* SCM_POSIXTYPE_H */
