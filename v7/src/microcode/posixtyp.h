/* -*-C-*-

$Id: posixtyp.h,v 1.14 1997/10/22 05:22:06 cph Exp $

Copyright (c) 1990-97 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

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
#define _PID_T
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
