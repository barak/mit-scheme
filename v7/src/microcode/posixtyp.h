/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/posixtyp.h,v 1.3 1990/12/11 04:16:23 cph Rel $

Copyright (c) 1990 Massachusetts Institute of Technology

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
#endif

#ifdef _BSD
#define _UID_T
#define _SIZE_T
#endif

#if defined(_SUNOS4) && defined(__sys_stdtypes_h)
#define _MODE_T
#define _NLINK_T
#define _PID_T
#define _CLOCK_T
#define _SIZE_T
#define _CC_T
#endif

#endif

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

#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned int size_t;
#endif

#ifndef _CC_T
#define _CC_T
typedef unsigned char cc_t;
#endif

#endif /* not _POSIX */

#endif /* SCM_POSIXTYPE_H */
