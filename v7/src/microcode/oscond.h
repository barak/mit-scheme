/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/oscond.h,v 1.8 1992/05/04 20:44:20 jinx Exp $

Copyright (c) 1990-92 Massachusetts Institute of Technology

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

/* Operating System Conditionalizations.
   Identify the operating system, its version, and generalizations. */

#ifndef SCM_OSCOND_H
#define SCM_OSCOND_H

/* _POSIX is assumed to be independent of all operating-system and
   machine specification macros.  */

#if defined(__hpux) && !defined(hpux)
#define hpux
#endif

#if defined(__hp9000s300) && !defined(hp9000s300)
#define hp9000s300
#endif

#if defined(__hp9000s400) && !defined(hp9000s400)
#define hp9000s400
#endif

#if defined(__hp9000s700) && !defined(hp9000s700)
#define hp9000s700
#endif

#if defined(__hp9000s800) && !defined(hp9000s800)
#define hp9000s800
#endif

#if defined(hpux) && !defined(_HPUX)
#define _HPUX
#endif

#ifdef _HPUX
#ifdef __hpux

#define _POSIX
#define _SYSV3

#include <a.out.h>
#ifdef SHL_MAGIC
#define _HPUX_VERSION 80
#else
#define _HPUX_VERSION 70
#endif

#else /* not __hpux */

#define _SYSV

/* Definitions in this file identify the operating system version. */
#include <signal.h>

#ifdef hp9000s300
#ifdef SV_BSDSIG
#define _HPUX_VERSION 65
#else
/* Versions prior to 6.2 aren't worth dealing with anymore. */
#define _HPUX_VERSION 62
#endif
#endif

#ifdef hp9000s800
#ifdef SV_RESETHAND
#define _HPUX_VERSION 65 /* actually, 3.0 */
#else
/* Versions prior to 2.0 aren't worth dealing with anymore. */
#define _HPUX_VERSION 62 /* actually, 2.0 */
#endif
#endif

#endif /* __hpux */
#endif /* _HPUX */
  
#ifdef _SYSV4
#define _POSIX
#define _SYSV3
#endif

#ifdef _SYSV3
#define _SYSV
#endif

#if defined(_NEXTOS)

#define _BSD4_3

#include <sys/port.h>
#ifdef PORT_BACKLOG_DEFAULT
#define _NEXTOS_VERSION 20
#else
#define _NEXTOS_VERSION 10
#endif

#endif /* _NEXTOS */

#if defined(_ULTRIX)
#define _POSIX
#define _BSD4_3
#endif

#if defined(_SUNOS3) || defined(_SUNOS4)
#define _SUNOS
#define _BSD4_2
#endif

#if defined(_BSD4_2) || defined(_BSD4_3)
#define _BSD
#endif

#if defined(_BSD) && defined(_SYSV)
#include "error: can't define both _BSD and _SYSV"
#endif

#if defined(DOS386)
#  define _DOS386
#  define _DOS386_VERSION	50
#endif

#if defined(_BSD) || defined(_SYSV) || defined(_PIXEL)
#  define _UNIX
#else
#  ifdef _DOS386
#    define _DOS
#  else
#    include "error: unknown unix system -- you must add customizations"
#  endif /* _DOS386 */
#endif /* _BSD || _SYSV || _PIXEL */

#endif /* SCM_OSCOND_H */
