/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchdrn.h,v 1.3 1992/02/29 19:35:23 mhwu Exp $

Copyright (c) 1991 Massachusetts Institute of Technology

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

/* Header file for overlapped I/O in bchscheme. */

#ifndef _BCHDRN_H_INCLUDED

#define _BCHDRN_H_INCLUDED

#include "ansidecl.h"
#include "oscond.h"
#include <errno.h>
#include <signal.h>

#if defined(_POSIX) || defined(_SUNOS4)
#  include <unistd.h>
#else
#ifndef DOS386
  extern int EXFUN (read, (int, PTR, unsigned int));
  extern int EXFUN (write, (int, PTR, unsigned int));
#endif
#endif

#if defined(HAVE_POSIX_SIGNALS) && defined(HAVE_BSD_SIGNALS)
#  define RE_INSTALL_HANDLER(signum,handler)	do { } while (0)
#else
#  define RE_INSTALL_HANDLER(signum,handler)	signal (signum, handler)
#endif

/* #define AVOID_SYSV_SHARED_MEMORY */

#ifndef AVOID_SYSV_SHARED_MEMORY
#  if defined(_SYSV) || defined(_SUNOS4) || defined(_ULTRIX)
#    define HAVE_SYSV_SHARED_MEMORY
#  endif
#endif

#if defined(_HPUX)

#  define HAVE_PREALLOC

#  include <magic.h>
#  if defined(SHL_MAGIC)
#    define hpux8 1
#  endif

/* Page tables can have no gaps in HP-UX < 8.0, leave a gap for malloc. */

#  ifdef hp9000s300
#    ifdef hpux8
#      define ATTACH_POINT	0x60000000
#    else /* not hpux8 */
#      define MALLOC_SPACE	(2 << 20)	/* 2 Meg */
#    endif /* hpux8 */
#  endif /* hp9000s300 */

#endif /* _HPUX */

#ifdef HAVE_SYSV_SHARED_MEMORY

#define DRONE_VERSION_NUMBER		((1 << 8) | 2)

#include <sys/time.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/wait.h>

#ifndef MALLOC_SPACE
#  define MALLOC_SPACE	0
#endif

#ifndef ATTACH_POINT
#  define ATTACH_POINT	0
#endif

#define DRONE_EXTRA_T

struct drone_extra_s
{
  pid_t my_pid;
  pid_t my_ppid;
};

typedef struct drone_extra_s drone_extra_t;

#define DRONE_PID	drone_extra.my_pid
#define DRONE_PPID	drone_extra.my_ppid

#endif /* HAVE_SYSV_SHARED_MEMORY */

/* Shared definitions for all versions */

enum buffer_state
{
  buffer_idle,			/* 0 */
  buffer_busy,			/* 1, used for scan or free */
  buffer_ready,			/* 2, after being read */
  buffer_queued,		/* 3, never written, use as if read */
  buffer_being_read,		/* 4 */
  buffer_read_error,		/* 5 */
  buffer_being_written,		/* 6 */
  buffer_write_error		/* 7 */
};

struct buffer_info
{
  int index;
  enum buffer_state state;
  long position;
  long size;
  PTR bottom;
  PTR top;
  PTR end;
};

enum drone_state
{
  drone_dead,			/* 0 */
  drone_not_ready,		/* 1 */
  drone_idle,			/* 2 */
  drone_reading,		/* 3 */
  drone_writing,		/* 4 */
  drone_aborting		/* 5 */
};

struct drone_info
{
  int index;
#ifdef DRONE_EXTRA_T
  drone_extra_t drone_extra;
#endif
  enum drone_state state;
  int buffer_index;
  long entry_offset;
};

enum queue_entry_state
{
  entry_idle,			/* 0 */
  entry_busy,			/* 1 */
  entry_error			/* 2 */
};

struct gc_queue_entry
{
  int index;
  enum queue_entry_state state;
  struct buffer_info * buffer;
  int drone_index;
  int error_code;
  int retry_count;
};

#endif /* _BCHDRN_H_INCLUDED */
