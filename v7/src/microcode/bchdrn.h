/* -*-C-*-

$Id: bchdrn.h,v 1.9 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1991-1999 Massachusetts Institute of Technology

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
#ifndef _OS2
#ifndef WINNT
  extern int EXFUN (read, (int, PTR, unsigned int));
  extern int EXFUN (write, (int, PTR, unsigned int));
#endif
#endif
#endif
#endif

#if defined(HAVE_POSIX_SIGNALS) && defined(HAVE_BSD_SIGNALS)
#  define RE_INSTALL_HANDLER(signum,handler)	do { } while (0)
#else
#  define RE_INSTALL_HANDLER(signum,handler)	signal (signum, handler)
#endif

/* #define AVOID_SYSV_SHARED_MEMORY */

#ifndef AVOID_SYSV_SHARED_MEMORY
#  if defined(_SYSV4) || defined(_SUNOS4) || defined(_ULTRIX)
#    define HAVE_SYSV_SHARED_MEMORY
#  endif
#  if defined(_HPUX) || defined(__osf__) || defined(_AIX)
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
