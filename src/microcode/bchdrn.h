/* -*-C-*-

$Id: bchdrn.h,v 1.12 2003/02/14 18:28:15 cph Exp $

Copyright (c) 1991-2000 Massachusetts Institute of Technology

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

/* Header file for overlapped I/O in bchscheme. */

#ifndef _BCHDRN_H_INCLUDED
#define _BCHDRN_H_INCLUDED

#include "config.h"
#include <errno.h>
#include <signal.h>

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#else
#  ifdef __unix__
     extern ssize_t EXFUN (read, (int, PTR, size_t));
     extern ssize_t EXFUN (write, (int, PTR, size_t));
#  endif
#endif

#ifdef HAVE_POSIX_SIGNALS
#  define RE_INSTALL_HANDLER(signum,handler)	do { } while (0)
#else
#  define RE_INSTALL_HANDLER(signum,handler)	signal (signum, handler)
#endif

/* Doesn't work on GNU/Linux or on FreeBSD.  Disable until we can
   figure out what is going on.  */
#define AVOID_SYSV_SHARED_MEMORY

#if !defined(AVOID_SYSV_SHARED_MEMORY) && defined(HAVE_SHMAT)
#  define USE_SYSV_SHARED_MEMORY
#endif

#if defined(__HPUX__)

#  define HAVE_PREALLOC

#  include <magic.h>
#  if defined(SHL_MAGIC)
#    define hpux8 1
#  endif

/* Page tables can have no gaps in HP-UX < 8.0, leave a gap for malloc. */

#  if defined(hp9000s300) || defined(__hp9000s300)
#    ifdef hpux8
#      define ATTACH_POINT	0x60000000
#    else /* not hpux8 */
#      define MALLOC_SPACE	(2 << 20)	/* 2 Meg */
#    endif /* hpux8 */
#  endif /* hp9000s300 */

#endif /* __HPUX__ */

#ifdef USE_SYSV_SHARED_MEMORY

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

#endif /* USE_SYSV_SHARED_MEMORY */

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
