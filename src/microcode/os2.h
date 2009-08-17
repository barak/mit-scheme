/* -*-C-*-

$Id: c92b7a25ea6adbbc325fcb58774b163453fbd4a2 $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* OS/2 system include file */

#ifndef SCM_OS2_H
#define SCM_OS2_H

#include "config.h"
#include "dstack.h"
#include "osscheme.h"
#include "syscall.h"

#define INCL_BASE
#define INCL_PM
#include <os2.h>
#include <setjmp.h>

#include "os2api.h"
#include "os2msg.h"
#include "os2io.h"
#include "os2thrd.h"
#include "os2ctty.h"
#include "os2cthrd.h"
#include "os2pm.h"

#define FILE_ANY							\
  (FILE_NORMAL | FILE_HIDDEN | FILE_SYSTEM | FILE_DIRECTORY | FILE_ARCHIVED)

extern HMTX OS2_create_mutex_semaphore  (PSZ, int);
extern void OS2_close_mutex_semaphore   (HMTX);
extern void OS2_request_mutex_semaphore (HMTX);
extern void OS2_release_mutex_semaphore (HMTX);

extern HEV   OS2_create_event_semaphore (PSZ, int);
extern void  OS2_close_event_semaphore  (HEV);
extern int   OS2_post_event_semaphore   (HEV);
extern ULONG OS2_reset_event_semaphore  (HEV);
extern int   OS2_wait_event_semaphore   (HEV, int);

extern HQUEUE OS2_create_queue (ULONG);
extern void   OS2_close_queue  (HQUEUE);
extern void   OS2_write_queue  (HQUEUE, ULONG, ULONG, PVOID, ULONG);
extern int    OS2_read_queue   (HQUEUE, ULONG *, ULONG *, PVOID *, HEV);

extern ULONG OS2_system_variable (ULONG);

/* Logic errors are fatal and can't be caught.  These are errors that
   should never happen, and if one does occur the program cannot
   proceed.  */
#define OS2_logic_error(d) OS2_logic_error_1 ((d), __FILE__, __LINE__)
extern void OS2_logic_error_1 (const char *, const char *, unsigned int);

#endif /* SCM_OS2_H */
