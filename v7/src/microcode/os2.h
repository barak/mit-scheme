/* -*-C-*-

$Id: os2.h,v 1.9 2002/11/20 19:46:11 cph Exp $

Copyright (c) 1994-2001 Massachusetts Institute of Technology

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
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <limits.h>

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
