/* -*-C-*-

$Id: os2.h,v 1.7 2000/12/05 21:23:46 cph Exp $

Copyright (c) 1994-2000 Massachusetts Institute of Technology

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

/* OS/2 system include file */

#ifndef SCM_OS2_H
#define SCM_OS2_H

#include "config.h"
#include "dstack.h"
#include "osscheme.h"
#include "syscall.h"

/* Defined by "scheme.h" and conflicts with definition in <os2.h>.
   Scheme's definition not needed in OS/2 files.  */
#ifdef END_OF_CHAIN
#  undef END_OF_CHAIN
#endif

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
