/* -*-C-*-

$Id: os2.h,v 1.4 1995/10/30 07:54:29 cph Exp $

Copyright (c) 1994-95 Massachusetts Institute of Technology

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

/* OS/2 system include file */

#ifndef SCM_OS2_H
#define SCM_OS2_H

#include "dstack.h"
#include "osscheme.h"
#include "syscall.h"

/* Defined by "scheme.h" and conflicts with definition in <os2.h>.
   Scheme's definition not needed in OS/2 files.  */
#undef END_OF_CHAIN

#define INCL_BASE
#define INCL_PM
#include <os2.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <setjmp.h>
#include <limits.h>

#include "os2api.h"
#include "os2msg.h"
#include "os2io.h"
#include "os2thrd.h"
#include "os2ctty.h"
#include "os2cthrd.h"
#include "os2pm.h"

#define OS2_MAX_FILE_HANDLES() 20

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
