/* -*-C-*-

$Id: ntscmlib.h,v 1.1 1993/07/27 20:53:51 gjr Exp $

Copyright (c) 1993 Massachusetts Institute of Technology

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

/* MIT Scheme under Windows system utilities exports. */

#ifndef SCM_NTLIB_H

#include <windows.h>

#define WIN32_ASYNC_TIMER_OK		0
#define WIN32_ASYNC_TIMER_NONE		1
#define WIN32_ASYNC_TIMER_EXHAUSTED	2
#define WIN32_ASYNC_TIMER_RESOLUTION	3
#define WIN32_ASYNC_TIMER_NOLOCK	4
#define WIN32_ASYNC_TIMER_NOMEM		5

extern BOOL win32_under_win32s_p (void);

extern char * win32_allocate_heap (unsigned long, unsigned long *);
extern void win32_release_heap (char *, unsigned long);

extern BOOL win32_lock_memory_area (void *, unsigned long);
extern void win32_unlock_memory_area (void *, unsigned long);

extern UINT win32_install_async_timer (unsigned long *, unsigned long *,
				       unsigned long *, unsigned long,
				       void **);
extern void win32_flush_async_timer (void *);

#endif /* SCM_NTLIB_H */
