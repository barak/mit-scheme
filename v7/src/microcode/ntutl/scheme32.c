/* -*-C-*-

$Id: scheme32.c,v 1.2 1993/08/03 22:27:42 gjr Exp $

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

/* MIT Scheme under Windows system utiltities DLL source.
   True NT (vs. Win32s) version 
 */

#include "ntscmlib.h"
#include <mmsystem.h>

BOOL
win32_under_win32s_p (void)
{
  return ((BOOL) 0);
}

char *
win32_allocate_heap (unsigned long size, unsigned long * handle)
{
  extern char * malloc (unsigned long);

  * handle = 0L;
  return ((char *) (malloc (size)));
}

void
win32_release_heap (char * base, unsigned long handle)
{
  extern void free (char *);

  free (base);
  return;
}

BOOL
win32_lock_memory_area (void * area, unsigned long size)
{
  return (VirtualLock (area, size));
}

void
win32_unlock_memory_area (void * area, unsigned long size)
{
  (void) VirtualUnlock (area, size);
}

/*   Asynchronous timer interrupt based on multimedia system
 *
 *   WARNING: the docs say that timer_tick and all that it references must
 *   be in a DLL witha a FIXED attribute.
 *   Also, it appears to need _stdcall, but mmsystem.h refutes this
 */

struct win32_timer_closure_s
{
  unsigned long * intcode_addr;
  unsigned long * intmask_addr;
  unsigned long * memtop_addr;
  unsigned long bit_mask;
  UINT timer_id;
};

static void _stdcall
win32_nt_timer_tick (UINT wID, UINT wMsg, DWORD dwUser, DWORD dw1, DWORD dw2)
{
  struct win32_timer_closure_s * timer_closure =
    ((struct win32_timer_closure_s *) dwUser);

  * timer_closure->intcode_addr |= timer_closure->bit_mask;
  if (((* (timer_closure->intmask_addr)) & timer_closure->bit_mask) != 0)
    * timer_closure->memtop_addr = ((unsigned long) -1);
  return;
}

void
win32_flush_async_timer (void * state)
{
  struct win32_timer_closure_s * timer_closure
    = ((struct win32_timer_closure_s *) state);
  
  if (timer_closure == ((struct win32_timer_closure_s *) NULL))
    return;
  if (timer_closure->timer_id != 0)
    (void) timeKillEvent (timer_closure->timer_id);
  
  (void) VirtualUnlock (((void *) win32_nt_timer_tick),
			(((char *) win32_flush_async_timer)
			 - ((char *) win32_nt_timer_tick)));
  (void) VirtualUnlock (timer_closure, (sizeof (struct win32_timer_closure_s)));
  (void) free (timer_closure);
  return;
}

UINT
win32_install_async_timer (unsigned long * intcode_addr,
			   unsigned long * intmask_addr,
			   unsigned long * memtop_addr,
			   unsigned long bit_mask,
			   void ** state_ptr)
{
  TIMECAPS tc;
  UINT wTimerRes;
  UINT msInterval = 75;
  UINT msTargetResolution = 50;
  struct win32_timer_closure_s * timer_closure;

  if ((timeGetDevCaps (&tc, sizeof (TIMECAPS))) != TIMERR_NOERROR)
    return (WIN32_ASYNC_TIMER_NONE);
  wTimerRes = (min ((max (tc.wPeriodMin, msTargetResolution)),
		    tc.wPeriodMax));
  if ((timeBeginPeriod (wTimerRes)) == TIMERR_NOCANDO)
    return (WIN32_ASYNC_TIMER_RESOLUTION);

  timer_closure = ((struct win32_timer_closure_s *)
		   (malloc (sizeof (struct win32_timer_closure_s))));

  if (timer_closure == ((struct win32_timer_closure_s *) NULL))
    return (WIN32_ASYNC_TIMER_NOMEM);

  timer_closure->intcode_addr = intcode_addr;
  timer_closure->intmask_addr = intmask_addr;
  timer_closure->memtop_addr = memtop_addr;
  timer_closure->bit_mask = bit_mask;
  timer_closure->timer_id = 0;

  if ((! (VirtualLock (((void *) timer_closure),
		       (sizeof (struct win32_timer_closure_s)))))
      || (! (VirtualLock (((void *) win32_nt_timer_tick),
			  (((char *) win32_flush_async_timer)
			   - ((char *) win32_nt_timer_tick))))))
  {
    win32_flush_async_timer ((void *) timer_closure);
    return (WIN32_ASYNC_TIMER_NOLOCK);
  }

  timer_closure->timer_id
    = (timeSetEvent (msInterval,
		     wTimerRes,
		     ((LPTIMECALLBACK) win32_nt_timer_tick),
		     ((DWORD) timer_closure),
		     TIME_PERIODIC));

  if (timer_closure->timer_id == 0)
  {
    win32_flush_async_timer ((void *) timer_closure);
    return (WIN32_ASYNC_TIMER_EXHAUSTED);
  }

  * state_ptr = ((void *) timer_closure);
  return (WIN32_ASYNC_TIMER_OK);
}
