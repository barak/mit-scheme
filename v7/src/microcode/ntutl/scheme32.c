/* -*-C-*-

$Id: scheme32.c,v 1.12 1997/01/01 22:58:18 cph Exp $

Copyright (c) 1993-97 Massachusetts Institute of Technology

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
#include <stdlib.h>
#include <mmsystem.h>

static void __cdecl win32_flush_async_timer (void *);

static BOOL __cdecl
win32_under_win32s_p (void)
{
  return ((BOOL) 0);
}

//char *
//win32_allocate_heap (unsigned long size, unsigned long * handle)
//{
//#ifdef CL386
//  extern char * malloc (unsigned long);
//#endif
//  * handle = 0L;
//  return ((char *) (malloc (size)));
//}
//
//void
//win32_release_heap (char * base, unsigned long handle)
//{
//  extern void free (char *);
//
//  free (base);
//  return;
//}


static char * __cdecl
win32_allocate_heap (unsigned long size, unsigned long * handle)
{
  LPVOID base;

  base = (VirtualAlloc (((LPVOID) NULL),
			((DWORD) size),
			((DWORD) (MEM_RESERVE | MEM_COMMIT)),
			((DWORD) PAGE_READWRITE)));
  * handle = size;
  return ((char *) base);
}

static void __cdecl
win32_release_heap (char * area, unsigned long handle)
{
  VirtualFree (((LPVOID) area),
	       ((DWORD) handle),
	       ((DWORD) MEM_DECOMMIT));
  VirtualFree (((LPVOID) area),
	       ((DWORD) 0),
	       ((DWORD) MEM_RELEASE));
  return;
}

static BOOL __cdecl
win32_lock_memory_area (void * area, unsigned long size)
{
  return (VirtualLock (area, size));
}

static void __cdecl
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
  UINT timer_id;
  unsigned long * base;
  long memtop_off;
  long int_code_off;
  long int_mask_off;
  unsigned long bit_mask;
  long ctr_off;
  unsigned long catatonia_message;
  unsigned long interrupt_message;
  HWND window;
};

#ifdef CL386
#define __STDCALL _stdcall
#endif

#ifdef __WATCOMC__
#define __STDCALL __stdcall
#endif

#define INTERRUPT_CODE(scm_timer)					\
  ((scm_timer -> base) [scm_timer -> int_code_off])

#define INTERRUPT_MASK(scm_timer)					\
  ((scm_timer -> base) [scm_timer -> int_mask_off])

#define MEMTOP(scm_timer)						\
  ((scm_timer -> base) [scm_timer -> memtop_off])

#define CATATONIA_COUNTER(scm_timer)					\
  ((scm_timer -> base) [scm_timer -> ctr_off])

#define CATATONIA_LIMIT(scm_timer)					\
  ((scm_timer -> base) [(scm_timer -> ctr_off) + 1])

#define CATATONIA_FLAG(scm_timer)					\
  ((scm_timer -> base) [(scm_timer -> ctr_off) + 2])

static void __STDCALL
win32_nt_timer_tick (UINT wID, UINT wMsg, DWORD dwUser, DWORD dw1, DWORD dw2)
{
  struct win32_timer_closure_s * scm_timer
    = ((struct win32_timer_closure_s *) dwUser);
  (INTERRUPT_CODE (scm_timer)) |= (scm_timer -> bit_mask);
  if (((INTERRUPT_CODE (scm_timer)) & (INTERRUPT_MASK (scm_timer))) != 0L)
    {
      /* Post an interrupt message to the window.  This forces it to
	 wake up and exit MsgWaitForMultipleObjects if needed.  */
      PostMessage ((scm_timer -> window),
		   (scm_timer -> interrupt_message),
		   ((WPARAM) 0),
		   ((LPARAM) 0));
      (MEMTOP (scm_timer)) = ((unsigned long) -1L);
    }
  (CATATONIA_COUNTER (scm_timer)) += 1L;
  if (((CATATONIA_COUNTER (scm_timer)) > (CATATONIA_LIMIT (scm_timer)))
      && ((CATATONIA_LIMIT (scm_timer)) != 0L))
    {
      if ((CATATONIA_FLAG (scm_timer)) == 0L)
	{
	  PostMessage ((scm_timer -> window),
		       (scm_timer -> catatonia_message),
		       ((WPARAM) 0),
		       ((LPARAM) 0));
	  (CATATONIA_FLAG (scm_timer)) = 1L;
	}
      (CATATONIA_COUNTER (scm_timer)) = 0L;
    }
}

static UINT __cdecl
win32_install_async_timer (void ** state_ptr,
			   unsigned long * base,
			   long memtop_off,
			   long int_code_off,
			   long int_mask_off,
			   unsigned long bit_mask,
			   long ctr_off,
			   unsigned long catatonia_message,
			   unsigned long interrupt_message,
			   HWND window)
{
  TIMECAPS tc;
  UINT wTimerRes;
  UINT msInterval = 50;
  UINT msTargetResolution = 50;
  struct win32_timer_closure_s * scm_timer;

  if ((timeGetDevCaps (&tc, sizeof (TIMECAPS))) != TIMERR_NOERROR)
    return (WIN32_ASYNC_TIMER_NONE);
  wTimerRes = (min ((max (tc.wPeriodMin, msTargetResolution)),
		    tc.wPeriodMax));
  if ((timeBeginPeriod (wTimerRes)) == TIMERR_NOCANDO)
    return (WIN32_ASYNC_TIMER_RESOLUTION);

  scm_timer = ((struct win32_timer_closure_s *)
	       (malloc (sizeof (struct win32_timer_closure_s))));

  if (scm_timer == ((struct win32_timer_closure_s *) NULL))
    return (WIN32_ASYNC_TIMER_NOMEM);

  scm_timer->timer_id = 0;
  scm_timer->base = base;
  scm_timer->memtop_off = memtop_off;
  scm_timer->int_code_off = int_code_off;
  scm_timer->int_mask_off = int_mask_off;
  scm_timer->bit_mask = bit_mask;
  scm_timer->ctr_off = ctr_off;
  scm_timer->catatonia_message = catatonia_message;
  scm_timer->interrupt_message = interrupt_message;
  scm_timer->window = window;

  if ((! (VirtualLock (((void *) scm_timer),
		       (sizeof (struct win32_timer_closure_s)))))
      || (! (VirtualLock (((void *) win32_nt_timer_tick),
			  (((char *) win32_flush_async_timer)
			   - ((char *) win32_nt_timer_tick))))))
  {
    win32_flush_async_timer ((void *) scm_timer);
    return (WIN32_ASYNC_TIMER_NOLOCK);
  }

  scm_timer->timer_id
    = (timeSetEvent (msInterval,
		     wTimerRes,
		     ((LPTIMECALLBACK) win32_nt_timer_tick),
		     ((DWORD) scm_timer),
		     TIME_PERIODIC));

  if (scm_timer->timer_id == 0)
  {
    win32_flush_async_timer ((void *) scm_timer);
    return (WIN32_ASYNC_TIMER_EXHAUSTED);
  }

  * state_ptr = ((void *) scm_timer);
  return (WIN32_ASYNC_TIMER_OK);
}

static void __cdecl
win32_flush_async_timer (void * state)
{
  struct win32_timer_closure_s * scm_timer
    = ((struct win32_timer_closure_s *) state);
  
  if (scm_timer == ((struct win32_timer_closure_s *) NULL))
    return;
  if (scm_timer->timer_id != 0)
    (void) timeKillEvent (scm_timer->timer_id);
  
  (void) VirtualUnlock (((void *) win32_nt_timer_tick),
			(((char *) win32_flush_async_timer)
			 - ((char *) win32_nt_timer_tick)));
  (void) VirtualUnlock (scm_timer, (sizeof (struct win32_timer_closure_s)));
  (void) free ((char *) scm_timer);
  return;
}

/* These are NOPs in this version. */

static BOOL __cdecl
win32_alloc_scheme_selectors (unsigned long base,
			      unsigned long size,
			      unsigned short * scheme_cs,
			      unsigned short * scheme_ds,
			      unsigned short * scheme_ss)
{
  return (FALSE);
}

static void __cdecl
win32_release_scheme_selectors (unsigned short scheme_cs,
				unsigned short scheme_ds,
				unsigned short scheme_ss)
{
  return;
}


void FAR WINAPI
install_win32_system_utilities (WIN32_SYSTEM_UTILITIES *utils)
{
#define EXPORT(field) utils->field = win32_##field
  EXPORT (under_win32s_p);
  EXPORT (allocate_heap);
  EXPORT (release_heap);
  EXPORT (lock_memory_area);
  EXPORT (unlock_memory_area);
  EXPORT (install_async_timer);
  EXPORT (flush_async_timer);
  EXPORT (alloc_scheme_selectors);
  EXPORT (release_scheme_selectors);
}
