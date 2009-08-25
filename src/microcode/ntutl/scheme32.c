/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

/* MIT/GNU Scheme under Windows system utiltities DLL source.
   True Win32 (vs. Win32s) version 
 */

#include "ntscmlib.h"
#include <stdlib.h>
#include <process.h>

static void __cdecl win32_flush_async_timer (void *);
static unsigned int WINAPI timer_thread_proc (void *);

#ifndef WIN32_TIMER_INTERVAL
#define WIN32_TIMER_INTERVAL 50
#endif

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

/* Asynchronous timer interrupt based on auxiliary thread.  */

struct win32_timer_closure_s
{
  unsigned long interval;	/* timer interval in milliseconds */
  unsigned long * base;		/* register-block base address */
  long memtop_off;		/* offset to memtop register */
  long int_code_off;		/* offset to int_code register */
  long int_mask_off;		/* offset to int_mask register */
  unsigned long bit_mask;	/* interrupt bits to signal */
  long ctr_off;			/* offset to catatonia-counter register */
  unsigned long catatonia_message; /* message to send for catatonia */
  unsigned long interrupt_message; /* message to send for interrupt */
  HWND window;			/* window to send the messages to */
  void (*grab_int_regs) (void);	/* grab interrupt registers */
  void (*release_int_regs) (void); /* release interrupt registers */
  HANDLE thread_handle;		/* handle of timer thread */
  int exit_thread;		/* set this true to terminate thread */
};

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
			   HWND window,
			   void (*grab_int_regs) (void),
			   void (*release_int_regs) (void))
{
  struct win32_timer_closure_s * scm_timer;
  unsigned int id;

  scm_timer
    = ((struct win32_timer_closure_s *)
       (malloc (sizeof (struct win32_timer_closure_s))));
  if (scm_timer == 0)
    return (WIN32_ASYNC_TIMER_NOMEM);
  (scm_timer -> interval) = WIN32_TIMER_INTERVAL;
  (scm_timer -> base) = base;
  (scm_timer -> memtop_off) = memtop_off;
  (scm_timer -> int_code_off) = int_code_off;
  (scm_timer -> int_mask_off) = int_mask_off;
  (scm_timer -> bit_mask) = bit_mask;
  (scm_timer -> ctr_off) = ctr_off;
  (scm_timer -> catatonia_message) = catatonia_message;
  (scm_timer -> interrupt_message) = interrupt_message;
  (scm_timer -> window) = window;
  (scm_timer -> grab_int_regs) = grab_int_regs;
  (scm_timer -> release_int_regs) = release_int_regs;
  (scm_timer -> exit_thread) = 0;
  (scm_timer -> thread_handle)
    = ((HANDLE)
       (_beginthreadex (0, 0x2000, timer_thread_proc, scm_timer, 0, (&id))));
  if (scm_timer -> thread_handle)
    {
      (*state_ptr) = scm_timer;
      return (WIN32_ASYNC_TIMER_OK);
    }
  else
    {
      win32_flush_async_timer (scm_timer);
      return (WIN32_ASYNC_TIMER_EXHAUSTED);
    }
}

static void __cdecl
win32_flush_async_timer (void * state)
{
  if (state != 0)
    {
      struct win32_timer_closure_s * scm_timer = state;
      if (scm_timer -> thread_handle)
	{
	  (scm_timer -> exit_thread) = 1;
	  (void) WaitForSingleObject ((scm_timer -> thread_handle), INFINITE);
	}
      (void) free (state);
    }
}

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

static unsigned int WINAPI
timer_thread_proc (void * envptr)
{
  struct win32_timer_closure_s * scm_timer = envptr;
  while (! (scm_timer -> exit_thread))
    {
      Sleep (scm_timer -> interval);
      (* (scm_timer -> grab_int_regs)) ();
      (INTERRUPT_CODE (scm_timer)) |= (scm_timer -> bit_mask);
      if (((INTERRUPT_CODE (scm_timer)) & (INTERRUPT_MASK (scm_timer))) != 0L)
	{
	  (MEMTOP (scm_timer)) = ((unsigned long) -1L);
	  /* Post an interrupt message to the window.  This forces it to
	     wake up and exit MsgWaitForMultipleObjects if needed.  */
	  (* (scm_timer -> release_int_regs)) ();
	  PostMessage ((scm_timer -> window),
		       (scm_timer -> interrupt_message),
		       ((WPARAM) 0),
		       ((LPARAM) 0));
	}
      else
	(* (scm_timer -> release_int_regs)) ();
      (CATATONIA_COUNTER (scm_timer)) += 1L;
      if (((CATATONIA_COUNTER (scm_timer)) > (CATATONIA_LIMIT (scm_timer)))
	  && ((CATATONIA_LIMIT (scm_timer)) != 0L))
	{
	  if ((CATATONIA_FLAG (scm_timer)) == 0L)
	    {
	      (CATATONIA_FLAG (scm_timer)) = 1L;
	      PostMessage ((scm_timer -> window),
			   (scm_timer -> catatonia_message),
			   ((WPARAM) 0),
			   ((LPARAM) 0));
	    }
	  (CATATONIA_COUNTER (scm_timer)) = 0L;
	}
    }
  return (0);
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
