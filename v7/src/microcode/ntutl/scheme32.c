/* -*-C-*-

$Id: scheme32.c,v 1.14 1997/04/02 07:44:09 cph Exp $

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
};

/* Setting this to non-zero requests the timer thread to exit.  */
static int exit_timer_thread;

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
  exit_timer_thread = 0;
  if (_beginthreadex (0, 0, timer_thread_proc, scm_timer, 0, (&id)))
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
      exit_timer_thread = 1;
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
  while (!exit_timer_thread)
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
