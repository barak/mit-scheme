/* -*-C-*-

$Id: scheme31.c,v 1.8 1997/01/01 22:58:17 cph Exp $

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
   Win32s (vs. true NT) version.
 */

#define W32SUT_32
#include "ntscmlib.h"

/* Utilities */

#ifdef CL386

unsigned short
getCS (void)
{
  _asm	mov	ax,cs
}

unsigned short
getDS (void)
{
  _asm	mov	ax,ds
}

unsigned short
getSS (void)
{
  _asm	mov	ax,ss
}

#else /* not CL386 */
#ifdef __WATCOMC__

extern unsigned short getCS (void);
#pragma aux getCS = "mov ax,cs" value [ax];

extern unsigned short getDS (void);
#pragma aux getDS = "mov ax,ds" value [ax];

extern unsigned short getSS (void);
#pragma aux getSS = "mov ax,ss" value [ax];

#endif /* __WATCOMC__ */
#endif /* not CL386 */

static UT32PROC call_16_bit_code = NULL;

DWORD WINAPI
called_from_16_bit_code (LPVOID buff, DWORD tag)
{
  return (0L);
}

BOOL WINAPI
ntw32lib_dllinit (HANDLE self, DWORD reason, LPVOID reserved)
{
  BOOL crt_result = TRUE;
  static counter = 0;

  switch (reason)
  {
    case DLL_PROCESS_ATTACH:
      if (counter++ == 0)
	return ((UTRegister (self,
			     NTW16LIB_DLL_NAME,
			     "ntw16lib_init",
			     "ntw16lib_handler",
			     & call_16_bit_code,
			     called_from_16_bit_code,
			     NULL))
		&& crt_result
		);
      break;

    case DLL_THREAD_ATTACH:
      break;    

    case DLL_THREAD_DETACH:
      break;    

    case DLL_PROCESS_DETACH:
      if (--counter == 0)
	UTUnRegister (self);
      break;

    default:
      return (FALSE);
  }
  return (crt_result);
}

static BOOL
win32_under_win32s_p (void)
{
  return (TRUE);
}

static char *
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

static void
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

static BOOL
win32_lock_memory_area (LPVOID area, unsigned long size)
{
  struct ntw32lib_vlock_s param;
  LPVOID translation[2];

  param.area = ((SCM_VDPTR) area);
  param.size = ((SCM_ULONG) size);
  translation[0] = ((LPVOID) & param.area);
  translation[1] = ((LPVOID) NULL);

  return ((BOOL) ((* call_16_bit_code)
		  (&param, NTW32LIB_VIRTUAL_LOCK, &translation[0])));
}

static void
win32_unlock_memory_area (LPVOID area, unsigned long size)
{
  struct ntw32lib_vulock_s param;
  LPVOID translation[2];

  param.area = ((SCM_VDPTR) area);
  param.size = ((SCM_ULONG) size);
  translation[0] = ((LPVOID) & param.area);
  translation[1] = ((LPVOID) NULL);

  (* call_16_bit_code) (&param, NTW32LIB_VIRTUAL_UNLOCK, &translation[0]);
  return;
}

static UINT
win32_install_async_timer (void ** state_ptr,
			   unsigned long * base,
			   long memtop_off,
			   long int_code_off,
			   long int_mask_off,
			   unsigned long bit_mask,
			   long ctr_off,
			   unsigned long ctr_message,
			   unsigned long interrupt_message,
			   HWND window)
{
  struct ntw32lib_itimer_s param;
  LPVOID translation[2];
  UINT result;

  param.base = ((SCM_ULPTR) base);
  param.memtop_off = ((SCM_LONG) memtop_off);
  param.int_code_off = ((SCM_LONG) int_code_off);
  param.int_mask_off = ((SCM_LONG) int_mask_off);
  param.bit_mask = ((SCM_ULONG) bit_mask);
  param.ctr_off = ((SCM_LONG) ctr_off);
  param.ctr_message = ((SCM_ULONG) ctr_message);
  param.interrupt_message = ((SCM_ULONG) interrupt_message);
  param.window = ((SCM_ULONG) window);

  translation[0] = ((LPVOID) & param.base);
  translation[1] = ((LPVOID) NULL);

  result = ((UINT) ((* call_16_bit_code)
		    (& param, NTW32LIB_INSTALL_TIMER, &translation[0])));
  * state_ptr = ((void *) param.handle);
  return (result);
}

static void
win32_flush_async_timer (void * timer_state)
{
  struct ntw32lib_ftimer_s param;
  LPVOID translation[1];
  
  param.handle = ((SCM_ULONG) timer_state);
  translation[0] = ((LPVOID) NULL);
  (* call_16_bit_code) (& param, NTW32LIB_FLUSH_TIMER, &translation[0]);
  return;
}

#define I386_PAGE_SIZE 0x1000

static BOOL
win32_alloc_scheme_selectors (unsigned long base,
			      unsigned long size,
			      unsigned short * scheme_cs,
			      unsigned short * scheme_ds,
			      unsigned short * scheme_ss)
{
  BOOL result;
  struct ntw32lib_selalloc_s param;
  LPVOID translation[1];

  param.base = base;
  param.limit = ((size + (I386_PAGE_SIZE - 1)) & (~ (I386_PAGE_SIZE - 1)));
  param.cs32 = (getCS ());
  param.ds32 = (getDS ());
  param.cs = 0;
  param.ds = 0;
  param.ss = 0;
  translation[0] = ((LPVOID) NULL);
  result = ((BOOL)
	    ((* call_16_bit_code) (& param, NTW32LIB_ALLOC_SELECTORS,
				   &translation[0])));
  * scheme_cs = param.cs;
  * scheme_ds = param.ds;
  * scheme_ss = param.ss;
  return (result);
}

static void
win32_release_scheme_selectors (unsigned short scheme_cs,
				unsigned short scheme_ds,
				unsigned short scheme_ss)
{
  struct ntw32lib_selfree_s param;
  LPVOID translation[1];

  param.cs32 = (getCS ());
  param.ds32 = (getDS ());
  param.cs = scheme_cs;
  param.ds = scheme_ds;
  param.ss = scheme_ss;
  translation[0] = ((LPVOID) NULL);
  (* call_16_bit_code) (& param, NTW32LIB_FREE_SELECTORS, &translation[0]);
  return;
}



void
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
