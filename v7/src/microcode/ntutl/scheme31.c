/* -*-C-*-

$Id: scheme31.c,v 1.1 1993/07/27 20:53:18 gjr Exp $

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
   Win32s (vs. true NT) version.
 */

#define W32SUT_32
#include <windows.h>
#include <w32sut.h>
#include "ntscmlib.h"
#include "ntw32lib.h"
#include <mmsystem.h>

#include <stdarg.h>

#ifndef STD_MSGBOX_STYLE
#  define STD_MSGBOX_STYLE MB_OK
#endif

static void
TellUser (char * format, ...)
{
  va_list arg_ptr;
  char buffer[128];
  
  va_start (arg_ptr, format);
  wvsprintf (&buffer[0], format, arg_ptr);
  MessageBox (NULL,
	      ((LPCSTR) &buffer[0]),
	      ((LPCSTR) "MIT Scheme Win32 Notification"),
	      STD_MSGBOX_STYLE);
  va_end (arg_ptr);
  return;
}

static UT32PROC call_16_bit_code = NULL;

DWORD WINAPI
called_from_16_bit_code (LPVOID buff, DWORD tag)
{
  return (0L);
}

BOOL WINAPI
ntw32lib_dllinit (HANDLE self, DWORD reason, LPVOID reserved)
{
  static counter = 0;

  switch (reason)
  {
    case DLL_PROCESS_ATTACH:
#ifndef DUMMY
      if (counter++ == 0)
	return ((UTRegister (self,
			     NTW16LIB_DLL_NAME,
			     "ntw16lib_init",
			     "ntw16lib_handler",
			     & call_16_bit_code,
			     called_from_16_bit_code,
			     NULL))
#if 0
		&& crt_result
#endif
		);
#endif /* DUMMY */
      break;

    case DLL_THREAD_ATTACH:
      break;    

    case DLL_THREAD_DETACH:
      break;    

    case DLL_PROCESS_DETACH:
#ifndef DUMMY
      if (--counter == 0)
	UTUnRegister (self);
#endif
      break;

    default:
      return (FALSE);
  }
  return (TRUE);
}

BOOL
win32_under_win32s_p (void)
{
  return (TRUE);
}

#ifdef DUMMY
#  define BASE_ADDRESS 0x1000
#endif

char *
win32_allocate_heap (unsigned long size, unsigned long * handle)
{
#ifndef DUMMY
  struct ntw32lib_malloc_s param;
  LPVOID translation[1];
  
  param.size = ((SCM_ULONG) size);
  translation[0] = ((LPVOID) NULL);

  if ((* call_16_bit_code) (&param, NTW32LIB_MALLOC, &translation[0]))
  {
    * handle = param.handle;
    return ((char *) (param.area));
  }
  else
  {
    * handle = 0L;
    return ((char *) NULL);
  }
#elif defined(USE_VIRTUAL_ALLOC)
  unsigned long ctr;
  unsigned long addr;
  unsigned long base;

  for (addr = BASE_ADDRESS, ctr = 0; addr != 0; addr += BASE_ADDRESS, ctr++)
  {
    base = (VirtualAlloc (((LPVOID) addr),
			  ((DWORD) size),
			  ((DWORD) (MEM_RESERVE | MEM_COMMIT)),
			  ((DWORD) PAGE_READWRITE)));
    if (base != NULL)
      break;
  }

  if (base != NULL)
    TellUser ("Succeeded allocating address 0x%lx", base);
  else
    TellUser ("Failed allocating %ld bytes after %ld attempts",
	      size, ctr);

  * handle = size;
  return ((char *) base);
#else
  return ((char *) NULL);
#endif
}

void
win32_release_heap (char * area, unsigned long handle)
{
#ifndef DUMMY
  struct ntw32lib_free_s param;
  LPVOID translation[2];
  
  param.area = ((SCM_VDPTR) area);
  param.handle = ((SCM_ULONG) handle);
  translation[0] = ((LPVOID) & param.area);
  translation[1] = ((LPVOID) NULL);
  (* call_16_bit_code) (&param, NTW32LIB_MALLOC, &translation[0]);
  return;
#elif defined(USE_VIRTUAL_ALLOC)
  VirtualFree (((LPVOID) area),
	       ((DWORD) handle),
	       ((DWORD) MEM_DECOMMIT));
  return;
#else
  return;
#endif
}

BOOL
win32_lock_memory_area (LPVOID area, unsigned long size)
{
#ifdef DUMMY
  return (TRUE);
#else
  struct ntw32lib_vlock_s param;
  LPVOID translation[2];

  param.area = ((SCM_VDPTR) area);
  param.size = ((SCM_ULONG) size);
  translation[0] = ((LPVOID) & param.area);
  translation[1] = ((LPVOID) NULL);

  return ((BOOL) ((* call_16_bit_code)
		  (&param, NTW32LIB_VIRTUAL_LOCK, &translation[0])));
#endif
}

void
win32_unlock_memory_area (LPVOID area, unsigned long size)
{
#ifdef DUMMY
  return;
#else
  struct ntw32lib_vulock_s param;
  LPVOID translation[2];

  param.area = ((SCM_VDPTR) area);
  param.size = ((SCM_ULONG) size);
  translation[0] = ((LPVOID) & param.area);
  translation[1] = ((LPVOID) NULL);

  (* call_16_bit_code) (&param, NTW32LIB_VIRTUAL_UNLOCK, &translation[0]);
  return;
#endif
}

UINT
win32_install_async_timer (unsigned long * intcode_addr,
			   unsigned long * intmask_addr,
			   unsigned long * memtop_addr,
			   unsigned long bit_mask,
			   void ** timer_state)
{
#ifdef DUMMY
  return (0);
#else
  struct ntw32lib_itimer_s param;
  LPVOID translation[4];

  param.intcode_addr = ((SCM_ULPTR) intcode_addr);
  param.intmask_addr = ((SCM_ULPTR) intmask_addr);
  param.memtop_addr = ((SCM_ULPTR) memtop_addr);
  param.bit_mask = ((SCM_ULONG) bit_mask);

  translation[0] = ((LPVOID) & param.intcode_addr);
  translation[1] = ((LPVOID) & param.intmask_addr);
  translation[2] = ((LPVOID) & param.memtop_addr);
  translation[3] = ((LPVOID) NULL);

  * timer_state = ((void *) NULL);
  return ((UINT) ((* call_16_bit_code)
		  (& param, NTW32LIB_INSTALL_TIMER, &translation[0])));
#endif
}

void
win32_flush_async_timer (void * timer_state)
{
#ifdef DUMMY
  return;
#else
  struct ntw32lib_ftimer_s param;
  LPVOID translation[1];
  
  translation[0] = ((LPVOID) NULL);
  (* call_16_bit_code) (& param, NTW32LIB_FLUSH_TIMER, &translation[0]);
  return;
#endif  
}
