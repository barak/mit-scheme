/* -*-C-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

/* MIT/GNU Scheme under Windows system-dependent utilities. */

#ifndef SCM_NTLIB_H

#include <windows.h>

/* Timer installation status codes. */

#define WIN32_ASYNC_TIMER_OK		0
#define WIN32_ASYNC_TIMER_NONE		1
#define WIN32_ASYNC_TIMER_EXHAUSTED	2
#define WIN32_ASYNC_TIMER_RESOLUTION	3
#define WIN32_ASYNC_TIMER_NOLOCK	4
#define WIN32_ASYNC_TIMER_NOMEM		5
#define WIN32_ASYNC_TIMER_NOLDT		6

#ifndef W32SUT_16

/* Exports to Scheme */

typedef struct {

  BOOL (__cdecl *under_win32s_p) ();

  char *
    (__cdecl *allocate_heap) (unsigned long,		/* size */
			      unsigned long *);		/* handle */
  void
    (__cdecl *release_heap) (char *,			/* base */
		     unsigned long);			/* handle */

  BOOL
    (__cdecl *lock_memory_area) (void *,		/* area */
				 unsigned long);	/* size */
  void
    (__cdecl *unlock_memory_area) (void *,		/* area */
				   unsigned long);	/* size */

  UINT
    (__cdecl *install_async_timer)
      (void **,			/* timer state */
       unsigned long *,		/* regs */
       long,			/* memtop_off */
       long,			/* int_code_off */
       long,			/* int_mask_off */
       unsigned long,		/* mask */
       long,			/* ctr_off */
       unsigned long,		/* catatonia_message */
       unsigned long,		/* interrupt_message */
       HWND,			/* window */
       void (*) (void),		/* procedure to grab int regs */
       void (*) (void)		/* procedure to release int regs */
       );

  void
    (__cdecl *flush_async_timer) (void *);

  BOOL
    (__cdecl *alloc_scheme_selectors) (unsigned long,		/* base */
				       unsigned long,		/* limit */
				       unsigned short *,	/* cs */
				       unsigned short *,	/* ds */
				       unsigned short *);	/* ss */

  void
    (__cdecl *release_scheme_selectors) (unsigned short,	/* cs */
					 unsigned short,	/* ds */
					 unsigned short);	/* ss */

} WIN32_SYSTEM_UTILITIES;

extern WIN32_SYSTEM_UTILITIES win32_system_utilities;
#endif /* not W32SUT_16 */

#if defined(W32SUT_32) || defined(W32SUT_16)

/* Under Win3.1, there is a 16-bit "universal thunk".
   The following define the communications protocol.
 */

#include <stdarg.h>
#include <w32sut.h>

#ifdef W32SUT_16
  typedef unsigned long FAR * SCM_ULPTR;
#else
  typedef unsigned long * SCM_ULPTR;
#endif

typedef LPVOID SCM_VDPTR;
typedef long SCM_LONG;
typedef unsigned long SCM_ULONG;
typedef unsigned short SCM_BOOL;
typedef unsigned short SCM_SEL;

#define STRINGIFY(arg) #arg

#define NTW16LIB_DLL_NAME	"scheme16.dll"
#define NTW16LIB_DLL_INIT	ntw16lib_init
#define NTW16LIB_DLL_ENTRY	ntw16lib_handler

#define NTW32LIB_RESERVED		0

#define NTW32LIB_VIRTUAL_LOCK		1
struct ntw32lib_vlock_s
{
  SCM_VDPTR area;		/* ->16 */
  SCM_ULONG size;		/* ->16 */
};

#define NTW32LIB_VIRTUAL_UNLOCK		2
struct ntw32lib_vulock_s
{
  SCM_VDPTR area;		/* ->16 */
  SCM_ULONG size;		/* ->16 */
};

#define NTW32LIB_INSTALL_TIMER		3
struct ntw32lib_itimer_s
{
  SCM_ULONG handle;		/* ->32 */
  SCM_ULPTR base;		/* ->16 */
  SCM_LONG memtop_off;		/* ->16 */
  SCM_LONG int_code_off;	/* ->16 */
  SCM_LONG int_mask_off;	/* ->16 */
  SCM_ULONG bit_mask;		/* ->16 */
  SCM_LONG ctr_off;		/* ->16 */
  SCM_ULONG catatonia_message;	/* ->16 */
  SCM_ULONG interrupt_message;	/* ->16 */
  SCM_ULONG window;		/* ->16 */
};

#define NTW32LIB_FLUSH_TIMER		4
struct ntw32lib_ftimer_s
{
  SCM_ULONG handle;		/* ->16 */
};

#define NTW32LIB_ALLOC_SELECTORS	5
struct ntw32lib_selalloc_s
{
  SCM_ULONG base;		/* ->16 */
  SCM_ULONG limit;		/* ->16 */
  SCM_SEL cs32;			/* ->16 */
  SCM_SEL ds32;			/* ->16 */
  SCM_SEL cs;			/* ->32 */
  SCM_SEL ds;			/* ->32 */
  SCM_SEL ss;			/* ->32 */
};

#define NTW32LIB_FREE_SELECTORS		6
struct ntw32lib_selfree_s
{
  SCM_SEL cs32;			/* ->16 */
  SCM_SEL ds32;			/* ->16 */
  SCM_SEL cs;			/* ->16 */
  SCM_SEL ds;			/* ->16 */
  SCM_SEL ss;			/* ->16 */
};

#endif /* defined(W32SUT_32) || defined(W32SUT_16) */

#endif /* SCM_NTLIB_H */
