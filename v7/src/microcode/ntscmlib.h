/* -*-C-*-

$Id: ntscmlib.h,v 1.2 1993/08/21 03:38:52 gjr Exp $

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

/* MIT Scheme under Windows system-dependent utilities. */

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

extern BOOL 
  win32_under_win32s_p (void);

extern char *
  win32_allocate_heap (unsigned long,			/* size */
		       unsigned long *);		/* handle */
extern void
  win32_release_heap (char *,				/* base */
		      unsigned long);			/* handle */

extern BOOL
  win32_lock_memory_area (void *,			/* area */
			  unsigned long);		/* size */
extern void
  win32_unlock_memory_area (void *,			/* area */
			    unsigned long);		/* size */

extern UINT
  win32_install_async_timer (unsigned long *,		/* regs */
			     unsigned long,		/* memtop off */
			     unsigned long,		/* int_code off */
			     unsigned long,		/* int_mask off */
			     unsigned long,		/* mask */
			     void **);			/* timer state */
extern void
  win32_flush_async_timer (void *);

extern BOOL
  win32_alloc_scheme_selectors (unsigned long,		/* base */
				unsigned long,		/* limit */
				unsigned short *,	/* cs */
				unsigned short *,	/* ds */
				unsigned short *);	/* ss */
extern void
  win32_release_scheme_selectors (unsigned short,	/* cs */
				  unsigned short,	/* ds */
				  unsigned short);	/* ss */
#endif /* W32SUT_16 */

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
typedef unsigned long SCM_ULONG;
typedef unsigned short SCM_BOOL;
typedef unsigned short SCM_SEL;

#define STRINGIFY(arg) #arg

#define NTW16LIB_DLL_NAME	"ntw16lib.dll"
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
  SCM_ULPTR base;		/* ->16 */
  SCM_ULONG memtop_off;		/* ->16 */
  SCM_ULONG int_code_off;	/* ->16 */
  SCM_ULONG int_mask_off;	/* ->16 */
  SCM_ULONG bit_mask;		/* ->16 */
  SCM_ULONG handle;		/* ->32 */
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
