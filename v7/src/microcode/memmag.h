/* -*-C-*-

$Id: memmag.h,v 1.2 1993/08/21 02:33:58 gjr Exp $

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

/* OS-dependent conditionalization of memory management stuff. */

#ifndef SCM_MEMMAG_H
#define SCM_MEMMAG_H

#ifdef WINNT

extern void winnt_allocate_registers (void);
extern void winnt_deallocate_registers (void);
#define ALLOCATE_REGISTERS winnt_allocate_registers
#define DEALLOCATE_REGISTERS winnt_deallocate_registers

#include "ntscmlib.h"

#ifdef WINNT_RAW_ADDRESSES

#define WIN32_ALLOCATE_HEAP win32_allocate_heap
#define WIN32_RELEASE_HEAP win32_release_heap

#else /* not WINNT_RAW_ADDRESSES */

extern unsigned long winnt_address_delta;
extern unsigned short
  Scheme_Code_Segment_Selector,
  Scheme_Data_Segment_Selector,
  Scheme_Stack_Segment_Selector;

unsigned long winnt_address_delta;
static unsigned long total_fudge;

#define SCM_FUDGE_1 0x1000L
#define SCM_FUDGE_2 0x10000L

static char * 
WIN32_ALLOCATE_HEAP (unsigned long size, unsigned long * handle)
{
  unsigned long actual_size, actual_fudge_1, actual_fudge_2;
  char * base, * virtual_base;

  if (! (win32_under_win32s_p ()))
  {
    actual_fudge_1 = 0;
    actual_fudge_2 = 0;
  }
  else
  {
    actual_fudge_1 = SCM_FUDGE_1;
    actual_fudge_2 = SCM_FUDGE_2;
  }
  total_fudge = (actual_fudge_1 + actual_fudge_2);
  actual_size = (size + total_fudge);

  base = (win32_allocate_heap (actual_size, handle));
  if (base == ((char *) NULL))
    return (base);

  virtual_base = (base + total_fudge);
  winnt_address_delta = (((unsigned long) base) + actual_fudge_1);
  if (! (win32_alloc_scheme_selectors (winnt_address_delta,
				       (size + actual_fudge_2),
				       &Scheme_Code_Segment_Selector,
				       &Scheme_Data_Segment_Selector,
				       &Scheme_Stack_Segment_Selector)))
    /* Let the higher-level code fail. */
    winnt_address_delta = 0L;
    
  return (virtual_base);
}

static void
WIN32_RELEASE_HEAP (char * area, unsigned long handle)
{
  if (winnt_address_delta != 0)
    win32_release_scheme_selectors (Scheme_Code_Segment_Selector,
				    Scheme_Data_Segment_Selector,
				    Scheme_Stack_Segment_Selector);  
  win32_release_heap ((area - total_fudge), handle);
  return;
}

#endif /* WINNT_RAW_ADDRESSES */

static unsigned long scheme_heap_handle;

#define HEAP_MALLOC(size) (WIN32_ALLOCATE_HEAP (size, &scheme_heap_handle))
#define HEAP_FREE(base) WIN32_RELEASE_HEAP (((char *) (base)), scheme_heap_handle)

#endif /* WINNT */

#ifndef HEAP_FREE
#  define HEAP_FREE free
#endif

#ifndef ALLOCATE_REGISTERS
#  define ALLOCATE_REGISTERS() do { } while (0)
#endif

#ifndef DEALLOCATE_REGISTERS
#  define DEALLOCATE_REGISTERS() do { } while (0)
#endif

#endif /* SCM_MEMMAG_H */
