/* -*-C-*-

$Id: memmag.h,v 1.7 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1993-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* OS-dependent conditionalization of memory management stuff. */

#ifndef SCM_MEMMAG_H
#define SCM_MEMMAG_H

#ifdef WINNT

extern void winnt_allocate_registers (void);
extern void winnt_deallocate_registers (void);
#define ALLOCATE_REGISTERS winnt_allocate_registers
#define DEALLOCATE_REGISTERS winnt_deallocate_registers

#include "ntscmlib.h"

extern BOOL win32_under_win32s_p (void);
extern char * NT_allocate_heap (unsigned long, unsigned long *);
extern void NT_release_heap (char *, unsigned long);

#ifdef WINNT_RAW_ADDRESSES

#define WIN32_ALLOCATE_HEAP NT_allocate_heap
#define WIN32_RELEASE_HEAP NT_release_heap

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

  base = (NT_allocate_heap (actual_size, handle));
  if (base == ((char *) NULL))
    return (base);

  virtual_base = (base + total_fudge);
  winnt_address_delta = (((unsigned long) base) + actual_fudge_1);
  if (! (win32_system_utilities.alloc_scheme_selectors
	 (winnt_address_delta,
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
    win32_system_utilities.release_scheme_selectors
      (Scheme_Code_Segment_Selector,
       Scheme_Data_Segment_Selector,
       Scheme_Stack_Segment_Selector);  
  NT_release_heap ((area - total_fudge), handle);
}

#endif /* WINNT_RAW_ADDRESSES */

static unsigned long scheme_heap_handle;

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
