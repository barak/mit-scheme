/* -*-C-*-

$Id: memmag.h,v 1.8 2000/12/05 21:23:45 cph Exp $

Copyright (c) 1993-2000 Massachusetts Institute of Technology

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

#ifdef __WIN32__
   extern void win32_allocate_registers (void);
   extern void win32_deallocate_registers (void);
#  define ALLOCATE_REGISTERS win32_allocate_registers
#  define DEALLOCATE_REGISTERS win32_deallocate_registers

#  include "ntscmlib.h"

   extern BOOL win32_under_win32s_p (void);

   extern char * NT_allocate_heap (unsigned long, unsigned long *);
   extern void NT_release_heap (char *, unsigned long);
#  define WIN32_ALLOCATE_HEAP NT_allocate_heap
#  define WIN32_RELEASE_HEAP NT_release_heap

   static unsigned long scheme_heap_handle;
#endif

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
