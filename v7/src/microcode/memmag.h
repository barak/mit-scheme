/* -*-C-*-

$Id: memmag.h,v 1.1 1993/07/27 20:56:07 gjr Exp $

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

static unsigned long scheme_heap_handle;

extern char * win32_allocate_heap (unsigned long, unsigned long *);
extern void win32_release_heap (char *, unsigned long);
extern void winnt_allocate_registers (void);
extern void winnt_deallocate_registers (void);

#define HEAP_MALLOC(size) (win32_allocate_heap (size, &scheme_heap_handle))
#define HEAP_FREE(base) win32_release_heap (((char *) (base)), scheme_heap_handle)
#define ALLOCATE_REGISTERS winnt_allocate_registers
#define DEALLOCATE_REGISTERS winnt_deallocate_registers

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
