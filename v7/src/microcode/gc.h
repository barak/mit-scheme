/* -*-C-*-

$Id: gc.h,v 9.35 2002/11/20 19:46:08 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* 
 * Garbage collection related macros of sufficient utility to be
 * included in all compilations.
 */

/* GC Types. */

#ifdef HAS_COMPILER_SUPPORT
#ifndef BAD_TYPES_LETHAL
#ifndef BAD_TYPES_INNOCUOUS
#define BAD_TYPES_INNOCUOUS
#endif /* BAD_TYPES_INNOCUOUS */
#endif /* BAD_TYPES_LETHAL */
#endif /* HAS_COMPILER_SUPPORT */

#ifdef BAD_TYPES_INNOCUOUS
#ifdef BAD_TYPES_LETHAL
#include "error: gc.h: BAD_TYPES both lethal and innocuous"
#endif /* BAD_TYPES_LETHAL */
#else /* not BAD_TYPES_INNOCUOUS */
#ifndef BAD_TYPES_LETHAL
#define BAD_TYPES_LETHAL
#endif /* BAD_TYPES_LETHAL */
#endif /* BAD_TYPES_INNOCUOUS */

#define GC_Non_Pointer 			0
#define GC_Cell				1
#define GC_Pair				2
#define GC_Triple			3
#define GC_Hunk3			3
#define GC_Quadruple    		4
#define GC_Hunk4        		4
#define GC_Undefined			-1 /* Undefined types */
#define GC_Special			-2 /* Internal GC types */
#define GC_Vector			-3
#define GC_Compiled			-4

#ifdef BAD_TYPES_INNOCUOUS
#define INVALID_TYPE_CODE(TC)		GC_Undefined

#else /* not BAD_TYPES_INNOCUOUS */

/* Some C compilers complain if the expression below does not yield
   a value, and Microcode_Termination yields void.
 */

#define INVALID_TYPE_CODE(TC)						\
  (outf_fatal  ("\nGC_Type_Code: Bad Type code = 0x%02x\n", TC),	\
   Microcode_Termination(TERM_INVALID_TYPE_CODE),			\
   GC_Undefined)

#endif /* BAD_TYPES_INNOCUOUS */

#define GC_Type_Code(TC)						\
 ((GC_Type_Map[TC] != GC_Undefined)	?				\
  GC_Type_Map[TC]			:				\
  (INVALID_TYPE_CODE(TC)))

#define GC_Type(Object)			GC_Type_Code(OBJECT_TYPE (Object))

#define GC_Type_Non_Pointer(Object)	(GC_Type(Object) == GC_Non_Pointer)
#define GC_Type_Cell(Object)		(GC_Type(Object) == GC_Cell)
#define GC_Type_List(Object)		(GC_Type(Object) == GC_Pair)
#define GC_Type_Triple(Object)		(GC_Type(Object) == GC_Triple)
#define GC_Type_Quadruple(Object)	(GC_Type(Object) == GC_Quadruple)
#define GC_Type_Undefined(Object)	(GC_Type(Object) == GC_Undefined)
#define GC_Type_Special(Object)		(GC_Type(Object) == GC_Special)
#define GC_Type_Vector(Object)		(GC_Type(Object) == GC_Vector)
#define GC_Type_Compiled(Object)	(GC_Type(Object) == GC_Compiled)

/* Overflow detection, various cases */

#define GC_ENABLED_P() (INTERRUPT_ENABLED_P (INT_GC))

#define GC_Check(Amount)						\
  (((Amount + Free) >= MemTop) && (GC_ENABLED_P ()))

#define Space_Before_GC()						\
  ((GC_ENABLED_P ())							\
   ? ((Free <= MemTop) ? (MemTop - Free) : 0)				\
   : (Heap_Top - Free))

#define Request_GC(Amount)						\
{									\
  REQUEST_INTERRUPT (INT_GC);						\
  GC_Space_Needed = Amount;						\
}

#define SET_MEMTOP(addr)						\
{									\
  MemTop = (addr);							\
  COMPILER_SETUP_INTERRUPT ();						\
}

#define SET_STACK_GUARD(addr)						\
{									\
  Stack_Guard = (addr);							\
  COMPILER_SETUP_INTERRUPT ();						\
}
