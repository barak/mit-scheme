/* -*-C-*-

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/gc.h,v 9.29 1989/09/20 23:08:43 cph Exp $
 *
 * Garbage collection related macros of sufficient utility to be
 * included in all compilations.
 */

/* GC Types. */

#ifdef CMPGCFILE
#ifndef BAD_TYPES_LETHAL
#ifndef BAD_TYPES_INNOCUOUS
#define BAD_TYPES_INNOCUOUS
#endif /* BAD_TYPES_INNOCUOUS */
#endif /* BAD_TYPES_LETHAL */
#endif /* CMPGCFILE */

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
  (fprintf(stderr, "\nGC_Type_Code: Bad Type code = 0x%02x\n", TC),	\
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

#define GC_ENABLED_P()		(INTERRUPT_ENABLED_P(INT_GC))

#define GC_Check(Amount)						\
(((Amount + Free) >= MemTop) && (GC_ENABLED_P()))

#define Space_Before_GC()						\
((GC_ENABLED_P()) ?							\
 ((Free <= MemTop) ? (MemTop - Free) : 0) :				\
 (Heap_Top - Free))

#define Request_GC(Amount)						\
{									\
  REQUEST_INTERRUPT(INT_GC);						\
  GC_Space_Needed = Amount;						\
}

#define SET_MEMTOP(Addr)						\
{									\
  MemTop = Addr;							\
  COMPILER_SET_MEMTOP();						\
}

#define Set_Stack_Guard(Addr)						\
{									\
  Stack_Guard = Addr;							\
}
