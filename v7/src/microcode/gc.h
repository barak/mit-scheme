/* Emacs, -*-C-*-an't you guess? */

/****************************************************************
*                                                               *
*                         Copyright (c) 1986                    *
*               Massachusetts Institute of Technology           *
*                                                               *
* This material was developed by the Scheme project at the      *
* Massachusetts Institute of Technology, Department of          *
* Electrical Engineering and Computer Science.  Permission to   *
* copy this software, to redistribute it, and to use it for any *
* purpose is granted, subject to the following restrictions and *
* understandings.                                               *
*                                                               *
* 1. Any copy made of this software must include this copyright *
* notice in full.                                               *
*                                                               *
* 2. Users of this software agree to make their best efforts (a)*
* to return to the MIT Scheme project any improvements or       *
* extensions that they make, so that these may be included in   *
* future releases; and (b) to inform MIT of noteworthy uses of  *
* this software.                                                *
*                                                               *
* 3.  All materials developed as a consequence of the use of    *
* this software shall duly acknowledge such use, in accordance  *
* with the usual standards of acknowledging credit in academic  *
* research.                                                     *
*                                                               *
* 4. MIT has made no warrantee or representation that the       *
* operation of this software will be error-free, and MIT is     *
* under no obligation to provide any services, by way of        *
* maintenance, update, or otherwise.                            *
*                                                               *
* 5.  In conjunction with products arising from the use of this *
* material, there shall be no use of the name of the            *
* Massachusetts Institute of Technology nor of any adaptation   *
* thereof in any advertising, promotional, or sales literature  *
* without prior written consent from MIT in each case.          *
*                                                               *
****************************************************************/

/* File: GC.H
 *
 * Garbage collection related macros of sufficient utility to be
 * included in all compilations.
 */

/* GC Types. */

#define GC_Non_Pointer 	0
#define GC_Cell		1
#define GC_Pair		2
#define GC_Triple	3
#define GC_Hunk3	3
#define GC_Quadruple    4
#define GC_Hunk4        4
#define GC_Undefined	-1 /* Undefined types */
#define GC_Special	-2 /* Internal GC types */
#define GC_Vector	-3
#define GC_Compiled	-4

#define GC_Type_Code(TC)					\
 ((GC_Type_Map[TC] != GC_Undefined)	?			\
  GC_Type_Map[TC]			:			\
  (fprintf(stderr, "Bad Type code = 0x%02x\n", TC),		\
   Invalid_Type_Code(), GC_Undefined))

#define GC_Type(Object)			GC_Type_Code(Safe_Type_Code(Object))

#define GC_Type_Non_Pointer(Object)	(GC_Type(Object) == GC_Non_Pointer)
#define GC_Type_Cell(Object)		(GC_Type(Object) == GC_Cell)
#define GC_Type_List(Object)		(GC_Type(Object) == GC_Pair)
#define GC_Type_Triple(Object)		(GC_Type(Object) == GC_Triple)
#define GC_Type_Quadruple(Object)	(GC_Type(Object) == GC_Quadruple)
#define GC_Type_Undefined(Object)	(GC_Type(Object) == GC_Undefined)
#define GC_Type_Special(Object)		(GC_Type(Object) == GC_Special)
#define GC_Type_Vector(Object)		(GC_Type(Object) == GC_Vector)
#define GC_Type_Compiled(Object)	(GC_Type(Object) == GC_Compiled)

#define Invalid_Type_Code()					\
  Microcode_Termination(TERM_INVALID_TYPE_CODE)

/* Overflow detection, various cases */

#define GC_Check(Amount)	(((Amount+Free) >= MemTop) &&	\
                                 ((IntEnb & INT_GC) != 0))

#define Space_Before_GC()	(((IntEnb & INT_GC) != 0) ?	\
				 (MemTop - Free) :		\
				 (Heap_Top - Free))

#define Request_Interrupt(code)					\
{								\
  IntCode |= (code);						\
  New_Compiler_MemTop();					\
}

#define Request_GC(Amount)					\
{								\
  Request_Interrupt( INT_GC);					\
  GC_Space_Needed = Amount;					\
}

#define Set_Mem_Top(Addr)	\
  MemTop = Addr; New_Compiler_MemTop()

#define Set_Stack_Guard(Addr) Stack_Guard = Addr

#define New_Compiler_MemTop()	\
  Registers[REGBLOCK_MEMTOP] =  \
    ((IntCode & IntEnb)==0) ? ((Pointer) MemTop) : ((Pointer) -1)
