/* -*-C-*-

$Id: history.h,v 9.36 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

/* History maintenance data structures and support. */

/* The history consists of a "vertebra" which is a doubly linked ring,
   each entry pointing to a "rib".  The rib consists of a singly
   linked ring whose entries contain expressions and environments. */

#define HIST_RIB		0
#define HIST_NEXT_SUBPROBLEM	1
#define HIST_PREV_SUBPROBLEM	2
#define HIST_MARK		1

#define RIB_EXP			0
#define RIB_ENV			1
#define RIB_NEXT_REDUCTION	2
#define RIB_MARK		2

#define HISTORY_MARK_TYPE (UNMARKED_HISTORY_TYPE ^ MARKED_HISTORY_TYPE)
#define HISTORY_MARK_MASK (((unsigned long) HISTORY_MARK_TYPE) << DATUM_LENGTH)

#if ((UNMARKED_HISTORY_TYPE | HISTORY_MARK_TYPE) != MARKED_HISTORY_TYPE)
#include "error: Bad history types in types.h and history.h"
#endif

#define HISTORY_MARK(object) (object) |= HISTORY_MARK_MASK
#define HISTORY_UNMARK(object) (object) &=~ HISTORY_MARK_MASK
#define HISTORY_MARKED_P(object) (((object) & HISTORY_MARK_MASK) != 0)

/* Save_History places a restore history frame on the stack. Such a
   frame consists of a normal continuation frame plus a pointer to the
   stacklet on which the last restore history is located and the
   offset within that stacklet.  If the last restore history is in
   this stacklet then the history pointer is #F to signify this.  If
   there is no previous restore history then the history pointer is #F
   and the offset is 0. */

#define Save_History(Return_Code)					\
{									\
  STACK_PUSH								\
    ((Prev_Restore_History_Stacklet == NULL)				\
     ? SHARP_F								\
     : (MAKE_POINTER_OBJECT						\
	(TC_CONTROL_POINT, Prev_Restore_History_Stacklet)));		\
  STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (Prev_Restore_History_Offset));	\
  exp_register								\
    = (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, history_register));	\
  Store_Return (Return_Code);						\
  Save_Cont ();								\
  history_register							\
    = (OBJECT_ADDRESS (Get_Fixed_Obj_Slot (Dummy_History)));		\
}

/* History manipulation in the interpreter. */

#ifndef DISABLE_HISTORY

#define New_Subproblem(expression, environment)				\
{									\
  history_register							\
    = (OBJECT_ADDRESS (history_register [HIST_NEXT_SUBPROBLEM]));	\
  HISTORY_MARK (history_register [HIST_MARK]);				\
  {									\
    SCHEME_OBJECT * Rib							\
      = (OBJECT_ADDRESS (history_register [HIST_RIB]));			\
    HISTORY_MARK (Rib [RIB_MARK]);					\
    (Rib [RIB_ENV]) = (environment);					\
    (Rib [RIB_EXP]) = (expression);					\
  }									\
}

#define Reuse_Subproblem(expression, environment)			\
{									\
  SCHEME_OBJECT * Rib = (OBJECT_ADDRESS (history_register [HIST_RIB]));	\
  HISTORY_MARK (Rib [RIB_MARK]);					\
  (Rib [RIB_ENV]) = (environment);					\
  (Rib [RIB_EXP]) = (expression);					\
}

#define New_Reduction(expression, environment)				\
{									\
  fast SCHEME_OBJECT * Rib =						\
    (OBJECT_ADDRESS							\
     (FAST_MEMORY_REF ((history_register [HIST_RIB]),			\
		       RIB_NEXT_REDUCTION)));				\
  (history_register [HIST_RIB]) =					\
    (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, Rib));			\
  (Rib [RIB_ENV]) = (environment);					\
  (Rib [RIB_EXP]) = (expression);					\
  HISTORY_UNMARK (Rib [RIB_MARK]);					\
}

#define End_Subproblem()						\
{									\
  HISTORY_UNMARK (history_register [HIST_MARK]);			\
  history_register							\
    = (OBJECT_ADDRESS (history_register [HIST_PREV_SUBPROBLEM]));	\
}

#else /* DISABLE_HISTORY */

#define New_Subproblem(Expr, Env) {}
#define Reuse_Subproblem(Expr, Env) {}
#define New_Reduction(Expr, Env) {}
#define End_Subproblem() {}

#endif /* DISABLE_HISTORY */

/* History manipulation for the compiled code interface. */

#ifndef DISABLE_HISTORY

#define Compiler_New_Reduction()					\
{									\
  New_Reduction								\
    (SHARP_F,								\
     (MAKE_OBJECT (TC_RETURN_CODE, RC_POP_FROM_COMPILED_CODE)));	\
}

#define Compiler_New_Subproblem()					\
{									\
  New_Subproblem							\
    (SHARP_F,								\
     (MAKE_OBJECT (TC_RETURN_CODE, RC_POP_FROM_COMPILED_CODE)));	\
}

#define Compiler_End_Subproblem End_Subproblem

#else /* DISABLE_HISTORY */

#define Compiler_New_Reduction()
#define Compiler_New_Subproblem()
#define Compiler_End_Subproblem()

#endif /* DISABLE_HISTORY */
