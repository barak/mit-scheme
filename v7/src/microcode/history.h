/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/history.h,v 9.26 1990/06/20 17:40:53 cph Exp $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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
#define HISTORY_MARK_MASK (HISTORY_MARK_TYPE << DATUM_LENGTH)

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
  Store_Expression							\
    (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, History));		\
  Store_Return (Return_Code);						\
  Save_Cont ();								\
  History = (OBJECT_ADDRESS (Get_Fixed_Obj_Slot (Dummy_History)));	\
}

/* History manipulation in the interpreter. */

#ifdef COMPILE_HISTORY

#define New_Subproblem(expression, environment)				\
{									\
  History = (OBJECT_ADDRESS (History [HIST_NEXT_SUBPROBLEM]));		\
  HISTORY_MARK (History [HIST_MARK]);					\
  {									\
    fast SCHEME_OBJECT * Rib = (OBJECT_ADDRESS (History [HIST_RIB]));	\
    HISTORY_MARK (Rib [RIB_MARK]);					\
    (Rib [RIB_ENV]) = (environment);					\
    (Rib [RIB_EXP]) = (expression);					\
  }									\
}

#define Reuse_Subproblem(expression, environment)			\
{									\
  fast SCHEME_OBJECT * Rib = (OBJECT_ADDRESS (History [HIST_RIB]));	\
  HISTORY_MARK (Rib [RIB_MARK]);					\
  (Rib [RIB_ENV]) = (environment);					\
  (Rib [RIB_EXP]) = (expression);					\
}

#define New_Reduction(expression, environment)				\
{									\
  fast SCHEME_OBJECT * Rib =						\
    (OBJECT_ADDRESS							\
     (FAST_MEMORY_REF ((History [HIST_RIB]), RIB_NEXT_REDUCTION)));	\
  (History [HIST_RIB]) =						\
    (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, Rib));			\
  (Rib [RIB_ENV]) = (environment);					\
  (Rib [RIB_EXP]) = (expression);					\
  HISTORY_UNMARK (Rib [RIB_MARK]);					\
}

#define End_Subproblem()						\
{									\
  HISTORY_UNMARK (History [HIST_MARK]);					\
  History = (OBJECT_ADDRESS (History [HIST_PREV_SUBPROBLEM]));		\
}

#else /* not COMPILE_HISTORY */

#define New_Subproblem(Expr, Env) {}
#define Reuse_Subproblem(Expr, Env) {}
#define New_Reduction(Expr, Env) {}
#define End_Subproblem() {}

#endif /* COMPILE_HISTORY */

/* History manipulation for the compiled code interface. */

#ifdef COMPILE_HISTORY

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

#else /* not COMPILE_HISTORY */

#define Compiler_New_Reduction()
#define Compiler_New_Subproblem()
#define Compiler_End_Subproblem()

#endif /* COMPILE_HISTORY */
