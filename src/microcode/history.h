/* -*-C-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

#define READ_DUMMY_HISTORY() VECTOR_REF (fixed_objects, DUMMY_HISTORY)

#define SAVE_HISTORY_LENGTH (2 + CONTINUATION_SIZE)
#define SAVE_HISTORY save_history
#define RESET_HISTORY reset_history

#ifndef DISABLE_HISTORY
#  define NEW_SUBPROBLEM new_subproblem
#  define REUSE_SUBPROBLEM reuse_subproblem
#  define NEW_REDUCTION new_reduction
#  define END_SUBPROBLEM end_subproblem
#  define COMPILER_NEW_SUBPROBLEM compiler_new_subproblem
#  define COMPILER_NEW_REDUCTION compiler_new_reduction
#  define COMPILER_END_SUBPROBLEM end_subproblem
#else
#  define NEW_SUBPROBLEM(exp, env) do {} while (false)
#  define REUSE_SUBPROBLEM(exp, env) do {} while (false)
#  define NEW_REDUCTION(exp, env) do {} while (false)
#  define END_SUBPROBLEM() do {} while (false)
#  define COMPILER_NEW_REDUCTION() do {} while (false)
#  define COMPILER_NEW_SUBPROBLEM() do {} while (false)
#  define COMPILER_END_SUBPROBLEM() do {} while (false)
#endif

extern SCHEME_OBJECT * history_register;
extern unsigned long prev_restore_history_offset;

extern void reset_history (void);
extern SCHEME_OBJECT * make_dummy_history (void);
extern void save_history (unsigned long);
extern bool restore_history (SCHEME_OBJECT);
extern void stop_history (void);
extern void new_subproblem (SCHEME_OBJECT, SCHEME_OBJECT);
extern void reuse_subproblem (SCHEME_OBJECT, SCHEME_OBJECT);
extern void new_reduction (SCHEME_OBJECT, SCHEME_OBJECT);
extern void end_subproblem (void);
extern void compiler_new_subproblem (void);
extern void compiler_new_reduction (void);

