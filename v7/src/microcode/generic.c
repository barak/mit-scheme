/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/generic.c,v 9.32 1989/10/26 07:49:47 cph Exp $

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

#include "scheme.h"
#include "prims.h"

#define INDIRECT_1(slot)						\
{									\
  PRIMITIVE_HEADER (1);							\
  PRIMITIVE_CANONICALIZE_CONTEXT ();					\
 Will_Push (STACK_ENV_EXTRA_SLOTS + 1);					\
  Push (Get_Fixed_Obj_Slot (slot));					\
  Push (STACK_FRAME_HEADER + 1);					\
 Pushed ();								\
  PRIMITIVE_ABORT (PRIM_APPLY);						\
  /*NOTREACHED*/							\
}

DEFINE_PRIMITIVE ("ZERO?", Prim_zero, 1, 1, 0)
     INDIRECT_1 (GENERIC_TRAMPOLINE_ZERO_P)
DEFINE_PRIMITIVE ("POSITIVE?", Prim_positive, 1, 1, 0)
     INDIRECT_1 (GENERIC_TRAMPOLINE_POSITIVE_P)
DEFINE_PRIMITIVE ("NEGATIVE?", Prim_negative, 1, 1, 0)
     INDIRECT_1 (GENERIC_TRAMPOLINE_NEGATIVE_P)
DEFINE_PRIMITIVE ("1+", Prim_add_one, 1, 1, 0)
     INDIRECT_1 (GENERIC_TRAMPOLINE_SUCCESSOR)
DEFINE_PRIMITIVE ("-1+", Prim_subtract_one, 1, 1, 0)
     INDIRECT_1 (GENERIC_TRAMPOLINE_PREDECESSOR)

#define INDIRECT_2(slot)						\
{									\
  PRIMITIVE_HEADER (2);							\
  PRIMITIVE_CANONICALIZE_CONTEXT ();					\
 Will_Push (STACK_ENV_EXTRA_SLOTS + 1);					\
  Push (Get_Fixed_Obj_Slot (slot));					\
  Push (STACK_FRAME_HEADER + 2);					\
 Pushed ();								\
  PRIMITIVE_ABORT (PRIM_APPLY);						\
  /*NOTREACHED*/							\
}

DEFINE_PRIMITIVE ("&=", Prim_equal_number, 2, 2, 0)
     INDIRECT_2 (GENERIC_TRAMPOLINE_EQUAL_P)
DEFINE_PRIMITIVE ("&<", Prim_less, 2, 2, 0)
     INDIRECT_2 (GENERIC_TRAMPOLINE_LESS_P)
DEFINE_PRIMITIVE ("&>", Prim_greater, 2, 2, 0)
     INDIRECT_2 (GENERIC_TRAMPOLINE_GREATER_P)
DEFINE_PRIMITIVE ("&+", Prim_add, 2, 2, 0)
     INDIRECT_2 (GENERIC_TRAMPOLINE_ADD)
DEFINE_PRIMITIVE ("&-", Prim_subtract, 2, 2, 0)
     INDIRECT_2 (GENERIC_TRAMPOLINE_SUBTRACT)
DEFINE_PRIMITIVE ("&*", Prim_multiply, 2, 2, 0)
     INDIRECT_2 (GENERIC_TRAMPOLINE_MULTIPLY)
DEFINE_PRIMITIVE ("&/", Prim_divide, 2, 2, 0)
     INDIRECT_2 (GENERIC_TRAMPOLINE_DIVIDE)
