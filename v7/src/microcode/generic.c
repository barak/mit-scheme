/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/generic.c,v 9.34 1990/06/20 17:40:46 cph Rel $

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

#include "scheme.h"
#include "prims.h"

#define INDIRECT(slot, arity)						\
{									\
  PRIMITIVE_CANONICALIZE_CONTEXT ();					\
 Will_Push (STACK_ENV_EXTRA_SLOTS + 1);					\
  STACK_PUSH (Get_Fixed_Obj_Slot (slot));				\
  STACK_PUSH (STACK_FRAME_HEADER + arity);				\
 Pushed ();								\
  PRIMITIVE_ABORT (PRIM_APPLY);						\
  /*NOTREACHED*/							\
}

#define INDIRECT_TEST_1(test, slot)					\
{									\
  PRIMITIVE_HEADER (1);							\
  {									\
    fast SCHEME_OBJECT x = (ARG_REF (1));				\
    if (FIXNUM_P (x))							\
      return (test (x));						\
  }									\
  INDIRECT (slot, 1);							\
}

DEFINE_PRIMITIVE ("ZERO?", Prim_zero, 1, 1, 0)
     INDIRECT_TEST_1 (FIXNUM_ZERO_P, GENERIC_TRAMPOLINE_ZERO_P)
DEFINE_PRIMITIVE ("POSITIVE?", Prim_positive, 1, 1, 0)
     INDIRECT_TEST_1 (FIXNUM_POSITIVE_P, GENERIC_TRAMPOLINE_POSITIVE_P)
DEFINE_PRIMITIVE ("NEGATIVE?", Prim_negative, 1, 1, 0)
     INDIRECT_TEST_1 (FIXNUM_NEGATIVE_P, GENERIC_TRAMPOLINE_NEGATIVE_P)

#define INDIRECT_INCREMENT(op, slot)					\
{									\
  PRIMITIVE_HEADER (1);							\
  {									\
    fast SCHEME_OBJECT x = (ARG_REF (1));				\
    if (FIXNUM_P (x))							\
      return (long_to_integer ((FIXNUM_TO_LONG (x)) op 1));		\
  }									\
  INDIRECT (slot, 1);							\
}

DEFINE_PRIMITIVE ("1+", Prim_add_one, 1, 1, 0)
     INDIRECT_INCREMENT (+, GENERIC_TRAMPOLINE_SUCCESSOR)
DEFINE_PRIMITIVE ("-1+", Prim_subtract_one, 1, 1, 0)
     INDIRECT_INCREMENT (-, GENERIC_TRAMPOLINE_PREDECESSOR)

#define INDIRECT_TEST_2(test, slot)					\
{									\
  PRIMITIVE_HEADER (2);							\
  {									\
    fast SCHEME_OBJECT x = (ARG_REF (1));				\
    fast SCHEME_OBJECT y = (ARG_REF (2));				\
    if ((FIXNUM_P (x)) && (FIXNUM_P (y)))				\
      return (test (x, y));						\
  }									\
  INDIRECT (slot, 2);							\
}

#define FIXNUM_GREATER_P(x, y) FIXNUM_LESS_P (y, x)

DEFINE_PRIMITIVE ("&=", Prim_equal_number, 2, 2, 0)
     INDIRECT_TEST_2 (FIXNUM_EQUAL_P, GENERIC_TRAMPOLINE_EQUAL_P)
DEFINE_PRIMITIVE ("&<", Prim_less, 2, 2, 0)
     INDIRECT_TEST_2 (FIXNUM_LESS_P, GENERIC_TRAMPOLINE_LESS_P)
DEFINE_PRIMITIVE ("&>", Prim_greater, 2, 2, 0)
     INDIRECT_TEST_2 (FIXNUM_GREATER_P, GENERIC_TRAMPOLINE_GREATER_P)

#define INDIRECT_SUM(op, slot)						\
{									\
  PRIMITIVE_HEADER (2);							\
  {									\
    fast SCHEME_OBJECT x = (ARG_REF (1));				\
    fast SCHEME_OBJECT y = (ARG_REF (2));				\
    if ((FIXNUM_P (x)) && (FIXNUM_P (y)))				\
      return (long_to_integer ((FIXNUM_TO_LONG (x)) op			\
			       (FIXNUM_TO_LONG (y))));			\
  }									\
  INDIRECT (slot, 2);							\
}

DEFINE_PRIMITIVE ("&+", Prim_add, 2, 2, 0)
     INDIRECT_SUM (+, GENERIC_TRAMPOLINE_ADD)
DEFINE_PRIMITIVE ("&-", Prim_subtract, 2, 2, 0)
     INDIRECT_SUM (-, GENERIC_TRAMPOLINE_SUBTRACT)

#define INDIRECT_2(slot)						\
{									\
  PRIMITIVE_HEADER (2);							\
  INDIRECT (slot, 2);							\
}

DEFINE_PRIMITIVE ("&*", Prim_multiply, 2, 2, 0)
     INDIRECT_2 (GENERIC_TRAMPOLINE_MULTIPLY)
DEFINE_PRIMITIVE ("&/", Prim_divide, 2, 2, 0)
     INDIRECT_2 (GENERIC_TRAMPOLINE_DIVIDE)
