/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/intprm.c,v 1.2 1989/09/24 15:13:01 cph Exp $

Copyright (c) 1989 Massachusetts Institute of Technology

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

/* Generic Integer Primitives */

#include "scheme.h"
#include "prims.h"
#include "zones.h"

#define INTEGER_TEST(test)						\
{									\
  PRIMITIVE_HEADER (1);							\
  Set_Time_Zone (Zone_Math);						\
  CHECK_ARG (1, INTEGER_P);						\
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (test (ARG_REF (1))));		\
}

DEFINE_PRIMITIVE ("INTEGER-ZERO?", Prim_integer_zero_p, 1, 1, 0)
     INTEGER_TEST (integer_zero_p)
DEFINE_PRIMITIVE ("INTEGER-NEGATIVE?", Prim_integer_negative_p, 1, 1, 0)
     INTEGER_TEST (integer_negative_p)
DEFINE_PRIMITIVE ("INTEGER-POSITIVE?", Prim_integer_positive_p, 1, 1, 0)
     INTEGER_TEST (integer_positive_p)

#define INTEGER_COMPARISON(comparison)					\
{									\
  PRIMITIVE_HEADER (2);							\
  Set_Time_Zone (Zone_Math);						\
  CHECK_ARG (1, INTEGER_P);						\
  CHECK_ARG (2, INTEGER_P);						\
  PRIMITIVE_RETURN							\
    (BOOLEAN_TO_OBJECT (comparison ((ARG_REF (1)), (ARG_REF (2)))));	\
}

DEFINE_PRIMITIVE ("INTEGER-EQUAL?", Prim_integer_equal_p, 2, 2, 0)
     INTEGER_COMPARISON (integer_equal_p)
DEFINE_PRIMITIVE ("INTEGER-LESS?", Prim_integer_less_p, 2, 2, 0)
     INTEGER_COMPARISON (integer_less_p)

#define INTEGER_BINARY_OPERATION(operator)				\
{									\
  PRIMITIVE_HEADER (2);							\
  Set_Time_Zone (Zone_Math);						\
  CHECK_ARG (1, INTEGER_P);						\
  CHECK_ARG (2, INTEGER_P);						\
  PRIMITIVE_RETURN (operator ((ARG_REF (1)), (ARG_REF (2))));		\
}

DEFINE_PRIMITIVE ("INTEGER-ADD", Prim_integer_add, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_add)
DEFINE_PRIMITIVE ("INTEGER-SUBTRACT", Prim_integer_subtract, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_subtract)
DEFINE_PRIMITIVE ("INTEGER-MULTIPLY", Prim_integer_multiply, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_multiply)

#define INTEGER_UNARY_OPERATION(operator)				\
{									\
  PRIMITIVE_HEADER (1);							\
  Set_Time_Zone (Zone_Math);						\
  CHECK_ARG (1, INTEGER_P);						\
  PRIMITIVE_RETURN (operator (ARG_REF (1)));				\
}

DEFINE_PRIMITIVE ("INTEGER-NEGATE", Prim_integer_negate, 1, 1, 0)
     INTEGER_UNARY_OPERATION (integer_negate)
DEFINE_PRIMITIVE ("INTEGER-ADD-1", Prim_integer_add_1, 1, 1, 0)
     INTEGER_UNARY_OPERATION (integer_add_1)
DEFINE_PRIMITIVE ("INTEGER-SUBTRACT-1", Prim_integer_subtract_1, 1, 1, 0)
     INTEGER_UNARY_OPERATION (integer_subtract_1)

DEFINE_PRIMITIVE ("INTEGER-DIVIDE", Prim_integer_divide, 2, 2, 0)
{
  SCHEME_OBJECT quotient;
  SCHEME_OBJECT remainder;
  PRIMITIVE_HEADER (2);
  Set_Time_Zone (Zone_Math);
  CHECK_ARG (1, INTEGER_P);
  CHECK_ARG (2, INTEGER_P);
  if (integer_divide ((ARG_REF (1)), (ARG_REF (2)), (&quotient), (&remainder)))
    error_bad_range_arg (2);
  PRIMITIVE_RETURN (cons (quotient, remainder));
}

#define INTEGER_QR(operator)						\
{									\
  SCHEME_OBJECT result;							\
  PRIMITIVE_HEADER (2);							\
  Set_Time_Zone (Zone_Math);						\
  CHECK_ARG (1, INTEGER_P);						\
  CHECK_ARG (2, INTEGER_P);						\
  result = (operator ((ARG_REF (1)), (ARG_REF (2))));			\
  if (result == SHARP_F)						\
    error_bad_range_arg (2);						\
  PRIMITIVE_RETURN (result);						\
}

DEFINE_PRIMITIVE ("INTEGER-QUOTIENT", Prim_integer_quotient, 2, 2, 0)
     INTEGER_QR (integer_quotient)
DEFINE_PRIMITIVE ("INTEGER-REMAINDER", Prim_integer_remainder, 2, 2, 0)
     INTEGER_QR (integer_remainder)

DEFINE_PRIMITIVE ("INTEGER?", Prim_integer_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    fast SCHEME_OBJECT integer = (ARG_REF (1));
    PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (INTEGER_P (integer)));
  }
}

DEFINE_PRIMITIVE ("INTEGER->FLONUM", Prim_integer_to_flonum, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Set_Time_Zone (Zone_Math);
  CHECK_ARG (1, INTEGER_P);
  {
    fast SCHEME_OBJECT integer = (ARG_REF (1));
    fast long control = (arg_index_integer (2, 4));
    if (FIXNUM_P (integer))
      PRIMITIVE_RETURN (FIXNUM_TO_FLONUM (integer));
    if (bignum_fits_in_word_p
	(integer,
	 (((control & 1) != 0) ? DBL_MANT_DIG : DBL_MAX_EXP),
	 0))
      PRIMITIVE_RETURN (BIGNUM_TO_FLONUM (integer));
    if ((control & 2) != 0)
      error_bad_range_arg (1);
    PRIMITIVE_RETURN (SHARP_F);
  }
}
