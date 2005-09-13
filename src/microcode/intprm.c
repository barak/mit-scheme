/* -*-C-*-

$Id: intprm.c,v 1.12 2004/11/21 04:18:27 cph Exp $

Copyright (c) 1989-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

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

DEFINE_PRIMITIVE ("INTEGER-GREATER?", Prim_integer_greater_p, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Set_Time_Zone (Zone_Math);
  CHECK_ARG (1, INTEGER_P);
  CHECK_ARG (2, INTEGER_P);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (integer_less_p ((ARG_REF (2)), (ARG_REF (1)))));
}

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
DEFINE_PRIMITIVE ("INTEGER-LENGTH-IN-BITS", Prim_integer_length_in_bits, 1, 1, 0)
     INTEGER_UNARY_OPERATION (integer_length_in_bits)

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
    {
      long X = (FIXNUM_TO_LONG (integer));
      double Y = ((double) X);

      if (((long) Y) == X)
	PRIMITIVE_RETURN (FIXNUM_TO_FLONUM (integer));
      if ((control & 2) != 0)
	error_bad_range_arg (1);
      PRIMITIVE_RETURN (SHARP_F);
    }
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

DEFINE_PRIMITIVE ("INTEGER-SHIFT-LEFT", Prim_integer_shift_left, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Set_Time_Zone (Zone_Math);
  CHECK_ARG (1, INTEGER_P);
  {
    SCHEME_OBJECT n = (ARG_REF (1));
    if (integer_negative_p (n))
      error_bad_range_arg (1);
    PRIMITIVE_RETURN (integer_shift_left (n, (arg_ulong_integer (2))));
  }
}

static unsigned int
DEFUN (list_to_integer_producer, (context), PTR context)
{
  SCHEME_OBJECT * digits = context;
  unsigned int digit = (UNSIGNED_FIXNUM_TO_LONG (PAIR_CAR (*digits)));
  (*digits) = (PAIR_CDR (*digits));
  return (digit);
}

DEFINE_PRIMITIVE ("LIST->INTEGER", Prim_list_to_integer, 3, 3,
  "(list radix negative?)\n\
LIST is a non-null list of digits in RADIX, most-significant first.\n\
Converts the list to an integer.  NEGATIVE? specifies the sign.")
{
  PRIMITIVE_HEADER (3);
  Set_Time_Zone (Zone_Math);
  CHECK_ARG (1, PAIR_P);
  {
    SCHEME_OBJECT digits = (ARG_REF (1));
    unsigned long radix = (arg_ulong_integer (2));
    unsigned int n_digits;
    if ((radix < 2)
	|| (radix >= ((unsigned long) (bignum_max_digit_stream_radix ()))))
      error_bad_range_arg (2);
    {
      SCHEME_OBJECT scan = digits;
      n_digits = 0;
      while (1)
	{
	  SCHEME_OBJECT digit = (PAIR_CAR (scan));
	  if (!UNSIGNED_FIXNUM_P (digit))
	    error_wrong_type_arg (1);
	  if (((unsigned long) (UNSIGNED_FIXNUM_TO_LONG (digit))) >= radix)
	    error_bad_range_arg (1);
	  n_digits += 1;
	  scan = (PAIR_CDR (scan));
	  if (EMPTY_LIST_P (scan))
	    break;
	  if (!PAIR_P (scan))
	    error_wrong_type_arg (1);
	}
    }
    PRIMITIVE_RETURN
      (bignum_to_integer
       (digit_stream_to_bignum (n_digits,
				list_to_integer_producer,
				(&digits),
				radix,
				(OBJECT_TO_BOOLEAN (ARG_REF (3))))));
  }
}
