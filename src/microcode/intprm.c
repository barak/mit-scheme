/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

/* Generic Integer Primitives */

#include "scheme.h"
#include "prims.h"

#define INTEGER_TEST(test)						\
{									\
  PRIMITIVE_HEADER (1);							\
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
  CHECK_ARG (1, INTEGER_P);
  CHECK_ARG (2, INTEGER_P);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (integer_less_p ((ARG_REF (2)), (ARG_REF (1)))));
}

#define INTEGER_BINARY_OPERATION(operator)				\
{									\
  PRIMITIVE_HEADER (2);							\
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
DEFINE_PRIMITIVE ("INTEGER-HAMMING-DISTANCE", Prim_integer_hamming_distance, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_hamming_distance)
DEFINE_PRIMITIVE ("INTEGER-BITWISE-AND", Prim_integer_bitwise_and, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_bitwise_and)
DEFINE_PRIMITIVE ("INTEGER-BITWISE-ANDC2", Prim_integer_bitwise_andc2, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_bitwise_andc2)
DEFINE_PRIMITIVE ("INTEGER-BITWISE-ANDC1", Prim_integer_bitwise_andc1, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_bitwise_andc1)
DEFINE_PRIMITIVE ("INTEGER-BITWISE-XOR", Prim_integer_bitwise_xor, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_bitwise_xor)
DEFINE_PRIMITIVE ("INTEGER-BITWISE-IOR", Prim_integer_bitwise_ior, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_bitwise_ior)
DEFINE_PRIMITIVE ("INTEGER-BITWISE-NOR", Prim_integer_bitwise_nor, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_bitwise_nor)
DEFINE_PRIMITIVE ("INTEGER-BITWISE-EQV", Prim_integer_bitwise_eqv, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_bitwise_eqv)
DEFINE_PRIMITIVE ("INTEGER-BITWISE-ORC2", Prim_integer_bitwise_orc2, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_bitwise_orc2)
DEFINE_PRIMITIVE ("INTEGER-BITWISE-ORC1", Prim_integer_bitwise_orc1, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_bitwise_orc1)
DEFINE_PRIMITIVE ("INTEGER-BITWISE-NAND", Prim_integer_bitwise_nand, 2, 2, 0)
     INTEGER_BINARY_OPERATION (integer_bitwise_nand)

#define INTEGER_UNARY_OPERATION(operator)				\
{									\
  PRIMITIVE_HEADER (1);							\
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
DEFINE_PRIMITIVE ("INTEGER-LENGTH", Prim_integer_length, 1, 1, 0)
     INTEGER_UNARY_OPERATION (integer_length)
DEFINE_PRIMITIVE ("INTEGER-FIRST-SET-BIT", Prim_integer_first_set_bit, 1, 1, 0)
     INTEGER_UNARY_OPERATION (integer_first_set_bit)
DEFINE_PRIMITIVE ("INTEGER-BIT-COUNT", Prim_integer_bit_count, 1, 1, 0)
     INTEGER_UNARY_OPERATION (integer_bit_count)
DEFINE_PRIMITIVE ("INTEGER-BITWISE-NOT", Prim_integer_bitwise_not, 1, 1, 0)
     INTEGER_UNARY_OPERATION (integer_bitwise_not)

DEFINE_PRIMITIVE ("INTEGER-DIVIDE", Prim_integer_divide, 2, 2, 0)
{
  SCHEME_OBJECT quotient;
  SCHEME_OBJECT remainder;
  PRIMITIVE_HEADER (2);
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
    SCHEME_OBJECT integer = (ARG_REF (1));
    PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (INTEGER_P (integer)));
  }
}

DEFINE_PRIMITIVE ("INTEGER->FLONUM", Prim_integer_to_flonum, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, INTEGER_P);
  {
    SCHEME_OBJECT integer = (ARG_REF (1));
    long control = (arg_index_integer (2, 4));
    if (FIXNUM_P (integer))
      {
	double d = (FIXNUM_TO_DOUBLE (integer));
	if ((0 == (control & 1))
	    || ((DOUBLE_TO_FIXNUM_P (d))
		&& (integer == (DOUBLE_TO_FIXNUM (d)))))
	  PRIMITIVE_RETURN (double_to_flonum (d));
	if ((control & 2) != 0)
	  error_bad_range_arg (1);
	PRIMITIVE_RETURN (SHARP_F);
      }
    if (((control & 1) != 0)
	? (LOSSLESS_BIGNUM_TO_DOUBLE_P (integer))
	: (BIGNUM_TO_DOUBLE_P (integer)))
      PRIMITIVE_RETURN (BIGNUM_TO_FLONUM (integer));
    if ((control & 2) != 0)
      error_bad_range_arg (1);
    PRIMITIVE_RETURN (SHARP_F);
  }
}

DEFINE_PRIMITIVE ("INTEGER-NONNEGATIVE-ONE-BITS", Prim_integer_nonnegative_one_bits, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (integer_nonnegative_one_bits
     ((arg_ulong_integer (1)), (arg_ulong_integer (2))));
}

DEFINE_PRIMITIVE ("INTEGER-NEGATIVE-ZERO-BITS", Prim_integer_negative_zero_bits, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (integer_negative_zero_bits
     ((arg_ulong_integer (1)), (arg_ulong_integer (2))));
}

DEFINE_PRIMITIVE ("INTEGER-SHIFT-LEFT", Prim_integer_shift_left, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, INTEGER_P);
  PRIMITIVE_RETURN
    (integer_shift_left ((ARG_REF (1)), (arg_ulong_integer (2))));
}

DEFINE_PRIMITIVE ("INTEGER-SHIFT-RIGHT", Prim_integer_shift_right, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, INTEGER_P);
  {
    SCHEME_OBJECT n = (ARG_REF (1));
    SCHEME_OBJECT m = (ARG_REF (2));
    if (FIXNUM_P (m))
      {
	if (FIXNUM_NEGATIVE_P (m))
	  error_bad_range_arg (2);
	PRIMITIVE_RETURN (integer_shift_right (n, (FIXNUM_TO_ULONG (m))));
      }
    else if (BIGNUM_P (m))
      {
	if (BIGNUM_NEGATIVE_P (m))
	  error_bad_range_arg (2);
	PRIMITIVE_RETURN (FIXNUM_ZERO);
      }
    else
      error_wrong_type_arg (2);
  }
}

static unsigned int
list_to_integer_producer (void * context)
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
