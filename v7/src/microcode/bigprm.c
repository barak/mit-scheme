/* -*-C-*-

$Id: bigprm.c,v 1.11 2007/04/22 16:31:22 cph Exp $

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

/* Bignum Primitives */

#include "scheme.h"
#include "prims.h"

#define BIGNUM_TEST(predicate)						\
{									\
  PRIMITIVE_HEADER (1);							\
  CHECK_ARG (1, BIGNUM_P);						\
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (predicate (ARG_REF (1))));	\
}

DEFINE_PRIMITIVE ("BIGNUM-ZERO?", Prim_bignum_zero_p, 1, 1, 0)
     BIGNUM_TEST (BIGNUM_ZERO_P)
DEFINE_PRIMITIVE ("BIGNUM-NEGATIVE?", Prim_bignum_negative_p, 1, 1, 0)
     BIGNUM_TEST (BIGNUM_NEGATIVE_P)
DEFINE_PRIMITIVE ("BIGNUM-POSITIVE?", Prim_bignum_positive_p, 1, 1, 0)
     BIGNUM_TEST (BIGNUM_POSITIVE_P)

#define BIGNUM_COMPARISON(predicate)					\
{									\
  PRIMITIVE_HEADER (2);							\
  CHECK_ARG (1, BIGNUM_P);						\
  CHECK_ARG (2, BIGNUM_P);						\
  PRIMITIVE_RETURN							\
    (BOOLEAN_TO_OBJECT (predicate ((ARG_REF (1)), (ARG_REF (2)))));	\
}

DEFINE_PRIMITIVE ("BIGNUM-EQUAL?", Prim_bignum_equal_p, 2, 2, 0)
     BIGNUM_COMPARISON (bignum_equal_p)
DEFINE_PRIMITIVE ("BIGNUM-LESS?", Prim_bignum_less_p, 2, 2, 0)
     BIGNUM_COMPARISON (BIGNUM_LESS_P)

#define BIGNUM_BINARY(operator)						\
{									\
  PRIMITIVE_HEADER (2);							\
  CHECK_ARG (1, BIGNUM_P);						\
  CHECK_ARG (2, BIGNUM_P);						\
  PRIMITIVE_RETURN (operator ((ARG_REF (1)), (ARG_REF (2))));		\
}

DEFINE_PRIMITIVE ("BIGNUM-ADD", Prim_bignum_add, 2, 2, 0)
     BIGNUM_BINARY (bignum_add)
DEFINE_PRIMITIVE ("BIGNUM-SUBTRACT", Prim_bignum_subtract, 2, 2, 0)
     BIGNUM_BINARY (bignum_subtract)
DEFINE_PRIMITIVE ("BIGNUM-MULTIPLY", Prim_bignum_multiply, 2, 2, 0)
     BIGNUM_BINARY (bignum_multiply)

DEFINE_PRIMITIVE ("BIGNUM-DIVIDE", Prim_bignum_divide, 2, 2, 0)
{
  SCHEME_OBJECT quotient;
  SCHEME_OBJECT remainder;
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, BIGNUM_P);
  CHECK_ARG (2, BIGNUM_P);
  if (bignum_divide ((ARG_REF (1)), (ARG_REF (2)), (&quotient), (&remainder)))
    error_bad_range_arg (2);
  PRIMITIVE_RETURN (cons (quotient, remainder));
}

#define BIGNUM_QR(operator)						\
{									\
  SCHEME_OBJECT result;							\
  PRIMITIVE_HEADER (2);							\
  CHECK_ARG (1, BIGNUM_P);						\
  CHECK_ARG (2, BIGNUM_P);						\
  result = (operator ((ARG_REF (1)), (ARG_REF (2))));			\
  if (result == SHARP_F)						\
    error_bad_range_arg (2);						\
  PRIMITIVE_RETURN (result);						\
}

DEFINE_PRIMITIVE ("BIGNUM-QUOTIENT", Prim_bignum_quotient, 2, 2, 0)
     BIGNUM_QR (bignum_quotient)
DEFINE_PRIMITIVE ("BIGNUM-REMAINDER", Prim_bignum_remainder, 2, 2, 0)
     BIGNUM_QR (bignum_remainder)

static void
listify_bignum_consumer (void * previous_cdr,
       long digit)
{
  (* ((SCHEME_OBJECT *) previous_cdr)) =
    (cons ((LONG_TO_UNSIGNED_FIXNUM (digit)),
	   (* ((SCHEME_OBJECT *) previous_cdr))));
}

DEFINE_PRIMITIVE ("LISTIFY-BIGNUM", Prim_listify_bignum, 2, 2,
  "Returns a list of the digits of BIGNUM in RADIX.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, BIGNUM_P);
  {
    SCHEME_OBJECT bignum = (ARG_REF (1));
    long radix =
      (arg_integer_in_range (2, 2, (bignum_max_digit_stream_radix ())));
    if (BIGNUM_ZERO_P (bignum))
      PRIMITIVE_RETURN (cons ((LONG_TO_UNSIGNED_FIXNUM (0)), EMPTY_LIST));
    {
      SCHEME_OBJECT previous_cdr = EMPTY_LIST;
      bignum_to_digit_stream
	(bignum, radix, listify_bignum_consumer, (&previous_cdr));
      PRIMITIVE_RETURN (previous_cdr);
    }
  }
}

DEFINE_PRIMITIVE ("FIXNUM->BIGNUM", Prim_fixnum_to_bignum, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, FIXNUM_P);
  PRIMITIVE_RETURN (FIXNUM_TO_BIGNUM (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("BIGNUM->FIXNUM", Prim_bignum_to_fixnum, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, BIGNUM_P);
  PRIMITIVE_RETURN (bignum_to_fixnum (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("FLONUM->BIGNUM", Prim_flonum_to_bignum, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, FLONUM_P);
  PRIMITIVE_RETURN (FLONUM_TO_BIGNUM (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("BIGNUM->FLONUM", Prim_bignum_to_flonum, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, BIGNUM_P);
  PRIMITIVE_RETURN (bignum_to_flonum (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("BIGNUM-LENGTH-IN-BITS", Prim_bignum_length_in_bits, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, BIGNUM_P);
  PRIMITIVE_RETURN (bignum_length_in_bits (ARG_REF (1)));
}
