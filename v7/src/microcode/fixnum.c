/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/fixnum.c,v 9.26 1988/08/15 20:46:58 cph Exp $

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

/* Support for fixed point arithmetic.  This should be used instead of
   generic arithmetic when it is desired to tell the compiler to perform
   open coding of fixnum arithmetic.  It is probably a short-term kludge
   that will eventually go away. */

#include "scheme.h"
#include "prims.h"

#define FIXNUM_PRIMITIVE_1(parameter_1)					\
  fast long parameter_1;						\
  Primitive_1_Arg ();							\
  CHECK_ARG (1, FIXNUM_P);						\
  Sign_Extend (Arg1, parameter_1)

#define FIXNUM_PRIMITIVE_2(parameter_1, parameter_2)			\
  fast long parameter_1, parameter_2;					\
  Primitive_2_Args ();							\
  CHECK_ARG (1, FIXNUM_P);						\
  CHECK_ARG (2, FIXNUM_P);						\
  Sign_Extend (Arg1, parameter_1);					\
  Sign_Extend (Arg2, parameter_2)

#define FIXNUM_RESULT(fixnum)						\
  if (! (Fixnum_Fits (fixnum)))						\
    error_bad_range_arg (1);						\
  return (Make_Signed_Fixnum (fixnum));

#define BOOLEAN_RESULT(x)						\
  return ((x) ? SHARP_T : NIL)

/* Predicates */

DEFINE_PRIMITIVE ("ZERO-FIXNUM?", Prim_zero_fixnum, 1, 1, 0)
{
  FIXNUM_PRIMITIVE_1 (x);
  BOOLEAN_RESULT ((Get_Integer (Arg1)) == 0);
}

DEFINE_PRIMITIVE ("NEGATIVE-FIXNUM?", Prim_negative_fixnum, 1, 1, 0)
{
  FIXNUM_PRIMITIVE_1 (x);
  BOOLEAN_RESULT (x < 0);
}

DEFINE_PRIMITIVE ("POSITIVE-FIXNUM?", Prim_positive_fixnum, 1, 1, 0)
{
  FIXNUM_PRIMITIVE_1 (x);
  BOOLEAN_RESULT (x > 0);
}

DEFINE_PRIMITIVE ("EQUAL-FIXNUM?", Prim_equal_fixnum, 2, 2, 0)
{
  FIXNUM_PRIMITIVE_2 (x, y);
  BOOLEAN_RESULT (x == y);
}

DEFINE_PRIMITIVE ("LESS-THAN-FIXNUM?", Prim_less_fixnum, 2, 2, 0)
{
  FIXNUM_PRIMITIVE_2 (x, y);
  BOOLEAN_RESULT (x < y);
}

DEFINE_PRIMITIVE ("GREATER-THAN-FIXNUM?", Prim_greater_fixnum, 2, 2, 0)
{
  FIXNUM_PRIMITIVE_2 (x, y);
  BOOLEAN_RESULT (x > y);
}

/* Operators */

DEFINE_PRIMITIVE ("ONE-PLUS-FIXNUM", Prim_one_plus_fixnum, 1, 1, 0)
{
  fast long result;
  FIXNUM_PRIMITIVE_1 (x);
  result = (x + 1);
  FIXNUM_RESULT (result);
}

DEFINE_PRIMITIVE ("MINUS-ONE-PLUS-FIXNUM", Prim_m_1_plus_fixnum, 1, 1, 0)
{
  fast long result;
  FIXNUM_PRIMITIVE_1 (x);
  result = (x - 1);
  FIXNUM_RESULT (result);
}

DEFINE_PRIMITIVE ("PLUS-FIXNUM", Prim_plus_fixnum, 2, 2, 0)
{
  fast long result;
  FIXNUM_PRIMITIVE_2 (x, y);
  result = (x + y);
  FIXNUM_RESULT (result);
}

DEFINE_PRIMITIVE ("MINUS-FIXNUM", Prim_minus_fixnum, 2, 2, 0)
{
  fast long result;
  FIXNUM_PRIMITIVE_2 (x, y);
  result = (x - y);
  FIXNUM_RESULT (result);
}

DEFINE_PRIMITIVE ("MULTIPLY-FIXNUM", Prim_multiply_fixnum, 2, 2, 0)
{
  /* Mul, which does the multiplication with overflow handling, is
     customized for some machines.  Therefore, it is in os.c */
  extern Pointer Mul();
  fast long result;
  Primitive_2_Args ();

  CHECK_ARG (1, FIXNUM_P);
  CHECK_ARG (2, FIXNUM_P);
  result = (Mul (Arg1, Arg2));
  if (result == NIL)
    error_bad_range_arg (1);
  return (result);
}

DEFINE_PRIMITIVE ("DIVIDE-FIXNUM", Prim_divide_fixnum, 2, 2, 0)
{
  /* Returns the CONS of quotient and remainder */
  fast long quotient;
  FIXNUM_PRIMITIVE_2 (numerator, denominator);

  if (denominator == 0)
    error_bad_range_arg (2);
  Primitive_GC_If_Needed (2);
  quotient = (numerator / denominator);
  if (! (Fixnum_Fits (quotient)))
    error_bad_range_arg (1);
  Free[CONS_CAR] = (Make_Signed_Fixnum (quotient));
  Free[CONS_CDR] = (Make_Signed_Fixnum (numerator % denominator));
  Free += 2;
  return (Make_Pointer (TC_LIST, (Free - 2)));
}

DEFINE_PRIMITIVE ("GCD-FIXNUM", Prim_gcd_fixnum, 2, 2, 0)
{
  fast long z;
  FIXNUM_PRIMITIVE_2 (x, y);

  while (y != 0)
    {
      z = x;
      x = y;
      y = (z % y);
    }
  return (Make_Signed_Fixnum (x));
}
