/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/fixnum.c,v 9.25 1987/11/17 08:11:05 jinx Rel $

Copyright (c) 1987 Massachusetts Institute of Technology

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
#include "primitive.h"

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
  return ((x) ? TRUTH : NIL)

/* Predicates */

Built_In_Primitive (Prim_Zero_Fixnum, 1, "ZERO-FIXNUM?", 0x46)
Define_Primitive (Prim_Zero_Fixnum, 1, "ZERO-FIXNUM?")
{
  FIXNUM_PRIMITIVE_1 (x);
  BOOLEAN_RESULT ((Get_Integer (Arg1)) == 0);
}

Built_In_Primitive (Prim_Negative_Fixnum, 1, "NEGATIVE-FIXNUM?", 0x7F)
Define_Primitive (Prim_Negative_Fixnum, 1, "NEGATIVE-FIXNUM?")
{
  FIXNUM_PRIMITIVE_1 (x);
  BOOLEAN_RESULT (x < 0);
}

Built_In_Primitive (Prim_Positive_Fixnum, 1, "POSITIVE-FIXNUM?", 0x41)
Define_Primitive (Prim_Positive_Fixnum, 1, "POSITIVE-FIXNUM?")
{
  FIXNUM_PRIMITIVE_1 (x);
  BOOLEAN_RESULT (x > 0);
}

Built_In_Primitive (Prim_Equal_Fixnum, 2, "EQUAL-FIXNUM?", 0x3F)
Define_Primitive (Prim_Equal_Fixnum, 2, "EQUAL-FIXNUM?")
{
  FIXNUM_PRIMITIVE_2 (x, y);
  BOOLEAN_RESULT (x == y);
}

Built_In_Primitive (Prim_Less_Fixnum, 2, "LESS-THAN-FIXNUM?", 0x40)
Define_Primitive (Prim_Less_Fixnum, 2, "LESS-THAN-FIXNUM?")
{
  FIXNUM_PRIMITIVE_2 (x, y);
  BOOLEAN_RESULT (x < y);
}

Built_In_Primitive (Prim_Greater_Fixnum, 2, "GREATER-THAN-FIXNUM?", 0x81)
Define_Primitive (Prim_Greater_Fixnum, 2, "GREATER-THAN-FIXNUM?")
{
  FIXNUM_PRIMITIVE_2 (x, y);
  BOOLEAN_RESULT (x > y);
}

/* Operators */

Built_In_Primitive (Prim_One_Plus_Fixnum, 1, "ONE-PLUS-FIXNUM", 0x42)
Define_Primitive (Prim_One_Plus_Fixnum, 1, "ONE-PLUS-FIXNUM")
{
  fast long result;
  FIXNUM_PRIMITIVE_1 (x);
  result = (x + 1);
  FIXNUM_RESULT (result);
}

Built_In_Primitive (Prim_M_1_Plus_Fixnum, 1, "MINUS-ONE-PLUS-FIXNUM", 0x43)
Define_Primitive (Prim_M_1_Plus_Fixnum, 1, "MINUS-ONE-PLUS-FIXNUM")
{
  fast long result;
  FIXNUM_PRIMITIVE_1 (x);
  result = (x - 1);
  FIXNUM_RESULT (result);
}

Built_In_Primitive (Prim_Plus_Fixnum, 2, "PLUS-FIXNUM", 0x3B)
Define_Primitive (Prim_Plus_Fixnum, 2, "PLUS-FIXNUM")
{
  fast long result;
  FIXNUM_PRIMITIVE_2 (x, y);
  result = (x + y);
  FIXNUM_RESULT (result);
}

Built_In_Primitive (Prim_Minus_Fixnum, 2, "MINUS-FIXNUM", 0x3C)
Define_Primitive (Prim_Minus_Fixnum, 2, "MINUS-FIXNUM")
{
  fast long result;
  FIXNUM_PRIMITIVE_2 (x, y);
  result = (x - y);
  FIXNUM_RESULT (result);
}

Built_In_Primitive (Prim_Multiply_Fixnum, 2, "MULTIPLY-FIXNUM", 0x3D)
Define_Primitive (Prim_Multiply_Fixnum, 2, "MULTIPLY-FIXNUM")
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

Built_In_Primitive (Prim_Divide_Fixnum, 2, "DIVIDE-FIXNUM", 0x3E)
Define_Primitive (Prim_Divide_Fixnum, 2, "DIVIDE-FIXNUM")
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

Built_In_Primitive (Prim_Gcd_Fixnum, 2, "GCD-FIXNUM", 0x66)
Define_Primitive (Prim_Gcd_Fixnum, 2, "GCD-FIXNUM")
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
