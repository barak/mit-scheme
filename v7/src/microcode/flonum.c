/* -*-C-*-

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/flonum.c,v 9.25 1988/08/15 20:47:15 cph Exp $
 *
 * This file contains support for floating point arithmetic.  Most
 * of these primitives have been superceded by generic arithmetic.
 */

#include "scheme.h"
#include "prims.h"
#include "flonum.h"
#include "zones.h"

                /************************************/
                /* BINARY FLOATING POINT OPERATIONS */
                /************************************/

/* The binary floating point operations return NIL if either argument
   is not a floating point number.  Otherwise they return the
   appropriate result.
*/

DEFINE_PRIMITIVE ("PLUS-FLONUM", Prim_plus_flonum, 2, 2, 0)
{
  Primitive_2_Args();

  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(Get_Float(Arg1) + Get_Float(Arg2));
}

DEFINE_PRIMITIVE ("MINUS-FLONUM", Prim_minus_flonum, 2, 2, 0)
{
  Primitive_2_Args();

  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(Get_Float(Arg1) - Get_Float(Arg2));
}

DEFINE_PRIMITIVE ("MULTIPLY-FLONUM", Prim_multiply_flonum, 2, 2, 0)
{
  Primitive_2_Args();

  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(Get_Float(Arg1) * Get_Float(Arg2));
}

DEFINE_PRIMITIVE ("DIVIDE-FLONUM", Prim_divide_flonum, 2, 2, 0)
{
  Primitive_2_Args();

  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  if (Get_Float(Arg2) == 0)
    Primitive_Error(ERR_ARG_2_BAD_RANGE);
  Flonum_Result(Get_Float(Arg1) / Get_Float(Arg2));
}

	        /************************************/
                /* BINARY FLOATING POINT PREDICATES */
	        /************************************/

/* The binary flonum predicates return NIL if either of the arguments
   is not a flonum. Otherwise, return a fixnum 1 if the predicate is
   true, or a fixnum 0 if it is false.
*/

DEFINE_PRIMITIVE ("EQUAL-FLONUM?", Prim_equal_flonum, 2, 2, 0)
{
  Primitive_2_Args();

  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  return
    Make_Unsigned_Fixnum(((Get_Float(Arg1)) == (Get_Float(Arg2))) ? 1 : 0);
}

DEFINE_PRIMITIVE ("GREATER-THAN-FLONUM?", Prim_greater_flonum, 2, 2, 0)
{
  Primitive_2_Args();

  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  return
    Make_Unsigned_Fixnum(((Get_Float(Arg1)) > (Get_Float(Arg2))) ? 1 : 0);
}

DEFINE_PRIMITIVE ("LESS-THAN-FLONUM?", Prim_less_flonum, 2, 2, 0)
{
  Primitive_2_Args();

  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  return
    Make_Unsigned_Fixnum(((Get_Float(Arg1)) < (Get_Float(Arg2))) ? 1 : 0);
}

	        /***********************************/
                /* UNARY FLOATING POINT OPERATIONS */
                /***********************************/

/* The unary flonum operations return NIL if their argument is
   not a flonum. Otherwise, they return the appropriate result.
*/

DEFINE_PRIMITIVE ("SINE-FLONUM", Prim_sine_flonum, 1, 1, 0)
{
  extern double sin();
  Primitive_1_Arg();

  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(sin(Get_Float(Arg1)));
}

DEFINE_PRIMITIVE ("COSINE-FLONUM", Prim_cosine_flonum, 1, 1, 0)
{
  extern double cos();
  Primitive_1_Arg();

  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(cos(Get_Float(Arg1)));
}

DEFINE_PRIMITIVE ("ARCTAN-FLONUM", Prim_arctan_flonum, 1, 1, 0)
{
  extern double atan();
  Primitive_1_Arg();

  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(atan(Get_Float(Arg1)));
}

DEFINE_PRIMITIVE ("EXP-FLONUM", Prim_exp_flonum, 1, 1, 0)
{
  extern double exp();
  Primitive_1_Arg();

  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(exp(Get_Float(Arg1)));
}

DEFINE_PRIMITIVE ("LN-FLONUM", Prim_ln_flonum, 1, 1, 0)
{
  extern double log();
  Primitive_1_Arg();

  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  if (Get_Float(Arg1) <= 0.0)
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  Flonum_Result(log(Get_Float(Arg1)));
}

DEFINE_PRIMITIVE ("SQRT-FLONUM", Prim_sqrt_flonum, 1, 1, 0)
{
  extern double sqrt();
  double Arg;
  Primitive_1_Arg();

  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Arg = Get_Float(Arg1);
  if (Arg < 0)
    return NIL;
  Flonum_Result(sqrt(Arg));
}

DEFINE_PRIMITIVE ("ZERO-FLONUM?", Prim_zero_flonum, 1, 1, 0)
{
  Primitive_1_Arg();

  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  return Make_Unsigned_Fixnum((Get_Float(Arg1) == 0.0) ? 1 : 0);
}

DEFINE_PRIMITIVE ("POSITIVE-FLONUM?", Prim_positive_flonum, 1, 1, 0)
{
  Primitive_1_Arg();

  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  return Make_Unsigned_Fixnum((Get_Float(Arg1) > 0.0) ? 1 : 0);
}

DEFINE_PRIMITIVE ("NEGATIVE-FLONUM?", Prim_negative_flonum, 1, 1, 0)
{
  Primitive_1_Arg();

  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  return Make_Unsigned_Fixnum((Get_Float(Arg1) < 0.0) ? 1 : 0);
}

/* (COERCE-INTEGER-TO-FLONUM FIXNUM-OR-BIGNUM)
      Returns the floating point number (flonum) corresponding to
      either a bignum or a fixnum.  If the bignum is too large or small
      to be converted to floating point, or if the argument isn't of
      the correct type, FIXNUM-OR-BIGNUM is returned unchanged.
*/
DEFINE_PRIMITIVE ("COERCE-INTEGER-TO-FLONUM", Prim_int_to_float, 1, 1, 0)
{
  Primitive_1_Arg();

  Set_Time_Zone(Zone_Math);
  if (Type_Code(Arg1)==TC_FIXNUM)
  {
    long Int;

    Sign_Extend(Arg1, Int);
    return Allocate_Float((double) Int);
  }
  if (Type_Code(Arg1) == TC_BIG_FIXNUM)
    return Big_To_Float(Arg1);
  return Arg1;
}

/* (TRUNCATE-FLONUM FLONUM)
      Returns the integer corresponding to FLONUM when truncated.
      Returns NIL if FLONUM isn't a floating point number
*/
DEFINE_PRIMITIVE ("TRUNCATE-FLONUM", Prim_truncate_flonum, 1, 1, 0)
{
  fast double A;
  long Answer;	/* Faulty VAX/UNIX C optimizer */
  Primitive_1_Arg();

  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  A = Get_Float(Arg1);
  if (flonum_exceeds_fixnum(A))
    return Float_To_Big(A);
  Answer = (long) A;
  return Make_Non_Pointer(TC_FIXNUM, Answer);
}

/* (ROUND-FLONUM FLONUM)
      Returns the integer found by rounding off FLONUM (upward), if
      FLONUM is a floating point number.  Otherwise returns FLONUM.
*/
DEFINE_PRIMITIVE ("ROUND-FLONUM", Prim_round_flonum, 1, 1, 0)
{
  fast double A;
  long Answer;	/* Faulty VAX/UNIX C optimizer */
  Primitive_1_Arg();

  Set_Time_Zone(Zone_Math);
  if (Type_Code(Arg1) != TC_BIG_FLONUM) return Arg1;
  A = Get_Float(Arg1);
  if (A >= 0)
    A += 0.5;
  else
    A -= 0.5;
  if (flonum_exceeds_fixnum(A))
    return Float_To_Big(A);
  Answer = (long) A;
  return Make_Non_Pointer(TC_FIXNUM, Answer);
}
