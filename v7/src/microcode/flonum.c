/*          Hey EMACS, this is -*- C -*- code!                 */

/****************************************************************
*                                                               *
*                         Copyright (c) 1984                    *
*               Massachusetts Institute of Technology           *
*                                                               *
* This material was developed by the Scheme project at the      *
* Massachusetts Institute of Technology, Department of          *
* Electrical Engineering and Computer Science.  Permission to   *
* copy this software, to redistribute it, and to use it for any *
* purpose is granted, subject to the following restrictions and *
* understandings.                                               *
*                                                               *
* 1. Any copy made of this software must include this copyright *
* notice in full.                                               *
*                                                               *
* 2. Users of this software agree to make their best efforts (a)*
* to return to the MIT Scheme project any improvements or       *
* extensions that they make, so that these may be included in   *
* future releases; and (b) to inform MIT of noteworthy uses of  *
* this software.                                                *
*                                                               *
* 3.  All materials developed as a consequence of the use of    *
* this software shall duly acknowledge such use, in accordance  *
* with the usual standards of acknowledging credit in academic  *
* research.                                                     *
*                                                               *
* 4. MIT has made no warrantee or representation that the       *
* operation of this software will be error-free, and MIT is     *
* under no obligation to provide any services, by way of        *
* maintenance, update, or otherwise.                            *
*                                                               *
* 5.  In conjunction with products arising from the use of this *
* material, there shall be no use of the name of the            *
* Massachusetts Institute of Technology nor of any adaptation   *
* thereof in any advertising, promotional, or sales literature  *
* without prior written consent from MIT in each case.          *
*                                                               *
****************************************************************/

/* File: FLONUM.C
 *
 * This file contains support for floating point arithmetic.  Most
 * of these primitives have been superceded by generic arithmetic.
 */

#include "scheme.h"
#include "primitive.h"
#include "flonum.h"
#include "zones.h"

                /************************************/
                /* BINARY FLOATING POINT OPERATIONS */
                /************************************/

/*          See flohead.c for floating point macros.                     */

/* The binary floating point operations return NIL if either argument
   is not a floating point number.  Otherwise they return the
   appropriate result.
*/

Built_In_Primitive(Prim_Divide_Flonum, 2, "DIVIDE-FLONUM")
{ Primitive_2_Args();
  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  if (Get_Float(Arg2) == 0) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  Flonum_Result(Get_Float(Arg1) / Get_Float(Arg2));
}

Built_In_Primitive(Prim_Minus_Flonum, 2, "MINUS-FLONUM")
{ Primitive_2_Args();
  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(Get_Float(Arg1) - Get_Float(Arg2));
}

Built_In_Primitive(Prim_Multiply_Flonum, 2, "MULTIPLY-FLONUM")
{ Primitive_2_Args();
  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(Get_Float(Arg1) * Get_Float(Arg2));
}

Built_In_Primitive(Prim_Plus_Flonum, 2, "PLUS-FLONUM")
{ Primitive_2_Args();
  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(Get_Float(Arg1) + Get_Float(Arg2));
}

	        /************************************/
                /* BINARY FLOATING POINT PREDICATES */
	        /************************************/

/* The binary flonum predicates return NIL if either of the arguments
   is not a flonum. Otherwise, return a fixnum 1 if the predicate is
   true, or a fixnum 0 if it is false.
*/

Built_In_Primitive(Prim_Equal_Flonum, 2, "EQUAL-FLONUM?")
{ Primitive_2_Args();
  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  return FIXNUM_0+
           (((Get_Float(Arg1)) == (Get_Float(Arg2))) ? 1 : 0);
}

Built_In_Primitive(Prim_Greater_Flonum, 2, "GREATER-FLONUM?")
{ Primitive_2_Args();
  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  return FIXNUM_0+
           (((Get_Float(Arg1)) > (Get_Float(Arg2))) ? 1 : 0);
}

Built_In_Primitive(Prim_Less_Flonum, 2, "LESS-FLONUM?")
{ Primitive_2_Args();
  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  return FIXNUM_0+(((Get_Float(Arg1)) < (Get_Float(Arg2))) ? 1 : 0);
}

	        /***********************************/
                /* UNARY FLOATING POINT OPERATIONS */
                /***********************************/

/* The unary flonum operations return NIL if their argument is
   not a flonum. Otherwise, they return the appropriate result.
*/

Built_In_Primitive(Prim_Arctan_Flonum, 1, "ARCTAN-FLONUM")
{ double atan();
  Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(atan(Get_Float(Arg1)));
}

Built_In_Primitive(Prim_Cosine_Flonum, 1, "COSINE-FLONUM")
{ double cos();
  Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(cos(Get_Float(Arg1)));
}

Built_In_Primitive(Prim_Exp_Flonum, 1, "EXP-FLONUM")
{ double exp();
  Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(exp(Get_Float(Arg1)));
}

Built_In_Primitive(Prim_Ln_Flonum, 1, "LN-FLONUM")
{ double log();
  Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  if (Arg1 <= 0.0)
  Primitive_Error(ERR_ARG_1_BAD_RANGE);
  Flonum_Result(log(Get_Float(Arg1)));
}

Built_In_Primitive(Prim_Sine_Flonum, 1, "SINE-FLONUM")
{ double sin();
  Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Flonum_Result(sin(Get_Float(Arg1)));
}

Built_In_Primitive(Prim_Sqrt_Flonum, 1, "SQRT-FLONUM")
{ double sqrt(), Arg;
  Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  Arg = Get_Float(Arg1);
  if (Arg < 0) return NIL;
  Flonum_Result(sqrt(Arg));
}

Built_In_Primitive(Prim_Negative_Flonum, 1, "NEGATIVE-FLONUM?")
{ Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  return FIXNUM_0+ ((Get_Float(Arg1) < 0.0) ? 1 : 0);
}

Built_In_Primitive(Prim_Positive_Flonum, 1, "POSITIVE-FLONUM?")
{ Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  return FIXNUM_0+ ((Get_Float(Arg1) > 0.0) ? 1 : 0);
}

Built_In_Primitive(Prim_Zero_Flonum, 1, "ZERO-FLONUM?")
{ Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  return FIXNUM_0+ ((Get_Float(Arg1) == 0.0) ? 1 : 0);
}

/* (INT_TO_FLOAT FIXNUM-OR-BIGNUM)
      [Primitive number 0x72]
      Returns the floating point number (flonum) corresponding to
      either a bignum or a fixnum.  If the bignum is too large or small
      to be converted to floating point, or if the argument isn't of
      the correct type, FIXNUM-OR-BIGNUM is returned unchanged.
*/
Built_In_Primitive(Prim_Int_To_Float, 1, "INT->FLOAT")
{ Primitive_1_Arg();
  Set_Time_Zone(Zone_Math);
  if (Type_Code(Arg1)==TC_FIXNUM)
  { long Int;
    Sign_Extend(Arg1, Int);
    return Allocate_Float((double) Int);
  }
  if (Type_Code(Arg1)==TC_BIG_FIXNUM) return Big_To_Float(Arg1);
  return Arg1;
}

/* (ROUND_FLONUM FLONUM)
      [Primitive number 0x71]
      Returns the integer found by rounding off FLONUM (upward), if
      FLONUM is a floating point number.  Otherwise returns FLONUM.
*/
Built_In_Primitive(Prim_Round_Flonum, 1, "ROUND-FLONUM")
{ fast double A;
  long Answer;	/* Faulty VAX/UNIX C optimizer */
  Primitive_1_Arg();
  Set_Time_Zone(Zone_Math);
  if (Type_Code(Arg1) != TC_BIG_FLONUM) return Arg1;
  A = Get_Float(Arg1);
  if (A >= 0) A += 0.5; else A -= 0.5;
  if (flonum_exceeds_fixnum(A)) return Float_To_Big(A);
  Answer = (long) A;
  return Make_Non_Pointer(TC_FIXNUM, Answer);
}

/* (TRUNCATE_FLONUM FLONUM)
      [Primitive number 0x70]
      Returns the integer corresponding to FLONUM when truncated.
      Returns NIL if FLONUM isn't a floating point number
*/
Built_In_Primitive(Prim_Truncate_Flonum, 1, "TRUNCATE-FLONUM")
{ fast double A;
  long Answer;	/* Faulty VAX/UNIX C optimizer */
  Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  Set_Time_Zone(Zone_Math);
  A = Get_Float(Arg1);
  if (flonum_exceeds_fixnum(A)) return Float_To_Big(A);
  Answer = (long) A;
  return Make_Non_Pointer(TC_FIXNUM, Answer);
}
