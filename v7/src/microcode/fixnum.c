/* -*-C-*-

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/fixnum.c,v 9.21 1987/01/22 14:25:24 jinx Exp $
 *
 * Support for fixed point arithmetic (24 bit).  Mostly superceded
 * by generic arithmetic.
 */

#include "scheme.h"
#include "primitive.h"

                    /***************************/
                    /* UNARY FIXNUM OPERATIONS */
                    /***************************/

/* These operations return NIL if their argument is not a fixnum.
   Otherwise, they return the appropriate fixnum if the result is
   expressible as a fixnum.  If the result is out of range, they
   return NIL.
*/

Built_In_Primitive(Prim_One_Plus_Fixnum, 1, "ONE-PLUS-FIXNUM")
{ fast long A, Result;
  Primitive_1_Arg();
  Arg_1_Type(TC_FIXNUM);
  Sign_Extend(Arg1, A);
  Result = A + 1;
  if (Fixnum_Fits(Result)) return Make_Non_Pointer(TC_FIXNUM, Result);
  else return NIL;
}

Built_In_Primitive(Prim_M_1_Plus_Fixnum, 1, "MINUS-ONE-PLUS-FIXNUM")
{ fast long A, Result;
  Primitive_1_Arg();
  Arg_1_Type(TC_FIXNUM);
  Sign_Extend(Arg1, A);
  Result = A - 1;
  if (Fixnum_Fits(Result)) return Make_Non_Pointer(TC_FIXNUM, Result);
  else return NIL;
}

                    /****************************/
                    /* BINARY FIXNUM PREDICATES */
                    /****************************/

/* Binary fixnum predicates return NIL if their argument is not a
   fixnum, 1 if the predicate is true, or 0 if the predicate is false.
*/

#define Binary_Predicate_Fixnum(Op)		\
  fast long A, B;				\
  Primitive_2_Args();				\
  Arg_1_Type(TC_FIXNUM);			\
  Arg_2_Type(TC_FIXNUM);			\
  Sign_Extend(Arg1, A); Sign_Extend(Arg2, B);	\
  return FIXNUM_0+ ((A Op B) ? 1 : 0);

Built_In_Primitive(Prim_Equal_Fixnum, 2, "EQUAL-FIXNUM?")
{ Binary_Predicate_Fixnum(==);
}

Built_In_Primitive(Prim_Greater_Fixnum, 2, "LESS-FIXNUM?")
{ Binary_Predicate_Fixnum(>);
}

Built_In_Primitive(Prim_Less_Fixnum, 2, "GREATER-FIXNUM?")
{ Binary_Predicate_Fixnum(<);
}

                    /****************************/
                    /* BINARY FIXNUM OPERATIONS */
                    /****************************/

/* All binary fixnum operations take two arguments and return NIL if
   either is not a fixnum.  If both arguments are fixnums and the
   result fits as a fixnum, then the result is returned.  If the
   result will not fit as a fixnum, NIL is returned.
*/

#define Binary_Fixnum(Op)					\
  fast long A, B, Result;					\
  Primitive_2_Args();						\
  Arg_1_Type(TC_FIXNUM);					\
  Arg_2_Type(TC_FIXNUM);					\
  Sign_Extend(Arg1, A); Sign_Extend(Arg2, B);			\
  Result = A Op B;						\
  if (Fixnum_Fits(Result))					\
    return Make_Non_Pointer(TC_FIXNUM, Result);			\
  else return NIL;						\

Built_In_Primitive(Prim_Minus_Fixnum, 2, "MINUS-FIXNUM")
{ Binary_Fixnum(-);
}

Built_In_Primitive(Prim_Plus_Fixnum, 2, "PLUS-FIXNUM")
{ Binary_Fixnum(+);
}

Built_In_Primitive(Prim_Multiply_Fixnum, 2, "MULTIPLY-FIXNUM")
{ /* Mul, which does the multiplication with overflow handling is
     machine dependent.  Therefore, it is in OS.C
  */
  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  return Mul(Arg1, Arg2);
}

Built_In_Primitive(Prim_Divide_Fixnum, 2, "DIVIDE-FIXNUM")
{ fast long A, B, Quotient, Remainder;
  /* Returns the CONS of quotient and remainder */
  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  Sign_Extend(Arg1, A); Sign_Extend(Arg2, B);
  if (B==0) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  Primitive_GC_If_Needed(2);
  Quotient = A/B;
  Remainder = A%B;
  if (Fixnum_Fits(Quotient))
  { Free[CONS_CAR] = Make_Non_Pointer(TC_FIXNUM, Quotient);
    Free[CONS_CDR] = Make_Non_Pointer(TC_FIXNUM, Remainder);
    Free += 2;
    return Make_Pointer(TC_LIST, Free-2);
  }
  return NIL;
}

Built_In_Primitive(Prim_Gcd_Fixnum, 2, "GCD-FIXNUM")
{ fast long A, B, C;
  /* Returns the Greatest Common Divisor */
  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  Sign_Extend(Arg1, A); Sign_Extend(Arg2, B);
  while (B != 0)
  { C = A;
    A = B;
    B = C % B;
  }
  return Make_Non_Pointer(TC_FIXNUM, A);
}

/* (NEGATIVE_FIXNUM NUMBER)
      [Primitive number 0x7F]
      Returns NIL if NUMBER isn't a fixnum.  Returns 0 if NUMBER < 0, 1
      if NUMBER >= 0.
*/
Built_In_Primitive(Prim_Negative_Fixnum, 1, "NEGATIVE-FIXNUM?")
{ long Value;
  Primitive_1_Arg();
  Arg_1_Type(TC_FIXNUM);
  Sign_Extend(Arg1, Value);
  return FIXNUM_0 + ((Value < 0) ? 1 : 0);
}

/* (POSITIVE_FIXNUM NUMBER)
      [Primitive number 0x41]
      Returns 1 if NUMBER is a positive fixnum, 0 for other fixnums,
      or NIL.
*/
Built_In_Primitive(Prim_Positive_Fixnum, 1, "POSITIVE-FIXNUM?")
{ long Value;
  Primitive_1_Arg();
  Arg_1_Type(TC_FIXNUM);
  Sign_Extend(Arg1, Value);
  return FIXNUM_0 + ((Value > 0) ? 1 : 0);
}

/* (ZERO_FIXNUM NUMBER)
      [Primitive number 0x46]
      Returns NIL if NUMBER isn't a fixnum.  Otherwise, returns 0 if
      NUMBER is 0 or 1 if it is.
*/
Built_In_Primitive(Prim_Zero_Fixnum, 1, "ZERO-FIXNUM?")
{ Primitive_1_Arg();
  Arg_1_Type(TC_FIXNUM);
  return FIXNUM_0+((Get_Integer(Arg1) == 0) ? 1 : 0);
}
