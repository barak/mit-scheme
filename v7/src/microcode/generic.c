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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/generic.c,v 9.24 1987/07/27 17:47:20 jinx Rel $ */

#include "scheme.h"
#include "primitive.h"
#include "bignum.h"
#include "flonum.h"
#include "zones.h"

Built_In_Primitive(Prim_Zero, 1, "ZERO?", 0xE6)
{
  Primitive_1_Arg();

  Set_Time_Zone(Zone_Math);
  switch (Type_Code(Arg1))
  { case TC_FIXNUM:     if (Get_Integer(Arg1) == 0) return TRUTH;
                        else return NIL;
    case TC_BIG_FLONUM: if (Get_Float(Arg1) == 0.0) return TRUTH;
                        else return NIL;
    case TC_BIG_FIXNUM: if (ZERO_BIGNUM(Fetch_Bignum(Arg1))) return TRUTH;
                        else return NIL;
    default:            Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  /*NOTREACHED*/
}

Pointer
C_Integer_To_Scheme_Integer(C)
     long C;
{
  fast bigdigit *Answer, *SCAN, *size;
  long Length;

  if (Fixnum_Fits(C))
    return Make_Non_Pointer(TC_FIXNUM, C);
  Length = Align(C_INTEGER_LENGTH_AS_BIGNUM);
  Primitive_GC_If_Needed(Length);
  Answer = BIGNUM(Free); 
  Prepare_Header(Answer, 0, (C >= 0) ? POSITIVE : NEGATIVE);
  size   = &LEN(Answer);
  if (C < 0)
    C = - C;
  for (SCAN = Bignum_Bottom(Answer); C != 0; *size += 1)
  {
    *SCAN++ = Rem_Radix(C);
    C = Div_Radix(C);
  }
  *((Pointer *) Answer) = Make_Header(Align(*size));
  Free += Length;
  Debug_Test(Free-Length);
  return Make_Pointer(TC_BIG_FIXNUM, Free-Length);
}

int
Scheme_Integer_To_C_Integer (Arg1, C)
     Pointer Arg1;
     long *C;
{
  int type = Type_Code(Arg1);  
  fast bigdigit *SCAN, *ARG1;
  fast long Answer, i;
  long Length;

  if (type == TC_FIXNUM)
  {
    Sign_Extend(Arg1, *C);
    return PRIM_DONE;
  }
  if (type != TC_BIG_FIXNUM)
    return ERR_ARG_1_WRONG_TYPE;
  ARG1 = BIGNUM(Get_Pointer(Arg1));
  Length = LEN(ARG1);
  if (Length == 0)
    Answer = 0;
  else if (Length > C_INTEGER_LENGTH_AS_BIGNUM)
    return ERR_ARG_1_BAD_RANGE;
  else if (Length < C_INTEGER_LENGTH_AS_BIGNUM)
    for (SCAN=Bignum_Top(ARG1), i=0, Answer=0; i< Length; i++)
      Answer = Mul_Radix(Answer) + *SCAN--;
  else
    /* Length == C_INTEGER_LENGTH_AS_BIGNUM */
    for (SCAN=Bignum_Top(ARG1), i=0, Answer=0; i< Length; i++)
    /* Attempting to take care of overflow problems */
    { Answer = Mul_Radix(Answer);
      if (Answer < 0)
	return ERR_ARG_1_BAD_RANGE;
      Answer = Answer + *SCAN--;
      if (Answer < 0)
	return ERR_ARG_1_BAD_RANGE;
    }
  if NEG_BIGNUM(ARG1)
    Answer = - Answer;
  *C = Answer;
  return PRIM_DONE;
}

Pointer
Fetch_Bignum_One()
{
  return Get_Fixed_Obj_Slot(Bignum_One);
}

/* This is more suitable than `Scheme_Integer_To_C_Integer'
   for some purposes. */

long
object_to_long (object, type_error, range_error)
     Pointer object;
     long type_error, range_error;
{
  fast long result;

  switch (OBJECT_TYPE (object))
    {
    case TC_FIXNUM:
      {
	Sign_Extend (object, result);
	return (result);
      }

    case TC_BIG_FIXNUM:
      {
	fast bigdigit *bignum, *scan;
	fast long length;

	bignum = (BIGNUM (Get_Pointer (object)));
	length = (LEN (bignum));
	if (length == 0)
	  return (0);
	if (length > C_INTEGER_LENGTH_AS_BIGNUM)
	  signal_error_from_primitive (range_error);
	scan = (Bignum_Top (bignum));
	result = 0;
	if (length < C_INTEGER_LENGTH_AS_BIGNUM)
	  while ((length--) > 0)
	    result = ((Mul_Radix (result)) + (*scan--));
	else
	  while ((length--) > 0)
	    {
	      result = (Mul_Radix (result));
	      if (result < 0)
		signal_error_from_primitive (range_error);
	      result = (result + (*scan--));
	      if (result < 0)
		signal_error_from_primitive (range_error);
	    }
	return ((NEG_BIGNUM (bignum)) ? (- result) : result);
      }

    default:
      signal_error_from_primitive (type_error);
    }
}

#define Sign_Check(Normal_Op, Big_Op)					\
  Primitive_1_Arg();							\
  Set_Time_Zone(Zone_Math);						\
  switch (Type_Code(Arg1))						\
  { case TC_FIXNUM:     { long Value;					\
			  Sign_Extend(Arg1, Value);			\
			  if (Value Normal_Op 0) return TRUTH;		\
			  else return NIL;				\
		        }						\
    case TC_BIG_FLONUM: if (Get_Float(Arg1) Normal_Op 0.0) return TRUTH;\
			else return NIL;				\
P2_Sign_Check(Big_Op)

#define P2_Sign_Check(Big_Op)						\
    case TC_BIG_FIXNUM: if ((LEN(Fetch_Bignum(Arg1)) != 0)		\
                            && Big_Op(Fetch_Bignum(Arg1)))		\
			return TRUTH;					\
			else return NIL;				\
    default:		Primitive_Error(ERR_ARG_1_WRONG_TYPE);		\
  }


Built_In_Primitive(Prim_Positive, 1, "POSITIVE?", 0xE7)
{
  Sign_Check(>, POS_BIGNUM);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Negative, 1, "NEGATIVE?", 0xE8)
{
  Sign_Check(<, NEG_BIGNUM);
  /*NOTREACHED*/
}

#define Inc_Dec(Normal_Op, Big_Op)					\
  Primitive_1_Arg();							\
  Set_Time_Zone(Zone_Math);						\
  switch (Type_Code(Arg1))						\
  { case TC_FIXNUM:							\
    { fast long A, Result;						\
      Sign_Extend(Arg1, A);						\
      Result = A Normal_Op 1;						\
      if (Fixnum_Fits(Result))						\
	return Make_Non_Pointer(TC_FIXNUM, Result);			\
P2_Inc_Dec(Normal_Op, Big_Op)

#define P2_Inc_Dec(Normal_Op, Big_Op)					\
      { Pointer Ans = Fix_To_Big(Arg1);					\
	Bignum_Operation(Big_Op(Fetch_Bignum(Ans),			\
			        Fetch_Bignum(Fetch_Bignum_One())),	\
			 Ans);						\
        return Ans;							\
      }									\
    }									\
P3_Inc_Dec(Normal_Op, Big_Op)

#define P3_Inc_Dec(Normal_Op, Big_Op)					\
    case TC_BIG_FLONUM:							\
     Reduced_Flonum_Result(Get_Float(Arg1) Normal_Op 1);		\
    case TC_BIG_FIXNUM:							\
     { Pointer Ans;							\
       Bignum_Operation(Big_Op(Fetch_Bignum(Arg1),			\
			       Fetch_Bignum(Fetch_Bignum_One())),	\
                        Ans);						\
       return Ans;							\
     }									\
    default:		Primitive_Error(ERR_ARG_1_WRONG_TYPE);		\
  }

Built_In_Primitive(Prim_One_Plus, 1, "1+", 0xF1)
{
  Inc_Dec(+, plus_signed_bignum);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_M_1_Plus, 1, "-1+", 0xF2)
{
  Inc_Dec(-, minus_signed_bignum);
  /*NOTREACHED*/
}

#define Two_Op_Comparator(GENERAL_OP, BIG_OP)				\
  Primitive_2_Args();							\
  Set_Time_Zone(Zone_Math);						\
  switch (Type_Code(Arg1))						\
  { case TC_FIXNUM:							\
     { switch (Type_Code(Arg2))						\
       { case TC_FIXNUM:						\
          { long A, B;							\
	    Sign_Extend(Arg1, A);					\
	    Sign_Extend(Arg2, B);					\
	    return (A GENERAL_OP B) ? TRUTH : NIL;			\
	  }								\
P2_Two_Op_Comparator(GENERAL_OP, BIG_OP)

#define P2_Two_Op_Comparator(GENERAL_OP, BIG_OP)			\
	 case TC_BIG_FLONUM:						\
	  { long A;							\
	    Sign_Extend(Arg1, A);					\
	    return (A GENERAL_OP (Get_Float(Arg2))) ? TRUTH : NIL;	\
	  }								\
	 case TC_BIG_FIXNUM:						\
	  { Pointer Ans = Fix_To_Big(Arg1);				\
	    return (big_compare(Fetch_Bignum(Ans),			\
			        Fetch_Bignum(Arg2)) == BIG_OP) ?	\
		   TRUTH : NIL;						\
	  }								\
P3_Two_Op_Comparator(GENERAL_OP, BIG_OP)

#define P3_Two_Op_Comparator(GENERAL_OP, BIG_OP)			\
	 default:							\
	  Primitive_Error(ERR_ARG_2_WRONG_TYPE);			\
       }								\
     }									\
    case TC_BIG_FLONUM:							\
     { switch (Type_Code(Arg2))						\
       { case TC_FIXNUM:						\
          { long B;							\
	    Sign_Extend(Arg2, B);					\
	    return (Get_Float(Arg1) GENERAL_OP B) ? TRUTH : NIL;	\
	  }								\
P4_Two_Op_Comparator(GENERAL_OP, BIG_OP)

#define P4_Two_Op_Comparator(GENERAL_OP, BIG_OP)			\
	 case TC_BIG_FLONUM:						\
	  return (Get_Float(Arg1) GENERAL_OP Get_Float(Arg2)) ? 	\
		 TRUTH : NIL;						\
	 case TC_BIG_FIXNUM:						\
	  { Pointer A;							\
	    A = Big_To_Float(Arg2);					\
	    if (Type_Code(A) == TC_BIG_FLONUM)				\
	      return (Get_Float(Arg1) GENERAL_OP Get_Float(A)) ? 	\
		     TRUTH : NIL;					\
P5_Two_Op_Comparator(GENERAL_OP, BIG_OP)

#define P5_Two_Op_Comparator(GENERAL_OP, BIG_OP)			\
	    Primitive_Error(ERR_ARG_2_FAILED_COERCION);			\
	    }		 						\
	 default:							\
	  Primitive_Error(ERR_ARG_2_WRONG_TYPE);			\
       }		 						\
     }									\
    case TC_BIG_FIXNUM:							\
     { switch (Type_Code(Arg2))						\
       { case TC_FIXNUM:						\
          { Pointer Ans = Fix_To_Big(Arg2);				\
	    return (big_compare(Fetch_Bignum(Arg1),			\
			        Fetch_Bignum(Ans)) == BIG_OP) ?		\
		   TRUTH : NIL;						\
          }								\
P6_Two_Op_Comparator(GENERAL_OP, BIG_OP)

#define P6_Two_Op_Comparator(GENERAL_OP, BIG_OP)			\
	 case TC_BIG_FLONUM:						\
	  { Pointer A = Big_To_Float(Arg1);				\
	    if (Type_Code(A) == TC_BIG_FLONUM)				\
	      return (Get_Float(A) GENERAL_OP Get_Float(Arg2)) ?	\
		     TRUTH : NIL;					\
	    Primitive_Error(ERR_ARG_1_FAILED_COERCION);			\
	  }	 							\
P7_Two_Op_Comparator(GENERAL_OP, BIG_OP)

#define P7_Two_Op_Comparator(GENERAL_OP, BIG_OP)			\
	 case TC_BIG_FIXNUM:						\
	  return (big_compare(Fetch_Bignum(Arg1),			\
			      Fetch_Bignum(Arg2)) == BIG_OP) ?		\
		 TRUTH : NIL;						\
	 default:							\
	  Primitive_Error(ERR_ARG_2_WRONG_TYPE);			\
       }								\
     }									\
    default:   Primitive_Error(ERR_ARG_1_WRONG_TYPE);			\
  }

Built_In_Primitive(Prim_Equal_Number, 2, "&=", 0xE9)
{
  Two_Op_Comparator(==, EQUAL);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Less, 2, "&<", 0xEA)
{
  Two_Op_Comparator(<, TWO_BIGGER);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Greater, 2, "&>", 0xEB)
{
  Two_Op_Comparator(>, ONE_BIGGER);
  /*NOTREACHED*/
}

#define Two_Op_Operator(GENERAL_OP, BIG_OP)				\
  Primitive_2_Args();							\
  Set_Time_Zone(Zone_Math);						\
  switch (Type_Code(Arg1))						\
  { case TC_FIXNUM:							\
     { switch (Type_Code(Arg2))						\
       { case TC_FIXNUM:						\
          { fast long A, B, Result;					\
	    Sign_Extend(Arg1, A);					\
	    Sign_Extend(Arg2, B);					\
	    Result = (A GENERAL_OP B);					\
	    if (Fixnum_Fits(Result))					\
	      return Make_Non_Pointer(TC_FIXNUM, Result);		\
P2_Two_Op_Operator(GENERAL_OP, BIG_OP)

#define P2_Two_Op_Operator(GENERAL_OP, BIG_OP)				\
	    { Pointer Big_Arg1, Big_Arg2, Big_Result;			\
	      Big_Arg1 =  Fix_To_Big(Arg1);				\
	      Big_Arg2 =  Fix_To_Big(Arg2);				\
	      Bignum_Operation(BIG_OP(Fetch_Bignum(Big_Arg1),		\
				      Fetch_Bignum(Big_Arg2)),		\
			       Big_Result);				\
	      return Big_Result;					\
	    }								\
          }								\
P3_Two_Op_Operator(GENERAL_OP, BIG_OP)

#define P3_Two_Op_Operator(GENERAL_OP, BIG_OP)				\
	 case TC_BIG_FLONUM:						\
	  { fast long A;						\
	    Sign_Extend(Arg1, A);					\
	    Reduced_Flonum_Result(A GENERAL_OP Get_Float(Arg2));	\
          }								\
P4_Two_Op_Operator(GENERAL_OP, BIG_OP)

#define P4_Two_Op_Operator(GENERAL_OP, BIG_OP)				\
	 case TC_BIG_FIXNUM:						\
	  { Pointer Big_Arg1 =  Fix_To_Big(Arg1);			\
	    Bignum_Operation(BIG_OP(Fetch_Bignum(Big_Arg1),		\
                                    Fetch_Bignum(Arg2)),		\
                             Big_Arg1);					\
            return Big_Arg1;						\
	  }								\
	 default:							\
	  Primitive_Error(ERR_ARG_2_WRONG_TYPE);			\
       }		 						\
     }									\
P5_Two_Op_Operator(GENERAL_OP, BIG_OP)

#define P5_Two_Op_Operator(GENERAL_OP, BIG_OP)				\
    case TC_BIG_FLONUM:							\
     { switch (Type_Code(Arg2))						\
       { case TC_FIXNUM:						\
          { fast long B;						\
	    Sign_Extend(Arg2, B);					\
	    Reduced_Flonum_Result(Get_Float(Arg1) GENERAL_OP B);	\
	  }								\
	 case TC_BIG_FLONUM:						\
	  Reduced_Flonum_Result(Get_Float(Arg1) GENERAL_OP		\
                                        Get_Float(Arg2));		\
P6_Two_Op_Operator(GENERAL_OP, BIG_OP)

#define P6_Two_Op_Operator(GENERAL_OP, BIG_OP)				\
         case TC_BIG_FIXNUM:						\
	  { Pointer B = Big_To_Float(Arg2);				\
	    if (Type_Code(B) == TC_BIG_FLONUM)				\
	    { Reduced_Flonum_Result(Get_Float(Arg1) GENERAL_OP		\
                                            Get_Float(B));		\
            }								\
	    Primitive_Error(ERR_ARG_2_FAILED_COERCION);			\
          }		 						\
	 default:							\
	  Primitive_Error(ERR_ARG_2_WRONG_TYPE);			\
       }		 						\
     }									\
P7_Two_Op_Operator(GENERAL_OP, BIG_OP)

#define P7_Two_Op_Operator(GENERAL_OP, BIG_OP)				\
    case TC_BIG_FIXNUM:							\
     { switch (Type_Code(Arg2))						\
       { case TC_FIXNUM:						\
          { Pointer Big_Arg2 = Fix_To_Big(Arg2);			\
	    Bignum_Operation(BIG_OP(Fetch_Bignum(Arg1),			\
	                            Fetch_Bignum(Big_Arg2)),		\
                             Big_Arg2);					\
            return Big_Arg2;						\
	  }								\
P8_Two_Op_Operator(GENERAL_OP, BIG_OP)	

#define P8_Two_Op_Operator(GENERAL_OP, BIG_OP)				\
	 case TC_BIG_FLONUM:						\
	  { Pointer A = Big_To_Float(Arg1);				\
	    if (Type_Code(A) == TC_BIG_FLONUM)				\
	    { Reduced_Flonum_Result(Get_Float(A) GENERAL_OP		\
	                                    Get_Float(Arg2));		\
	    }								\
	    Primitive_Error(ERR_ARG_1_FAILED_COERCION);			\
          }		 						\
P9_Two_Op_Operator(GENERAL_OP, BIG_OP)

#define P9_Two_Op_Operator(GENERAL_OP, BIG_OP)				\
	 case TC_BIG_FIXNUM:						\
	  { Pointer Ans;						\
	    Bignum_Operation(BIG_OP(Fetch_Bignum(Arg1),			\
                                    Fetch_Bignum(Arg2)),		\
      		             Ans);					\
            return Ans;							\
	  }								\
	 default:							\
	   Primitive_Error(ERR_ARG_2_WRONG_TYPE);			\
       }		 						\
     }									\
    default:  Primitive_Error(ERR_ARG_1_WRONG_TYPE);			\
  }

Built_In_Primitive(Prim_Plus, 2, "&+", 0xEC)
{
  Two_Op_Operator(+, plus_signed_bignum);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Minus, 2, "&-", 0xED)
{
  Two_Op_Operator(-, minus_signed_bignum);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Multiply, 2, "&*", 0xEE)
{
  /* Mul is machine dependent and lives in os.c */
  extern Pointer Mul();
  Primitive_2_Args();

  Set_Time_Zone(Zone_Math);
  switch (Type_Code(Arg1))
  { case TC_FIXNUM:
     { switch (Type_Code(Arg2))
       { case TC_FIXNUM:
          { fast Pointer Result;
	    Result = Mul(Arg1, Arg2);
	    if (Result != NIL) return Result;
	    { Pointer Big_Arg1, Big_Arg2;
              Big_Arg1 = Fix_To_Big(Arg1);
              Big_Arg2 = Fix_To_Big(Arg2);
	      Bignum_Operation(multiply_signed_bignum(Fetch_Bignum(Big_Arg1),
						      Fetch_Bignum(Big_Arg2)),
			       Big_Arg1);
             return Big_Arg1;
            }
          }
	 case TC_BIG_FLONUM:
	  { fast long A;
	    Sign_Extend(Arg1, A);
	    Reduced_Flonum_Result(A * Get_Float(Arg2));
          }

/* Prim_Multiply continues on the next page */

/* Prim_Multiply, continued */

	 case TC_BIG_FIXNUM:
	  { Pointer Big_Arg1 = Fix_To_Big(Arg1);
	    Bignum_Operation(multiply_signed_bignum(Fetch_Bignum(Big_Arg1),
			                            Fetch_Bignum(Arg2)),
                             Big_Arg1);
	   return Big_Arg1;
	  }
	 default:
	  Primitive_Error(ERR_ARG_2_WRONG_TYPE);
       }
       /*NOTREACHED*/
     }
    case TC_BIG_FLONUM:
     { switch (Type_Code(Arg2))
       { case TC_FIXNUM:
          { fast long B;
	    Sign_Extend(Arg2, B);
	    Reduced_Flonum_Result(Get_Float(Arg1) * B);
          }
	 case TC_BIG_FLONUM:
	  Reduced_Flonum_Result(Get_Float(Arg1) * Get_Float(Arg2));
         case TC_BIG_FIXNUM:
	  { Pointer B = Big_To_Float(Arg2);
	    if (Type_Code(B) == TC_BIG_FLONUM)
	    { Reduced_Flonum_Result(Get_Float(Arg1) * Get_Float(B));
            }
	    Primitive_Error(ERR_ARG_2_FAILED_COERCION);
          }
	  /*NOTREACHED*/
	 default:
	  Primitive_Error(ERR_ARG_2_WRONG_TYPE);
       }
       /*NOTREACHED*/
     }

/* Prim_Multiply continues on the next page */

/* Prim_Multiply, continued */

    case TC_BIG_FIXNUM:
     { switch (Type_Code(Arg2))
       { case TC_FIXNUM:
          { Pointer Big_Arg2 = Fix_To_Big(Arg2);
	    Bignum_Operation(multiply_signed_bignum(Fetch_Bignum(Arg1),
                                                    Fetch_Bignum(Big_Arg2)),
                             Big_Arg2);
            return Big_Arg2;
	  }
	 case TC_BIG_FLONUM:
	  { Pointer A = Big_To_Float(Arg1);
	    if (Type_Code(A) == TC_BIG_FLONUM)
	    { Reduced_Flonum_Result(Get_Float(A) * Get_Float(Arg2));
            }					 
	    Primitive_Error(ERR_ARG_1_FAILED_COERCION);
          }
	  /*NOTREACHED*/
	 case TC_BIG_FIXNUM:
          { Pointer Ans;
            Bignum_Operation(multiply_signed_bignum(Fetch_Bignum(Arg1), 
                                                    Fetch_Bignum(Arg2)),
			     Ans);
	    return Ans;
	  }
	 default:
	   Primitive_Error(ERR_ARG_2_WRONG_TYPE);
       }
       /*NOTREACHED*/
     }
    default:  Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Divide, 2, "&/", 0xEF)
{
  Primitive_2_Args();

  Set_Time_Zone(Zone_Math);
  switch (Type_Code(Arg1))
  { case TC_FIXNUM:
     { switch (Type_Code(Arg2))
       { case TC_FIXNUM:
          { fast long A, B;
	    double Result;
	    Sign_Extend(Arg1, A);
	    Sign_Extend(Arg2, B);
	    if (B==0) Primitive_Error(ERR_ARG_2_BAD_RANGE);
            Result = (double) A / (double) B;
	    Reduced_Flonum_Result(Result);
          }
	 case TC_BIG_FLONUM:
	  { fast long A;
	    Sign_Extend(Arg1, A);
	    if (Get_Float(Arg2) == 0)
	    Primitive_Error(ERR_ARG_2_BAD_RANGE);
	    Reduced_Flonum_Result(((double) A) / Get_Float(Arg2));
          }

/* Prim_Divide continues on the next page */

/* Prim_Divide, continued */

	 case TC_BIG_FIXNUM:
	  { Pointer Big_Arg1, Result, B;
	    long A;
	    if (ZERO_BIGNUM(Fetch_Bignum(Arg2)))
	    Primitive_Error(ERR_ARG_2_BAD_RANGE);
	    Big_Arg1 = Fix_To_Big(Arg1);
	    Divide_Bignum_Operation(div_signed_bignum(Fetch_Bignum(Big_Arg1),
				                      Fetch_Bignum(Arg2)),
				    Result);
	    if (Vector_Ref(Result, CONS_CDR) == Make_Unsigned_Fixnum(0))
	      return (Vector_Ref(Result, CONS_CAR));
	    Sign_Extend(Arg1, A);
	    { B = Big_To_Float(Arg2);
	      if (Type_Code(B) == TC_BIG_FLONUM)
	      { Reduced_Flonum_Result(A / Get_Float(B));
	      }
	      Primitive_Error(ERR_ARG_2_FAILED_COERCION);
	    }
	    /*NOTREACHED*/
	  }
	 default:
	  Primitive_Error(ERR_ARG_2_WRONG_TYPE);
       }
       /*NOTREACHED*/
     }
    case TC_BIG_FLONUM:
     { switch (Type_Code(Arg2))
       { case TC_FIXNUM:
          { fast long B;
            Sign_Extend(Arg2, B);
	    if (B == 0) Primitive_Error(ERR_ARG_2_BAD_RANGE);
	    { Reduced_Flonum_Result(Get_Float(Arg1) / ((double) B));
            }					
          }

/* Prim_Divide continues on the next page */

/* Prim_Divide, continued */

	 case TC_BIG_FLONUM:
	  if (Get_Float(Arg2) == 0)
	    Primitive_Error(ERR_ARG_2_BAD_RANGE);
	  Reduced_Flonum_Result(Get_Float(Arg1) / Get_Float(Arg2));
         case TC_BIG_FIXNUM:
	  { Pointer B;
	    if (ZERO_BIGNUM(Fetch_Bignum(Arg2)))
	    Primitive_Error(ERR_ARG_2_BAD_RANGE);
	    B = Big_To_Float(Arg2);
	    if (Type_Code(B) == TC_BIG_FLONUM)
	    { Reduced_Flonum_Result(Get_Float(Arg1) / Get_Float(B));
            }
	    Primitive_Error(ERR_ARG_2_FAILED_COERCION);
          }
	  /*NOTREACHED*/
	 default:
	  Primitive_Error(ERR_ARG_2_WRONG_TYPE);
       }
       /*NOTREACHED*/
     }

/* Prim_Divide continues on the next page */

/* Prim_Divide, continued */

    case TC_BIG_FIXNUM:
     { switch (Type_Code(Arg2))
       { case TC_FIXNUM:
	  { Pointer Big_Arg2, Result, A;
            Big_Arg2 = Fix_To_Big(Arg2);
	    if (ZERO_BIGNUM(Fetch_Bignum(Big_Arg2)))
              Primitive_Error(ERR_ARG_2_BAD_RANGE);
	    Divide_Bignum_Operation(div_signed_bignum(Fetch_Bignum(Arg1),
				 	             Fetch_Bignum(Big_Arg2)),
				    Result);
	    if (Vector_Ref(Result, CONS_CDR) == Make_Unsigned_Fixnum(0))
	      return (Vector_Ref(Result, CONS_CAR));
	    A = Big_To_Float(Arg1);
	    if (Type_Code(A) == TC_BIG_FLONUM)
	    { long B;
	      Sign_Extend(Arg2, B);
	      Reduced_Flonum_Result(Get_Float(A) / ((double) B));
	    }
	    Primitive_Error(ERR_ARG_1_FAILED_COERCION);
	  }
	  /*NOTREACHED*/
	 case TC_BIG_FLONUM:
	  { Pointer A;
	    if (Get_Float(Arg2) == 0.0)
	    Primitive_Error(ERR_ARG_2_BAD_RANGE);
	    A = Big_To_Float(Arg1);
	    if (Type_Code(A) == TC_BIG_FLONUM)
	    { Reduced_Flonum_Result(Get_Float(A) / Get_Float(Arg2));
	    }
	    Primitive_Error(ERR_ARG_1_FAILED_COERCION);
          }
	  /*NOTREACHED*/

/* Prim_Divide continues on the next page */

/* Prim_Divide, continued */

	 case TC_BIG_FIXNUM:
	  { Pointer Result, A, B;
	    if (ZERO_BIGNUM(Fetch_Bignum(Arg2)))
	      Primitive_Error(ERR_ARG_2_BAD_RANGE);
	    Divide_Bignum_Operation(div_signed_bignum(Fetch_Bignum(Arg1),
                                                      Fetch_Bignum(Arg2)),
                                    Result);
	    if (Vector_Ref(Result, CONS_CDR) == Make_Unsigned_Fixnum(0))
	      return (Vector_Ref(Result, CONS_CAR));
	    A = Big_To_Float(Arg1);
	    if (Type_Code(A) == TC_BIG_FLONUM)
	    { B = Big_To_Float(Arg2);
	      if (Type_Code(B) == TC_BIG_FLONUM)
	      { if (Get_Float(B) == 0)
		  Primitive_Error(ERR_ARG_2_BAD_RANGE);
   	        { Reduced_Flonum_Result(Get_Float(A) / Get_Float(B));
	        }
	      }
	      Primitive_Error(ERR_ARG_2_FAILED_COERCION);
	    }
	    /*NOTREACHED*/
	    Primitive_Error(ERR_ARG_1_FAILED_COERCION);
	  }
	 default:
	   Primitive_Error(ERR_ARG_2_WRONG_TYPE);
       }
       /*NOTREACHED*/
     }
    default:  Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Integer_Divide, 2, "INTEGER-DIVIDE", 0xF0)
{
  Primitive_2_Args();

  Set_Time_Zone(Zone_Math);
  switch (Type_Code(Arg1))
  { case TC_FIXNUM:
     { switch (Type_Code(Arg2))
       { case TC_FIXNUM:
          { fast long A, B, C, D;
	    Pointer *Cons_Cell;
	    Sign_Extend(Arg1, A);
	    Sign_Extend(Arg2, B);
	    if (B == 0)
	    Primitive_Error(ERR_ARG_2_BAD_RANGE);
	    Primitive_GC_If_Needed(2);
	    /* These (C & D) are necessary because Make_Non_Pointer casts to
	       Pointer which is unsigned long, and then the arithmetic is wrong
	       if the operations are placed in the macro "call". */
	    C = A / B;
	    D = A % B;
	    Cons_Cell = Free;
	    Free += 2;
	    Cons_Cell[CONS_CAR] = Make_Non_Pointer(TC_FIXNUM, C);
	    Cons_Cell[CONS_CDR] = Make_Non_Pointer(TC_FIXNUM, D);
	    return Make_Pointer(TC_LIST, Cons_Cell);
          }
	 case TC_BIG_FIXNUM:
	  { Pointer Big_Arg1, Pair;
	    if (ZERO_BIGNUM(Fetch_Bignum(Arg2)))
	    Primitive_Error(ERR_ARG_2_BAD_RANGE);
	    Big_Arg1 = Fix_To_Big(Arg1);
	    Divide_Bignum_Operation(div_signed_bignum(Fetch_Bignum(Big_Arg1),
				                      Fetch_Bignum(Arg2)),
  				    Pair);
	    return Pair;
	  }
	 default:
	  Primitive_Error(ERR_ARG_2_WRONG_TYPE);
       }
       /*NOTREACHED*/
     }

/* Prim_Integer_Divide continues on the next page */

/* Prim_Integer_Divide, continued */

    case TC_BIG_FIXNUM:
     { switch (Type_Code(Arg2))
       { case TC_FIXNUM:
	  { Pointer Big_Arg2, Pair;
	    if (Get_Integer(Arg2) == 0)
	    Primitive_Error(ERR_ARG_2_BAD_RANGE);
	    Big_Arg2 = Fix_To_Big(Arg2);
	    Divide_Bignum_Operation(div_signed_bignum(Fetch_Bignum(Arg1),
				                     Fetch_Bignum(Big_Arg2)),
    				    Pair);
	    return Pair;
	  }
	 case TC_BIG_FIXNUM:
	  { Pointer Pair;
	    if (ZERO_BIGNUM(Fetch_Bignum(Arg2)))
	    Primitive_Error(ERR_ARG_2_BAD_RANGE);
            Divide_Bignum_Operation(div_signed_bignum(Fetch_Bignum(Arg1),
				                      Fetch_Bignum(Arg2)),
                                    Pair);
	    return Pair;
          }
	 default:
	   Primitive_Error(ERR_ARG_2_WRONG_TYPE);
       }
       /*NOTREACHED*/
     }
    default:  Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  /*NOTREACHED*/
}

/* Generic sqrt and transcendental functions are created by generalizing
   their floating point counterparts.
*/

#define Generic_Function(Routine)					\
  double Routine();							\
  Primitive_1_Arg();							\
									\
  Set_Time_Zone(Zone_Math);						\
  switch (Type_Code(Arg1))						\
  { case TC_FIXNUM:							\
     { long Arg;							\
       Sign_Extend(Arg1, Arg);						\
       Reduced_Flonum_Result(Routine((double) Arg));			\
     }									\
    case TC_BIG_FLONUM:							\
     Reduced_Flonum_Result(Routine(Get_Float(Arg1)));			\
    case TC_BIG_FIXNUM:							\
     { Pointer A = Big_To_Float(Arg1);					\
       if (Type_Code(A) != TC_BIG_FLONUM)				\
         Primitive_Error(ERR_ARG_1_FAILED_COERCION);			\
       Reduced_Flonum_Result(Routine(Get_Float(A)));			\
     }									\
    default: Primitive_Error(ERR_ARG_1_WRONG_TYPE);			\
  }

/* This horrible hack because there are no lambda-expressions in C. */

#define Generic_Restriction(Lambda, Routine, Restriction)		\
double									\
Lambda(arg)								\
    fast double arg;							\
{									\
  double Routine();							\
									\
  if (arg Restriction 0.0)						\
    Primitive_Error(ERR_ARG_1_BAD_RANGE);				\
  return Routine(arg);							\
}

/* And here the functions themselves */

Generic_Restriction(Scheme_Sqrt, sqrt, <)
Generic_Restriction(Scheme_Ln, log, <=)

Built_In_Primitive(Prim_Sqrt, 1, "SQRT", 0xF7)
{
  Generic_Function(Scheme_Sqrt);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Exp, 1, "EXP", 0xF8)
{
  Generic_Function(exp);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Ln, 1, "LOG", 0xF9)
{
  Generic_Function(Scheme_Ln);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Sine, 1, "SIN", 0xFA)
{
  Generic_Function(sin);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Cosine, 1, "COS", 0xFB)
{
  Generic_Function(cos);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Arctan, 1, "&ATAN", 0xFC)
{
  Generic_Function(atan);
  /*NOTREACHED*/
}

/* Coercions from Floating point to integers.

   There are four possible ways to coerce:

   - Truncate   : towards 0.
   - Round      : towards closest integer.
   - Floor	: towards -infinity.
   - Ceiling    : towards +infinity.

   All these primitives differ only in how floating point numbers
   are mapped before they are truncated.
*/

#ifdef HAS_FLOOR

extern double floor(), ceil();

#else

double 
floor(arg)
     double arg;
{
  long temp;
  double narg;

  temp = ((long) arg);
  narg = ((double) temp);
  if ((narg == arg) || (arg > 0.0))
    return (narg);
  else
    return (narg - 1.0);
}

double
ceil(arg)
     double arg;
{
  long temp;
  double narg;

  temp = ((long) arg);
  narg = ((double) temp);
  if ((narg == arg) || (arg < 0.0))
    return (narg);
  else
    return (narg + 1.0);
}

#endif

#define Truncate_Mapping(arg)	arg
#define Round_Mapping(arg)	((arg) >= 0.0 ? ((arg) + 0.5) : ((arg) - 0.5))
#define Floor_Mapping(arg)	floor(arg)
#define Ceiling_Mapping(arg)    ceil(arg)

#define Flonum_To_Integer(How_To_Do_It)					\
  Primitive_1_Arg();							\
									\
  Set_Time_Zone(Zone_Math);						\
  switch (Type_Code(Arg1))						\
  {									\
    case TC_FIXNUM :							\
    case TC_BIG_FIXNUM:							\
      return Arg1;							\
    case TC_BIG_FLONUM:							\
      {									\
	fast double Arg, temp;						\
	Pointer Result;							\
									\
	Arg = Get_Float(Arg1);						\
	temp = How_To_Do_It(Arg);					\
	if (flonum_exceeds_fixnum(temp))				\
	  Result = Float_To_Big(temp);					\
        else								\
	  double_into_fixnum(temp, Result);				\
        return Result;							\
      }									\
    default: Primitive_Error(ERR_ARG_1_WRONG_TYPE);			\
  }

Built_In_Primitive(Prim_Truncate, 1, "TRUNCATE", 0xF3)
{
  Flonum_To_Integer(Truncate_Mapping);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Round, 1, "ROUND", 0xF4)
{
  Flonum_To_Integer(Round_Mapping);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Floor, 1, "FLOOR", 0xF5)
{
  Flonum_To_Integer(Floor_Mapping);
  /*NOTREACHED*/
}

Built_In_Primitive(Prim_Ceiling, 1, "CEILING", 0xF6)
{
  Flonum_To_Integer(Ceiling_Mapping);
  /*NOTREACHED*/
}
