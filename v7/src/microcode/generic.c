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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/generic.c,v 9.28 1988/08/16 17:49:29 markf Exp $ */

#include "scheme.h"
#include "prims.h"
#include "bignum.h"
#include "flonum.h"
#include "zones.h"

/* Complex Number Macros.  Should have its own file. */

#define REAL_PART(arg) (Vector_Ref ((arg), COMPLEX_REAL))
#define IMAG_PART(arg) (Vector_Ref ((arg), COMPLEX_IMAG))

/* Expands ARG thrice. Be careful */
#define COERCE_REAL_PART(arg) ((COMPLEX_P (arg)) ? (REAL_PART (arg)) : (arg))

#define COERCE_IMAG_PART(arg)						\
  ((COMPLEX_P (arg)) ? (IMAG_PART (arg)) : FIXNUM_ZERO)

#define RETURN_COMPLEX(real, imag)					\
{									\
  Pointer _real_value = (real);						\
  Pointer _imag_value = (imag);						\
									\
  if (basic_zero_p (_imag_value))					\
    PRIMITIVE_RETURN (_real_value);					\
  else									\
    {									\
      Primitive_GC_If_Needed (2);					\
      (*Free++) = _real_value;						\
      (*Free++) = _imag_value;						\
      PRIMITIVE_RETURN (Make_Pointer (TC_COMPLEX, (Free - 2)));		\
    }									\
}

static Pointer
basic_zero_p (number)
     fast Pointer number;
{ 
  switch (OBJECT_TYPE (number))
    {
    case TC_FIXNUM:
      return ((Get_Integer (number)) == 0);
    case TC_BIG_FLONUM:
      return ((Get_Float (number)) == 0.0);
    case TC_BIG_FIXNUM:
      return (ZERO_BIGNUM (Fetch_Bignum (number)));
    default:
      error_wrong_type_arg (1);
    }
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("ZERO?", Prim_zero, 1, 1, 0)
{
  Primitive_1_Arg();
  Set_Time_Zone(Zone_Math);
  
  if (COMPLEX_P (Arg1))
    PRIMITIVE_RETURN
      ((basic_zero_p (REAL_PART (Arg1)))
       ? (BOOLEAN_TO_OBJECT (basic_zero_p (IMAG_PART (Arg1))))
       : SHARP_F);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (basic_zero_p (Arg1)));
}

Pointer
C_Integer_To_Scheme_Integer (C)
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

#define SIGN_CHECK(Normal_Op, Big_Op)					\
{									\
  Primitive_1_Arg ();							\
									\
  Set_Time_Zone (Zone_Math);						\
  switch (OBJECT_TYPE (Arg1))						\
    {									\
    case TC_FIXNUM:							\
      {									\
	long Value;							\
									\
	Sign_Extend (Arg1, Value);					\
	PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (Value Normal_Op 0));	\
      }									\
									\
    case TC_BIG_FLONUM:							\
      PRIMITIVE_RETURN							\
	(BOOLEAN_TO_OBJECT ((Get_Float (Arg1)) Normal_Op (0.0)));	\
									\
    case TC_BIG_FIXNUM:							\
      PRIMITIVE_RETURN							\
	(BOOLEAN_TO_OBJECT (((LEN (Fetch_Bignum (Arg1))) != 0) &&	\
			    (Big_Op (Fetch_Bignum (Arg1)))));		\
									\
    default:								\
      error_wrong_type_arg (1);						\
    }									\
}

DEFINE_PRIMITIVE ("POSITIVE?", Prim_positive, 1, 1, 0)
     SIGN_CHECK (>, POS_BIGNUM)

DEFINE_PRIMITIVE ("NEGATIVE?", Prim_negative, 1, 1, 0)
     SIGN_CHECK (<, NEG_BIGNUM)

#define Inc_Dec(Normal_Op, Big_Op, Complex_Op)				\
  Primitive_1_Arg();							\
  Set_Time_Zone(Zone_Math);						\
  switch (Type_Code(Arg1))						\
  { case TC_COMPLEX:							\
    { Primitive_GC_If_Needed(2);					\
      *Free++ = Complex_Op(REAL_PART(Arg1));				\
      *Free++ = IMAG_PART(Arg1);					\
      return Make_Pointer(TC_COMPLEX, (Free - 2));			\
    }									\
Inc_Dec_Basic_Cases(Normal_Op, Big_Op)

#define Basic_Inc_Dec(Normal_Op, Big_Op)				\
  switch (Type_Code(Arg1))						\
  {									\
Inc_Dec_Basic_Cases(Normal_Op, Big_Op)

#define Inc_Dec_Basic_Cases(Normal_Op, Big_Op)				\
    case TC_FIXNUM:							\
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

Pointer
C_One_Plus (Arg1)
  fast Pointer Arg1;
{ 
  Basic_Inc_Dec(+, plus_signed_bignum);
}

Pointer
C_One_Minus (Arg1)
     fast Pointer Arg1;
{ 
  Basic_Inc_Dec(-, minus_signed_bignum);
}

DEFINE_PRIMITIVE ("1+", Prim_one_plus, 1, 1, 0)
{
  Inc_Dec(+, plus_signed_bignum, C_One_Plus);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("-1+", Prim_m_1_plus, 1, 1, 0)
{
  Inc_Dec(-, minus_signed_bignum, C_One_Minus);
  /*NOTREACHED*/
}

#define Two_Op_Comparator(GENERAL_OP, BIG_OP)				\
  Primitive_2_Args();							\
  Set_Time_Zone(Zone_Math);						\
  Basic_Two_Op_Comparator(GENERAL_OP, BIG_OP)

#define Basic_Two_Op_Comparator(GENERAL_OP, BIG_OP)			\
  switch (Type_Code(Arg1))						\
  { case TC_FIXNUM:							\
     { switch (Type_Code(Arg2))						\
       { case TC_FIXNUM:						\
          { long A, B;							\
	    Sign_Extend(Arg1, A);					\
	    Sign_Extend(Arg2, B);					\
	    return (BOOLEAN_TO_OBJECT (A GENERAL_OP B));		\
	  }								\
	 P2_Two_Op_Comparator(GENERAL_OP, BIG_OP)

#define P2_Two_Op_Comparator(GENERAL_OP, BIG_OP)			\
	 case TC_BIG_FLONUM:						\
	  { long A;							\
	    Sign_Extend(Arg1, A);					\
	    return (BOOLEAN_TO_OBJECT (A GENERAL_OP (Get_Float(Arg2)))); \
	  }								\
	 case TC_BIG_FIXNUM:						\
	  { Pointer Ans = Fix_To_Big(Arg1);				\
	    return (BOOLEAN_TO_OBJECT (big_compare(Fetch_Bignum(Ans),	\
			        Fetch_Bignum(Arg2)) == BIG_OP));	\
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
	    return (BOOLEAN_TO_OBJECT (Get_Float(Arg1) GENERAL_OP B));	\
	  }								\
	 P4_Two_Op_Comparator(GENERAL_OP, BIG_OP)

#define P4_Two_Op_Comparator(GENERAL_OP, BIG_OP)			\
	 case TC_BIG_FLONUM:						\
	  return							\
	    (BOOLEAN_TO_OBJECT (Get_Float(Arg1) GENERAL_OP		\
				Get_Float(Arg2)));			\
	 case TC_BIG_FIXNUM:						\
	  { Pointer A;							\
	    A = Big_To_Float(Arg2);					\
	    if (Type_Code(A) == TC_BIG_FLONUM)				\
	      return							\
		(BOOLEAN_TO_OBJECT (Get_Float(Arg1) GENERAL_OP		\
				    Get_Float(A)));			\
	    P5_Two_Op_Comparator(GENERAL_OP, BIG_OP)

#define P5_Two_Op_Comparator(GENERAL_OP, BIG_OP)			\
	    Primitive_Error(ERR_ARG_2_FAILED_COERCION);			\
	    }								\
	 default:							\
	  Primitive_Error(ERR_ARG_2_WRONG_TYPE);			\
       }								\
     }									\
    case TC_BIG_FIXNUM:							\
     { switch (Type_Code(Arg2))						\
       { case TC_FIXNUM:						\
          { Pointer Ans = Fix_To_Big(Arg2);				\
	    return (BOOLEAN_TO_OBJECT (big_compare(Fetch_Bignum(Arg1),	\
			        Fetch_Bignum(Ans)) == BIG_OP));		\
          }								\
	 P6_Two_Op_Comparator(GENERAL_OP, BIG_OP)

#define P6_Two_Op_Comparator(GENERAL_OP, BIG_OP)			\
	 case TC_BIG_FLONUM:						\
	  { Pointer A = Big_To_Float(Arg1);				\
	    if (Type_Code(A) == TC_BIG_FLONUM)				\
	      return							\
		(BOOLEAN_TO_OBJECT (Get_Float(A) GENERAL_OP		\
				    Get_Float(Arg2)));			\
	    Primitive_Error(ERR_ARG_1_FAILED_COERCION);			\
	  }								\
	 P7_Two_Op_Comparator(GENERAL_OP, BIG_OP)

#define P7_Two_Op_Comparator(GENERAL_OP, BIG_OP)			\
	 case TC_BIG_FIXNUM:						\
	  return (BOOLEAN_TO_OBJECT (big_compare(Fetch_Bignum(Arg1),	\
			      Fetch_Bignum(Arg2)) == BIG_OP));		\
	 default:							\
	  Primitive_Error(ERR_ARG_2_WRONG_TYPE);			\
       }								\
     }									\
    default:   Primitive_Error(ERR_ARG_1_WRONG_TYPE);			\
  }

Pointer
Basic_Equal_Number (Arg1, Arg2)
     fast Pointer Arg1, Arg2;
{
  Basic_Two_Op_Comparator (==, EQUAL);
}
  
DEFINE_PRIMITIVE ("&=", Prim_equal_number, 2, 2, 0)
{
  Primitive_2_Args ();
  Set_Time_Zone (Zone_Math);
  
  if ((COMPLEX_P (Arg1)) && (COMPLEX_P (Arg2)))
    PRIMITIVE_RETURN
      (((Basic_Equal_Number ((REAL_PART (Arg1)), (REAL_PART (Arg2)))) == SHARP_T)
       ? (Basic_Equal_Number ((IMAG_PART (Arg1)), (IMAG_PART (Arg2))))
       : SHARP_F);
  else if ((COMPLEX_P (Arg1)) || (COMPLEX_P (Arg2)))
    PRIMITIVE_RETURN (SHARP_F);
  Basic_Two_Op_Comparator (==, EQUAL)
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("&<", Prim_less, 2, 2, 0)
{
  Two_Op_Comparator (<, TWO_BIGGER);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("&>", Prim_greater, 2, 2, 0)
{
  Two_Op_Comparator (>, ONE_BIGGER);
  /*NOTREACHED*/
}

#define Two_Op_Operator(GENERAL_OP, BIG_OP, COMPLEX_OP)			\
  Primitive_2_Args();							\
  Set_Time_Zone(Zone_Math);						\
									\
  if ((COMPLEX_P (Arg2))) goto complex_handler;				\
									\
  switch (Type_Code(Arg1))						\
  {									\
  case TC_COMPLEX:							\
  complex_handler:							\
    RETURN_COMPLEX							\
      ((COMPLEX_OP ((COERCE_REAL_PART (Arg1)),				\
		    (COERCE_REAL_PART (Arg2)))),			\
       (COMPLEX_OP ((COERCE_IMAG_PART (Arg1)),				\
		    (COERCE_IMAG_PART (Arg2)))));			\
Two_Op_Operator_Basic_Cases(GENERAL_OP, BIG_OP)

#define Basic_Two_Op_Operator(GENERAL_OP, BIG_OP)			\
  switch (Type_Code(Arg1))						\
  {									\
Two_Op_Operator_Basic_Cases(GENERAL_OP, BIG_OP)

#define Two_Op_Operator_Basic_Cases(GENERAL_OP, BIG_OP)			\
    case TC_FIXNUM:							\
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

static Pointer basic_plus(Arg1, Arg2)
fast Pointer Arg1, Arg2;
{ 
  Basic_Two_Op_Operator(+, plus_signed_bignum);
}

static Pointer basic_minus(Arg1, Arg2)
fast Pointer Arg1, Arg2;
{ 
  Basic_Two_Op_Operator(-, minus_signed_bignum);
}

DEFINE_PRIMITIVE ("&+", Prim_plus, 2, 2, 0)
{
  Two_Op_Operator(+, plus_signed_bignum, basic_plus);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("&-", Prim_minus, 2, 2, 0)
{
  Two_Op_Operator(-, minus_signed_bignum, basic_minus);
  /*NOTREACHED*/
}

static Pointer
basic_multiply (Arg1, Arg2)
     fast Pointer Arg1, Arg2;
{
  extern Pointer Mul ();

  switch (Type_Code(Arg1))
  { case TC_FIXNUM:
     { switch (Type_Code(Arg2))
       { case TC_FIXNUM:
          { fast Pointer Result;
	    Result = Mul(Arg1, Arg2);
	    if (Result != SHARP_F) return Result;
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

static Pointer
complex_multiply (Arg1, Arg2)
     fast Pointer Arg1, Arg2;
{
  RETURN_COMPLEX
    ((basic_minus ((basic_multiply ((COERCE_REAL_PART (Arg1)),
				    (COERCE_REAL_PART (Arg2)))),
		   (basic_multiply ((COERCE_IMAG_PART (Arg1)),
				    (COERCE_IMAG_PART (Arg2)))))),
     (basic_plus ((basic_multiply ((COERCE_REAL_PART (Arg1)),
				   (COERCE_IMAG_PART (Arg2)))),
		  (basic_multiply ((COERCE_REAL_PART (Arg2)),
				   (COERCE_IMAG_PART (Arg1)))))));
}
  
DEFINE_PRIMITIVE ("&*", Prim_multiply, 2, 2, 0)
{
  /* Mul is machine dependent and lives in "os.c" */
  Primitive_2_Args ();
  Set_Time_Zone (Zone_Math);

  PRIMITIVE_RETURN
    (((COMPLEX_P (Arg1)) || (COMPLEX_P (Arg2)))
     ? (complex_multiply (Arg1, Arg2))
     : (basic_multiply (Arg1, Arg2)));
}

static Pointer basic_divide(Arg1, Arg2)
fast Pointer Arg1, Arg2;
{
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

static Pointer
complex_divide (Arg1, Arg2)
     Pointer Arg1, Arg2;
{
  fast Pointer real1, real2, imag1, imag2;
  fast Pointer temp;

  real1 = (COERCE_REAL_PART (Arg1));
  real2 = (COERCE_REAL_PART (Arg2));
  imag1 = (COERCE_IMAG_PART (Arg1));
  imag2 = (COERCE_IMAG_PART (Arg2));

  temp = (basic_divide ((MAKE_UNSIGNED_FIXNUM (1)),
			(basic_plus ((basic_multiply (real2, real2)),
				     (basic_multiply (imag2, imag2))))));
  RETURN_COMPLEX
    ((basic_multiply ((basic_plus ((basic_multiply (real1, real2)),
				   (basic_multiply (imag1, imag2)))),
		      temp)),
     (basic_multiply ((basic_minus ((basic_multiply (real2, imag1)),
				    (basic_multiply (real1, imag2)))),
		      temp)));
}

DEFINE_PRIMITIVE ("&/", Prim_divide, 2, 2, 0)
{
  Primitive_2_Args ();

  Set_Time_Zone (Zone_Math);
  PRIMITIVE_RETURN
    (((COMPLEX_P (Arg1)) || (COMPLEX_P (Arg2)))
     ? (complex_divide (Arg1, Arg2))
     : (basic_divide (Arg1, Arg2)));
}

DEFINE_PRIMITIVE ("INTEGER-DIVIDE", Prim_integer_divide, 2, 2, 0)
{
  Primitive_2_Args ();

  Set_Time_Zone (Zone_Math);
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

DEFINE_PRIMITIVE ("SQRT", Prim_sqrt, 1, 1, 0)
{
  Generic_Function(Scheme_Sqrt);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("EXP", Prim_exp, 1, 1, 0)
{
  Generic_Function(exp);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("LOG", Prim_ln, 1, 1, 0)
{
  Generic_Function(Scheme_Ln);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("SIN", Prim_sine, 1, 1, 0)
{
  Generic_Function(sin);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("COS", Prim_cosine, 1, 1, 0)
{
  Generic_Function(cos);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("&ATAN", Prim_arctan, 1, 1, 0)
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

DEFINE_PRIMITIVE ("TRUNCATE", Prim_truncate, 1, 1, 0)
{
  Flonum_To_Integer(Truncate_Mapping);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("ROUND", Prim_round, 1, 1, 0)
{
  Flonum_To_Integer(Round_Mapping);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("FLOOR", Prim_floor, 1, 1, 0)
{
  Flonum_To_Integer(Floor_Mapping);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("CEILING", Prim_ceiling, 1, 1, 0)
{
  Flonum_To_Integer(Ceiling_Mapping);
  /*NOTREACHED*/
}
