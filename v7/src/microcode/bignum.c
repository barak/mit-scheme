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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/bignum.c,v 9.21 1987/01/22 14:16:05 jinx Exp $

   This file contains the procedures for handling BIGNUM Arithmetic. 
*/

#include "scheme.h"
#include <math.h>
#include "primitive.h"
#include "bignum.h"
#include "flonum.h"
#include "zones.h"

/* Bignum Comparison Primitives */

/* big_compare() will return either of three cases, determining whether
 * ARG1 is bigger, smaller, or equal to ARG2.
 */

big_compare(ARG1, ARG2)
bigdigit *ARG1, *ARG2;
{ switch(Categorize_Sign(ARG1, ARG2))
  { case BOTH_NEGATIVE : return big_compare_unsigned(ARG2, ARG1);
    case BOTH_POSITIVE : return big_compare_unsigned(ARG1, ARG2);
    case ARG1_NEGATIVE : return TWO_BIGGER;
    case ARG2_NEGATIVE : return ONE_BIGGER;
    default: Sign_Error("big_compare()");
  }
}

/* big_compare_unsigned() compares the magnitudes of two BIGNUM's.
 * Called by big_compare() and minus_unsigned_bignum().
 */

big_compare_unsigned(ARG1, ARG2)
fast bigdigit *ARG1, *ARG2;
{ fast bigdigit *LIMIT;

  if ((LEN(ARG1)) > (LEN(ARG2))) return ONE_BIGGER;
  if ((LEN(ARG1)) < (LEN(ARG2))) return TWO_BIGGER;
  if ((LEN(ARG1)) == 0) return EQUAL;
  LIMIT = Bignum_Bottom(ARG1); 
  ARG1  = Bignum_Top(ARG1);
  ARG2  = Bignum_Top(ARG2);
  while (ARG1 >=  LIMIT)
  { if (*ARG1 > *ARG2) return ONE_BIGGER;
    if (*ARG1 < *ARG2) return TWO_BIGGER;
    ARG1 -= 1;
    ARG2 -= 1;
  }
  return EQUAL;
}

/* Primitives for Coercion */

/* (FIX_TO_BIG FIXNUM)
      [Primitive number 0x67]
      Returns its argument if FIXNUM isn't a fixnum.  Otherwise 
      it returns the corresponding bignum.
*/
Built_In_Primitive(Prim_Fix_To_Big, 1, "FIX->BIG")
{ Primitive_1_Arg();
  Arg_1_Type(TC_FIXNUM);
  return Fix_To_Big(Arg1);
}

Pointer Fix_To_Big(Arg1)
Pointer Arg1;
{ fast bigdigit *Answer, *SCAN, *size;
  long Length, ARG1;
  if (Type_Code(Arg1) != TC_FIXNUM) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Get_Integer(Arg1) == 0)
  { long Align_0 = Align(0);
    bigdigit *REG;
    Primitive_GC_If_Needed(2);
    REG = BIGNUM(Free);
    Prepare_Header(REG, 0, POSITIVE);
    Free += Align_0;
    return Make_Pointer(TC_BIG_FIXNUM, Free-Align_0);
  }
  Length = Align(FIXNUM_LENGTH_AS_BIGNUM);
  Primitive_GC_If_Needed(Length);
  Sign_Extend(Arg1, ARG1);
  Answer = BIGNUM(Free); 
  Prepare_Header(Answer, 0, (ARG1 >= 0) ? POSITIVE : NEGATIVE);
  size   = &LEN(Answer);
  if (ARG1 < 0) ARG1 = - ARG1;
  for (SCAN = Bignum_Bottom(Answer); ARG1 != 0; *size += 1)
  { *SCAN++ = Rem_Radix(ARG1);
    ARG1    = Div_Radix(ARG1);
  }
  Length = Align(*size);
  *((Pointer *) Answer) = Make_Header(Length);
  Free  += Length;
  Debug_Test(Free-Length);
  return Make_Pointer(TC_BIG_FIXNUM, Free-Length);
}

/* (BIG_TO_FIX BIGNUM)
   [Primitive number 0x68]
   When given a bignum, returns the equivalent fixnum if there is
   one. If BIGNUM is out of range, or isn't a bignum, returns
   BIGNUM. */

Built_In_Primitive (Prim_Big_To_Fix, 1, "BIG->FIX")
{
  Primitive_1_Arg ();

  Arg_1_Type (TC_BIG_FIXNUM);
  return (Big_To_Fix (Arg1));
}

Pointer
Big_To_Fix (bignum_object)
     Pointer bignum_object;
{
  fast bigdigit *bptr, *scan;
  fast long result, i;
  long Length;

  if ((Type_Code (bignum_object)) != TC_BIG_FIXNUM)
    return (bignum_object);
  bptr = BIGNUM (Get_Pointer (bignum_object));
  Length = LEN (bptr);
  if (Length == 0)
    return (FIXNUM_0);
  if (Length > FIXNUM_LENGTH_AS_BIGNUM)
    return (bignum_object);

  scan = Bignum_Top (bptr);
  result = *scan--;
#if ((FIXNUM_LENGTH_AS_BIGNUM * SHIFT) > ULONG_SIZE)
  /* Mask the MS digit to prevent overflow during left shift. */
  if (Length = FIXNUM_LENGTH_AS_BIGNUM)
    result &=
      ((1 << ((FIXNUM_LENGTH + 1) - ((FIXNUM_LENGTH + 1) % SHIFT))) - 1);
#endif
  for (i = (Length - 1); (i > 0); i -= 1)
    result = (Mul_Radix (result) + *scan--);
  if (result < 0)
    return (bignum_object);
  if (NEG_BIGNUM (bptr))
    result = (- result);
  return
    (Fixnum_Fits (result)
     ? Make_Signed_Fixnum (result)
     : bignum_object);
}

Boolean Fits_Into_Flonum(Bignum)
bigdigit *Bignum;
{ fast int k;
  quick bigdigit top_digit;

  k = (LEN(Bignum) - 1) * SHIFT;
  for (top_digit = *Bignum_Top(Bignum); top_digit != 0; k++)
    top_digit >>= 1;

/* If precision should not be lost,
  if (k <= FLONUM_MANTISSA_BITS) return true;
   Otherwise,
*/

  if (k <= MAX_FLONUM_EXPONENT) return true;
  return false;
}

Pointer Big_To_Float(Arg1)
Pointer Arg1;
{ fast bigdigit *ARG1, *LIMIT;
  fast double F = 0.0;

  ARG1 = BIGNUM(Get_Pointer(Arg1));
  if (!Fits_Into_Flonum(ARG1)) return Arg1;
  Primitive_GC_If_Needed(FLONUM_SIZE+1);
  LIMIT = Bignum_Bottom(ARG1);
  ARG1 = Bignum_Top(ARG1);
  while (ARG1 >= LIMIT)  F = (F * ((double) RADIX)) + ((double) *ARG1--);
  if (NEG_BIGNUM(BIGNUM(Get_Pointer(Arg1)))) F = -F;
  return Allocate_Float(F);
}


#ifdef HAS_FREXP
extern double frexp(), ldexp();
#else
#include "missing.c"
#endif

Pointer Float_To_Big(flonum)
double flonum;
{ fast double mantissa;
  fast bigdigit *Answer, size;
  int exponent;
  long Align_size;
  if (flonum == 0.0) return return_bignum_zero();
  mantissa = frexp(flonum, &exponent);
  if (flonum < 0) mantissa = -mantissa;
  if (mantissa >= 1.0)
  { mantissa = mantissa/2.0;
    exponent += 1;
  }
  size = (exponent + (SHIFT - 1)) / SHIFT;
  exponent = exponent % SHIFT;
  mantissa = ldexp(mantissa, (exponent == 0) ? 0: exponent - SHIFT);
  Align_size = Align(size);
  Primitive_GC_If_Needed(Align_size);
  Answer = BIGNUM(Free);
  Prepare_Header(Answer, size, (flonum < 0) ? NEGATIVE : POSITIVE);
  Answer = Bignum_Top(Answer)+1;
  while ((size > 0) && (mantissa != 0))
    {
      long temporary;

      mantissa = mantissa * ((double) RADIX);
      /* explicit intermediate required by compiler bug. -- cph */
      temporary = ((long) mantissa);
      *--Answer = ((bigdigit) temporary);
      mantissa = mantissa - ((double) *Answer);
      size -= 1;
    }
  while (size-- != 0) *--Answer = (bigdigit) 0;
  Free += Align_size;
  Debug_Test(Free-Align_size);
  return Make_Pointer(TC_BIG_FIXNUM, Free-Align_size);
}

/* Addition */

plus_signed_bignum(ARG1, ARG2)
bigdigit *ARG1, *ARG2;
{ /* Special Case for answer being zero */
  if (ZERO_BIGNUM(ARG1) && ZERO_BIGNUM(ARG2))
     return return_bignum_zero();
  switch(Categorize_Sign(ARG1, ARG2))
  { case BOTH_POSITIVE : return(plus_unsigned_bignum(ARG1, ARG2, POSITIVE));
    case ARG1_NEGATIVE : return(minus_unsigned_bignum(ARG2, ARG1, POSITIVE));
    case ARG2_NEGATIVE : return(minus_unsigned_bignum(ARG1, ARG2, POSITIVE));
    case BOTH_NEGATIVE : return(plus_unsigned_bignum(ARG1, ARG2, NEGATIVE));
    default : Sign_Error("plus_bignum()");
  }
}

plus_unsigned_bignum(ARG1,ARG2,sign)
fast bigdigit *ARG1, *ARG2;
bigdigit sign;
{ fast unsigned bigdouble Sum;
  long Size;
  fast bigdigit *Answer;
  fast bigdigit *TOP2, *TOP1;

/* Swap ARG1 and ARG2 so that ARG1 is always longer */

  if (LEN(ARG1) < LEN(ARG2))
  { Answer = ARG1;
    ARG1  = ARG2;
    ARG2  = Answer;
  }

/* Allocate Storage and do GC if needed */

  Size = Align(LEN(ARG1) + 1);
  Primitive_GC_If_Needed(Size);
  Answer = BIGNUM(Free);
  Prepare_Header(Answer, LEN(ARG1)+1, sign);

/* plus_unsigned_bignum continues on the next page */

/* plus_unsigned_bignum, continued */

/* Prepare Scanning Pointers and delimiters */

  TOP1 = Bignum_Top(ARG1);
  TOP2 = Bignum_Top(ARG2);
  ARG1 = Bignum_Bottom(ARG1);
  ARG2 = Bignum_Bottom(ARG2);
  Answer = Bignum_Bottom(Answer);
  Sum  = 0;
/* Starts Looping */
  while (TOP2 >= ARG2)
  { Sum       = *ARG1++ + *ARG2++ + Get_Carry(Sum);
    *Answer++ = Get_Digit(Sum);
  }
/* Let remaining carry propagate */
  while ((TOP1 >= ARG1) && (Get_Carry(Sum) != 0))
  { Sum       = *ARG1++ + 1;
    *Answer++ = Get_Digit(Sum);
  }
/* Copy rest of ARG1 into Answer */
  while (TOP1 >= ARG1) *Answer++ = *ARG1++;
  *Answer = Get_Carry(Sum);
/* Trims Answer.  The trim function is not used because there is at
 * most one leading zero.
 */
  if (*Answer == 0)
  { Answer = BIGNUM(Free);
    LEN(Answer) -= 1;
    *((Pointer *) Answer) = Make_Header(Align(LEN(Answer)));
  }
  Free  += Size;
  return Make_Pointer(TC_BIG_FIXNUM, Free-Size);
}

/* Subtraction */

minus_signed_bignum(ARG1, ARG2)
bigdigit *ARG1, *ARG2;
{ /* Special Case for answer being zero */
  if (ZERO_BIGNUM(ARG1) && ZERO_BIGNUM(ARG2))
     return return_bignum_zero();

/* Dispatches According to Sign of Args */

  switch(Categorize_Sign(ARG1, ARG2))
  { case BOTH_POSITIVE : return(minus_unsigned_bignum(ARG1, ARG2, POSITIVE));
    case ARG1_NEGATIVE : return(plus_unsigned_bignum(ARG1, ARG2, NEGATIVE));
    case ARG2_NEGATIVE : return(plus_unsigned_bignum(ARG1, ARG2, POSITIVE));
    case BOTH_NEGATIVE : return(minus_unsigned_bignum(ARG2, ARG1, POSITIVE));
    default : Sign_Error("minus_bignum()");
  }
}

minus_unsigned_bignum(ARG1, ARG2, sign)
fast bigdigit *ARG1, *ARG2;
bigdigit sign;
{ fast bigdouble Diff;
  fast bigdigit *Answer, *TOP2, *TOP1;
  long Size;

  if (big_compare_unsigned(ARG1, ARG2) == TWO_BIGGER)
  { Answer = ARG1;
    ARG1  = ARG2;
    ARG2  = Answer;
    sign  = !sign;
  }

  Size   = Align(LEN(ARG1));
  Primitive_GC_If_Needed(Size);
  Answer = BIGNUM(Free);
  Prepare_Header(Answer, LEN(ARG1), sign);

/* minus_unsigned_bignum continues on the next page */

/* minus_unsigned_bignum, continued */

  TOP1 = Bignum_Top(ARG1);
  TOP2 = Bignum_Top(ARG2);
  ARG1  = Bignum_Bottom(ARG1);
  ARG2  = Bignum_Bottom(ARG2);
  Answer = Bignum_Bottom(Answer);
  Diff = RADIX;

/* Main Loops for minus_unsigned_bignum */

  while (TOP2 >= ARG2)
  { Diff      =  *ARG1++ + (MAX_DIGIT_SIZE - *ARG2++) + Get_Carry(Diff);
    *Answer++ = Get_Digit(Diff);
  }
  while ((TOP1 >= ARG1) && (Get_Carry(Diff) == 0))
  { Diff      = *ARG1++ + MAX_DIGIT_SIZE;
    *Answer++ = Get_Digit(Diff);
  }
  while (TOP1 >= ARG1) *Answer++ = *ARG1++;
  trim_bignum((bigdigit *) Free);
  Free  += Size;
  return Make_Pointer(TC_BIG_FIXNUM, Free-Size);
}

/* Multiplication */

multiply_signed_bignum(ARG1, ARG2)
bigdigit *ARG1, *ARG2;
{ if (ZERO_BIGNUM(ARG1) || ZERO_BIGNUM(ARG2))
     return return_bignum_zero();

  switch(Categorize_Sign(ARG1,ARG2))
  { case BOTH_POSITIVE :
    case BOTH_NEGATIVE :
      return multiply_unsigned_bignum(ARG1, ARG2, POSITIVE);
    case ARG1_NEGATIVE :
    case ARG2_NEGATIVE :
      return multiply_unsigned_bignum(ARG1, ARG2, NEGATIVE);
    default : Sign_Error("multiply_bignum()");
  }
}

multiply_unsigned_bignum(ARG1, ARG2, sign)
fast bigdigit *ARG1, *ARG2;
bigdigit sign;
{ bigdigit *TOP1, *TOP2;
  fast bigdigit *Answer;
  fast bigdouble Prod;
  fast int size;
  long Size;

  Prod   = LEN(ARG1) + LEN(ARG2);
  Size   = Align(Prod);
  Primitive_GC_If_Needed(Size);
  Answer = BIGNUM(Free);
  Prepare_Header(Answer, Prod, sign);
  TOP1 = Bignum_Top(Answer);
  TOP2 = Bignum_Bottom(Answer);
  while (TOP1 >= TOP2) *TOP2++ = 0;

/* multiply_unsigned_bignum continues */

/* Main Loops for MULTIPLY */

  size   = LEN(ARG2);
  Answer = Bignum_Bottom(Answer) +  size;
  TOP1   = Bignum_Top(ARG1);
  TOP2   = Bignum_Top(ARG2);
  ARG2   = TOP2;

  for (ARG1 = Bignum_Bottom(ARG1); TOP1 >= ARG1; ARG1++, Answer++)
  { if (*ARG1 != 0)
    { Prod = 0;
      Answer -= size;
      for (ARG2 = TOP2 - size + 1; TOP2 >= ARG2; ++ARG2)
      { Prod = *ARG1 * *ARG2 + *Answer + Get_Carry(Prod);
        *Answer++  = Get_Digit(Prod);
      }
      *Answer = Get_Carry(Prod);
    }
  }

/* Trims Answer */
  Answer = BIGNUM(Free);
  if (*(Bignum_Top(Answer)) == 0)
  { LEN(Answer) -= 1;
    *((Pointer *) Answer) = Make_Header(Align(LEN(Answer)));
  }
  Free  += Size;
  return Make_Pointer(TC_BIG_FIXNUM, Free-Size);
}

/* (DIVIDE-BIGNUM ONE-BIGNUM ANOTHER_BIGNUM)
 * [Primitive number 0x4F]
 * returns a cons of the bignum quotient and remainder of both arguments.
 */

Built_In_Primitive(Prim_Divide_Bignum, 2, "DIVIDE-BIGNUM")
{ Pointer Result, *End_Of_First, *First, *Second, *Orig_Free=Free;
  Primitive_2_Args();
  Arg_1_Type(TC_BIG_FIXNUM);
  Arg_2_Type(TC_BIG_FIXNUM);
  Set_Time_Zone(Zone_Math);
  Result = div_signed_bignum(BIGNUM(Get_Pointer(Arg1)),
                             BIGNUM(Get_Pointer(Arg2)));
  if (Bignum_Debug)
    printf("\nResult=0x%x [%x %x]\n",
           Result, Fast_Vector_Ref(Result, 0), Fast_Vector_Ref(Result, 1));
  First = Get_Pointer(Fast_Vector_Ref(Result, CONS_CAR));
  Second = Get_Pointer(Fast_Vector_Ref(Result, CONS_CDR));
  if (Bignum_Debug)
    printf("\nFirst=0x%x [%x %x]; Second=0x%x [%x %x]\n",
           First, First[0], First[1], Second, Second[0], Second[1]);
  if (Consistency_Check)
  { if (First > Second)
    { printf("\nBignum_Divide: results swapped.\n");
      Microcode_Termination(TERM_EXIT);
    }
    else if (First != Orig_Free+2)
    { printf("\nBignum Divide: hole at start\n");
      Microcode_Termination(TERM_EXIT);
    }
  }
  End_Of_First = First+1+Get_Integer(First[0]);
  if (Bignum_Debug) printf("\nEnd_Of_First=0x%x\n", End_Of_First);
  if (End_Of_First != Second)
  { *End_Of_First =
      Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, (Second-End_Of_First)-1);
    if (Bignum_Debug) printf("\nGap=0x%x\n", (Second-End_Of_First)-1);
  }
  Free = Second+1+Get_Integer(Second[0]);
  if (Bignum_Debug) printf("\nEnd=0x%x\n", Free);
  return Result;
}

/* div_signed_bignum() differentiates between all the possible
 * cases and allocates storage for the quotient, remainder, and
 * any intrmediate storage needed.
 */

div_signed_bignum(ARG1, ARG2)
bigdigit *ARG1, *ARG2;
{ bigdigit *SARG2;
  bigdigit *QUOT, *REMD;
  Pointer *Cons_Cell;

  if ZERO_BIGNUM(ARG2) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  Primitive_GC_If_Needed(2);
  Cons_Cell = Free;
  Free += 2;

  if (big_compare_unsigned(ARG1, ARG2) == TWO_BIGGER)
/* Trivial Solution for ARG1 > ARG2 
 * Quotient is zero and the remainder is just a copy of Arg_1.
 */
  { Primitive_GC_If_Needed(Align(0)+Align(LEN(ARG1)));
    QUOT = BIGNUM(Free);
    Free += Align(0);
    Prepare_Header(QUOT, 0, POSITIVE);
    REMD = BIGNUM(Free);
    Free += Align(LEN(ARG1));
    copy_bignum(ARG1, REMD);
  }
  else if (LEN(ARG2)==1)
  /* Divisor is only one digit long.
   * unscale() is used to divide out Arg_1 and the remainder is the
   * single digit returned by unscale(), coerced to a bignum.
   */
  { Primitive_GC_If_Needed(Align(LEN(ARG1))+Align(1));
    QUOT = BIGNUM(Free);
    Free += Align(LEN(ARG1));
    REMD = BIGNUM(Free);
    Free += Align(1);
    Prepare_Header(QUOT, LEN(ARG1), POSITIVE);
    Prepare_Header(REMD, 1, POSITIVE);
    *(Bignum_Bottom(REMD)) =
      unscale(ARG1, QUOT, (long) *(Bignum_Bottom(ARG2)));
    trim_bignum(REMD);
    trim_bignum(QUOT);
  }
  else

/* Usual case. div_internal() is called.  A normalized copy of Arg_1
 * resides in REMD, which ultimately becomes the remainder.  The
 * normalized copy of Arg_2 is in SARG2.
 */
  { bigdouble temp;
    temp = (Align(LEN(ARG1)-LEN(ARG2)+1) + Align(LEN(ARG1)+1)
	    + Align(LEN(ARG2)+1));
    Primitive_GC_If_Needed(temp);
    QUOT = BIGNUM(Free);
    *Free = Make_Header(Align(LEN(ARG1)-LEN(ARG2)+1));
    Free += Align(LEN(ARG1)-LEN(ARG2)+1);
    REMD = BIGNUM(Free);
    *Free = Make_Header(Align(LEN(ARG1)+1));
    Free += Align(LEN(ARG1)+1);
    SARG2 = BIGNUM(Free);
    *Free = Make_Header(Align(LEN(ARG2)+1));
    Free += Align(LEN(ARG2)+1);

    temp = RADIX / (1 + *(Bignum_Top(ARG2)));
    scale(ARG1, REMD, temp);
    scale(ARG2, SARG2, temp);
    div_internal(REMD, SARG2, QUOT);
    unscale(REMD, REMD, temp);
    trim_bignum(REMD);
    trim_bignum(QUOT);
  }

/* Determines sign of the quotient and remainder */

  SIGN(REMD) = POSITIVE;
  SIGN(QUOT) = POSITIVE;
  switch(Categorize_Sign(ARG1,ARG2))
  { case ARG2_NEGATIVE :
      SIGN(QUOT) = NEGATIVE;
      break;
    case ARG1_NEGATIVE :
      SIGN(QUOT) = NEGATIVE;
    case BOTH_NEGATIVE :
      SIGN(REMD) = NEGATIVE;
      break;
    case BOTH_POSITIVE : break;
    default : Sign_Error("divide_bignum()");
  } /* Glue the two results in a list and return as answer */
  Cons_Cell[CONS_CAR] = Make_Pointer(TC_BIG_FIXNUM, (Pointer *) QUOT);
  Cons_Cell[CONS_CDR] = Make_Pointer(TC_BIG_FIXNUM, (Pointer *) REMD);
  return Make_Pointer(TC_LIST, Cons_Cell);
}

/* Utility for debugging */

print_digits(name, num, how_many)
char *name;
bigdigit *num;
int how_many;
{ int NDigits = LEN(num);
  int limit;
  printf("\n%s = 0x%08x", name, num);
  printf("\n  Sign: %c, Vector length: %d, # Digits: %d",
         ((SIGN(num) == NEGATIVE) ? '-' :
	  ((SIGN(num) == POSITIVE) ? '+' : '?')),
	 Datum(((Pointer *) num)[VECTOR_LENGTH]),
	 NDigits);
  if (how_many == -1) limit = NDigits;
  else limit = ((how_many < NDigits) ? how_many : NDigits);
  num = Bignum_Bottom(num);
  while (--how_many >= 0) printf("\n    0x%04x", *num++);
  if (limit < NDigits) printf("\n    ...");
  printf("\n");
  return;
}

/* This is the guts of the division algorithm. The storage
 * allocation and other hairy prep work is done in the superior
 * routines. ARG1 and ARG2 are fresh copies, ARG1 will 
 * ultimately become the Remainder.  Storage already 
 * allocated for all four parameters.
 */

static Pointer BIG_A[TEMP_SIZE], BIG_B[TEMP_SIZE];

div_internal(ARG1, ARG2, Quotient)
bigdigit *ARG1, *ARG2, *Quotient;
{ fast bigdigit *SCAN,*PROD;
  fast bigdouble Digit, Prod;
  fast bigdouble guess, dvsr2, dvsr1;
  fast bigdigit *LIMIT, *QUOT_SCAN;
  bigdigit *Big_A = BIGNUM(BIG_A);
  bigdigit *Big_B = BIGNUM(BIG_B);

  SCAN = Bignum_Top(ARG2);
  if (*SCAN == 0)
  { LEN(ARG2) -= 1;
    SCAN -= 1;
  }
  dvsr1 = *SCAN--;
  dvsr2 = *SCAN;

  Prepare_Header(Quotient, (LEN(ARG1)-LEN(ARG2)), POSITIVE);

  QUOT_SCAN = Bignum_Top(Quotient);
  ARG1      = Bignum_Top(ARG1);
  SCAN      = ARG1 - LEN(ARG2);
  Quotient  = Bignum_Bottom(Quotient);

/* div_internal() continues */

/* Main Loop for div_internal() */

  while (QUOT_SCAN >= Quotient)
   { if (dvsr1 <= *ARG1) guess = RADIX - 1;
     else
     { /* This should be
	* guess = (Mul_Radix(*ARG1) + *(ARG1 - 1)) / dvsr1;
	* but because of overflow problems ...
	*/

       Prepare_Header(Big_A, 2, POSITIVE);
       *Bignum_Top(Big_A) = *ARG1;
       *Bignum_Bottom(Big_A) = *(ARG1-1);
       unscale(Big_A, Big_A, dvsr1);
       guess = *Bignum_Bottom(Big_A);
     }
     guess += 1; /* To counter first decrementing below. */
     do
     { guess -= 1;
       Prepare_Header(Big_A, 3, POSITIVE);
       LIMIT = Bignum_Top(Big_A);
       *LIMIT-- = *ARG1;
       *LIMIT-- = *(ARG1-1);
       *LIMIT   = *(ARG1-2);
       Prepare_Header(Big_B, 2, POSITIVE);
       *Bignum_Top(Big_B)    = dvsr1;
       *Bignum_Bottom(Big_B) = dvsr2;
       scale(Big_B, Big_B, guess);
       if ((*Bignum_Top(Big_B)) == 0) LEN(Big_B) -= 1;
     } while (big_compare_unsigned(Big_B, Big_A) == ONE_BIGGER);

/* div_internal() continues */

/* div_internal() continued */

     LIMIT = Bignum_Top(ARG2);
     PROD  = Bignum_Bottom(ARG2);
     Digit = RADIX + *SCAN;
     while (LIMIT >= PROD)
      { Prod    = *PROD++ * guess;
        Digit   = Digit - Get_Digit(Prod);
        *SCAN++ = Get_Digit(Digit);
        Digit   = ((*SCAN - Get_Carry(Prod)) +
		   (MAX_DIGIT_SIZE +
		    ((Digit < 0) ? -1 : Get_Carry(Digit))));
      }
     *SCAN++ = Get_Digit(Digit);

     if (Get_Carry(Digit) == 0)
     /* Guess is one too big, add back. */
     { Digit = 0;
       guess -= 1;
       LIMIT = Bignum_Top(ARG2);
       SCAN  = SCAN - LEN(ARG2);
       PROD  = Bignum_Bottom(ARG2);
       while (LIMIT >= PROD)
       { Digit   = *SCAN + *PROD++ + Get_Carry(Digit);
         *SCAN++ = Get_Digit(Digit);
       }
       *SCAN = 0;
     }
     *QUOT_SCAN-- = guess;
     ARG1 -= 1;
     SCAN = ARG1 - LEN(ARG2);
   }
}


/* (LISTIFY_BIGNUM BIGNUM RADIX)
      [Primitive number 0x50]
      Returns a list of numbers, in the range 0 through RADIX-1, which
      represent the BIGNUM in that radix.
*/
Built_In_Primitive(Prim_Listify_Bignum, 2, "LISTIFY-BIGNUM")
{ fast bigdigit *TOP1, *size;
  quick Pointer *RFree;
  fast bigdigit *ARG1;
  fast long pradix;
  Primitive_2_Args();

  Arg_1_Type(TC_BIG_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  Set_Time_Zone(Zone_Math);

  ARG1 = BIGNUM(Get_Pointer(Arg1));
  size = &LEN(ARG1);  
  if (*size == 0)
  { Primitive_GC_If_Needed(2);
    *Free++ = FIXNUM_0;
    *Free++ = NIL;
    return Make_Pointer(TC_LIST, Free-2);
  }
  Sign_Extend(Arg2, pradix);
  Primitive_GC_If_Needed(Find_Length(pradix, *size)+Align(*size));
  ARG1  = BIGNUM(Free);
  copy_bignum(BIGNUM(Get_Pointer(Arg1)), ARG1);
  Free += Align(*size);
  RFree = Free;
  size = &LEN(ARG1);
  TOP1 = Bignum_Top(ARG1);
  while (*size > 0)
  { *RFree++ = FIXNUM_0+unscale(ARG1, ARG1, pradix);
    *RFree = Make_Pointer(TC_LIST, RFree-3); 
    RFree += 1; 
    if (*TOP1 == 0) 
    { *size -= 1;
      TOP1--;
    }
  }
  Free[CONS_CDR] = NIL;
  Free = RFree;
  return Make_Pointer(TC_LIST, RFree-2);
}

/* General Purpose Utilities */

return_bignum_zero()
{ bigdigit *REG;
  long Align_0 = Align(0);
  Primitive_GC_If_Needed(Align_0);
  REG = BIGNUM(Free);
  Prepare_Header(REG, 0, POSITIVE);
  Free += Align_0;
  return Make_Pointer(TC_BIG_FIXNUM, Free-Align_0);
}

trim_bignum(ARG)
bigdigit *ARG;
{ fast bigdigit *SCAN;
  fast bigdigit size;
  bigdigit sign;

  sign = SIGN(ARG);
  size = LEN(ARG);

  for (SCAN=Bignum_Top(ARG); ((size!=0)&&(*SCAN==0)); SCAN--)
    size -= 1;

  if (size == 0) sign = POSITIVE;
  Prepare_Header(ARG, size, sign);
}

copy_bignum(SOURCE, TARGET)
fast bigdigit *SOURCE, *TARGET;
{ fast bigdigit *LIMIT = Bignum_Top(SOURCE);
  while (LIMIT >= SOURCE) *TARGET++ = *SOURCE++;
}

Find_Length(pradix, length)
fast long pradix;
bigdigit length;
{ fast int log_pradix = 0;
  while (pradix != 1)
  { pradix = pradix >> 1;
   log_pradix += 1;
  }
  return(((SHIFT / log_pradix) + 1) * length);
}

/* scale() and unscale() used by Division and Listify */

scale(SOURCE, DEST, how_much)
fast bigdigit *SOURCE, *DEST;
fast long how_much;
{ fast unsigned bigdouble prod = 0;
  bigdigit *LIMIT;

  if (how_much == 1)
  { if (SOURCE != DEST) copy_bignum(SOURCE, DEST);
    Prepare_Header(DEST, LEN(SOURCE)+1, SIGN(SOURCE));
    *Bignum_Top(DEST) = 0;
    return;
  }
  /* This must happen before the Prepare_Header if DEST = SOURCE */
  LIMIT = Bignum_Top(SOURCE);
  Prepare_Header(DEST, LEN(SOURCE)+1, SIGN(SOURCE));
  SOURCE = Bignum_Bottom(SOURCE);
  DEST = Bignum_Bottom(DEST);
  while (LIMIT >= SOURCE)
  { prod    = *SOURCE++ * how_much + Get_Carry(prod);
    *DEST++ = Get_Digit(prod);
  }
  *DEST = Get_Carry(prod);
}

unscale(SOURCE, DEST, how_much)
bigdigit *SOURCE;
fast bigdigit *DEST;
fast long how_much;
{ bigdigit carry = 0;
  fast unsigned bigdouble digits;
  fast bigdigit *SCAN;

  if (how_much == 1)
  { if (SOURCE != DEST) copy_bignum(SOURCE, DEST);
    return 0;
  }
  Prepare_Header(DEST, LEN(SOURCE), SIGN(DEST));
  SCAN   = Bignum_Top(SOURCE);
  DEST   = Bignum_Top(DEST);
  SOURCE = Bignum_Bottom(SOURCE);
  while (SCAN >= SOURCE)
  { fast unsigned bigdouble digits, temp;	/* Bug fix by JMiller */
    digits = Mul_Radix(carry) + *SCAN--;
    temp = digits / how_much;
    *DEST--  = temp;
    temp = temp * how_much;
    carry  = digits - temp;
  }
  return carry;   /* returns remainder */
}

/* Top level bignum primitives */

/* All the binary bignum primtives take two arguments and return NIL
   if either of them is not a bignum.  If both arguments are bignums,
   the perform the operation and return the answer.
*/

#define Binary_Primitive(C_Name, S_Name, Op)				\
Built_In_Primitive(C_Name, 2, S_Name)					\
{ Pointer Result, *Orig_Free=Free;					\
  Primitive_2_Args();							\
  Arg_1_Type(TC_BIG_FIXNUM);                                            \
  Arg_2_Type(TC_BIG_FIXNUM);                                            \
  Set_Time_Zone(Zone_Math);						\
  Result = Op(BIGNUM(Get_Pointer(Arg1)), BIGNUM(Get_Pointer(Arg2)));	\
  if (Consistency_Check && (Get_Pointer(Result) != Orig_Free))		\
  { printf("\nBignum operation result at 0x%x, Free was 0x%x\n",	\
           Address(Result), Free);					\
    Microcode_Termination(TERM_EXIT);					\
  }									\
  Free = Nth_Vector_Loc(Result, Vector_Length(Result)+1);		\
  if (Consistency_Check && (Free > Heap_Top))		\
  { printf("\nBignum operation result at 0x%x, length 0x%x\n",		\
           Address(Result), Vector_Length(Result));			\
    Microcode_Termination(TERM_EXIT);					\
  }									\
  return Result;							\
}

Binary_Primitive(Prim_Plus_Bignum, "PLUS-BIGNUM", plus_signed_bignum);
Binary_Primitive(Prim_Minus_Bignum, "MINUS-BIGNUM", minus_signed_bignum);
Binary_Primitive(Prim_Multiply_Bignum,
		 "TIMES-BIGNUM",
		 multiply_signed_bignum);

/* All the unary bignum predicates take one argument and return NIL if
   it is not a bignum.  Otherwise, they return a fixnum 1 if the
   predicate is true or a fixnum 0 if it is false.  This convention of
   NIL/0/1 is used for all numeric predicates so that the generic
   dispatch can detect "inapplicable" as distinct from "false" answer.
*/

#define Unary_Predicate(C_Name, S_Name, Test)				\
Built_In_Primitive(C_Name, 1, S_Name)					\
{ bigdigit *ARG;							\
  Primitive_1_Arg();							\
  Arg_1_Type(TC_BIG_FIXNUM);                                            \
  Set_Time_Zone(Zone_Math);						\
  ARG = BIGNUM(Get_Pointer(Arg1));					\
  return FIXNUM_0 + ((Test) ? 1 : 0);					\
}

Unary_Predicate(Prim_Zero_Bignum, "ZERO-BIGNUM?", LEN(ARG)==0)
Unary_Predicate(Prim_Positive_Bignum,
		"POSITIVE-BIGNUM?",
                (LEN(ARG) != 0) && POS_BIGNUM(ARG))
Unary_Predicate(Prim_Negative_Bignum,
		"NEGATIVE-BIGNUM?",
                (LEN(ARG) != 0) && NEG_BIGNUM(ARG))

/* All the binary bignum predicates take two arguments and return NIL
   if either of them is not a bignum.  Otherwise, they return an
   answer as described above for the unary predicates.
*/

#define Binary_Predicate(C_Name, S_Name, Code)				\
Built_In_Primitive(C_Name, 2, S_Name)					\
{ Primitive_2_Args();							\
  Arg_1_Type(TC_BIG_FIXNUM);                                           \
  Arg_2_Type(TC_BIG_FIXNUM);                                           \
  Set_Time_Zone(Zone_Math);						\
  return FIXNUM_0 +							\
  	  ((big_compare(BIGNUM(Get_Pointer(Arg1)),			\
	                BIGNUM(Get_Pointer(Arg2))) == Code) ? 1 : 0);	\
}

Binary_Predicate(Prim_Equal_Bignum, "EQUAL-BIGNUM?", EQUAL)
Binary_Predicate(Prim_Greater_Bignum, "GREATER-BIGNUM?", ONE_BIGGER)
Binary_Predicate(Prim_Less_Bignum, "LESS-BIGNUM?", TWO_BIGGER)
