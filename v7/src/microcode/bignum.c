/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/bignum.c,v 9.27 1989/08/28 18:28:19 cph Exp $

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

/* BIG NUMber arithmetic */

#include "scheme.h"
#include <math.h>
#include "prims.h"
#include "bignum.h"
#include "flonum.h"
#include "zones.h"

/* The following macros are the beginnings of a redesign of the bignum
   code.  Some of the procedures and primitives defined here use these
   new conventions.  Please update things as you work on them. */

#define DIGITS_PER_POINTER ((sizeof (Pointer)) / (sizeof (bigdigit)))

#define DIGITS_TO_POINTERS(n_digits)					\
  (((n_digits) + (DIGITS_PER_POINTER - 1)) / DIGITS_PER_POINTER)

#define DIGITS_TO_GC_LENGTH(n_digits) (DIGITS_TO_POINTERS ((n_digits) + 2))

#define DIGITS_TO_GC_HEADER(n_digits)					\
  (Make_Non_Pointer (TC_MANIFEST_NM_VECTOR, (DIGITS_TO_GC_LENGTH (n_digits))))

#define BIGNUM_PTR(bignum, index)					\
  (((bigdigit *) (Nth_Vector_Loc ((bignum), 1))) + (index))

#define BIGNUM_REF(bignum, index) (* (BIGNUM_PTR ((bignum), (index))))
#define BIGNUM_SIGN(bignum) (BIGNUM_REF ((bignum), 0))
#define BIGNUM_LENGTH(bignum) (BIGNUM_REF ((bignum), 1))
#define BIGNUM_START_PTR(bignum) (BIGNUM_PTR ((bignum), 2))
#define BIGNUM_END_PTR(bignum)						\
  (BIGNUM_PTR ((bignum), (2 + (BIGNUM_LENGTH (bignum)))))

#define BIGNUM_NEGATIVE_P(bignum) ((BIGNUM_SIGN (bignum)) == 0)
#define BIGNUM_ZERO_P(bignum) ((BIGNUM_LENGTH (bignum)) == 0)

static Pointer
make_bignum_zero ()
{
  Pointer bignum =
    (allocate_non_marked_vector
     (TC_BIG_FIXNUM, (DIGITS_TO_GC_LENGTH (0)), true));
  (BIGNUM_SIGN (bignum)) = 1;
  (BIGNUM_LENGTH (bignum)) = 0;
  return (bignum);
}

static Pointer
bignum_allocate (n_digits, negative_p)
     long n_digits;
     Boolean negative_p;
{
  Pointer bignum =
    (allocate_non_marked_vector
     (TC_BIG_FIXNUM, (DIGITS_TO_GC_LENGTH (n_digits)), true));
  (BIGNUM_SIGN (bignum)) = (negative_p ? 0 : 1);
  (BIGNUM_LENGTH (bignum)) = n_digits;
  return (bignum);
}

static void
bignum_destructive_copy (source, target)
     Pointer source;
     Pointer target;
{
  fast bigdigit * scan_source;
  fast bigdigit * end_source;
  fast bigdigit * scan_target;

  (BIGNUM_SIGN (target)) = (BIGNUM_SIGN (source));
  (BIGNUM_LENGTH (target)) = (BIGNUM_LENGTH (source));
  scan_source = (BIGNUM_START_PTR (source));
  end_source = (BIGNUM_END_PTR (source));
  scan_target = (BIGNUM_START_PTR (target));
  while (scan_source < end_source)
    (*scan_target++) = (*scan_source++);
  return;
}

static Pointer
bignum_copy (source)
     Pointer source;
{
  Pointer target =
    (allocate_non_marked_vector
     (TC_BIG_FIXNUM, (DIGITS_TO_GC_LENGTH (BIGNUM_LENGTH (source))), true));
  bignum_destructive_copy (source, target);
  return (target);
}

static int
bignum_length_in_bits (bignum)
     Pointer bignum;
{
  if (BIGNUM_ZERO_P (bignum))
    return (0);
  {
    int max_index = ((BIGNUM_LENGTH (bignum)) - 1);
    fast int result = (max_index * SHIFT);
    fast unsigned long max_digit = (BIGNUM_REF (bignum, max_index));
    while (max_digit > 0)
      {
	result += 1;
	max_digit >>= 1;
      }
    return (result);
  }
}

static unsigned long
scale_down (source, target, denominator)
     Pointer source;
     Pointer target;
     unsigned long denominator;
{
  fast unsigned long remainder;
  fast unsigned long quotient;
  fast bigdigit * scan_source;
  fast bigdigit * scan_target;
  fast bigdigit * start_source;

  (BIGNUM_SIGN (target)) = (BIGNUM_SIGN (source));
  (BIGNUM_LENGTH (target)) = (BIGNUM_LENGTH (source));
  scan_source = (BIGNUM_END_PTR (source));
  start_source = (BIGNUM_START_PTR (source));
  scan_target = (BIGNUM_END_PTR (target));
  remainder = 0;
  while (scan_source > start_source)
    {
      remainder = ((remainder << SHIFT) + (*--scan_source));
      quotient = (remainder / denominator);
      remainder = (remainder % denominator);
      (*--scan_target) = quotient;
    }
  return (remainder);
}

static unsigned long
scale_down_self (bignum, denominator)
     Pointer bignum;
     unsigned long denominator;
{
  fast unsigned long remainder;
  fast unsigned long quotient;
  fast bigdigit * scan;
  fast bigdigit * start;

  scan = (BIGNUM_END_PTR (bignum));
  start = (BIGNUM_START_PTR (bignum));
  remainder = 0;
  while (scan > start)
    {
      remainder = ((remainder << SHIFT) + (*--scan));
      quotient = (remainder / denominator);
      remainder = (remainder % denominator);
      (*scan) = quotient;
    }
  return (remainder);
}

void
trim_bignum(ARG)
     bigdigit *ARG;
{
  fast bigdigit *SCAN;
  fast bigdigit size;
  bigdigit sign;

  sign = SIGN(ARG);
  size = LEN(ARG);

  for (SCAN = Bignum_Top(ARG); ((size != 0) && (*SCAN == 0)); SCAN--)
    size -= 1;

  if (size == 0)
    sign = POSITIVE;
  Prepare_Header(ARG, size, sign);
  return;
}

void
copy_bignum(SOURCE, TARGET)
     fast bigdigit *SOURCE, *TARGET;
{
  fast bigdigit *LIMIT;

  LIMIT = Bignum_Top(SOURCE);
  while (LIMIT >= SOURCE)
    *TARGET++ = *SOURCE++;
  return;
}

/* scale() and unscale() used by Division and Listify */

void
scale(SOURCE, DEST, how_much)
     fast bigdigit *SOURCE, *DEST;
     fast long how_much;
{
  fast unsigned bigdouble prod = 0;
  bigdigit *LIMIT;

  if (how_much == 1)
  {
    if (SOURCE != DEST)
      copy_bignum(SOURCE, DEST);
    Prepare_Header(DEST, (LEN(SOURCE) + 1), SIGN(SOURCE));
    *Bignum_Top(DEST) = 0;
    return;
  }

  /* This must happen before the Prepare_Header if DEST = SOURCE */

  LIMIT = Bignum_Top(SOURCE);
  Prepare_Header(DEST, (LEN(SOURCE) + 1), SIGN(SOURCE));
  SOURCE = Bignum_Bottom(SOURCE);
  DEST = Bignum_Bottom(DEST);
  while (LIMIT >= SOURCE)
  {
    prod    = *SOURCE++ * how_much + Get_Carry(prod);
    *DEST++ = Get_Digit(prod);
  }
  *DEST = Get_Carry(prod);
  return;
}

/* returns remainder */

long
unscale(SOURCE, DEST, how_much)
     bigdigit *SOURCE;
     fast bigdigit *DEST;
     fast long how_much;
{
  bigdigit carry = 0;
  fast unsigned bigdouble digits;
  fast bigdigit *SCAN;

  if (how_much == 1)
  {
    if (SOURCE != DEST)
      copy_bignum(SOURCE, DEST);
    return 0;
  }
  Prepare_Header(DEST, LEN(SOURCE), SIGN(DEST));
  SCAN   = Bignum_Top(SOURCE);
  DEST   = Bignum_Top(DEST);
  SOURCE = Bignum_Bottom(SOURCE);
  while (SCAN >= SOURCE)
  {
    /* Bug fix by JMiller */
    fast unsigned bigdouble digits, temp;

    digits = Mul_Radix(carry) + *SCAN--;
    temp = digits / how_much;
    *DEST--  = temp;
    temp = temp * how_much;
    carry  = digits - temp;
  }
  return carry;
}

/* Bignum Comparison utilities */

/* big_compare_unsigned() compares the magnitudes of two BIGNUM's.
 * Called by big_compare() and minus_unsigned_bignum().
 */

int
big_compare_unsigned(ARG1, ARG2)
     fast bigdigit *ARG1, *ARG2;
{
  fast bigdigit *LIMIT;

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

/* big_compare() will return either of three cases, determining whether
 * ARG1 is bigger, smaller, or equal to ARG2.
 */

Pointer
big_compare(ARG1, ARG2)
     bigdigit *ARG1, *ARG2;
{
  switch(Categorize_Sign(ARG1, ARG2))
  { case BOTH_NEGATIVE : return big_compare_unsigned(ARG2, ARG1);
    case BOTH_POSITIVE : return big_compare_unsigned(ARG1, ARG2);
    case ARG1_NEGATIVE : return TWO_BIGGER;
    case ARG2_NEGATIVE : return ONE_BIGGER;
    default: Sign_Error("big_compare()");
  }
  /*NOTREACHED*/
}

Pointer
Fix_To_Big (object)
     Pointer object;
{
  fast long value;
  fast Pointer result;

  FIXNUM_VALUE (object, value);
  if (value == 0)
    return (make_bignum_zero ());
  else if (value > 0)
    result = (bignum_allocate (FIXNUM_LENGTH_AS_BIGNUM, false));
  else
    {
      result = (bignum_allocate (FIXNUM_LENGTH_AS_BIGNUM, true));
      value = (- value);
    }
  {
    fast bigdigit * scan = (BIGNUM_START_PTR (result));
    fast long length = 0;
    while (value > 0)
      {
	(*scan++) = (value & DIGIT_MASK);
	value = (value >> SHIFT);
	length += 1;
      }
    (BIGNUM_LENGTH (result)) = length;
    Fast_Vector_Set (result, 0, (DIGITS_TO_GC_HEADER (length)));
  }
  return (result);
}

Pointer
Big_To_Fix (object)
     Pointer object;
{
  if (! (BIGNUM_P (object)))
    return (object);
  if (BIGNUM_ZERO_P (object))
    return (MAKE_UNSIGNED_FIXNUM (0));
  {
    long length = (BIGNUM_LENGTH (object));
    if (length > FIXNUM_LENGTH_AS_BIGNUM)
      return (object);
    {
      fast bigdigit * start = (BIGNUM_START_PTR (object));
      fast bigdigit * scan = (start + length);
      fast long result = (*--scan);
      if (length == FIXNUM_LENGTH_AS_BIGNUM)
	{
	  long max_value = (1 << (FIXNUM_LENGTH - ((length - 1) * SHIFT)));

	  if ((result > max_value) ||
	      ((result == max_value) && (! (BIGNUM_NEGATIVE_P (object)))))
	    return (object);
	}
      while (scan > start)
	result = ((result << SHIFT) + (*--scan));
      if (BIGNUM_NEGATIVE_P (object))
	result = (- result);
      return ((Fixnum_Fits (result)) ? (MAKE_SIGNED_FIXNUM (result)) : object);
    }
  }
}

Pointer
Big_To_Float (bignum)
     Pointer bignum;
{
  /* If precision should not be lost,
     compare to FLONUM_MANTISSA_BITS instead. */
  if ((bignum_length_in_bits (bignum)) > MAX_FLONUM_EXPONENT)
    return (bignum);
  {
    fast bigdigit * start = (BIGNUM_START_PTR (bignum));
    fast bigdigit * scan = (BIGNUM_END_PTR (bignum));
    fast double accumulator = (0.0);
    while (scan > start)
      accumulator = ((accumulator * ((double) RADIX)) + ((double) (*--scan)));
    if (BIGNUM_NEGATIVE_P (bignum))
      accumulator = (- accumulator);
    Primitive_GC_If_Needed (FLONUM_SIZE + 1);
    return (Allocate_Float (accumulator));
  }
}

#ifdef HAS_FREXP
extern double frexp(), ldexp();
#else
#include "missing.c"
#endif

Pointer
Float_To_Big(flonum)
     double flonum;
{
  fast double mantissa;
  fast bigdigit *Answer, size;
  int exponent;
  long Align_size;

  if (flonum == 0.0)
    return (make_bignum_zero ());
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

Pointer
plus_unsigned_bignum(ARG1, ARG2, sign)
     fast bigdigit *ARG1, *ARG2;
     bigdigit sign;
{
  fast unsigned bigdouble Sum;
  long Size;
  fast bigdigit *Answer;
  fast bigdigit *TOP2, *TOP1;

  /* Swap ARG1 and ARG2 so that ARG1 is always longer */

  if (LEN(ARG1) < LEN(ARG2))
  {
    Answer = ARG1;
    ARG1  = ARG2;
    ARG2  = Answer;
  }

  /* Allocate Storage and do GC if needed */

  Size = Align(LEN(ARG1) + 1);
  Primitive_GC_If_Needed(Size);
  Answer = BIGNUM(Free);
  Prepare_Header(Answer, (LEN(ARG1) + 1), sign);

  /* Prepare Scanning Pointers and delimiters */

  TOP1 = Bignum_Top(ARG1);
  TOP2 = Bignum_Top(ARG2);
  ARG1 = Bignum_Bottom(ARG1);
  ARG2 = Bignum_Bottom(ARG2);
  Answer = Bignum_Bottom(Answer);
  Sum  = 0;

  /* Starts Looping */

  while (TOP2 >= ARG2)
  {
    Sum       = *ARG1++ + *ARG2++ + Get_Carry(Sum);
    *Answer++ = Get_Digit(Sum);
  }

  /* Let remaining carry propagate */

  while ((TOP1 >= ARG1) && (Get_Carry(Sum) != 0))
  {
    Sum       = *ARG1++ + 1;
    *Answer++ = Get_Digit(Sum);
  }

  /* Copy rest of ARG1 into Answer */
  while (TOP1 >= ARG1)
    *Answer++ = *ARG1++;
  *Answer = Get_Carry(Sum);

  /* Trims Answer.  The trim function is not used because there is at
   * most one leading zero.
   */

  if (*Answer == 0)
  {
    Answer = BIGNUM(Free);
    LEN(Answer) -= 1;
    *((Pointer *) Answer) = Make_Header(Align(LEN(Answer)));
  }
  Free  += Size;
  return Make_Pointer(TC_BIG_FIXNUM, Free-Size);
}

Pointer
minus_unsigned_bignum(ARG1, ARG2, sign)
     fast bigdigit *ARG1, *ARG2;
     bigdigit sign;
{
  fast bigdouble Diff;
  fast bigdigit *Answer, *TOP2, *TOP1;
  long Size;

  if (big_compare_unsigned(ARG1, ARG2) == TWO_BIGGER)
  {
    Answer = ARG1;
    ARG1  = ARG2;
    ARG2  = Answer;
    sign  = !sign;
  }

  Size   = Align(LEN(ARG1));
  Primitive_GC_If_Needed(Size);
  Answer = BIGNUM(Free);
  Prepare_Header(Answer, LEN(ARG1), sign);

  TOP1 = Bignum_Top(ARG1);
  TOP2 = Bignum_Top(ARG2);
  ARG1  = Bignum_Bottom(ARG1);
  ARG2  = Bignum_Bottom(ARG2);
  Answer = Bignum_Bottom(Answer);
  Diff = RADIX;

  /* Main loops for minus_unsigned_bignum */

  while (TOP2 >= ARG2)
  {
    Diff      =  *ARG1++ + (MAX_DIGIT_SIZE - *ARG2++) + Get_Carry(Diff);
    *Answer++ = Get_Digit(Diff);
  }

  while ((TOP1 >= ARG1) && (Get_Carry(Diff) == 0))
  {
    Diff      = *ARG1++ + MAX_DIGIT_SIZE;
    *Answer++ = Get_Digit(Diff);
  }

  while (TOP1 >= ARG1)
    *Answer++ = *ARG1++;
  trim_bignum((bigdigit *) Free);
  Free  += Size;
  return Make_Pointer(TC_BIG_FIXNUM, Free-Size);
}

/* Addition */

Pointer
plus_signed_bignum(ARG1, ARG2)
     bigdigit *ARG1, *ARG2;
{ /* Special Case for answer being zero */
  if (ZERO_BIGNUM(ARG1) && ZERO_BIGNUM(ARG2))
     return (make_bignum_zero ());
  switch(Categorize_Sign(ARG1, ARG2))
  { case BOTH_POSITIVE : return(plus_unsigned_bignum(ARG1, ARG2, POSITIVE));
    case ARG1_NEGATIVE : return(minus_unsigned_bignum(ARG2, ARG1, POSITIVE));
    case ARG2_NEGATIVE : return(minus_unsigned_bignum(ARG1, ARG2, POSITIVE));
    case BOTH_NEGATIVE : return(plus_unsigned_bignum(ARG1, ARG2, NEGATIVE));
    default : Sign_Error("plus_bignum()");
  }
  /*NOTREACHED*/
}

/* Subtraction */

Pointer
minus_signed_bignum(ARG1, ARG2)
     bigdigit *ARG1, *ARG2;
{
  /* Special Case for answer being zero */

  if (ZERO_BIGNUM(ARG1) && ZERO_BIGNUM(ARG2))
     return (make_bignum_zero ());

  /* Dispatches According to Sign of Args */

  switch(Categorize_Sign(ARG1, ARG2))
  { case BOTH_POSITIVE : return(minus_unsigned_bignum(ARG1, ARG2, POSITIVE));
    case ARG1_NEGATIVE : return(plus_unsigned_bignum(ARG1, ARG2, NEGATIVE));
    case ARG2_NEGATIVE : return(plus_unsigned_bignum(ARG1, ARG2, POSITIVE));
    case BOTH_NEGATIVE : return(minus_unsigned_bignum(ARG2, ARG1, POSITIVE));
    default : Sign_Error("minus_bignum()");
  }
  /*NOTREACHED*/
}

/* Multiplication */

Pointer
multiply_unsigned_bignum(ARG1, ARG2, sign)
     fast bigdigit *ARG1, *ARG2;
     bigdigit sign;
{
  bigdigit *TOP1, *TOP2;
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
  while (TOP1 >= TOP2)
    *TOP2++ = 0;

  /* Main loops for MULTIPLY */

  size   = LEN(ARG2);
  Answer = Bignum_Bottom(Answer) +  size;
  TOP1   = Bignum_Top(ARG1);
  TOP2   = Bignum_Top(ARG2);
  ARG2   = TOP2;

  for (ARG1 = Bignum_Bottom(ARG1); TOP1 >= ARG1; ARG1++, Answer++)
  {
    if (*ARG1 != 0)
    {
      Prod = 0;
      Answer -= size;
      for (ARG2 = TOP2 - size + 1; TOP2 >= ARG2; ++ARG2)
      {
	Prod = *ARG1 * *ARG2 + *Answer + Get_Carry(Prod);
        *Answer++  = Get_Digit(Prod);
      }
      *Answer = Get_Carry(Prod);
    }
  }

  /* Trims Answer */

  Answer = BIGNUM(Free);
  if (*(Bignum_Top(Answer)) == 0)
  {
    LEN(Answer) -= 1;
    *((Pointer *) Answer) = Make_Header(Align(LEN(Answer)));
  }
  Free  += Size;
  return Make_Pointer(TC_BIG_FIXNUM, Free-Size);
}

Pointer
multiply_signed_bignum(ARG1, ARG2)
     bigdigit *ARG1, *ARG2;
{
  if (ZERO_BIGNUM(ARG1) || ZERO_BIGNUM(ARG2))
     return (make_bignum_zero ());

  switch(Categorize_Sign(ARG1,ARG2))
  { case BOTH_POSITIVE :
    case BOTH_NEGATIVE :
      return multiply_unsigned_bignum(ARG1, ARG2, POSITIVE);
    case ARG1_NEGATIVE :
    case ARG2_NEGATIVE :
      return multiply_unsigned_bignum(ARG1, ARG2, NEGATIVE);
    default : Sign_Error("multiply_bignum()");
  }
  /*NOTREACHED*/
}

/* This is the guts of the division algorithm. The storage
 * allocation and other hairy prep work is done in the superior
 * routines. ARG1 and ARG2 are fresh copies, ARG1 will 
 * ultimately become the Remainder.  Storage already 
 * allocated for all four parameters.
 */

static Pointer BIG_A[TEMP_SIZE], BIG_B[TEMP_SIZE];

Pointer
div_internal(ARG1, ARG2, Quotient)
     bigdigit *ARG1, *ARG2, *Quotient;
{
  fast bigdigit *SCAN,*PROD;
  fast bigdouble Digit, Prod;
  fast bigdouble guess, dvsr2, dvsr1;
  fast bigdigit *LIMIT, *QUOT_SCAN;
  bigdigit *Big_A, *Big_B;

  Big_A = BIGNUM(BIG_A);
  Big_B = BIGNUM(BIG_B);
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

  /* Main Loop for div_internal() */

  while (QUOT_SCAN >= Quotient)
   {
     if (dvsr1 <= *ARG1) guess = RADIX - 1;
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
     {
       guess -= 1;
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

     LIMIT = Bignum_Top(ARG2);
     PROD  = Bignum_Bottom(ARG2);
     Digit = RADIX + *SCAN;
     while (LIMIT >= PROD)
     {
       Prod    = *PROD++ * guess;
       Digit   = Digit - Get_Digit(Prod);
       *SCAN++ = Get_Digit(Digit);
       Digit   = ((*SCAN - Get_Carry(Prod)) +
		  (MAX_DIGIT_SIZE +
		   ((Digit < 0) ? -1 : Get_Carry(Digit))));
     }
     *SCAN = Get_Digit(Digit);

     if (Get_Carry(Digit) == 0)
     {
       /* Guess is one too big, add back. */

       Digit = 0;
       guess -= 1;
       LIMIT = Bignum_Top(ARG2);
       SCAN  = SCAN - LEN(ARG2);
       PROD  = Bignum_Bottom(ARG2);
       while (LIMIT >= PROD)
       {
	 Digit   = *SCAN + *PROD++ + Get_Carry(Digit);
         *SCAN++ = Get_Digit(Digit);
       }
       *SCAN = 0;
     }
     *QUOT_SCAN-- = guess;
     ARG1 -= 1;
     SCAN = ARG1 - LEN(ARG2);
   }
}

/* div_signed_bignum() differentiates between all the possible
 * cases and allocates storage for the quotient, remainder, and
 * any intrmediate storage needed.
 */

Pointer
div_signed_bignum (ARG1, ARG2)
     bigdigit *ARG1, *ARG2;
{
  bigdigit *SARG2;
  bigdigit *QUOT, *REMD;
  Pointer *Cons_Cell;

  if (ZERO_BIGNUM(ARG2))
    Primitive_Error(ERR_ARG_2_BAD_RANGE);
  Primitive_GC_If_Needed(2);
  Cons_Cell = Free;
  Free += 2;

  if (big_compare_unsigned(ARG1, ARG2) == TWO_BIGGER)
  {
    /* Trivial Solution for ARG1 > ARG2 
       Quotient is zero and the remainder is just a copy of Arg_1. */

    Primitive_GC_If_Needed(Align(0)+Align(LEN(ARG1)));
    QUOT = BIGNUM(Free);
    Free += Align(0);
    Prepare_Header(QUOT, 0, POSITIVE);
    REMD = BIGNUM(Free);
    Free += Align(LEN(ARG1));
    copy_bignum(ARG1, REMD);
  }
  else if (LEN(ARG2)==1)
  {
    /* Divisor is only one digit long.
       unscale() is used to divide out Arg_1 and the remainder is the
       single digit returned by unscale(), coerced to a bignum. */

    Primitive_GC_If_Needed(Align(LEN(ARG1))+Align(1));
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
  {
    /* Usual case. div_internal() is called.  A normalized copy of Arg_1
       resides in REMD, which ultimately becomes the remainder.  The
       normalized copy of Arg_2 is in SARG2. */

    bigdouble temp =
      (Align(LEN(ARG1)-LEN(ARG2)+1) + Align(LEN(ARG1)+1) + Align(LEN(ARG2)+1));
    Primitive_GC_If_Needed (temp);
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
  }
  /* Glue the two results in a list and return as answer */
  Cons_Cell[CONS_CAR] = Make_Pointer(TC_BIG_FIXNUM, (Pointer *) QUOT);
  Cons_Cell[CONS_CDR] = Make_Pointer(TC_BIG_FIXNUM, (Pointer *) REMD);
  return Make_Pointer(TC_LIST, Cons_Cell);
}

/* Utility for debugging */

#ifdef ENABLE_DEBUGGING_TOOLS
void
print_digits(name, num, how_many)
     char *name;
     bigdigit *num;
     int how_many;
{
  int NDigits = LEN(num);
  int limit;

  printf("\n%s = 0x%08x", name, num);
  printf("\n  Sign: %c, Vector length: %d, # Digits: %d",
         ((SIGN(num) == NEGATIVE) ? '-' :
	  ((SIGN(num) == POSITIVE) ? '+' : '?')),
	 Datum(((Pointer *) num)[VECTOR_LENGTH]),
	 NDigits);
  if (how_many == -1)
    limit = NDigits;
  else
    limit = ((how_many < NDigits) ? how_many : NDigits);
  num = Bignum_Bottom(num);
  while (--how_many >= 0)
    printf("\n    0x%04x", *num++);
  if (limit < NDigits)
    printf("\n    ...");
  printf("\n");
  return;
}
#endif

DEFINE_PRIMITIVE ("COERCE-FIXNUM-TO-BIGNUM", Prim_fix_to_big, 1, 1,
  "Returns the bignum that corresponds to FIXNUM.")
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, FIXNUM_P);
  PRIMITIVE_RETURN (Fix_To_Big (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("COERCE-BIGNUM-TO-FIXNUM", Prim_big_to_fix, 1, 1,
  "Returns the fixnum that corresponds to BIGNUM.
If BIGNUM cannot be represented as a fixnum, returns BIGNUM.")
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, BIGNUM_P);
  PRIMITIVE_RETURN (Big_To_Fix (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("LISTIFY-BIGNUM", Prim_listify_bignum, 2, 2,
  "Returns a list of the digits of BIGNUM in RADIX.")
{
  Pointer bignum;
  long radix;
  PRIMITIVE_HEADER (2);

  Set_Time_Zone (Zone_Math);

  CHECK_ARG (1, BIGNUM_P);
  bignum = (ARG_REF (1));
  radix = (arg_nonnegative_integer (2, (BIGGEST_FIXNUM + 1)));
  if (BIGNUM_ZERO_P (bignum))
    PRIMITIVE_RETURN (cons ((MAKE_UNSIGNED_FIXNUM (0)), EMPTY_LIST));
  {
    Pointer working_copy = (bignum_copy (bignum));
    fast bigdigit * start_copy = (BIGNUM_START_PTR (working_copy));
    fast bigdigit * end_copy = (BIGNUM_END_PTR (working_copy));
    fast Pointer previous_cdr = EMPTY_LIST;
    while (end_copy > start_copy)
      {
	if ((end_copy [-1]) == 0)
	  end_copy -= 1;
	else
	  previous_cdr =
	    (cons
	     ((MAKE_UNSIGNED_FIXNUM (scale_down_self (working_copy, radix))),
	      previous_cdr));
      }
    PRIMITIVE_RETURN (previous_cdr);
  }
}

#define BINARY_PRIMITIVE(operator)					\
{									\
  PRIMITIVE_HEADER (2);							\
									\
  Set_Time_Zone (Zone_Math);						\
  CHECK_ARG (1, BIGNUM_P);						\
  CHECK_ARG (2, BIGNUM_P);						\
  {									\
    Pointer * original_free = Free;					\
    Pointer result =							\
      (operator								\
       ((BIGNUM (Get_Pointer (ARG_REF (1)))),				\
	(BIGNUM (Get_Pointer (ARG_REF (2))))));				\
    if (Consistency_Check && ((Get_Pointer (result)) != original_free))	\
      {									\
	fprintf (stderr,						\
		 "\nBignum operation result at 0x%x, Free was 0x%x\n",	\
		 (Address (result)),					\
		 Free);							\
	Microcode_Termination (TERM_EXIT);				\
      }									\
    Free = (Nth_Vector_Loc (result, ((Vector_Length (result)) + 1)));	\
    if (Consistency_Check && (Free > Heap_Top))				\
      {									\
	fprintf (stderr,						\
		 "\nBignum operation result at 0x%x, length 0x%x\n",	\
		 (Address (result)),					\
		 (Vector_Length (result)));				\
	Microcode_Termination (TERM_EXIT);				\
      }									\
    PRIMITIVE_RETURN (result);						\
  }									\
}

DEFINE_PRIMITIVE ("PLUS-BIGNUM", Prim_plus_bignum, 2, 2, 0)
BINARY_PRIMITIVE (plus_signed_bignum)

DEFINE_PRIMITIVE ("MINUS-BIGNUM", Prim_minus_bignum, 2, 2, 0)
BINARY_PRIMITIVE (minus_signed_bignum)

DEFINE_PRIMITIVE ("MULTIPLY-BIGNUM", Prim_multiply_bignum, 2, 2, 0)
BINARY_PRIMITIVE (multiply_signed_bignum)

/* (DIVIDE-BIGNUM ONE-BIGNUM ANOTHER_BIGNUM)
 * returns a cons of the bignum quotient and remainder of both arguments.
 */

DEFINE_PRIMITIVE ("DIVIDE-BIGNUM", Prim_divide_bignum, 2, 2, 0)
{
  Pointer Result, *End_Of_First, *First, *Second, *original_free=Free;
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
    {
      fprintf(stderr, "\nBignum_Divide: results swapped.\n");
      Microcode_Termination(TERM_EXIT);
    }
    else if (First != original_free+2)
    {
      fprintf(stderr, "\nBignum Divide: hole at start\n");
      Microcode_Termination(TERM_EXIT);
    }
  }
  End_Of_First = First + 1 + (OBJECT_DATUM (First[0]));
  if (Bignum_Debug)
    printf("\nEnd_Of_First=0x%x\n", End_Of_First);
  if (End_Of_First != Second)
  {
    *End_Of_First =
      Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, (Second-End_Of_First)-1);
    if (Bignum_Debug)
      printf("\nGap=0x%x\n", (Second-End_Of_First)-1);
  }
  Free = Second + 1 + (OBJECT_DATUM (Second[0]));
  if (Bignum_Debug)
    printf("\nEnd=0x%x\n", Free);
  return Result;
}

/* All the unary bignum predicates take one argument and return NIL if
   it is not a bignum.  Otherwise, they return a fixnum 1 if the
   predicate is true or a fixnum 0 if it is false.  This convention of
   NIL/0/1 is used for all numeric predicates so that the generic
   dispatch can detect "inapplicable" as distinct from "false" answer.
*/

#define Unary_Predicate(Test)						\
{									\
  bigdigit *ARG;							\
  Primitive_1_Arg();							\
									\
  Arg_1_Type(TC_BIG_FIXNUM);						\
  Set_Time_Zone(Zone_Math);						\
  ARG = BIGNUM(Get_Pointer(Arg1));					\
  return (MAKE_UNSIGNED_FIXNUM (((Test) ? 1 : 0)));			\
}

DEFINE_PRIMITIVE ("ZERO-BIGNUM?", Prim_zero_bignum, 1, 1, 0)
Unary_Predicate(LEN(ARG) == 0)

DEFINE_PRIMITIVE ("POSITIVE-BIGNUM?", Prim_positive_bignum, 1, 1, 0)
Unary_Predicate((LEN(ARG) != 0) && POS_BIGNUM(ARG))

DEFINE_PRIMITIVE ("NEGATIVE-BIGNUM?", Prim_negative_bignum, 1, 1, 0)
Unary_Predicate((LEN(ARG) != 0) && NEG_BIGNUM(ARG))

/* All the binary bignum predicates take two arguments and return NIL
   if either of them is not a bignum.  Otherwise, they return an
   answer as described above for the unary predicates.
*/

#define Binary_Predicate(Code)						\
{									\
  int result;								\
  Primitive_2_Args();							\
									\
  Arg_1_Type(TC_BIG_FIXNUM);						\
  Arg_2_Type(TC_BIG_FIXNUM);						\
  Set_Time_Zone(Zone_Math);						\
  if (big_compare(BIGNUM(Get_Pointer(Arg1)),				\
		  BIGNUM(Get_Pointer(Arg2))) == Code)			\
    result = 1;								\
  else									\
    result = 0;								\
  return (MAKE_UNSIGNED_FIXNUM (result));				\
}

DEFINE_PRIMITIVE ("EQUAL-BIGNUM?", Prim_equal_bignum, 2, 2, 0)
Binary_Predicate(EQUAL)

DEFINE_PRIMITIVE ("GREATER-THAN-BIGNUM?", Prim_greater_bignum, 2, 2, 0)
Binary_Predicate(ONE_BIGGER)

DEFINE_PRIMITIVE ("LESS-THAN-BIGNUM?", Prim_less_bignum, 2, 2, 0)
Binary_Predicate(TWO_BIGGER)
