/* -*-C-*-

$Id: bignum.c,v 9.59 2007/07/09 00:46:44 cph Exp $

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

/* Implementation of Bignums (unlimited precision integers) */

#ifdef MIT_SCHEME
#  include "scheme.h"
#else
#  include "bignum.h"
#endif

#include "bignmint.h"

#ifndef MIT_SCHEME

static bignum_type
bignum_malloc (bignum_length_type length)
{
  extern char * malloc ();
  char * result = (malloc ((length + 1) * (sizeof (bignum_digit_type))));
  BIGNUM_ASSERT (result != ((char *) 0));
  return ((bignum_type) result);
}

static bignum_type
bignum_realloc (bignum_type bignum, bignum_length_type length)
{
  extern char * realloc ();
  char * result =
    (realloc (((char *) bignum),
	      ((length + 1) * (sizeof (bignum_digit_type)))));
  BIGNUM_ASSERT (result != ((char *) 0));
  return ((bignum_type) result);
}

#endif /* not MIT_SCHEME */

/* Forward references */
static int bignum_equal_p_unsigned (bignum_type, bignum_type);
static enum bignum_comparison bignum_compare_unsigned
  (bignum_type, bignum_type);
static bignum_type bignum_add_unsigned (bignum_type, bignum_type, int);
static bignum_type bignum_subtract_unsigned (bignum_type, bignum_type);
static bignum_type bignum_multiply_unsigned (bignum_type, bignum_type, int);
static bignum_type bignum_multiply_unsigned_small_factor
  (bignum_type, bignum_digit_type, int);
static void bignum_destructive_scale_up (bignum_type, bignum_digit_type);
static void bignum_destructive_add (bignum_type, bignum_digit_type);
static void bignum_divide_unsigned_large_denominator
  (bignum_type, bignum_type, bignum_type *, bignum_type *, int, int);
static void bignum_destructive_normalization (bignum_type, bignum_type, int);
static void bignum_destructive_unnormalization (bignum_type, int);
static void bignum_divide_unsigned_normalized
  (bignum_type, bignum_type, bignum_type);
static bignum_digit_type bignum_divide_subtract
  (bignum_digit_type *, bignum_digit_type *,
   bignum_digit_type, bignum_digit_type *);
static void bignum_divide_unsigned_medium_denominator
  (bignum_type, bignum_digit_type, bignum_type *, bignum_type *, int, int);
static bignum_digit_type bignum_digit_divide
  (bignum_digit_type, bignum_digit_type,
   bignum_digit_type, bignum_digit_type *);
static bignum_digit_type bignum_digit_divide_subtract
  (bignum_digit_type, bignum_digit_type,
   bignum_digit_type, bignum_digit_type *);
static void bignum_divide_unsigned_small_denominator
  (bignum_type, bignum_digit_type, bignum_type *, bignum_type *, int, int);
static bignum_digit_type bignum_destructive_scale_down
  (bignum_type, bignum_digit_type);
static bignum_type bignum_remainder_unsigned_small_denominator
  (bignum_type, bignum_digit_type, int);
static bignum_type bignum_digit_to_bignum (bignum_digit_type, int);
static bignum_type bignum_allocate (bignum_length_type, int);
static bignum_type bignum_allocate_zeroed (bignum_length_type, int);
static bignum_type bignum_shorten_length (bignum_type, bignum_length_type);
static bignum_type bignum_trim (bignum_type);
static bignum_type bignum_copy (bignum_type);
static bignum_type bignum_new_sign (bignum_type, int);
static bignum_type bignum_maybe_new_sign (bignum_type, int);
static void bignum_destructive_copy (bignum_type, bignum_type);

#define ULONG_LENGTH_IN_BITS(digit, len) do				\
{									\
  unsigned long w = digit;						\
  len = 0;								\
  while (w > 0xff) { len += 8; w >>= 8; }				\
  while (w > 0)    { len += 1; w >>= 1; }				\
} while (0)

/* Exports */

bignum_type
bignum_make_zero (void)
{
  bignum_type result = (BIGNUM_ALLOCATE (0));
  BIGNUM_SET_HEADER (result, 0, 0);
  return (result);
}

bignum_type
bignum_make_one (int negative_p)
{
  bignum_type result = (BIGNUM_ALLOCATE (1));
  BIGNUM_SET_HEADER (result, 1, negative_p);
  (BIGNUM_REF (result, 0)) = 1;
  return (result);
}

int
bignum_equal_p (bignum_type x, bignum_type y)
{
  return
    ((BIGNUM_ZERO_P (x))
     ? (BIGNUM_ZERO_P (y))
     : ((! (BIGNUM_ZERO_P (y)))
	&& ((BIGNUM_NEGATIVE_P (x))
	    ? (BIGNUM_NEGATIVE_P (y))
	    : (! (BIGNUM_NEGATIVE_P (y))))
	&& (bignum_equal_p_unsigned (x, y))));
}

enum bignum_comparison
bignum_test (bignum_type bignum)
{
  return
    ((BIGNUM_ZERO_P (bignum))
     ? bignum_comparison_equal
     : (BIGNUM_NEGATIVE_P (bignum))
     ? bignum_comparison_less
     : bignum_comparison_greater);
}

enum bignum_comparison
bignum_compare (bignum_type x, bignum_type y)
{
  return
    ((BIGNUM_ZERO_P (x))
     ? ((BIGNUM_ZERO_P (y))
	? bignum_comparison_equal
	: (BIGNUM_NEGATIVE_P (y))
	? bignum_comparison_greater
	: bignum_comparison_less)
     : (BIGNUM_ZERO_P (y))
     ? ((BIGNUM_NEGATIVE_P (x))
	? bignum_comparison_less
	: bignum_comparison_greater)
     : (BIGNUM_NEGATIVE_P (x))
     ? ((BIGNUM_NEGATIVE_P (y))
	? (bignum_compare_unsigned (y, x))
	: (bignum_comparison_less))
     : ((BIGNUM_NEGATIVE_P (y))
	? (bignum_comparison_greater)
	: (bignum_compare_unsigned (x, y))));
}

bignum_type
bignum_add (bignum_type x, bignum_type y)
{
  return
    ((BIGNUM_ZERO_P (x))
     ? (BIGNUM_MAYBE_COPY (y))
     : (BIGNUM_ZERO_P (y))
     ? (BIGNUM_MAYBE_COPY (x))
     : ((BIGNUM_NEGATIVE_P (x))
	? ((BIGNUM_NEGATIVE_P (y))
	   ? (bignum_add_unsigned (x, y, 1))
	   : (bignum_subtract_unsigned (y, x)))
	: ((BIGNUM_NEGATIVE_P (y))
	   ? (bignum_subtract_unsigned (x, y))
	   : (bignum_add_unsigned (x, y, 0)))));
}

bignum_type
bignum_subtract (bignum_type x, bignum_type y)
{
  return
    ((BIGNUM_ZERO_P (x))
     ? ((BIGNUM_ZERO_P (y))
	? (BIGNUM_MAYBE_COPY (y))
	: (bignum_new_sign (y, (! (BIGNUM_NEGATIVE_P (y))))))
     : ((BIGNUM_ZERO_P (y))
	? (BIGNUM_MAYBE_COPY (x))
	: ((BIGNUM_NEGATIVE_P (x))
	   ? ((BIGNUM_NEGATIVE_P (y))
	      ? (bignum_subtract_unsigned (y, x))
	      : (bignum_add_unsigned (x, y, 1)))
	   : ((BIGNUM_NEGATIVE_P (y))
	      ? (bignum_add_unsigned (x, y, 0))
	      : (bignum_subtract_unsigned (x, y))))));
}

bignum_type
bignum_negate (bignum_type x)
{
  return
    ((BIGNUM_ZERO_P (x))
     ? (BIGNUM_MAYBE_COPY (x))
     : (bignum_new_sign (x, (! (BIGNUM_NEGATIVE_P (x))))));
}

bignum_type
bignum_multiply (bignum_type x, bignum_type y)
{
  bignum_length_type x_length = (BIGNUM_LENGTH (x));
  bignum_length_type y_length = (BIGNUM_LENGTH (y));
  int negative_p =
    ((BIGNUM_NEGATIVE_P (x))
     ? (! (BIGNUM_NEGATIVE_P (y)))
     : (BIGNUM_NEGATIVE_P (y)));
  if (BIGNUM_ZERO_P (x))
    return (BIGNUM_MAYBE_COPY (x));
  if (BIGNUM_ZERO_P (y))
    return (BIGNUM_MAYBE_COPY (y));
  if (x_length == 1)
    {
      bignum_digit_type digit = (BIGNUM_REF (x, 0));
      if (digit == 1)
	return (bignum_maybe_new_sign (y, negative_p));
      if (digit < BIGNUM_RADIX_ROOT)
	return (bignum_multiply_unsigned_small_factor (y, digit, negative_p));
    }
  if (y_length == 1)
    {
      bignum_digit_type digit = (BIGNUM_REF (y, 0));
      if (digit == 1)
	return (bignum_maybe_new_sign (x, negative_p));
      if (digit < BIGNUM_RADIX_ROOT)
	return (bignum_multiply_unsigned_small_factor (x, digit, negative_p));
    }
  return (bignum_multiply_unsigned (x, y, negative_p));
}

int
bignum_divide (bignum_type numerator, bignum_type denominator,
	       bignum_type * quotient, bignum_type * remainder)
{
  if (BIGNUM_ZERO_P (denominator))
    return (1);
  if (BIGNUM_ZERO_P (numerator))
    {
      (*quotient) = (BIGNUM_MAYBE_COPY (numerator));
      (*remainder) = (BIGNUM_MAYBE_COPY (numerator));
    }
  else
    {
      int r_negative_p = (BIGNUM_NEGATIVE_P (numerator));
      int q_negative_p =
	((BIGNUM_NEGATIVE_P (denominator)) ? (! r_negative_p) : r_negative_p);
      switch (bignum_compare_unsigned (numerator, denominator))
	{
	case bignum_comparison_equal:
	  {
	    (*quotient) = (BIGNUM_ONE (q_negative_p));
	    (*remainder) = (BIGNUM_ZERO ());
	    break;
	  }
	case bignum_comparison_less:
	  {
	    (*quotient) = (BIGNUM_ZERO ());
	    (*remainder) = (BIGNUM_MAYBE_COPY (numerator));
	    break;
	  }
	case bignum_comparison_greater:
	  {
	    if ((BIGNUM_LENGTH (denominator)) == 1)
	      {
		bignum_digit_type digit = (BIGNUM_REF (denominator, 0));
		if (digit == 1)
		  {
		    (*quotient) =
		      (bignum_maybe_new_sign (numerator, q_negative_p));
		    (*remainder) = (BIGNUM_ZERO ());
		    break;
		  }
		else if (digit < BIGNUM_RADIX_ROOT)
		  {
		    bignum_divide_unsigned_small_denominator
		      (numerator, digit,
		       quotient, remainder,
		       q_negative_p, r_negative_p);
		    break;
		  }
		else
		  {
		    bignum_divide_unsigned_medium_denominator
		      (numerator, digit,
		       quotient, remainder,
		       q_negative_p, r_negative_p);
		    break;
		  }
	      }
	    bignum_divide_unsigned_large_denominator
	      (numerator, denominator,
	       quotient, remainder,
	       q_negative_p, r_negative_p);
	    break;
	  }
	}
    }
  return (0);
}

bignum_type
bignum_quotient (bignum_type numerator, bignum_type denominator)
{
  if (BIGNUM_ZERO_P (denominator))
    return (BIGNUM_OUT_OF_BAND);
  if (BIGNUM_ZERO_P (numerator))
    return (BIGNUM_MAYBE_COPY (numerator));
  {
    int q_negative_p =
      ((BIGNUM_NEGATIVE_P (denominator))
       ? (! (BIGNUM_NEGATIVE_P (numerator)))
       : (BIGNUM_NEGATIVE_P (numerator)));
    switch (bignum_compare_unsigned (numerator, denominator))
      {
      case bignum_comparison_equal:
	return (BIGNUM_ONE (q_negative_p));
      case bignum_comparison_less:
	return (BIGNUM_ZERO ());
      case bignum_comparison_greater:
	{
	  bignum_type quotient;
	  if ((BIGNUM_LENGTH (denominator)) == 1)
	    {
	      bignum_digit_type digit = (BIGNUM_REF (denominator, 0));
	      if (digit == 1)
		return (bignum_maybe_new_sign (numerator, q_negative_p));
	      if (digit < BIGNUM_RADIX_ROOT)
		bignum_divide_unsigned_small_denominator
		  (numerator, digit,
		   (&quotient), ((bignum_type *) 0),
		   q_negative_p, 0);
	      else
		bignum_divide_unsigned_medium_denominator
		  (numerator, digit,
		   (&quotient), ((bignum_type *) 0),
		   q_negative_p, 0);
	    }
	  else
	    bignum_divide_unsigned_large_denominator
	      (numerator, denominator,
	       (&quotient), ((bignum_type *) 0),
	       q_negative_p, 0);
	  return (quotient);
	}
      default:
	/*NOTREACHED*/
	return (0);
      }
  }
}

bignum_type
bignum_remainder (bignum_type numerator, bignum_type denominator)
{
  if (BIGNUM_ZERO_P (denominator))
    return (BIGNUM_OUT_OF_BAND);
  if (BIGNUM_ZERO_P (numerator))
    return (BIGNUM_MAYBE_COPY (numerator));
  switch (bignum_compare_unsigned (numerator, denominator))
    {
    case bignum_comparison_equal:
      return (BIGNUM_ZERO ());
    case bignum_comparison_less:
      return (BIGNUM_MAYBE_COPY (numerator));
    case bignum_comparison_greater:
      {
	bignum_type remainder;
	if ((BIGNUM_LENGTH (denominator)) == 1)
	  {
	    bignum_digit_type digit = (BIGNUM_REF (denominator, 0));
	    if ((digit & (digit-1)) == 0) /* i.e. digit = 2^k, including 1 */
	      {
		bignum_digit_type unsigned_remainder;
		if (BIGNUM_LENGTH (numerator) == 0)
		  return (BIGNUM_ZERO ());
		unsigned_remainder = (digit-1) & (BIGNUM_REF (numerator, 0));
		if (unsigned_remainder == 0)
		  return (BIGNUM_ZERO ());
		return (bignum_digit_to_bignum
			(unsigned_remainder, BIGNUM_NEGATIVE_P (numerator)));
	      }
	    if (digit < BIGNUM_RADIX_ROOT)
	      return
		(bignum_remainder_unsigned_small_denominator
		 (numerator, digit, (BIGNUM_NEGATIVE_P (numerator))));
	    bignum_divide_unsigned_medium_denominator
	      (numerator, digit,
	       ((bignum_type *) 0), (&remainder),
	       0, (BIGNUM_NEGATIVE_P (numerator)));
	  }
	else
	  bignum_divide_unsigned_large_denominator
	    (numerator, denominator,
	     ((bignum_type *) 0), (&remainder),
	     0, (BIGNUM_NEGATIVE_P (numerator)));
	return (remainder);
      }
    default:
      /*NOTREACHED*/
      return (0);
    }
}

/* These procedures depend on the non-portable type `unsigned long'.
   If your compiler doesn't support this type, either define the
   switch `BIGNUM_NO_ULONG' to disable them (in "bignum.h"), or write
   new versions that don't use this type. */

#ifndef BIGNUM_NO_ULONG

bignum_type
long_to_bignum (long n)
{
  int negative_p;
  bignum_digit_type result_digits [BIGNUM_DIGITS_FOR_LONG];
  bignum_digit_type * end_digits = result_digits;
  /* Special cases win when these small constants are cached. */
  if (n == 0) return (BIGNUM_ZERO ());
  if (n == 1) return (BIGNUM_ONE (0));
  if (n == -1) return (BIGNUM_ONE (1));
  {
    unsigned long accumulator = ((negative_p = (n < 0)) ? (-n) : n);
    do
      {
	(*end_digits++) = (accumulator & BIGNUM_DIGIT_MASK);
	accumulator >>= BIGNUM_DIGIT_LENGTH;
      }
    while (accumulator != 0);
  }
  {
    bignum_type result =
      (bignum_allocate ((end_digits - result_digits), negative_p));
    bignum_digit_type * scan_digits = result_digits;
    bignum_digit_type * scan_result = (BIGNUM_START_PTR (result));
    while (scan_digits < end_digits)
      (*scan_result++) = (*scan_digits++);
    return (result);
  }
}

long
bignum_to_long (bignum_type bignum)
{
  if (BIGNUM_ZERO_P (bignum))
    return (0);
  {
    unsigned long accumulator = 0;
    bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
    bignum_digit_type * scan = (start + (BIGNUM_LENGTH (bignum)));
    while (start < scan)
      accumulator = ((accumulator << BIGNUM_DIGIT_LENGTH) + (*--scan));
    return
      ((BIGNUM_NEGATIVE_P (bignum))
       ? (- ((long) accumulator))
       : ((long) accumulator));
  }
}

bignum_type
ulong_to_bignum (unsigned long n)
{
  bignum_digit_type result_digits [BIGNUM_DIGITS_FOR_LONG];
  bignum_digit_type * end_digits = result_digits;
  /* Special cases win when these small constants are cached. */
  if (n == 0) return (BIGNUM_ZERO ());
  if (n == 1) return (BIGNUM_ONE (0));
  {
    unsigned long accumulator = n;
    do
      {
	(*end_digits++) = (accumulator & BIGNUM_DIGIT_MASK);
	accumulator >>= BIGNUM_DIGIT_LENGTH;
      }
    while (accumulator != 0);
  }
  {
    bignum_type result =
      (bignum_allocate ((end_digits - result_digits), 0));
    bignum_digit_type * scan_digits = result_digits;
    bignum_digit_type * scan_result = (BIGNUM_START_PTR (result));
    while (scan_digits < end_digits)
      (*scan_result++) = (*scan_digits++);
    return (result);
  }
}

unsigned long
bignum_to_ulong (bignum_type bignum)
{
  if (BIGNUM_ZERO_P (bignum))
    return (0);
  {
    unsigned long accumulator = 0;
    bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
    bignum_digit_type * scan = (start + (BIGNUM_LENGTH (bignum)));
    while (start < scan)
      accumulator = ((accumulator << BIGNUM_DIGIT_LENGTH) + (*--scan));
    return (accumulator);
  }
}

#endif /* not BIGNUM_NO_ULONG */

#define DTB_WRITE_DIGIT(n_bits) do					\
{									\
  significand *= (1UL << (n_bits));					\
  digit = ((bignum_digit_type) significand);				\
  (*--scan) = digit;							\
  significand -= ((double) digit);					\
  n_valid_bits -= (n_bits);						\
} while (0)

bignum_type
double_to_bignum (double x)
{
  int exponent;
  double significand = (frexp (x, (&exponent)));
  if (exponent <= 0) return (BIGNUM_ZERO ());
  if (exponent == 1) return (BIGNUM_ONE (x < 0));
  if (significand < 0) significand = (-significand);
  {
    bignum_length_type length = (BIGNUM_BITS_TO_DIGITS (exponent));
    bignum_type result = (bignum_allocate (length, (x < 0)));
    bignum_digit_type * start = (BIGNUM_START_PTR (result));
    bignum_digit_type * scan = (start + length);
    unsigned int n_valid_bits = DBL_MANT_DIG;
    bignum_digit_type digit;
    {
      int odd_bits = (exponent % BIGNUM_DIGIT_LENGTH);
      if (odd_bits > 0)
	DTB_WRITE_DIGIT (odd_bits);
    }
    while (start < scan)
      {
	if ((significand == 0)  || (n_valid_bits == 0))
	  {
	    while (start < scan)
	      (*--scan) = 0;
	    break;
	  }
	if (n_valid_bits >= BIGNUM_DIGIT_LENGTH)
	  DTB_WRITE_DIGIT (BIGNUM_DIGIT_LENGTH);
	else
	  {
	    significand *= (1UL << BIGNUM_DIGIT_LENGTH);
	    digit = ((bignum_digit_type) significand);
	    (*--scan)
	      = (digit
		 & (((1UL << n_valid_bits) - 1UL)
		    << (BIGNUM_DIGIT_LENGTH - n_valid_bits)));
	    significand = 0.0;
	    n_valid_bits = 0;
	  }
      }
    return (result);
  }
}

#undef DTB_WRITE_DIGIT

/*  This version sometimes gets the answer wrong due to rounding errors.
    It would be slightly better if the digits were accumulated lsb to msb.
 */
/*
double
bignum_to_double (bignum_type bignum)
{
  if (BIGNUM_ZERO_P (bignum))
    return (0);
  {
    double accumulator = 0;
    bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
    bignum_digit_type * scan = (start + (BIGNUM_LENGTH (bignum)));
    while (start < scan)
      accumulator = ((accumulator * BIGNUM_RADIX) + (*--scan));
    return ((BIGNUM_NEGATIVE_P (bignum)) ? (-accumulator) : accumulator);
  }
}
*/

double
bignum_to_double (bignum_type bignum)
{
  if (BIGNUM_ZERO_P (bignum))
    return (0.0);

  {
    bignum_length_type length = (BIGNUM_LENGTH (bignum));
    bignum_length_type index = length - 1;
    bignum_length_type scale_words = length - 1;
    bignum_digit_type msd = (BIGNUM_REF (bignum, (index)));
#if (FLT_RADIX == 2)
    int bits_to_get = DBL_MANT_DIG; /* includes implicit 1 */
#else
#  include "error: must have FLT_RADIX==2"
#endif
    double value = 0;
    bignum_digit_type mask = 0;
    bignum_digit_type guard_bit_mask = BIGNUM_RADIX>>1;
    bignum_digit_type rounding_correction = 0;
    int current_digit_bit_count = 0;

    ULONG_LENGTH_IN_BITS (msd, current_digit_bit_count);
    mask = ((1UL << current_digit_bit_count) - 1UL);

    while (1) {
      if (current_digit_bit_count > bits_to_get) {
	guard_bit_mask = (1UL << (current_digit_bit_count - bits_to_get - 1));
	mask &= ~((guard_bit_mask << 1) - 1UL);
	current_digit_bit_count = bits_to_get;
	bits_to_get = 0;
      } else {
	bits_to_get -= current_digit_bit_count;
      }

      value = (value * BIGNUM_RADIX) + ((BIGNUM_REF (bignum, index)) & mask);

      if (bits_to_get == 0) {
	scale_words = index;
	if (current_digit_bit_count == BIGNUM_DIGIT_LENGTH) {
	  if (index == 0) /* there is no guard bit */
	    goto finished;
	  guard_bit_mask = (1UL << (BIGNUM_DIGIT_LENGTH - 1));
	  rounding_correction = 1;
	  index -= 1;
	} else {
	  rounding_correction = (guard_bit_mask << 1);
	}
	break;
      }
      if (index == 0)  /* fewer than DBL_MANT_DIG bits */
	goto finished;

      index -= 1;
      current_digit_bit_count = BIGNUM_DIGIT_LENGTH;
      mask = BIGNUM_DIGIT_MASK;
    }

    /* round-to-even depending on lsb, guard and following bits: lgfffff */

    if ((BIGNUM_REF(bignum,index) & guard_bit_mask) == 0) /* case x0xxxx */
      goto round_down;

    if ((BIGNUM_REF(bignum,index) & (guard_bit_mask-1)) != 0) /* case x1xx1x */
      goto round_up;

    /* cases 110000 and 1101xx: test "odd?", i.e. round-to-even rounds up */
    if ((guard_bit_mask << 1) == BIGNUM_RADIX) {
      if (((BIGNUM_REF (bignum, index+1)) & 1UL) != 0)  /* "odd?" */
	goto round_up;
    } else {
      if (((BIGNUM_REF (bignum, index)) & (guard_bit_mask << 1)) != 0)
	goto round_up;
    }

    if (index==0)   /* case 010000, no more words of following bits */
      goto finished;

    { /* distinguish between cases 0100...00 and 0100..1xx, multiple words */
      bignum_length_type index2 = index - 1;
      while (index2 >= 0) {
	if ((BIGNUM_REF (bignum, index2)) != 0)
	  goto round_up;
	index2--;
      }
      goto round_down;
    }

  round_up:
    value += rounding_correction;
  round_down:
    /* note, ldexp `sticks' at the maximal non-infinity value, which
       is a reasonable compromise for numbers with DBL_MAX_EXP bits
       that round up */
    if (scale_words > 0)
      value = ldexp (value, scale_words * BIGNUM_DIGIT_LENGTH);

  finished:
    return ((BIGNUM_NEGATIVE_P (bignum)) ? (-value) : value);
  }
}

/*
;; Code to test bignum_to_double

(declare (usual-integrations))

(define integer->flonum (make-primitive-procedure 'integer->flonum 2))

(define (check n)
  (let ((f1 (integer->flonum n #b10))
	(f2 (exact->inexact n)))
    (if (and f1 (= f1 f2))
	(number->string f1 2)
	(begin
	  (pp n)
	  (pp (number->string f1 2))
	  (pp (number->string f2 2))))))

(define (test)
  (define n 0)
  (do ((i 0 (+ i 1)))			; guard bit zone patterns
      ((= i 256))
    (do ((j 0 (+ j 1)))			; general word alignment
	((= j 30))
      (do ((e 0 (+ e 1)))		; random `insignificant' bit
	  ((= e 2))
	(do ((up 0 (+ up 1)))		; test potential for carry
	    ((= up 2))
	  (do ((end-pad 0 (+ end-pad 100)))
	      ((> end-pad 100))
	    (set! n (+ n 1)) (if (= (remainder n 1000) 0) (pp `(test ,n)))
	    (let ((s (string-append "1"
				   (make-string 48 (vector-ref '#(#\0 #\1) up))
				   (string-pad-left (number->string i 2) 8 #\0)
				    (make-string (* j 23) #\0) ; gcd(23,30)=1
				    (number->string e)
				    (make-string end-pad #\0))))
	      (check (string->number s 2)))))))))
*/

int
bignum_fits_in_word_p (bignum_type bignum, long word_length,
		       int twos_complement_p)
{
  unsigned int n_bits = (twos_complement_p ? (word_length - 1) : word_length);
  BIGNUM_ASSERT (n_bits > 0);
  {
    bignum_length_type length = (BIGNUM_LENGTH (bignum));
    unsigned int max_digits = (BIGNUM_BITS_TO_DIGITS (n_bits));
    if (((unsigned int) length) < max_digits)
      return (1);
    if (((unsigned int) length) > max_digits)
      return (0);
    {
      bignum_digit_type msd = (BIGNUM_REF (bignum, (length - 1)));
      bignum_digit_type max
	= (1UL << (n_bits - ((length - 1) * BIGNUM_DIGIT_LENGTH)));
      return
	(((msd < max)
	  || (twos_complement_p
	      && (msd == max)
	      && (BIGNUM_NEGATIVE_P (bignum)))));
    }
  }
}

bignum_type
bignum_length_in_bits (bignum_type bignum)
{
  if (BIGNUM_ZERO_P (bignum))
    return (BIGNUM_ZERO ());
  {
    bignum_length_type index = ((BIGNUM_LENGTH (bignum)) - 1);
    bignum_digit_type digit = (BIGNUM_REF (bignum, index));
    bignum_digit_type delta = 0;
    bignum_type result = (bignum_allocate (2, 0));
    (BIGNUM_REF (result, 0)) = index;
    (BIGNUM_REF (result, 1)) = 0;
    bignum_destructive_scale_up (result, BIGNUM_DIGIT_LENGTH);
    ULONG_LENGTH_IN_BITS (digit, delta);
    bignum_destructive_add (result, ((bignum_digit_type) delta));
    return (bignum_trim (result));
  }
}

bignum_type
bignum_length_upper_limit (void)
{
  bignum_type result = (bignum_allocate (2, 0));
  (BIGNUM_REF (result, 0)) = 0;
  (BIGNUM_REF (result, 1)) = BIGNUM_DIGIT_LENGTH;
  return (result);
}

bignum_type
bignum_shift_left (bignum_type n, unsigned long m)
{
  unsigned long ln = (BIGNUM_LENGTH (n));
  unsigned long delta = 0;
  if (m == 0)
    return (n);

  ULONG_LENGTH_IN_BITS ((BIGNUM_REF (n, (ln - 1))), delta);

  {
    unsigned long zeroes = (m / BIGNUM_DIGIT_LENGTH);
    unsigned long shift = (m % BIGNUM_DIGIT_LENGTH);
    unsigned long ln2
      = (((ln - 1) + ((delta + m) / BIGNUM_DIGIT_LENGTH))
	 + (((delta + m) % BIGNUM_DIGIT_LENGTH) != 0));
    bignum_type result = (bignum_allocate (ln2, (BIGNUM_NEGATIVE_P (n))));
    bignum_digit_type * scan_n = (BIGNUM_START_PTR (n));
    bignum_digit_type * end_n = (scan_n + ln);
    bignum_digit_type * scan_result = (BIGNUM_START_PTR (result));
    while ((zeroes--) > 0)
      (*scan_result++) = 0;
    if (shift == 0)
      while (scan_n < end_n)
	(*scan_result++) = (*scan_n++);
    else
      {
	unsigned long temp = 0;
	while (scan_n < end_n)
	  {
	    bignum_digit_type digit = (*scan_n++);
	    (*scan_result++) = (((digit << shift) & BIGNUM_DIGIT_MASK) | temp);
	    temp = (digit >> (BIGNUM_DIGIT_LENGTH - shift));
	  }
	if (temp != 0)
	  (*scan_result) = temp;
      }
    return (result);
  }
}

bignum_type
unsigned_long_to_shifted_bignum (unsigned long n, unsigned long m, int sign)
{
  unsigned long delta = 0;
  if (n == 0)
    return (BIGNUM_ZERO ());

  ULONG_LENGTH_IN_BITS (n, delta);

  {
    unsigned long zeroes = (m / BIGNUM_DIGIT_LENGTH);
    unsigned long shift = (m % BIGNUM_DIGIT_LENGTH);
    unsigned long ln
      = (((delta + m) / BIGNUM_DIGIT_LENGTH)
	 + (((delta + m) % BIGNUM_DIGIT_LENGTH) != 0));
    bignum_type result = (bignum_allocate (ln, sign));
    bignum_digit_type * scan_result = (BIGNUM_START_PTR (result));
    while ((zeroes--) > 0)
      (*scan_result++) = 0;
    (*scan_result++) = ((n << shift) & BIGNUM_DIGIT_MASK);
    n >>= (BIGNUM_DIGIT_LENGTH - shift);
    while (n > 0)
      {
	(*scan_result++) = (n & BIGNUM_DIGIT_MASK);
	n >>= BIGNUM_DIGIT_LENGTH;
      }
    return (result);
  }
}

bignum_type
digit_stream_to_bignum (unsigned int n_digits,
			unsigned int (*producer) (bignum_procedure_context),
			bignum_procedure_context context,
			unsigned int radix,
			int negative_p)
{
  BIGNUM_ASSERT ((radix > 1) && (radix <= BIGNUM_RADIX_ROOT));
  if (n_digits == 0)
    return (BIGNUM_ZERO ());
  if (n_digits == 1)
    {
      long digit = ((long) ((*producer) (context)));
      return (long_to_bignum (negative_p ? (- digit) : digit));
    }
  {
    bignum_length_type length;
    {
      unsigned int log_radix = 0;
      ULONG_LENGTH_IN_BITS (radix, log_radix);
      /* This length will be at least as large as needed. */
      length = (BIGNUM_BITS_TO_DIGITS (n_digits * log_radix));
    }
    {
      bignum_type result = (bignum_allocate_zeroed (length, negative_p));
      while ((n_digits--) > 0)
	{
	  bignum_destructive_scale_up (result, ((bignum_digit_type) radix));
	  bignum_destructive_add
	    (result, ((bignum_digit_type) ((*producer) (context))));
	}
      return (bignum_trim (result));
    }
  }
}

void
bignum_to_digit_stream (bignum_type bignum,
			unsigned int radix,
			void (*consumer) (bignum_procedure_context, long),
			bignum_procedure_context context)
{
  BIGNUM_ASSERT ((radix > 1) && (radix <= BIGNUM_RADIX_ROOT));
  if (! (BIGNUM_ZERO_P (bignum)))
    {
      bignum_type working_copy = (bignum_copy (bignum));
      bignum_digit_type * start = (BIGNUM_START_PTR (working_copy));
      bignum_digit_type * scan = (start + (BIGNUM_LENGTH (working_copy)));
      while (start < scan)
	{
	  if ((scan[-1]) == 0)
	    scan -= 1;
	  else
	    (*consumer)
	      (context, (bignum_destructive_scale_down (working_copy, radix)));
	}
      BIGNUM_DEALLOCATE (working_copy);
    }
  return;
}

long
bignum_max_digit_stream_radix (void)
{
  return (BIGNUM_RADIX_ROOT);
}

/* Comparisons */

static int
bignum_equal_p_unsigned (bignum_type x, bignum_type y)
{
  bignum_length_type length = (BIGNUM_LENGTH (x));
  if (length != ((bignum_length_type) (BIGNUM_LENGTH (y))))
    return (0);
  else
    {
      bignum_digit_type * scan_x = (BIGNUM_START_PTR (x));
      bignum_digit_type * scan_y = (BIGNUM_START_PTR (y));
      bignum_digit_type * end_x = (scan_x + length);
      while (scan_x < end_x)
	if ((*scan_x++) != (*scan_y++))
	  return (0);
      return (1);
    }
}

static enum bignum_comparison
bignum_compare_unsigned (bignum_type x, bignum_type y)
{
  bignum_length_type x_length = (BIGNUM_LENGTH (x));
  bignum_length_type y_length = (BIGNUM_LENGTH (y));
  if (x_length < y_length)
    return (bignum_comparison_less);
  if (x_length > y_length)
    return (bignum_comparison_greater);
  {
    bignum_digit_type * start_x = (BIGNUM_START_PTR (x));
    bignum_digit_type * scan_x = (start_x + x_length);
    bignum_digit_type * scan_y = ((BIGNUM_START_PTR (y)) + y_length);
    while (start_x < scan_x)
      {
	bignum_digit_type digit_x = (*--scan_x);
	bignum_digit_type digit_y = (*--scan_y);
	if (digit_x < digit_y)
	  return (bignum_comparison_less);
	if (digit_x > digit_y)
	  return (bignum_comparison_greater);
      }
  }
  return (bignum_comparison_equal);
}

/* Addition */

static bignum_type
bignum_add_unsigned (bignum_type x, bignum_type y, int negative_p)
{
  if ((BIGNUM_LENGTH (y)) > (BIGNUM_LENGTH (x)))
    {
      bignum_type z = x;
      x = y;
      y = z;
    }
  {
    bignum_length_type x_length = (BIGNUM_LENGTH (x));
    bignum_type r = (bignum_allocate ((x_length + 1), negative_p));
    bignum_digit_type sum;
    bignum_digit_type carry = 0;
    bignum_digit_type * scan_x = (BIGNUM_START_PTR (x));
    bignum_digit_type * scan_r = (BIGNUM_START_PTR (r));
    {
      bignum_digit_type * scan_y = (BIGNUM_START_PTR (y));
      bignum_digit_type * end_y = (scan_y + (BIGNUM_LENGTH (y)));
      while (scan_y < end_y)
	{
	  sum = ((*scan_x++) + (*scan_y++) + carry);
	  if (sum < BIGNUM_RADIX)
	    {
	      (*scan_r++) = sum;
	      carry = 0;
	    }
	  else
	    {
	      (*scan_r++) = (sum - BIGNUM_RADIX);
	      carry = 1;
	    }
	}
    }
    {
      bignum_digit_type * end_x = ((BIGNUM_START_PTR (x)) + x_length);
      if (carry != 0)
	while (scan_x < end_x)
	  {
	    sum = ((*scan_x++) + 1);
	    if (sum < BIGNUM_RADIX)
	      {
		(*scan_r++) = sum;
		carry = 0;
		break;
	      }
	    else
	      (*scan_r++) = (sum - BIGNUM_RADIX);
	  }
      while (scan_x < end_x)
	(*scan_r++) = (*scan_x++);
    }
    if (carry != 0)
      {
	(*scan_r) = 1;
	return (r);
      }
    return (bignum_shorten_length (r, x_length));
  }
}

/* Subtraction */

static bignum_type
bignum_subtract_unsigned (bignum_type x, bignum_type y)
{
  int negative_p = 0;
  switch (bignum_compare_unsigned (x, y))
    {
    case bignum_comparison_equal:
      return (BIGNUM_ZERO ());
    case bignum_comparison_less:
      {
	bignum_type z = x;
	x = y;
	y = z;
      }
      negative_p = 1;
      break;
    case bignum_comparison_greater:
      negative_p = 0;
      break;
    }
  {
    bignum_length_type x_length = (BIGNUM_LENGTH (x));
    bignum_type r = (bignum_allocate (x_length, negative_p));
    bignum_digit_type difference;
    bignum_digit_type borrow = 0;
    bignum_digit_type * scan_x = (BIGNUM_START_PTR (x));
    bignum_digit_type * scan_r = (BIGNUM_START_PTR (r));
    {
      bignum_digit_type * scan_y = (BIGNUM_START_PTR (y));
      bignum_digit_type * end_y = (scan_y + (BIGNUM_LENGTH (y)));
      while (scan_y < end_y)
	{
	  difference = (((*scan_x++) - (*scan_y++)) - borrow);
	  if (difference < 0)
	    {
	      (*scan_r++) = (difference + BIGNUM_RADIX);
	      borrow = 1;
	    }
	  else
	    {
	      (*scan_r++) = difference;
	      borrow = 0;
	    }
	}
    }
    {
      bignum_digit_type * end_x = ((BIGNUM_START_PTR (x)) + x_length);
      if (borrow != 0)
	while (scan_x < end_x)
	  {
	    difference = ((*scan_x++) - borrow);
	    if (difference < 0)
	      (*scan_r++) = (difference + BIGNUM_RADIX);
	    else
	      {
		(*scan_r++) = difference;
		borrow = 0;
		break;
	      }
	  }
      BIGNUM_ASSERT (borrow == 0);
      while (scan_x < end_x)
	(*scan_r++) = (*scan_x++);
    }
    return (bignum_trim (r));
  }
}

/* Multiplication
   Maximum value for product_low or product_high:
	((R * R) + (R * (R - 2)) + (R - 1))
   Maximum value for carry: ((R * (R - 1)) + (R - 1))
	where R == BIGNUM_RADIX_ROOT */

static bignum_type
bignum_multiply_unsigned (bignum_type x, bignum_type y, int negative_p)
{
  if ((BIGNUM_LENGTH (y)) > (BIGNUM_LENGTH (x)))
    {
      bignum_type z = x;
      x = y;
      y = z;
    }
  {
    bignum_digit_type carry;
    bignum_digit_type y_digit_low;
    bignum_digit_type y_digit_high;
    bignum_digit_type x_digit_low;
    bignum_digit_type x_digit_high;
    bignum_digit_type product_low;
    bignum_digit_type * scan_r;
    bignum_digit_type * scan_y;
    bignum_length_type x_length = (BIGNUM_LENGTH (x));
    bignum_length_type y_length = (BIGNUM_LENGTH (y));
    bignum_type r =
      (bignum_allocate_zeroed ((x_length + y_length), negative_p));
    bignum_digit_type * scan_x = (BIGNUM_START_PTR (x));
    bignum_digit_type * end_x = (scan_x + x_length);
    bignum_digit_type * start_y = (BIGNUM_START_PTR (y));
    bignum_digit_type * end_y = (start_y + y_length);
    bignum_digit_type * start_r = (BIGNUM_START_PTR (r));
#define x_digit x_digit_high
#define y_digit y_digit_high
#define product_high carry
    while (scan_x < end_x)
      {
	x_digit = (*scan_x++);
	x_digit_low = (HD_LOW (x_digit));
	x_digit_high = (HD_HIGH (x_digit));
	carry = 0;
	scan_y = start_y;
	scan_r = (start_r++);
	while (scan_y < end_y)
	  {
	    y_digit = (*scan_y++);
	    y_digit_low = (HD_LOW (y_digit));
	    y_digit_high = (HD_HIGH (y_digit));
	    product_low =
	      ((*scan_r) +
	       (x_digit_low * y_digit_low) +
	       (HD_LOW (carry)));
	    product_high =
	      ((x_digit_high * y_digit_low) +
	       (x_digit_low * y_digit_high) +
	       (HD_HIGH (product_low)) +
	       (HD_HIGH (carry)));
	    (*scan_r++) =
	      (HD_CONS ((HD_LOW (product_high)), (HD_LOW (product_low))));
	    carry =
	      ((x_digit_high * y_digit_high) +
	       (HD_HIGH (product_high)));
	  }
	(*scan_r) += carry;
      }
    return (bignum_trim (r));
#undef x_digit
#undef y_digit
#undef product_high
  }
}

static bignum_type
bignum_multiply_unsigned_small_factor (bignum_type x, bignum_digit_type y,
				       int negative_p)
{
  bignum_length_type length_x = (BIGNUM_LENGTH (x));
  bignum_type p = (bignum_allocate ((length_x + 1), negative_p));
  bignum_destructive_copy (x, p);
  (BIGNUM_REF (p, length_x)) = 0;
  bignum_destructive_scale_up (p, y);
  return (bignum_trim (p));
}

static void
bignum_destructive_scale_up (bignum_type bignum, bignum_digit_type factor)
{
  bignum_digit_type carry = 0;
  bignum_digit_type * scan = (BIGNUM_START_PTR (bignum));
  bignum_digit_type two_digits;
  bignum_digit_type product_low;
#define product_high carry
  bignum_digit_type * end = (scan + (BIGNUM_LENGTH (bignum)));
  BIGNUM_ASSERT ((factor > 1) && (factor < BIGNUM_RADIX_ROOT));
  while (scan < end)
    {
      two_digits = (*scan);
      product_low = ((factor * (HD_LOW (two_digits))) + (HD_LOW (carry)));
      product_high =
	((factor * (HD_HIGH (two_digits))) +
	 (HD_HIGH (product_low)) +
	 (HD_HIGH (carry)));
      (*scan++) = (HD_CONS ((HD_LOW (product_high)), (HD_LOW (product_low))));
      carry = (HD_HIGH (product_high));
    }
  /* A carry here would be an overflow, i.e. it would not fit.
     Hopefully the callers allocate enough space that this will
     never happen.
   */
  BIGNUM_ASSERT (carry == 0);
  return;
#undef product_high
}

static void
bignum_destructive_add (bignum_type bignum, bignum_digit_type n)
{
  bignum_digit_type * scan = (BIGNUM_START_PTR (bignum));
  bignum_digit_type digit;
  digit = ((*scan) + n);
  if (digit < BIGNUM_RADIX)
    {
      (*scan) = digit;
      return;
    }
  (*scan++) = (digit - BIGNUM_RADIX);
  while (1)
    {
      digit = ((*scan) + 1);
      if (digit < BIGNUM_RADIX)
	{
	  (*scan) = digit;
	  return;
	}
      (*scan++) = (digit - BIGNUM_RADIX);
    }
}

/* Division */

/* For help understanding this algorithm, see:
   Knuth, Donald E., "The Art of Computer Programming",
   volume 2, "Seminumerical Algorithms"
   section 4.3.1, "Multiple-Precision Arithmetic". */

static void
bignum_divide_unsigned_large_denominator (bignum_type numerator,
					  bignum_type denominator,
					  bignum_type * quotient,
					  bignum_type * remainder,
					  int q_negative_p,
					  int r_negative_p)
{
  bignum_length_type length_n = ((BIGNUM_LENGTH (numerator)) + 1);
  bignum_length_type length_d = (BIGNUM_LENGTH (denominator));
  bignum_type q =
    ((quotient != ((bignum_type *) 0))
     ? (bignum_allocate ((length_n - length_d), q_negative_p))
     : BIGNUM_OUT_OF_BAND);
  bignum_type u = (bignum_allocate (length_n, r_negative_p));
  int shift = 0;
  BIGNUM_ASSERT (length_d > 1);
  {
    bignum_digit_type v1 = (BIGNUM_REF ((denominator), (length_d - 1)));
    while (v1 < (BIGNUM_RADIX / 2))
      {
	v1 <<= 1;
	shift += 1;
      }
  }
  if (shift == 0)
    {
      bignum_destructive_copy (numerator, u);
      (BIGNUM_REF (u, (length_n - 1))) = 0;
      bignum_divide_unsigned_normalized (u, denominator, q);
    }
  else
    {
      bignum_type v = (bignum_allocate (length_d, 0));
      bignum_destructive_normalization (numerator, u, shift);
      bignum_destructive_normalization (denominator, v, shift);
      bignum_divide_unsigned_normalized (u, v, q);
      BIGNUM_DEALLOCATE (v);
      if (remainder != ((bignum_type *) 0))
	bignum_destructive_unnormalization (u, shift);
    }
  if (quotient != ((bignum_type *) 0))
    (*quotient) = (bignum_trim (q));
  if (remainder != ((bignum_type *) 0))
    (*remainder) = (bignum_trim (u));
  else
    BIGNUM_DEALLOCATE (u);
  return;
}

static void
bignum_divide_unsigned_normalized (bignum_type u, bignum_type v, bignum_type q)
{
  bignum_length_type u_length = (BIGNUM_LENGTH (u));
  bignum_length_type v_length = (BIGNUM_LENGTH (v));
  bignum_digit_type * u_start = (BIGNUM_START_PTR (u));
  bignum_digit_type * u_scan = (u_start + u_length);
  bignum_digit_type * u_scan_limit = (u_start + v_length);
  bignum_digit_type * u_scan_start = (u_scan - v_length);
  bignum_digit_type * v_start = (BIGNUM_START_PTR (v));
  bignum_digit_type * v_end = (v_start + v_length);
  bignum_digit_type * q_scan = 0;
  bignum_digit_type v1 = (v_end[-1]);
  bignum_digit_type v2 = (v_end[-2]);
  bignum_digit_type ph;	/* high half of double-digit product */
  bignum_digit_type pl;	/* low half of double-digit product */
  bignum_digit_type guess;
  bignum_digit_type gh;	/* high half-digit of guess */
  bignum_digit_type ch;	/* high half of double-digit comparand */
  bignum_digit_type v2l = (HD_LOW (v2));
  bignum_digit_type v2h = (HD_HIGH (v2));
  bignum_digit_type cl;	/* low half of double-digit comparand */
#define gl ph			/* low half-digit of guess */
#define uj pl
#define qj ph
  bignum_digit_type gm;		/* memory loc for reference parameter */
  if (q != BIGNUM_OUT_OF_BAND)
    q_scan = ((BIGNUM_START_PTR (q)) + (BIGNUM_LENGTH (q)));
  while (u_scan_limit < u_scan)
    {
      uj = (*--u_scan);
      if (uj != v1)
	{
	  /* comparand =
	     (((((uj * BIGNUM_RADIX) + uj1) % v1) * BIGNUM_RADIX) + uj2);
	     guess = (((uj * BIGNUM_RADIX) + uj1) / v1); */
	  cl = (u_scan[-2]);
	  ch = (bignum_digit_divide (uj, (u_scan[-1]), v1, (&gm)));
	  guess = gm;
	}
      else
	{
	  cl = (u_scan[-2]);
	  ch = ((u_scan[-1]) + v1);
	  guess = (BIGNUM_RADIX - 1);
	}
      while (1)
	{
	  /* product = (guess * v2); */
	  gl = (HD_LOW (guess));
	  gh = (HD_HIGH (guess));
	  pl = (v2l * gl);
	  ph = ((v2l * gh) + (v2h * gl) + (HD_HIGH (pl)));
	  pl = (HD_CONS ((HD_LOW (ph)), (HD_LOW (pl))));
	  ph = ((v2h * gh) + (HD_HIGH (ph)));
	  /* if (comparand >= product) */
	  if ((ch > ph) || ((ch == ph) && (cl >= pl)))
	    break;
	  guess -= 1;
	  /* comparand += (v1 << BIGNUM_DIGIT_LENGTH) */
	  ch += v1;
	  /* if (comparand >= (BIGNUM_RADIX * BIGNUM_RADIX)) */
	  if (ch >= BIGNUM_RADIX)
	    break;
	}
      qj = (bignum_divide_subtract (v_start, v_end, guess, (--u_scan_start)));
      if (q != BIGNUM_OUT_OF_BAND)
	(*--q_scan) = qj;
    }
  return;
#undef gl
#undef uj
#undef qj
}

static bignum_digit_type
bignum_divide_subtract (bignum_digit_type * v_start,
			bignum_digit_type * v_end,
			bignum_digit_type guess,
			bignum_digit_type * u_start)
{
  bignum_digit_type * v_scan = v_start;
  bignum_digit_type * u_scan = u_start;
  bignum_digit_type carry = 0;
  if (guess == 0) return (0);
  {
    bignum_digit_type gl = (HD_LOW (guess));
    bignum_digit_type gh = (HD_HIGH (guess));
    bignum_digit_type v;
    bignum_digit_type pl;
    bignum_digit_type vl;
#define vh v
#define ph carry
#define diff pl
    while (v_scan < v_end)
      {
	v = (*v_scan++);
	vl = (HD_LOW (v));
	vh = (HD_HIGH (v));
	pl = ((vl * gl) + (HD_LOW (carry)));
	ph = ((vl * gh) + (vh * gl) + (HD_HIGH (pl)) + (HD_HIGH (carry)));
	diff = ((*u_scan) - (HD_CONS ((HD_LOW (ph)), (HD_LOW (pl)))));
	if (diff < 0)
	  {
	    (*u_scan++) = (diff + BIGNUM_RADIX);
	    carry = ((vh * gh) + (HD_HIGH (ph)) + 1);
	  }
	else
	  {
	    (*u_scan++) = diff;
	    carry = ((vh * gh) + (HD_HIGH (ph)));
	  }
      }
    if (carry == 0)
      return (guess);
    diff = ((*u_scan) - carry);
    if (diff < 0)
      (*u_scan) = (diff + BIGNUM_RADIX);
    else
      {
	(*u_scan) = diff;
	return (guess);
      }
#undef vh
#undef ph
#undef diff
  }
  /* Subtraction generated carry, implying guess is one too large.
     Add v back in to bring it back down. */
  v_scan = v_start;
  u_scan = u_start;
  carry = 0;
  while (v_scan < v_end)
    {
      bignum_digit_type sum = ((*v_scan++) + (*u_scan) + carry);
      if (sum < BIGNUM_RADIX)
	{
	  (*u_scan++) = sum;
	  carry = 0;
	}
      else
	{
	  (*u_scan++) = (sum - BIGNUM_RADIX);
	  carry = 1;
	}
    }
  if (carry == 1)
    {
      bignum_digit_type sum = ((*u_scan) + carry);
      (*u_scan) = ((sum < BIGNUM_RADIX) ? sum : (sum - BIGNUM_RADIX));
    }
  return (guess - 1);
}

static void
bignum_divide_unsigned_medium_denominator (bignum_type numerator,
					   bignum_digit_type denominator,
					   bignum_type * quotient,
					   bignum_type * remainder,
					   int q_negative_p,
					   int r_negative_p)
{
  bignum_length_type length_n = (BIGNUM_LENGTH (numerator));
  bignum_length_type length_q;
  bignum_type q;
  int shift = 0;
  /* Because `bignum_digit_divide' requires a normalized denominator. */
  while (denominator < (BIGNUM_RADIX / 2))
    {
      denominator <<= 1;
      shift += 1;
    }
  if (shift == 0)
    {
      length_q = length_n;
      q = (bignum_allocate (length_q, q_negative_p));
      bignum_destructive_copy (numerator, q);
    }
  else
    {
      length_q = (length_n + 1);
      q = (bignum_allocate (length_q, q_negative_p));
      bignum_destructive_normalization (numerator, q, shift);
    }
  {
    bignum_digit_type r = 0;
    bignum_digit_type * start = (BIGNUM_START_PTR (q));
    bignum_digit_type * scan = (start + length_q);
    bignum_digit_type qj;
    if (quotient != ((bignum_type *) 0))
      {
	while (start < scan)
	  {
	    r = (bignum_digit_divide (r, (*--scan), denominator, (&qj)));
	    (*scan) = qj;
	  }
	(*quotient) = (bignum_trim (q));
      }
    else
      {
	while (start < scan)
	  r = (bignum_digit_divide (r, (*--scan), denominator, (&qj)));
	BIGNUM_DEALLOCATE (q);
      }
    if (remainder != ((bignum_type *) 0))
      {
	if (shift != 0)
	  r >>= shift;
	(*remainder) = (bignum_digit_to_bignum (r, r_negative_p));
      }
  }
  return;
}

static void
bignum_destructive_normalization (bignum_type source, bignum_type target,
				  int shift_left)
{
  bignum_digit_type digit;
  bignum_digit_type * scan_source = (BIGNUM_START_PTR (source));
  bignum_digit_type carry = 0;
  bignum_digit_type * scan_target = (BIGNUM_START_PTR (target));
  bignum_digit_type * end_source = (scan_source + (BIGNUM_LENGTH (source)));
  bignum_digit_type * end_target = (scan_target + (BIGNUM_LENGTH (target)));
  int shift_right = (BIGNUM_DIGIT_LENGTH - shift_left);
  bignum_digit_type mask = ((1UL << shift_right) - 1UL);
  while (scan_source < end_source)
    {
      digit = (*scan_source++);
      (*scan_target++) = (((digit & mask) << shift_left) | carry);
      carry = (digit >> shift_right);
    }
  if (scan_target < end_target)
    (*scan_target) = carry;
  else
    BIGNUM_ASSERT (carry == 0);
  return;
}

static void
bignum_destructive_unnormalization (bignum_type bignum, int shift_right)
{
  bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
  bignum_digit_type * scan = (start + (BIGNUM_LENGTH (bignum)));
  bignum_digit_type digit;
  bignum_digit_type carry = 0;
  int shift_left = (BIGNUM_DIGIT_LENGTH - shift_right);
  bignum_digit_type mask = ((1UL << shift_right) - 1UL);
  while (start < scan)
    {
      digit = (*--scan);
      (*scan) = ((digit >> shift_right) | carry);
      carry = ((digit & mask) << shift_left);
    }
  BIGNUM_ASSERT (carry == 0);
}

/* This is a reduced version of the division algorithm, applied to the
   case of dividing two bignum digits by one bignum digit.  It is
   assumed that the numerator and denominator are normalized. */

#define BDD_STEP(qn, j)							\
{									\
  uj = (u[j]);								\
  if (uj != v1)								\
    {									\
      uj_uj1 = (HD_CONS (uj, (u[j + 1])));				\
      guess = (uj_uj1 / v1);						\
      comparand = (HD_CONS ((uj_uj1 % v1), (u[j + 2])));		\
    }									\
  else									\
    {									\
      guess = (BIGNUM_RADIX_ROOT - 1);					\
      comparand = (HD_CONS (((u[j + 1]) + v1), (u[j + 2])));		\
    }									\
  while ((guess * v2) > comparand)					\
    {									\
      guess -= 1;							\
      comparand += (v1 << BIGNUM_HALF_DIGIT_LENGTH);			\
      if (comparand >= BIGNUM_RADIX)					\
	break;								\
    }									\
  qn = (bignum_digit_divide_subtract (v1, v2, guess, (&u[j])));		\
}

static bignum_digit_type
bignum_digit_divide (bignum_digit_type uh, bignum_digit_type ul,
		     bignum_digit_type v, bignum_digit_type * q)
{
  bignum_digit_type guess;
  bignum_digit_type comparand;
  bignum_digit_type v1 = (HD_HIGH (v));
  bignum_digit_type v2 = (HD_LOW (v));
  bignum_digit_type uj;
  bignum_digit_type uj_uj1;
  bignum_digit_type q1;
  bignum_digit_type q2;
  bignum_digit_type u [4];
  if (uh == 0)
    {
      if (ul < v)
	{
	  (*q) = 0;
	  return (ul);
	}
      else if (ul == v)
	{
	  (*q) = 1;
	  return (0);
	}
    }
  (u[0]) = (HD_HIGH (uh));
  (u[1]) = (HD_LOW (uh));
  (u[2]) = (HD_HIGH (ul));
  (u[3]) = (HD_LOW (ul));
  v1 = (HD_HIGH (v));
  v2 = (HD_LOW (v));
  BDD_STEP (q1, 0);
  BDD_STEP (q2, 1);
  (*q) = (HD_CONS (q1, q2));
  return (HD_CONS ((u[2]), (u[3])));
}

#undef BDD_STEP

#define BDDS_MULSUB(vn, un, carry_in)					\
{									\
  product = ((vn * guess) + carry_in);					\
  diff = (un - (HD_LOW (product)));					\
  if (diff < 0)								\
    {									\
      un = (diff + BIGNUM_RADIX_ROOT);					\
      carry = ((HD_HIGH (product)) + 1);				\
    }									\
  else									\
    {									\
      un = diff;							\
      carry = (HD_HIGH (product));					\
    }									\
}

#define BDDS_ADD(vn, un, carry_in)					\
{									\
  sum = (vn + un + carry_in);						\
  if (sum < BIGNUM_RADIX_ROOT)						\
    {									\
      un = sum;								\
      carry = 0;							\
    }									\
  else									\
    {									\
      un = (sum - BIGNUM_RADIX_ROOT);					\
      carry = 1;							\
    }									\
}

static bignum_digit_type
bignum_digit_divide_subtract (bignum_digit_type v1, bignum_digit_type v2,
			      bignum_digit_type guess, bignum_digit_type * u)
{
  {
    bignum_digit_type product;
    bignum_digit_type diff;
    bignum_digit_type carry;
    BDDS_MULSUB (v2, (u[2]), 0);
    BDDS_MULSUB (v1, (u[1]), carry);
    if (carry == 0)
      return (guess);
    diff = ((u[0]) - carry);
    if (diff < 0)
      (u[0]) = (diff + BIGNUM_RADIX);
    else
      {
	(u[0]) = diff;
	return (guess);
      }
  }
  {
    bignum_digit_type sum;
    bignum_digit_type carry;
    BDDS_ADD(v2, (u[2]), 0);
    BDDS_ADD(v1, (u[1]), carry);
    if (carry == 1)
      (u[0]) += 1;
  }
  return (guess - 1);
}

#undef BDDS_MULSUB
#undef BDDS_ADD

static void
bignum_divide_unsigned_small_denominator (bignum_type numerator,
       bignum_digit_type denominator,
       bignum_type * quotient,
       bignum_type * remainder,
       int q_negative_p,
       int r_negative_p)
{
  bignum_type q = (bignum_new_sign (numerator, q_negative_p));
  bignum_digit_type r = (bignum_destructive_scale_down (q, denominator));
  (*quotient) = (bignum_trim (q));
  if (remainder != ((bignum_type *) 0))
    (*remainder) = (bignum_digit_to_bignum (r, r_negative_p));
  return;
}

/* Given (denominator > 1), it is fairly easy to show that
   (quotient_high < BIGNUM_RADIX_ROOT), after which it is easy to see
   that all digits are < BIGNUM_RADIX. */

static bignum_digit_type
bignum_destructive_scale_down (bignum_type bignum,
			       bignum_digit_type denominator)
{
  bignum_digit_type numerator;
  bignum_digit_type remainder = 0;
  bignum_digit_type two_digits;
#define quotient_high remainder
  bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
  bignum_digit_type * scan = (start + (BIGNUM_LENGTH (bignum)));
  BIGNUM_ASSERT ((denominator > 1) && (denominator < BIGNUM_RADIX_ROOT));
  while (start < scan)
    {
      two_digits = (*--scan);
      numerator = (HD_CONS (remainder, (HD_HIGH (two_digits))));
      quotient_high = (numerator / denominator);
      numerator = (HD_CONS ((numerator % denominator), (HD_LOW (two_digits))));
      (*scan) = (HD_CONS (quotient_high, (numerator / denominator)));
      remainder = (numerator % denominator);
    }
  return (remainder);
#undef quotient_high
}

static bignum_type
bignum_remainder_unsigned_small_denominator (bignum_type n,
					     bignum_digit_type d,
					     int negative_p)
{
  bignum_digit_type two_digits;
  bignum_digit_type * start = (BIGNUM_START_PTR (n));
  bignum_digit_type * scan = (start + (BIGNUM_LENGTH (n)));
  bignum_digit_type r = 0;
  BIGNUM_ASSERT ((d > 1) && (d < BIGNUM_RADIX_ROOT));
  while (start < scan)
    {
      two_digits = (*--scan);
      r =
	((HD_CONS (((HD_CONS (r, (HD_HIGH (two_digits)))) % d),
		   (HD_LOW (two_digits))))
	 % d);
    }
  return (bignum_digit_to_bignum (r, negative_p));
}

static bignum_type
bignum_digit_to_bignum (bignum_digit_type digit, int negative_p)
{
  if (digit == 0)
    return (BIGNUM_ZERO ());
  else
    {
      bignum_type result = (bignum_allocate (1, negative_p));
      (BIGNUM_REF (result, 0)) = digit;
      return (result);
    }
}

/* Allocation */

static bignum_type
bignum_allocate (bignum_length_type length, int negative_p)
{
  BIGNUM_ASSERT ((length >= 0) || (length < BIGNUM_RADIX));
  {
    bignum_type result = (BIGNUM_ALLOCATE (length));
    BIGNUM_SET_HEADER (result, length, negative_p);
    return (result);
  }
}

static bignum_type
bignum_allocate_zeroed (bignum_length_type length, int negative_p)
{
  BIGNUM_ASSERT ((length >= 0) || (length < BIGNUM_RADIX));
  {
    bignum_type result = (BIGNUM_ALLOCATE (length));
    bignum_digit_type * scan = (BIGNUM_START_PTR (result));
    bignum_digit_type * end = (scan + length);
    BIGNUM_SET_HEADER (result, length, negative_p);
    while (scan < end)
      (*scan++) = 0;
    return (result);
  }
}

static bignum_type
bignum_shorten_length (bignum_type bignum, bignum_length_type length)
{
  bignum_length_type current_length = (BIGNUM_LENGTH (bignum));
  BIGNUM_ASSERT ((length >= 0) || (length <= current_length));
  if (length < current_length)
    {
      BIGNUM_SET_HEADER
	(bignum, length, ((length != 0) && (BIGNUM_NEGATIVE_P (bignum))));
      BIGNUM_REDUCE_LENGTH (bignum, bignum, length);
    }
  return (bignum);
}

static bignum_type
bignum_trim (bignum_type bignum)
{
  bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
  bignum_digit_type * end = (start + (BIGNUM_LENGTH (bignum)));
  bignum_digit_type * scan = end;
  while ((start <= scan) && ((*--scan) == 0))
    ;
  scan += 1;
  if (scan < end)
    {
      bignum_length_type length = (scan - start);
      BIGNUM_SET_HEADER
	(bignum, length, ((length != 0) && (BIGNUM_NEGATIVE_P (bignum))));
      BIGNUM_REDUCE_LENGTH (bignum, bignum, length);
    }
  return (bignum);
}

/* Copying */

static bignum_type
bignum_copy (bignum_type source)
{
  bignum_type target =
    (bignum_allocate ((BIGNUM_LENGTH (source)), (BIGNUM_NEGATIVE_P (source))));
  bignum_destructive_copy (source, target);
  return (target);
}

static bignum_type
bignum_new_sign (bignum_type bignum, int negative_p)
{
  bignum_type result =
    (bignum_allocate ((BIGNUM_LENGTH (bignum)), negative_p));
  bignum_destructive_copy (bignum, result);
  return (result);
}

static bignum_type
bignum_maybe_new_sign (bignum_type bignum, int negative_p)
{
#ifndef BIGNUM_FORCE_NEW_RESULTS
  if ((BIGNUM_NEGATIVE_P (bignum)) ? negative_p : (! negative_p))
    return (bignum);
  else
#endif
    {
      bignum_type result =
	(bignum_allocate ((BIGNUM_LENGTH (bignum)), negative_p));
      bignum_destructive_copy (bignum, result);
      return (result);
    }
}

static void
bignum_destructive_copy (bignum_type source, bignum_type target)
{
  bignum_digit_type * scan_source = (BIGNUM_START_PTR (source));
  bignum_digit_type * end_source =
    (scan_source + (BIGNUM_LENGTH (source)));
  bignum_digit_type * scan_target = (BIGNUM_START_PTR (target));
  while (scan_source < end_source)
    (*scan_target++) = (*scan_source++);
  return;
}
