/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/bignum.c,v 9.28 1989/09/20 23:05:57 cph Exp $

Copyright (c) 1989 Massachusetts Institute of Technology

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

/* Implementation of Bignums (unlimited precision integers) */

#ifdef MIT_SCHEME
#include "scheme.h"
#else
#include "bignum.h"
#endif
#include "bignumint.h"
#include "limits.h"

#ifndef MIT_SCHEME

static bignum_type
bignum_malloc (length)
      bignum_length_type length;
{
  extern char * malloc ();
  char * result = (malloc (length * (sizeof (bignum_digit_type))));
  BIGNUM_ASSERT (result != ((char *) 0));
  return ((bignum_type) result);
}

static bignum_type
bignum_realloc (bignum, length)
      bignum_type bignum;
      bignum_length_type length;
{
  extern char * realloc ();
  char * result =
    (realloc (((char *) bignum), (length * (sizeof (bignum_digit_type)))));
  BIGNUM_ASSERT (result != ((char *) 0));
  return ((bignum_type) result);
}

#endif /* not MIT_SCHEME */

/* Forward references */
static int bignum_equal_p_unsigned ();
static enum bignum_comparison bignum_compare_unsigned ();
static bignum_type bignum_add_unsigned ();
static bignum_type bignum_subtract_unsigned ();
static bignum_type bignum_multiply_unsigned ();
static bignum_type bignum_multiply_unsigned_small_factor ();
static void bignum_destructive_scale_up ();
static void bignum_destructive_add ();
static void bignum_divide_unsigned_large_denominator ();
static int bignum_compute_normalization_shift ();
static void bignum_destructive_normalization ();
static void bignum_destructive_unnormalization ();
static void bignum_divide_unsigned_normalized ();
static bignum_digit_type bignum_divide_subtract ();
static void bignum_divide_unsigned_medium_denominator ();
static bignum_digit_type bignum_digit_divide ();
static bignum_digit_type bignum_digit_divide_subtract ();
static void bignum_divide_unsigned_small_denominator ();
static bignum_digit_type bignum_destructive_scale_down ();
static bignum_type bignum_remainder_unsigned_small_denominator ();
static bignum_type bignum_digit_to_bignum ();
static bignum_type bignum_allocate ();
static bignum_type bignum_allocate_zeroed ();
static bignum_type bignum_shorten_length ();
static bignum_type bignum_trim ();
static bignum_type bignum_copy ();
static bignum_type bignum_new_sign ();
static bignum_type bignum_maybe_new_sign ();
static void bignum_destructive_copy ();
static void bignum_destructive_zero ();

/* Exports */

bignum_type
bignum_make_zero ()
{
  fast bignum_type result = (BIGNUM_ALLOCATE (0));
  BIGNUM_SET_HEADER (result, 0, 0);
  return (result);
}

bignum_type
bignum_make_one (negative_p)
     int negative_p;
{
  fast bignum_type result = (BIGNUM_ALLOCATE (1));
  BIGNUM_SET_HEADER (result, 1, negative_p);
  (BIGNUM_REF (result, 0)) = 1;
  return (result);
}

int
bignum_equal_p (x, y)
     fast bignum_type x;
     fast bignum_type y;
{
  return
    ((BIGNUM_ZERO_P (x))
     ? (BIGNUM_ZERO_P (y))
     : ((! (BIGNUM_ZERO_P (y))) &&
	((BIGNUM_NEGATIVE_P (x))
	 ? (BIGNUM_NEGATIVE_P (x))
	 : (! (BIGNUM_NEGATIVE_P (x)))) &&
	(bignum_equal_p_unsigned (x, y))));
}

enum bignum_comparison
bignum_test (bignum)
     fast bignum_type bignum;
{
  return
    ((BIGNUM_ZERO_P (bignum))
     ? bignum_comparison_equal
     : (BIGNUM_NEGATIVE_P (bignum))
     ? bignum_comparison_less
     : bignum_comparison_greater);
}

enum bignum_comparison
bignum_compare (x, y)
     fast bignum_type x;
     fast bignum_type y;
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
bignum_add (x, y)
     fast bignum_type x;
     fast bignum_type y;
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
bignum_subtract (x, y)
     fast bignum_type x;
     fast bignum_type y;
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
bignum_negate (x)
     fast bignum_type x;
{
  return
    ((BIGNUM_ZERO_P (x))
     ? (BIGNUM_MAYBE_COPY (x))
     : (bignum_new_sign (x, (! (BIGNUM_NEGATIVE_P (x))))));
}

bignum_type
bignum_multiply (x, y)
     fast bignum_type x;
     fast bignum_type y;
{
  fast bignum_length_type x_length = (BIGNUM_LENGTH (x));
  fast bignum_length_type y_length = (BIGNUM_LENGTH (y));
  fast int negative_p =
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
bignum_divide (numerator, denominator, quotient, remainder)
     bignum_type numerator;
     bignum_type denominator;
     bignum_type * quotient;
     bignum_type * remainder;
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
bignum_quotient (numerator, denominator)
     bignum_type numerator;
     bignum_type denominator;
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
      }
  }
}

bignum_type
bignum_remainder (numerator, denominator)
     bignum_type numerator;
     bignum_type denominator;
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
	    if (digit == 1)
	      return (BIGNUM_ZERO ());
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
    }
}

/* These procedures depend on the non-portable type `unsigned long'.
   If your compiler doesn't support this type, either define the
   switch `BIGNUM_NO_ULONG' to disable them (in "bignum.h"), or write
   new versions that don't use this type. */

#ifndef BIGNUM_NO_ULONG

bignum_type
long_to_bignum (n)
     long n;
{
  int negative_p;
  bignum_digit_type result_digits [BIGNUM_DIGITS_FOR_LONG];
  fast bignum_digit_type * end_digits = result_digits;
  /* Special cases win when these small constants are cached. */
  if (n == 0) return (BIGNUM_ZERO ());
  if (n == 1) return (BIGNUM_ONE (0));
  if (n == -1) return (BIGNUM_ONE (1));
  {
    fast unsigned long accumulator = ((negative_p = (n < 0)) ? (-n) : n);
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
    fast bignum_digit_type * scan_digits = result_digits;
    fast bignum_digit_type * scan_result = (BIGNUM_START_PTR (result));
    while (scan_digits < end_digits)
      (*scan_result++) = (*scan_digits++);
    return (result);
  }
}

long
bignum_to_long (bignum)
     bignum_type bignum;
{
  if (BIGNUM_ZERO_P (bignum))
    return (0);
  {
    fast unsigned long accumulator = 0;
    fast bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
    fast bignum_digit_type * scan = (start + (BIGNUM_LENGTH (bignum)));
    while (start < scan)
      accumulator = ((accumulator << BIGNUM_DIGIT_LENGTH) + (*--scan));
    return ((BIGNUM_NEGATIVE_P (bignum)) ? (-accumulator) : accumulator);
  }
}

#endif /* not BIGNUM_NO_ULONG */

#define DTB_WRITE_DIGIT(factor)						\
{									\
  significand *= (factor);						\
  digit = ((bignum_digit_type) significand);				\
  (*--scan) = digit;							\
  significand -= ((double) digit);					\
}

bignum_type
double_to_bignum (x)
     double x;
{
  extern double frexp ();
  int exponent;
  fast double significand = (frexp (x, (&exponent)));
  if (exponent <= 0) return (BIGNUM_ZERO ());
  if (exponent == 1) return (BIGNUM_ONE (x < 0));
  if (significand < 0) significand = (-significand);
  {
    bignum_length_type length = (BIGNUM_BITS_TO_DIGITS (exponent));
    bignum_type result = (bignum_allocate (length, (x < 0)));
    bignum_digit_type * start = (BIGNUM_START_PTR (result));
    fast bignum_digit_type * scan = (start + length);
    fast bignum_digit_type digit;
    int odd_bits = (exponent % BIGNUM_DIGIT_LENGTH);
    if (odd_bits > 0)
      DTB_WRITE_DIGIT (1 << odd_bits);
    while (start < scan)
      {
	if (significand == 0)
	  {
	    while (start < scan)
	      (*--scan) = 0;
	    break;
	  }
	DTB_WRITE_DIGIT (BIGNUM_RADIX);
      }
    return (result);
  }
}

#undef DTB_WRITE_DIGIT

double
bignum_to_double (bignum)
     bignum_type bignum;
{
  if (BIGNUM_ZERO_P (bignum))
    return (0);
  {
    fast double accumulator = 0;
    fast bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
    fast bignum_digit_type * scan = (start + (BIGNUM_LENGTH (bignum)));
    while (start < scan)
      accumulator = ((accumulator * BIGNUM_RADIX) + (*--scan));
    return ((BIGNUM_NEGATIVE_P (bignum)) ? (-accumulator) : accumulator);
  }
}

int
bignum_fits_in_word_p (bignum, word_length, twos_complement_p)
     bignum_type bignum;
     long word_length;
     int twos_complement_p;
{
  unsigned int n_bits = (twos_complement_p ? (word_length - 1) : word_length);
  BIGNUM_ASSERT (n_bits > 0);
  {
    fast bignum_length_type length = (BIGNUM_LENGTH (bignum));
    fast unsigned int max_digits = (BIGNUM_BITS_TO_DIGITS (n_bits));
    bignum_digit_type msd, max;
    return
      ((length < max_digits) ||
       ((length == max_digits) &&
	((((msd = (BIGNUM_REF (bignum, (length - 1)))) <
	   (max = (1 << (n_bits - ((length - 1) * BIGNUM_DIGIT_LENGTH))))) ||
	  (twos_complement_p &&
	   (msd == max) &&
	   (BIGNUM_NEGATIVE_P (bignum)))))));
  }
}

bignum_type
bignum_length_in_bits (bignum)
     bignum_type bignum;
{
  if (BIGNUM_ZERO_P (bignum))
    return (BIGNUM_ZERO ());
  {
    bignum_length_type index = ((BIGNUM_LENGTH (bignum)) - 1);
    fast bignum_digit_type digit = (BIGNUM_REF (bignum, index));
    fast bignum_type result = (bignum_allocate (2, 0));
    (BIGNUM_REF (result, 0)) = index;
    (BIGNUM_REF (result, 1)) = 0;
    bignum_destructive_scale_up (result, BIGNUM_DIGIT_LENGTH);
    while (digit > 0)
      {
	bignum_destructive_add (result, ((bignum_digit_type) 1));
	digit >>= 1;
      }
    return (bignum_trim (result));
  }
}

bignum_type
bignum_length_upper_limit ()
{
  fast bignum_type result = (bignum_allocate (2, 0));
  (BIGNUM_REF (result, 0)) = 0;
  (BIGNUM_REF (result, 1)) = BIGNUM_DIGIT_LENGTH;
  return (result);
}

bignum_type
digit_stream_to_bignum (n_digits, producer, context, radix, negative_p)
     fast unsigned int n_digits;
     unsigned int (*producer) ();
     bignum_procedure_context context;
     fast unsigned int radix;
     int negative_p;
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
      fast unsigned int radix_copy = radix;
      fast unsigned int log_radix = 0;
      while (radix_copy > 0)
	{
	  radix_copy >>= 1;
	  log_radix += 1;
	}
      /* This length will be at least as large as needed. */
      length = (BIGNUM_BITS_TO_DIGITS (n_digits * log_radix));
    }
    {
      fast bignum_type result = (bignum_allocate_zeroed (length, negative_p));
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
bignum_to_digit_stream (bignum, radix, consumer, context)
     bignum_type bignum;
     unsigned int radix;
     void (*consumer) ();
     bignum_procedure_context context;
{
  BIGNUM_ASSERT ((radix > 1) && (radix <= BIGNUM_RADIX_ROOT));
  if (! (BIGNUM_ZERO_P (bignum)))
    {
      fast bignum_type working_copy = (bignum_copy (bignum));
      fast bignum_digit_type * start = (BIGNUM_START_PTR (working_copy));
      fast bignum_digit_type * scan = (start + (BIGNUM_LENGTH (working_copy)));
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
bignum_max_digit_stream_radix ()
{
  return (BIGNUM_RADIX_ROOT);
}

/* Comparisons */

static int
bignum_equal_p_unsigned (x, y)
     bignum_type x;
     bignum_type y;
{
  bignum_length_type length = (BIGNUM_LENGTH (x));
  if (length != (BIGNUM_LENGTH (y)))
    return (0);
  else
    {
      fast bignum_digit_type * scan_x = (BIGNUM_START_PTR (x));
      fast bignum_digit_type * scan_y = (BIGNUM_START_PTR (y));
      fast bignum_digit_type * end_x = (scan_x + length);
      while (scan_x < end_x)
	if ((*scan_x++) != (*scan_y++))
	  return (0);
      return (1);
    }
}

static enum bignum_comparison
bignum_compare_unsigned (x, y)
     bignum_type x;
     bignum_type y;
{
  bignum_length_type x_length = (BIGNUM_LENGTH (x));
  bignum_length_type y_length = (BIGNUM_LENGTH (y));
  if (x_length < y_length)
    return (bignum_comparison_less);
  if (x_length > y_length)
    return (bignum_comparison_greater);
  {
    fast bignum_digit_type * start_x = (BIGNUM_START_PTR (x));
    fast bignum_digit_type * scan_x = (start_x + x_length);
    fast bignum_digit_type * scan_y = ((BIGNUM_START_PTR (y)) + y_length);
    while (start_x < scan_x)
      {
	fast bignum_digit_type digit_x = (*--scan_x);
	fast bignum_digit_type digit_y = (*--scan_y);
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
bignum_add_unsigned (x, y, negative_p)
     bignum_type x;
     bignum_type y;
     int negative_p;
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
    fast bignum_digit_type sum;
    fast bignum_digit_type carry = 0;
    fast bignum_digit_type * scan_x = (BIGNUM_START_PTR (x));
    fast bignum_digit_type * scan_r = (BIGNUM_START_PTR (r));
    {
      fast bignum_digit_type * scan_y = (BIGNUM_START_PTR (y));
      fast bignum_digit_type * end_y = (scan_y + (BIGNUM_LENGTH (y)));
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
      fast bignum_digit_type * end_x = ((BIGNUM_START_PTR (x)) + x_length);
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
bignum_subtract_unsigned (x, y)
     bignum_type x;
     bignum_type y;
{
  int negative_p;
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
    fast bignum_digit_type difference;
    fast bignum_digit_type borrow = 0;
    fast bignum_digit_type * scan_x = (BIGNUM_START_PTR (x));
    fast bignum_digit_type * scan_r = (BIGNUM_START_PTR (r));
    {
      fast bignum_digit_type * scan_y = (BIGNUM_START_PTR (y));
      fast bignum_digit_type * end_y = (scan_y + (BIGNUM_LENGTH (y)));
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
      fast bignum_digit_type * end_x = (scan_x + x_length);
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
bignum_multiply_unsigned (x, y, negative_p)
     bignum_type x;
     bignum_type y;
     int negative_p;
{
  if ((BIGNUM_LENGTH (y)) > (BIGNUM_LENGTH (x)))
    {
      bignum_type z = x;
      x = y;
      y = z;
    }
  {
    fast bignum_digit_type carry;
    fast bignum_digit_type y_digit_low;
    fast bignum_digit_type y_digit_high;
    fast bignum_digit_type x_digit_low;
    fast bignum_digit_type x_digit_high;
    bignum_digit_type product_low;
    fast bignum_digit_type * scan_r;
    fast bignum_digit_type * scan_y;
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
bignum_multiply_unsigned_small_factor (x, y, negative_p)
     bignum_type x;
     bignum_digit_type y;
     int negative_p;
{
  bignum_length_type length_x = (BIGNUM_LENGTH (x));
  bignum_type p = (bignum_allocate ((length_x + 1), negative_p));
  bignum_destructive_copy (x, p);
  (BIGNUM_REF (p, length_x)) = 0;
  bignum_destructive_scale_up (p, y);
  return (bignum_trim (p));
}

static void
bignum_destructive_scale_up (bignum, factor)
     bignum_type bignum;
     bignum_digit_type factor;
{
  fast bignum_digit_type carry = 0;
  fast bignum_digit_type * scan = (BIGNUM_START_PTR (bignum));
  fast bignum_digit_type two_digits;
  fast bignum_digit_type product_low;
#define product_high carry
  bignum_digit_type * end = (scan + ((BIGNUM_LENGTH (bignum)) - 1));
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
  (*scan) += carry;
  return;
#undef product_high
}

static void
bignum_destructive_add (bignum, n)
     bignum_type bignum;
     bignum_digit_type n;
{
  fast bignum_digit_type * scan = (BIGNUM_START_PTR (bignum));
  fast bignum_digit_type digit;
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
  return;
}

/* Division */

/* For help understanding this algorithm, see:
   Knuth, Donald E., "The Art of Computer Programming",
   volume 2, "Seminumerical Algorithms"
   section 4.3.1, "Multiple-Precision Arithmetic". */

static void
bignum_divide_unsigned_large_denominator (numerator, denominator,
					  quotient, remainder,
					  q_negative_p, r_negative_p)
     bignum_type numerator;
     bignum_type denominator;
     bignum_type * quotient;
     bignum_type * remainder;
     int q_negative_p;
     int r_negative_p;
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
    fast bignum_digit_type v1 = (BIGNUM_REF ((denominator), (length_d - 1)));
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
bignum_divide_unsigned_normalized (u, v, q)
     bignum_type u;
     bignum_type v;
     bignum_type q;
{
  bignum_length_type u_length = (BIGNUM_LENGTH (u));
  bignum_length_type v_length = (BIGNUM_LENGTH (v));
  bignum_digit_type * u_start = (BIGNUM_START_PTR (u));
  bignum_digit_type * u_scan = (u_start + u_length);
  bignum_digit_type * u_scan_limit = (u_start + v_length);
  bignum_digit_type * u_scan_start = (u_scan - v_length);
  bignum_digit_type * v_start = (BIGNUM_START_PTR (v));
  bignum_digit_type * v_end = (v_start + v_length);
  bignum_digit_type * q_scan;
  bignum_digit_type v1 = (v_end[-1]);
  bignum_digit_type v2 = (v_end[-2]);
  fast bignum_digit_type ph;	/* high half of double-digit product */
  fast bignum_digit_type pl;	/* low half of double-digit product */
  fast bignum_digit_type guess;
  fast bignum_digit_type gh;	/* high half-digit of guess */
  fast bignum_digit_type ch;	/* high half of double-digit comparand */
  fast bignum_digit_type v2l = (HD_LOW (v2));
  fast bignum_digit_type v2h = (HD_HIGH (v2));
  fast bignum_digit_type cl;	/* low half of double-digit comparand */
#define gl ph			/* low half-digit of guess */
#define uj pl
#define qj ph
  bignum_digit_type gm;		/* memory loc for reference parameter */
  if (q != BIGNUM_OUT_OF_BAND)
    q_scan = ((BIGNUM_START_PTR (q)) + (BIGNUM_LENGTH (q)));
  while (u_scan_limit < u_scan)
    {
      uj = (*--u_scan);
      /* comparand =
	   (((((uj * BIGNUM_RADIX) + uj1) % v1) * BIGNUM_RADIX) + uj2);
	 guess = (((uj * BIGNUM_RADIX) + uj1) / v1); */
      cl = (u_scan[-2]);
      ch = (bignum_digit_divide (uj, (u_scan[-1]), v1, (&gm)));
      guess = gm;
      if (guess > (BIGNUM_RADIX - 1))
	guess = (BIGNUM_RADIX - 1);
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
bignum_divide_subtract (v_start, v_end, guess, u_start)
     bignum_digit_type * v_start;
     bignum_digit_type * v_end;
     bignum_digit_type guess;
     bignum_digit_type * u_start;
{
  bignum_digit_type * v_scan = v_start;
  bignum_digit_type * u_scan = u_start;
  fast bignum_digit_type carry = 0;
  if (guess == 0) return (0);
  {
    bignum_digit_type gl = (HD_LOW (guess));
    bignum_digit_type gh = (HD_HIGH (guess));
    fast bignum_digit_type v;
    fast bignum_digit_type pl;
    fast bignum_digit_type vl;
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
    (*u_scan) += 1;
  return (guess - 1);
}

static void
bignum_divide_unsigned_medium_denominator (numerator, denominator,
					   quotient, remainder,
					   q_negative_p, r_negative_p)
     bignum_type numerator;
     bignum_digit_type denominator;
     bignum_type * quotient;
     bignum_type * remainder;
     int q_negative_p;
     int r_negative_p;
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
    fast bignum_digit_type r = 0;
    fast bignum_digit_type * start = (BIGNUM_START_PTR (q)); 
    fast bignum_digit_type * scan = (start + length_q);
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
bignum_destructive_normalization (source, target, shift_left)
     bignum_type source;
     bignum_type target;
     int shift_left;
{
  fast bignum_digit_type digit;
  fast bignum_digit_type * scan_source = (BIGNUM_START_PTR (source));
  fast bignum_digit_type carry = 0;
  fast bignum_digit_type * scan_target = (BIGNUM_START_PTR (target));
  bignum_digit_type * end_source = (scan_source + (BIGNUM_LENGTH (source)));
  bignum_digit_type * end_target = (scan_target + (BIGNUM_LENGTH (target)));
  int shift_right = (BIGNUM_DIGIT_LENGTH - shift_left);
  bignum_digit_type mask = ((1 << shift_right) - 1);
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
bignum_destructive_unnormalization (bignum, shift_right)
     bignum_type bignum;
     int shift_right;
{
  bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
  fast bignum_digit_type * scan = (start + (BIGNUM_LENGTH (bignum)));
  fast bignum_digit_type digit;
  fast bignum_digit_type carry = 0;
  int shift_left = (BIGNUM_DIGIT_LENGTH - shift_right);
  bignum_digit_type mask = ((1 << shift_right) - 1);
  while (start < scan)
    {
      digit = (*--scan);
      (*scan) = ((digit >> shift_right) | carry);
      carry = ((digit & mask) << shift_left);
    }
  BIGNUM_ASSERT (carry == 0);
  return;
}

/* This is a reduced version of the division algorithm, applied to the
   case of dividing two bignum digits by one bignum digit.  It is
   assumed that the numerator and denominator are normalized. */

#define BDD_STEP(qn, j)							\
{									\
  uj = (u[j]);								\
  uj_uj1 = (HD_CONS (uj, (u[j + 1])));					\
  guess = (uj_uj1 / v1);						\
  comparand = (HD_CONS ((uj_uj1 % v1), (u[j + 2])));			\
  if (guess > (BIGNUM_RADIX_ROOT - 1))					\
    guess = (BIGNUM_RADIX_ROOT - 1);					\
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
bignum_digit_divide (uh, ul, v, q)
     bignum_digit_type uh;
     bignum_digit_type ul;
     bignum_digit_type v;
     bignum_digit_type * q;	/* return value */
{
  fast bignum_digit_type guess;
  fast bignum_digit_type comparand;
  fast bignum_digit_type v1 = (HD_HIGH (v));
  fast bignum_digit_type v2 = (HD_LOW (v));
  fast bignum_digit_type uj;
  fast bignum_digit_type uj_uj1;
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
bignum_digit_divide_subtract (v1, v2, guess, u)
     bignum_digit_type v1;
     bignum_digit_type v2;
     bignum_digit_type guess;
     bignum_digit_type u [];
{
  {
    fast bignum_digit_type product;
    fast bignum_digit_type diff;
    fast bignum_digit_type carry;
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
    fast bignum_digit_type sum;
    fast bignum_digit_type carry;
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
bignum_divide_unsigned_small_denominator (numerator, denominator,
					  quotient, remainder,
					  q_negative_p, r_negative_p)
     bignum_type numerator;
     bignum_digit_type denominator;
     bignum_type * quotient;
     bignum_type * remainder;
     int q_negative_p;
     int r_negative_p;
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
bignum_destructive_scale_down (bignum, denominator)
     bignum_type bignum;
     fast bignum_digit_type denominator;
{
  fast bignum_digit_type numerator;
  fast bignum_digit_type remainder = 0;
  fast bignum_digit_type two_digits;
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
bignum_remainder_unsigned_small_denominator (n, d, negative_p)
     bignum_type n;
     bignum_digit_type d;
     int negative_p;
{
  fast bignum_digit_type two_digits;
  bignum_digit_type * start = (BIGNUM_START_PTR (n));
  fast bignum_digit_type * scan = (start + (BIGNUM_LENGTH (n)));
  fast bignum_digit_type r = 0;
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
bignum_digit_to_bignum (digit, negative_p)
     fast bignum_digit_type digit;
     int negative_p;
{
  if (digit == 0)
    return (BIGNUM_ZERO ());
  else
    {
      fast bignum_type result = (bignum_allocate (1, negative_p));
      (BIGNUM_REF (result, 0)) = digit;
      return (result);
    }
}

/* Allocation */

static bignum_type
bignum_allocate (length, negative_p)
     fast bignum_length_type length;
     int negative_p;
{
  BIGNUM_ASSERT ((length >= 0) || (length < BIGNUM_RADIX));
  {
    fast bignum_type result = (BIGNUM_ALLOCATE (length));
    BIGNUM_SET_HEADER (result, length, negative_p);
    return (result);
  }
}

static bignum_type
bignum_allocate_zeroed (length, negative_p)
     fast bignum_length_type length;
     int negative_p;
{
  BIGNUM_ASSERT ((length >= 0) || (length < BIGNUM_RADIX));
  {
    fast bignum_type result = (BIGNUM_ALLOCATE (length));
    fast bignum_digit_type * scan = (BIGNUM_START_PTR (result));
    fast bignum_digit_type * end = (scan + length);
    BIGNUM_SET_HEADER (result, length, negative_p);
    while (scan < end)
      (*scan++) = 0;
    return (result);
  }
}

static bignum_type
bignum_shorten_length (bignum, length)
     fast bignum_type bignum;
     fast bignum_length_type length;
{
  fast bignum_length_type current_length = (BIGNUM_LENGTH (bignum));
  BIGNUM_ASSERT ((length >= 0) || (length <= current_length));
  if (length < current_length)
    {
      BIGNUM_SET_HEADER
	(bignum, length, ((length != 0) && (BIGNUM_NEGATIVE_P (bignum))));
      BIGNUM_REDUCE_LENGTH (bignum, bignum, length)
    }
  return (bignum);
}

static bignum_type
bignum_trim (bignum)
     bignum_type bignum;
{
  fast bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
  fast bignum_digit_type * end = (start + (BIGNUM_LENGTH (bignum)));
  fast bignum_digit_type * scan = end;
  while ((start < scan) && ((*--scan) == 0))
    ;
  scan += 1;
  if (scan < end)
    {
      fast bignum_length_type length = (scan - start);
      BIGNUM_SET_HEADER
	(bignum, length, ((length != 0) && (BIGNUM_NEGATIVE_P (bignum))));
      BIGNUM_REDUCE_LENGTH (bignum, bignum, length);
    }
  return (bignum);
}

/* Copying */

static bignum_type
bignum_copy (source)
     fast bignum_type source;
{
  fast bignum_type target =
    (bignum_allocate ((BIGNUM_LENGTH (source)), (BIGNUM_NEGATIVE_P (source))));
  bignum_destructive_copy (source, target);
  return (target);
}

static bignum_type
bignum_new_sign (bignum, negative_p)
     fast bignum_type bignum;
     int negative_p;
{
  fast bignum_type result =
    (bignum_allocate ((BIGNUM_LENGTH (bignum)), negative_p));
  bignum_destructive_copy (bignum, result);
  return (result);
}

static bignum_type
bignum_maybe_new_sign (bignum, negative_p)
     fast bignum_type bignum;
     int negative_p;
{
#ifndef BIGNUM_FORCE_NEW_RESULTS
  if ((BIGNUM_NEGATIVE_P (bignum)) ? negative_p : (! negative_p))
    return (bignum);
  else
#endif /* not BIGNUM_FORCE_NEW_RESULTS */
    {
      fast bignum_type result =
	(bignum_allocate ((BIGNUM_LENGTH (bignum)), negative_p));
      bignum_destructive_copy (bignum, result);
      return (result);
    }
}

static void
bignum_destructive_copy (source, target)
     bignum_type source;
     bignum_type target;
{
  fast bignum_digit_type * scan_source = (BIGNUM_START_PTR (source));
  fast bignum_digit_type * end_source =
    (scan_source + (BIGNUM_LENGTH (source)));
  fast bignum_digit_type * scan_target = (BIGNUM_START_PTR (target));
  while (scan_source < end_source)
    (*scan_target++) = (*scan_source++);
  return;
}

static void
bignum_destructive_zero (bignum)
     fast bignum_type bignum;
{
  fast bignum_digit_type * scan = (BIGNUM_START_PTR (bignum));
  fast bignum_digit_type * end = (scan + (BIGNUM_LENGTH (bignum)));
  while (scan < end)
    (*scan++) = 0;
  return;
}
