/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

/* Arithmetic Utilities */

#include "scheme.h"
#include "bits.h"

/* Conversions between Scheme types and C types. */

long
fixnum_to_long (SCHEME_OBJECT fixnum)
{
  return (FIXNUM_TO_LONG (fixnum));
}

SCHEME_OBJECT
double_to_fixnum (double value)
{
#ifdef HAVE_DOUBLE_TO_LONG_BUG
  long temp = ((long) value);
  return (LONG_TO_FIXNUM (temp));
#else
  return (LONG_TO_FIXNUM ((long) value));
#endif
}

bool
integer_to_long_p (SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) || (BIGNUM_TO_LONG_P (n)));
}

long
integer_to_long (SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) ? (FIXNUM_TO_LONG (n)) : (bignum_to_long (n)));
}

bool
integer_to_intmax_p (SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) || (BIGNUM_TO_INTMAX_P (n)));
}

intmax_t
integer_to_intmax (SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) ? (FIXNUM_TO_LONG (n)) : (bignum_to_intmax (n)));
}

SCHEME_OBJECT
long_to_integer (long number)
{
  return
    ((LONG_TO_FIXNUM_P (number))
     ? (LONG_TO_FIXNUM (number))
     : (long_to_bignum (number)));
}

SCHEME_OBJECT
intmax_to_integer (intmax_t number)
{
  return
    (((LONG_MIN <= number) && (number <= LONG_MAX))
     ? (long_to_integer (number))
     : (intmax_to_bignum (number)));
}

bool
integer_to_ulong_p (SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) ? (!FIXNUM_NEGATIVE_P (n)) : (BIGNUM_TO_ULONG_P (n)));
}

unsigned long
integer_to_ulong (SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n))
	  ? ((unsigned long) (FIXNUM_TO_LONG (n)))
	  : (bignum_to_ulong (n)));
}

bool
integer_to_uintmax_p (SCHEME_OBJECT n)
{
  return
    ((FIXNUM_P (n)) ? (!FIXNUM_NEGATIVE_P (n)) : (BIGNUM_TO_UINTMAX_P (n)));
}

uintmax_t
integer_to_uintmax (SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n))
          ? ((uintmax_t) (FIXNUM_TO_LONG (n)))
          : (bignum_to_uintmax (n)));
}

SCHEME_OBJECT
ulong_to_integer (unsigned long number)
{
  long s_number = ((long) number);
  if (s_number >= 0)
    return
      ((LONG_TO_FIXNUM_P (s_number))
       ? (LONG_TO_FIXNUM (s_number))
       : (long_to_bignum (s_number)));
  else
    return (ulong_to_bignum (number));
}

SCHEME_OBJECT
uintmax_to_integer (uintmax_t number)
{
  return ((number <= ULONG_MAX)
          ? (ulong_to_integer (number))
          : (uintmax_to_bignum (number)));
}

bool
integer_to_double_p (SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) || (BIGNUM_TO_DOUBLE_P (n)));
}

double
integer_to_double (SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) ? (FIXNUM_TO_DOUBLE (n)) : (bignum_to_double (n)));
}

SCHEME_OBJECT
double_to_integer (double x)
{
  return
    ((DOUBLE_TO_FIXNUM_P (x))
     ? (DOUBLE_TO_FIXNUM (x))
     : (double_to_bignum (x)));
}

double
double_truncate (double x)
{
  double iptr;
  (void) modf (x, (&iptr));
  return (iptr);
}

double
double_round (double x)
{
  double integral;
  double fractional = (fabs (modf (x, (&integral))));

  if ((fractional == 0.5)
      ? ((fmod (integral, 2.0)) == 0.0)
      : (! (0.5 < fractional)))
    return (integral);
  else if (x < 0.0)
    return (integral - 1.0);
  else
    return (integral + 1.0);
}

/* Conversions between Scheme types and Scheme types. */

SCHEME_OBJECT
bignum_to_fixnum (SCHEME_OBJECT bignum)
{
  return
    ((BIGNUM_TO_FIXNUM_P (bignum))
     ? (BIGNUM_TO_FIXNUM (bignum))
     : SHARP_F);
}

SCHEME_OBJECT
bignum_to_integer (SCHEME_OBJECT bignum)
{
  return
    ((BIGNUM_TO_FIXNUM_P (bignum))
     ? (BIGNUM_TO_FIXNUM (bignum))
     : bignum);
}

SCHEME_OBJECT
bignum_to_flonum (SCHEME_OBJECT bignum)
{
  return
    ((BIGNUM_TO_FLONUM_P (bignum))
     ? (BIGNUM_TO_FLONUM (bignum))
     : SHARP_F);
}

bool
finite_flonum_p (SCHEME_OBJECT x)
{
  return ((FLONUM_P (x)) && (flonum_is_finite_p (x)));
}

bool
flonum_is_finite_p (SCHEME_OBJECT x)
{
  return double_is_finite_p (FLONUM_TO_DOUBLE (x));
}

bool
double_is_finite_p (double x)
{
  return
    (((x > 1.0) || (x < -1.0))
     ? (x != (x / 2.0))
     : ((x <= 1.0) && (x >= -1.0)));
}

bool
flonum_integer_p (SCHEME_OBJECT x)
{
  double iptr;
  return ((modf ((FLONUM_TO_DOUBLE (x)), (&iptr))) == 0);
}

SCHEME_OBJECT
flonum_floor (SCHEME_OBJECT x)
{
  return (double_to_flonum (floor (FLONUM_TO_DOUBLE (x))));
}

SCHEME_OBJECT
flonum_ceiling (SCHEME_OBJECT x)
{
  return (double_to_flonum (ceil (FLONUM_TO_DOUBLE (x))));
}

SCHEME_OBJECT
flonum_round (SCHEME_OBJECT x)
{
  return (double_to_flonum (double_round (FLONUM_TO_DOUBLE (x))));
}

SCHEME_OBJECT
flonum_normalize (SCHEME_OBJECT x)
{
  int exponent;
  double significand = (frexp ((FLONUM_TO_DOUBLE (x)), (&exponent)));
  return (cons ((double_to_flonum (significand)),
		(long_to_integer ((long) exponent))));
}

SCHEME_OBJECT
flonum_denormalize (SCHEME_OBJECT x, SCHEME_OBJECT e)
{
  return (double_to_flonum (ldexp ((FLONUM_TO_DOUBLE (x)),
				   ((int) (integer_to_long (e))))));
}

/* Generic Integer Operations */

bool
integer_zero_p (SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) ? (FIXNUM_ZERO_P (n)) : (BIGNUM_ZERO_P (n)));
}

bool
integer_negative_p (SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) ? (FIXNUM_NEGATIVE_P (n)) : (BIGNUM_NEGATIVE_P (n)));
}

bool
integer_positive_p (SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) ? (FIXNUM_POSITIVE_P (n)) : (BIGNUM_POSITIVE_P (n)));
}

bool
integer_equal_p (SCHEME_OBJECT n, SCHEME_OBJECT m)
{
  return
    ((FIXNUM_P (n))
     ? ((FIXNUM_P (m))
	? (FIXNUM_EQUAL_P (n, m))
	: (bignum_equal_p ((FIXNUM_TO_BIGNUM (n)), m)))
     : (bignum_equal_p (n, ((FIXNUM_P (m)) ? (FIXNUM_TO_BIGNUM (m)) : m))));
}

bool
integer_less_p (SCHEME_OBJECT n, SCHEME_OBJECT m)
{
  return
    ((FIXNUM_P (n))
     ? ((FIXNUM_P (m))
	? (FIXNUM_LESS_P (n, m))
	: (BIGNUM_LESS_P ((FIXNUM_TO_BIGNUM (n)), m)))
     : (BIGNUM_LESS_P (n, ((FIXNUM_P (m)) ? (FIXNUM_TO_BIGNUM (m)) : m))));
}

SCHEME_OBJECT
integer_negate (SCHEME_OBJECT n)
{
  return
    ((FIXNUM_P (n))
     ? (long_to_integer (- (FIXNUM_TO_LONG (n))))
     : (bignum_to_integer (bignum_negate (n))));
}

SCHEME_OBJECT
integer_add (SCHEME_OBJECT n, SCHEME_OBJECT m)
{
  return
    ((FIXNUM_P (n))
     ? ((FIXNUM_P (m))
	? (long_to_integer ((FIXNUM_TO_LONG (n)) + (FIXNUM_TO_LONG (m))))
	: (bignum_to_integer (bignum_add ((FIXNUM_TO_BIGNUM (n)), m))))
     : (bignum_to_integer
	(bignum_add (n, ((FIXNUM_P (m)) ? (FIXNUM_TO_BIGNUM (m)) : m)))));
}

SCHEME_OBJECT
integer_add_1 (SCHEME_OBJECT n)
{
  return
    ((FIXNUM_P (n))
     ? (long_to_integer ((FIXNUM_TO_LONG (n)) + 1))
     : (bignum_to_integer (bignum_add (n, (long_to_bignum (1))))));
}

SCHEME_OBJECT
integer_subtract (SCHEME_OBJECT n, SCHEME_OBJECT m)
{
  return
    ((FIXNUM_P (n))
     ? ((FIXNUM_P (m))
	? (long_to_integer ((FIXNUM_TO_LONG (n)) - (FIXNUM_TO_LONG (m))))
	: (bignum_to_integer (bignum_subtract ((FIXNUM_TO_BIGNUM (n)), m))))
     : (bignum_to_integer
	(bignum_subtract (n, ((FIXNUM_P (m)) ? (FIXNUM_TO_BIGNUM (m)) : m)))));
}

SCHEME_OBJECT
integer_subtract_1 (SCHEME_OBJECT n)
{
  return
    ((FIXNUM_P (n))
     ? (long_to_integer ((FIXNUM_TO_LONG (n)) - 1))
     : (bignum_to_integer (bignum_subtract (n, (long_to_bignum (1))))));
}

SCHEME_OBJECT
integer_multiply (SCHEME_OBJECT n, SCHEME_OBJECT m)
{
  SCHEME_OBJECT result;
  return
    ((FIXNUM_P (n))
     ? ((FIXNUM_P (m))
	? ((result = (Mul (n, m))),
	   ((result != SHARP_F)
	    ? result
	    : (bignum_to_integer
	       (bignum_multiply ((FIXNUM_TO_BIGNUM (n)),
				 (FIXNUM_TO_BIGNUM (m)))))))
	: (bignum_to_integer (bignum_multiply ((FIXNUM_TO_BIGNUM (n)), m))))
     : (bignum_to_integer
	(bignum_multiply (n, ((FIXNUM_P (m)) ? (FIXNUM_TO_BIGNUM (m)) : m)))));
}

bool
integer_divide (SCHEME_OBJECT n, SCHEME_OBJECT d,
		SCHEME_OBJECT * q, SCHEME_OBJECT * r)
{
  if (FIXNUM_P (n))
    {
      if (FIXNUM_P (d))
	{
	  /* Now, unbelievable hair because C doesn't fully specify
	     / and % when their arguments are negative.  We must get
	     consistent answers for all valid arguments. */
	  long lx = (FIXNUM_TO_LONG (n));
	  long ly = (FIXNUM_TO_LONG (d));
	  long quotient;
	  long remainder;
	  if (ly == 0)
	    return (true);
	  if (lx < 0)
	    {
	      lx = (-lx);
	      if (ly < 0)
		{
		  ly = (-ly);
		  quotient = (lx / ly);
		}
	      else
		quotient = (- (lx / ly));
	      remainder = (- (lx % ly));
	    }
	  else
	    {
	      if (ly < 0)
		{
		  ly = (-ly);
		  quotient = (- (lx / ly));
		}
	      else
		quotient = (lx / ly);
	      remainder = (lx % ly);
	    }
	  (*q) = (long_to_integer (quotient));
	  (*r) = (LONG_TO_FIXNUM (remainder));
	  return (false);
	}
      n = (FIXNUM_TO_BIGNUM (n));
    }
  else
    {
      if (FIXNUM_P (d))
	d = (FIXNUM_TO_BIGNUM (d));
    }
  {
    SCHEME_OBJECT quotient;
    SCHEME_OBJECT remainder;
    if (bignum_divide (n, d, (&quotient), (&remainder)))
      return (true);
    (*q) = (bignum_to_integer (quotient));
    (*r) = (bignum_to_integer (remainder));
    return (false);
  }
}

SCHEME_OBJECT
integer_quotient (SCHEME_OBJECT n, SCHEME_OBJECT d)
{
  if (FIXNUM_P (n))
    {
      if (FIXNUM_P (d))
	{
	  long lx = (FIXNUM_TO_LONG (n));
	  long ly = (FIXNUM_TO_LONG (d));
	  return
	    ((ly == 0)
	     ? SHARP_F
	     : (long_to_integer
		((lx < 0)
		 ? ((ly < 0)
		    ? ((-lx) / (-ly))
		    : (- ((-lx) / ly)))
		 : ((ly < 0)
		    ? (- (lx / (-ly)))
		    : (lx / ly)))));
	}
      n = (FIXNUM_TO_BIGNUM (n));
    }
  else
    {
      if (FIXNUM_P (d))
	d = (FIXNUM_TO_BIGNUM (d));
    }
  {
    SCHEME_OBJECT result = (bignum_quotient (n, d));
    return ((result == SHARP_F) ? SHARP_F : (bignum_to_integer (result)));
  }
}

SCHEME_OBJECT
integer_remainder (SCHEME_OBJECT n, SCHEME_OBJECT d)
{
  if (FIXNUM_P (n))
    {
      if (FIXNUM_P (d))
	{
	  long lx = (FIXNUM_TO_LONG (n));
	  long ly = (FIXNUM_TO_LONG (d));
	  return
	    ((ly == 0)
	     ? SHARP_F
	     : (long_to_integer
		((lx < 0)
		 ? (- ((-lx) % ((ly < 0) ? (-ly) : ly)))
		 : (lx % ((ly < 0) ? (-ly) : ly)))));
	}
      n = (FIXNUM_TO_BIGNUM (n));
    }
  else
    {
      if (FIXNUM_P (d))
	d = (FIXNUM_TO_BIGNUM (d));
    }
  {
    SCHEME_OBJECT result = (bignum_remainder (n, d));
    return
      ((result == SHARP_F)
       ? SHARP_F
       : (bignum_to_integer (result)));
  }
}

/* Length and Bit Counts */

/* Ones-complement length.  */

SCHEME_OBJECT
integer_length_in_bits (SCHEME_OBJECT n)
{
  if (FIXNUM_P (n))
    {
      long n1 = (FIXNUM_TO_LONG (n));
      return (ULONG_TO_FIXNUM (ulong_length_in_bits ((n1 < 0) ? (- n1) : n1)));
    }
  else
    return (ulong_to_integer (bignum_length_in_bits (n)));
}

/* Two's-complement length.  */

SCHEME_OBJECT
integer_length (SCHEME_OBJECT n)
{
  if (FIXNUM_P (n))
    {
      long n1 = (FIXNUM_TO_LONG (n));
      return (ULONG_TO_FIXNUM (ulong_length_in_bits ((n1 < 0) ? (~n1) : n1)));
    }
  else
    return (ulong_to_integer (bignum_integer_length (n)));
}

SCHEME_OBJECT
integer_first_set_bit (SCHEME_OBJECT n)
{
  if (FIXNUM_P (n))
    {
      long n1 = (FIXNUM_TO_LONG (n));
      return
	(LONG_TO_FIXNUM
	 (ulong_first_set_bit ((n1 < 0) ? (~ ((unsigned long) (~n1))) : n1)));
    }
  else
    return (long_to_integer (bignum_first_set_bit (n)));
}

SCHEME_OBJECT
integer_bit_count (SCHEME_OBJECT n)
{
  if (FIXNUM_P (n))
    {
      long n1 = (FIXNUM_TO_LONG (n));
      return (ULONG_TO_FIXNUM (ulong_bit_count ((n1 < 0) ? (~n1) : n1)));
    }
  else
    return (ulong_to_integer (bignum_bit_count (n)));
}

SCHEME_OBJECT
integer_hamming_distance (SCHEME_OBJECT n, SCHEME_OBJECT m)
{
  if ((FIXNUM_P (n)) && (FIXNUM_P (m)))
    {
      long x = ((FIXNUM_TO_LONG (n)) ^ (FIXNUM_TO_LONG (m)));
      return
	((x < 0)
	 ? (LONG_TO_FIXNUM (-1))
	 : (ULONG_TO_FIXNUM (ulong_bit_count (x))));
    }
  else
    return
      (long_to_integer
       (bignum_hamming_distance
	(((FIXNUM_P (n)) ? (FIXNUM_TO_BIGNUM (n)) : n),
	 ((FIXNUM_P (m)) ? (FIXNUM_TO_BIGNUM (m)) : m))));
}

/* Bitwise Operations */

SCHEME_OBJECT
integer_bitwise_not (SCHEME_OBJECT n)
{
  if (FIXNUM_P (n))
    return (LONG_TO_FIXNUM (~ (FIXNUM_TO_LONG (n))));
  else
    return (bignum_bitwise_not (n));
}

#define DEFINE_BITWISE(NAME, OP)					\
SCHEME_OBJECT								\
NAME (SCHEME_OBJECT n, SCHEME_OBJECT m)					\
{									\
  if ((FIXNUM_P (n)) && (FIXNUM_P (m)))					\
    return								\
      (LONG_TO_FIXNUM							\
       (BITWISE_##OP ((FIXNUM_TO_LONG (n)), (FIXNUM_TO_LONG (m)))));	\
  else									\
    return								\
      (bignum_to_integer						\
       (bignum_bitwise_##OP						\
	(((FIXNUM_P (n)) ? (FIXNUM_TO_BIGNUM (n)) : n),			\
	 ((FIXNUM_P (m)) ? (FIXNUM_TO_BIGNUM (m)) : m))));		\
}

#define BITWISE_and(x, y) ((x) & (y))
#define BITWISE_andc2(x, y) ((x) &~ (y))
#define BITWISE_andc1(x, y) ((y) &~ (x))
#define BITWISE_xor(x, y) ((x) ^ (y))
#define BITWISE_ior(x, y) ((x) | (y))
#define BITWISE_nor(x, y) (~ ((x) | (y)))
#define BITWISE_eqv(x, y) (~ ((x) ^ (y)))
#define BITWISE_orc2(x, y) ((x) |~ (y))
#define BITWISE_orc1(x, y) ((y) |~ (x))
#define BITWISE_nand(x, y) (~ ((x) & (y)))

DEFINE_BITWISE (integer_bitwise_and, and)
DEFINE_BITWISE (integer_bitwise_andc2, andc2)
DEFINE_BITWISE (integer_bitwise_andc1, andc1)
DEFINE_BITWISE (integer_bitwise_xor, xor)
DEFINE_BITWISE (integer_bitwise_ior, ior)
DEFINE_BITWISE (integer_bitwise_nor, nor)
DEFINE_BITWISE (integer_bitwise_eqv, eqv)
DEFINE_BITWISE (integer_bitwise_orc2, orc2)
DEFINE_BITWISE (integer_bitwise_orc1, orc1)
DEFINE_BITWISE (integer_bitwise_nand, nand)

SCHEME_OBJECT
integer_nonnegative_one_bits (unsigned long n, unsigned long m)
{
  if (n == 0)
    return (LONG_TO_FIXNUM (0));
  else if ((n + m) <= FIXNUM_LENGTH)
    return (ULONG_TO_FIXNUM ((~ ((~ ((unsigned long) 0)) << n)) << m));
  else
    return (bignum_nonnegative_one_bits (n, m));
}

SCHEME_OBJECT
integer_negative_zero_bits (unsigned long n, unsigned long m)
{
  if (n == 0)
    return (LONG_TO_FIXNUM (-1));
  else if ((n + m) <= FIXNUM_LENGTH)
    return
      (LONG_TO_FIXNUM (~ ((long) ((~ ((~ ((unsigned long) 0)) << n)) << m))));
  else
    return (bignum_negative_zero_bits (n, m));
}

/* Shift: multiplication and Euclidean division by 2^m */

SCHEME_OBJECT
integer_shift_left (SCHEME_OBJECT n, unsigned long m)
{
  if (m == 0)
    return (n);
  if (FIXNUM_P (n))
    {
      long n1 = (FIXNUM_TO_LONG (n));
      if (n1 < 0)
	{
	  if ((m + (ulong_length_in_bits (~n1))) <= FIXNUM_LENGTH)
	    /* The behaviour of shifting a negative integer is
	       undefined in C.  */
	    return (LONG_TO_FIXNUM (- ((-n1) << m)));
	  else
	    return
	      (bignum_negate (unsigned_long_to_shifted_bignum ((-n1), m, 0)));
	}
      else if (0 < n1)
	{
	  if ((m + (ulong_length_in_bits (n1))) <= FIXNUM_LENGTH)
	    return (LONG_TO_FIXNUM (n1 << m));
	  else
	    return (unsigned_long_to_shifted_bignum (n1, m, 0));
	}
      else
	return (LONG_TO_FIXNUM (0));
    }
  else
    return (bignum_shift_left (n, m));
}

SCHEME_OBJECT
integer_shift_right (SCHEME_OBJECT n, unsigned long m)
{
  if (m == 0)
    return (n);
  if (FIXNUM_P (n))
    {
      long n1 = (FIXNUM_TO_LONG (n));
      return (LONG_TO_FIXNUM ((n1 < 0) ? (~ ((~n1) >> m)) : (n1 >> m)));
    }
  else
    return (bignum_to_integer (bignum_shift_right (n, m)));
}
