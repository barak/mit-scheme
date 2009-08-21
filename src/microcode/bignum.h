/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

/* External Interface to Bignum Code */

#ifndef SCM_BIGNUM_H_INCLUDED
#define SCM_BIGNUM_H_INCLUDED 1

/* The `unsigned long' type is used for the conversion procedures
   `bignum_to_long' and `long_to_bignum'.  Older implementations of C
   don't support this type; if you have such an implementation you can
   disable these procedures using the following flag (alternatively
   you could write alternate versions that don't require this type). */
/* #define BIGNUM_NO_ULONG */

#ifdef MIT_SCHEME

typedef SCHEME_OBJECT bignum_type;
#define BIGNUM_OUT_OF_BAND SHARP_F

#else

typedef long * bignum_type;
#define BIGNUM_OUT_OF_BAND ((bignum_type) 0)

#endif

enum bignum_comparison
{
  bignum_comparison_equal, bignum_comparison_less, bignum_comparison_greater
};

typedef void * bignum_procedure_context;
extern bignum_type bignum_make_zero (void);
extern bignum_type bignum_make_one (int negative_p);
extern int bignum_equal_p (bignum_type, bignum_type);
extern enum bignum_comparison bignum_test (bignum_type);
extern enum bignum_comparison bignum_compare
  (bignum_type, bignum_type);
extern bignum_type bignum_add (bignum_type, bignum_type);
extern bignum_type bignum_subtract (bignum_type, bignum_type);
extern bignum_type bignum_negate (bignum_type);
extern bignum_type bignum_multiply (bignum_type, bignum_type);
extern int bignum_divide
  (bignum_type numerator,
		   bignum_type denominator,
		   bignum_type * quotient,
		   bignum_type * remainder);
extern bignum_type bignum_quotient (bignum_type, bignum_type);
extern bignum_type bignum_remainder (bignum_type, bignum_type);
#ifndef BIGNUM_NO_ULONG
extern bignum_type long_to_bignum (long);
extern bignum_type ulong_to_bignum (unsigned long);
extern long bignum_to_long (bignum_type);
extern unsigned long bignum_to_ulong (bignum_type);
#endif /* not BIGNUM_NO_ULONG */
extern bignum_type double_to_bignum (double);
extern double bignum_to_double (bignum_type);
extern int bignum_fits_in_word_p
  (bignum_type,
			   long word_length,
			   int twos_complement_p);
extern bignum_type bignum_length_in_bits (bignum_type);
extern bignum_type bignum_length_upper_limit (void);
extern bignum_type bignum_shift_left (bignum_type, unsigned long);
extern bignum_type unsigned_long_to_shifted_bignum
  (unsigned long, unsigned long, int);
extern bignum_type digit_stream_to_bignum
  (unsigned int n_digits,
    unsigned int (*producer) (bignum_procedure_context),
    bignum_procedure_context context,
    unsigned int radix,
    int negative_p);
extern void bignum_to_digit_stream
  (bignum_type, unsigned int radix,
    void (*consumer) (bignum_procedure_context, long),
    bignum_procedure_context context);
extern long bignum_max_digit_stream_radix (void);

#endif /* !SCM_BIGNUM_H_INCLUDED */
