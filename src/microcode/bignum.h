/* -*-C-*-

$Id: bignum.h,v 9.31 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1989-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* External Interface to Bignum Code */

/* The `unsigned long' type is used for the conversion procedures
   `bignum_to_long' and `long_to_bignum'.  Older implementations of C
   don't support this type; if you have such an implementation you can
   disable these procedures using the following flag (alternatively
   you could write alternate versions that don't require this type). */
/* #define BIGNUM_NO_ULONG */

#include "ansidecl.h"

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

typedef PTR bignum_procedure_context;
extern bignum_type EXFUN (bignum_make_zero, (void));
extern bignum_type EXFUN (bignum_make_one, (int negative_p));
extern int EXFUN (bignum_equal_p, (bignum_type, bignum_type));
extern enum bignum_comparison EXFUN (bignum_test, (bignum_type));
extern enum bignum_comparison EXFUN
  (bignum_compare, (bignum_type, bignum_type));
extern bignum_type EXFUN (bignum_add, (bignum_type, bignum_type));
extern bignum_type EXFUN (bignum_subtract, (bignum_type, bignum_type));
extern bignum_type EXFUN (bignum_negate, (bignum_type));
extern bignum_type EXFUN (bignum_multiply, (bignum_type, bignum_type));
extern int EXFUN
  (bignum_divide, (bignum_type numerator,
		   bignum_type denominator,
		   bignum_type * quotient,
		   bignum_type * remainder));
extern bignum_type EXFUN (bignum_quotient, (bignum_type, bignum_type));
extern bignum_type EXFUN (bignum_remainder, (bignum_type, bignum_type));
#ifndef BIGNUM_NO_ULONG
extern bignum_type EXFUN (long_to_bignum, (long));
extern bignum_type EXFUN (ulong_to_bignum, (unsigned long));
extern long EXFUN (bignum_to_long, (bignum_type));
extern unsigned long EXFUN (bignum_to_ulong, (bignum_type));
#endif /* not BIGNUM_NO_ULONG */
extern bignum_type EXFUN (double_to_bignum, (double));
extern double EXFUN (bignum_to_double, (bignum_type));
extern int EXFUN
  (bignum_fits_in_word_p, (bignum_type,
			   long word_length,
			   int twos_complement_p));
extern bignum_type EXFUN (bignum_length_in_bits, (bignum_type));
extern bignum_type EXFUN (bignum_length_upper_limit, (void));
extern bignum_type EXFUN (bignum_shift_left, (bignum_type, unsigned long));
extern bignum_type EXFUN
  (unsigned_long_to_shifted_bignum, (unsigned long, unsigned long, int));
extern bignum_type EXFUN
  (digit_stream_to_bignum,
   (unsigned int n_digits,
    unsigned int EXFUN ((*producer), (bignum_procedure_context)),
    bignum_procedure_context context,
    unsigned int radix,
    int negative_p));
extern void EXFUN
  (bignum_to_digit_stream,
   (bignum_type, unsigned int radix,
    void EXFUN ((*consumer), (bignum_procedure_context, long)),
    bignum_procedure_context context));
extern long EXFUN (bignum_max_digit_stream_radix, (void));
