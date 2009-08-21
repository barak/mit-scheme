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

/* Internal Interface to Bignum Code */

#undef BIGNUM_ZERO_P
#undef BIGNUM_NEGATIVE_P

/* The memory model is based on the following definitions, and on the
   definition of the type `bignum_type'.  The only other special
   definition is `CHAR_BIT', which is defined in the header file
   "limits.h".  */

typedef long bignum_digit_type;
typedef long bignum_length_type;

#ifdef MIT_SCHEME

#include "prims.h"

/* BIGNUM_ALLOCATE allocates a (length + 1)-element array of
   `bignum_digit_type'; deallocation is the responsibility of the
   user (in Scheme, the garbage collector handles this). */
#define BIGNUM_ALLOCATE(length)						\
  (allocate_non_marked_vector						\
   (TC_BIG_FIXNUM, (BIGNUM_LENGTH_TO_GC_LENGTH (length)), 1))

/* BIGNUM_TO_POINTER casts a bignum object to a digit array pointer. */
#define BIGNUM_TO_POINTER(bignum)					\
  ((bignum_digit_type *) (VECTOR_LOC ((bignum), 0)))

/* BIGNUM_REDUCE_LENGTH allows the memory system to reclaim some
   space when a bignum's length is reduced from its original value. */
#define BIGNUM_REDUCE_LENGTH(target, source, length)			\
{									\
  SET_VECTOR_LENGTH ((source), (BIGNUM_LENGTH_TO_GC_LENGTH (length)));	\
  (target) = (source);							\
}

#define BIGNUM_LENGTH_TO_GC_LENGTH(length)				\
  (BYTES_TO_WORDS (((length) + 1) * (sizeof (bignum_digit_type))))

/* BIGNUM_DEALLOCATE is called when disposing of bignums which are
   created as intermediate temporaries; Scheme doesn't need this. */
#define BIGNUM_DEALLOCATE(bignum) do {} while (0)

/* If BIGNUM_FORCE_NEW_RESULTS is defined, all bignum-valued operations
   return freshly-allocated results.  This is useful for some kinds of
   memory deallocation strategies. */
/* #define BIGNUM_FORCE_NEW_RESULTS */

/* BIGNUM_EXCEPTION is invoked to handle assertion violations. */
#define BIGNUM_EXCEPTION error_external_return

#else /* not MIT_SCHEME */

#define BIGNUM_ALLOCATE bignum_malloc
#define BIGNUM_TO_POINTER(bignum) ((bignum_digit_type *) (bignum))
#define BIGNUM_REDUCE_LENGTH(target, source, length)			\
  (target) = (bignum_realloc ((source), (length)))
#define BIGNUM_DEALLOCATE free
#define BIGNUM_FORCE_NEW_RESULTS
#define BIGNUM_EXCEPTION abort
extern void free ();
extern void abort ();

#endif /* not MIT_SCHEME */

#define BIGNUM_DIGIT_LENGTH (((sizeof (bignum_digit_type)) * CHAR_BIT) - 2)
#define BIGNUM_HALF_DIGIT_LENGTH (BIGNUM_DIGIT_LENGTH / 2)
#define BIGNUM_RADIX		(1UL << BIGNUM_DIGIT_LENGTH)
#define BIGNUM_RADIX_ROOT	(1UL << BIGNUM_HALF_DIGIT_LENGTH)
#define BIGNUM_DIGIT_MASK	(BIGNUM_RADIX - 1UL)
#define BIGNUM_HALF_DIGIT_MASK	(BIGNUM_RADIX_ROOT - 1UL)

#define BIGNUM_START_PTR(bignum)					\
  ((BIGNUM_TO_POINTER (bignum)) + 1)

#define BIGNUM_SET_HEADER(bignum, length, negative_p)			\
  (* (BIGNUM_TO_POINTER (bignum))) =					\
    ((length) | ((negative_p) ? BIGNUM_RADIX : 0))

#define BIGNUM_LENGTH(bignum)						\
  ((* (BIGNUM_TO_POINTER (bignum))) & BIGNUM_DIGIT_MASK)

#define BIGNUM_NEGATIVE_P(bignum)					\
  (((* (BIGNUM_TO_POINTER (bignum))) & BIGNUM_RADIX) != 0)

#define BIGNUM_ZERO_P(bignum)						\
  ((BIGNUM_LENGTH (bignum)) == 0)

#define BIGNUM_REF(bignum, index)					\
  (* ((BIGNUM_START_PTR (bignum)) + (index)))

#ifdef BIGNUM_FORCE_NEW_RESULTS
#define BIGNUM_MAYBE_COPY bignum_copy
#else
#define BIGNUM_MAYBE_COPY(bignum) bignum
#endif

/* These definitions are here to facilitate caching of the constants
   0, 1, and -1. */
#define BIGNUM_ZERO bignum_make_zero
#define BIGNUM_ONE bignum_make_one

#define HD_LOW(digit) ((digit) & BIGNUM_HALF_DIGIT_MASK)
#define HD_HIGH(digit) ((digit) >> BIGNUM_HALF_DIGIT_LENGTH)
#define HD_CONS(high, low) (((high) << BIGNUM_HALF_DIGIT_LENGTH) | (low))

#define BIGNUM_BITS_TO_DIGITS(n)					\
  (((n) + (BIGNUM_DIGIT_LENGTH - 1)) / BIGNUM_DIGIT_LENGTH)

#define BIGNUM_DIGITS_FOR_LONG						\
  (BIGNUM_BITS_TO_DIGITS ((sizeof (long)) * CHAR_BIT))

#ifndef BIGNUM_DISABLE_ASSERTION_CHECKS

#define BIGNUM_ASSERT(expression)					\
{									\
  if (! (expression))							\
    BIGNUM_EXCEPTION ();						\
}

#endif /* not BIGNUM_DISABLE_ASSERTION_CHECKS */
