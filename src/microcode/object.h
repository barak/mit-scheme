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

/* This file defines the macros which define and manipulate Scheme
   objects.  This is the lowest level of abstraction in this program.
*/
#ifndef SCM_OBJECT_H
#define SCM_OBJECT_H

#include "config.h"
#include "types.h"

#define TYPE_CODE_LENGTH (6U)

#if defined(MIN_TYPE_CODE_LENGTH) && (TYPE_CODE_LENGTH < MIN_TYPE_CODE_LENGTH)
#  include ";; inconsistency: TYPE_CODE_LENGTH < MIN_TYPE_CODE_LENGTH"
#endif

typedef unsigned long SCHEME_OBJECT;
#define SIZEOF_SCHEME_OBJECT SIZEOF_UNSIGNED_LONG
#define OBJECT_LENGTH ((unsigned int) (CHAR_BIT * SIZEOF_UNSIGNED_LONG))

/* A convenience definition since "unsigned char" is so verbose.  */
typedef unsigned char byte_t;

#if (TYPE_CODE_LENGTH == 6U)
#  define N_TYPE_CODES (0x40)
#  if (SIZEOF_UNSIGNED_LONG == 4) /* 32 bit word versions */
#    define DATUM_LENGTH	(26U)
#    define DATUM_MASK		(0x03FFFFFFL)
#    define TYPE_CODE_MASK	(0XFC000000L)
#    define FIXNUM_LENGTH	(25U) /* doesn't include sign */
#    define FIXNUM_MASK		(0x01FFFFFFL)
#    define FIXNUM_SIGN_BIT	(0x02000000L)
#    define SIGN_MASK		(0xFE000000L)
#    define SMALLEST_FIXNUM	(-33554432L)
#    define BIGGEST_FIXNUM	(33554431L)
#    define HALF_DATUM_LENGTH	(13U)
#    define HALF_DATUM_MASK	(0x00001FFFL)
#  endif
#  if (SIZEOF_UNSIGNED_LONG == 8) /* 64 bit word versions */
#    define DATUM_LENGTH	(58U)
#    define DATUM_MASK		(0x03FFFFFFFFFFFFFFL)
#    define TYPE_CODE_MASK	(0XFC00000000000000L)
#    define FIXNUM_LENGTH	(57U) /* doesn't include sign */
#    define FIXNUM_MASK		(0x01FFFFFFFFFFFFFFL)
#    define FIXNUM_SIGN_BIT	(0x0200000000000000L)
#    define SIGN_MASK		(0xFE00000000000000L)
#    define SMALLEST_FIXNUM	(-144115188075855872L)
#    define BIGGEST_FIXNUM	(144115188075855871L)
#    define HALF_DATUM_LENGTH	(29U)
#    define HALF_DATUM_MASK	(0x000000001FFFFFFFL)
#  endif
#endif

#ifndef DATUM_LENGTH		/* Safe versions */
#  define N_TYPE_CODES		(1U << TYPE_CODE_LENGTH)
#  define DATUM_LENGTH		(OBJECT_LENGTH - TYPE_CODE_LENGTH)
#  define DATUM_MASK		((1UL << DATUM_LENGTH) - 1UL)
#  define TYPE_CODE_MASK	((N_TYPE_CODES - 1U) << DATUM_LENGTH)
#  define FIXNUM_LENGTH		(DATUM_LENGTH - 1U) /* doesn't include sign */
#  define FIXNUM_MASK		((1UL << FIXNUM_LENGTH) - 1UL)
#  define FIXNUM_SIGN_BIT	(1UL << FIXNUM_LENGTH)
#  define SIGN_MASK							\
  (((unsigned long) ((N_TYPE_CODES * 2U) - 1U)) << FIXNUM_LENGTH)
#  define SMALLEST_FIXNUM	SIGN_MASK
#  define BIGGEST_FIXNUM	((1UL << FIXNUM_LENGTH) - 1UL)
#  define HALF_DATUM_LENGTH	(DATUM_LENGTH / 2U)
#  define HALF_DATUM_MASK	((1UL << HALF_DATUM_LENGTH) - 1UL)
#endif

/* Basic object structure */

#define OBJECT_TYPE(object) ((object) >> DATUM_LENGTH)
#define OBJECT_DATUM(object) ((object) & DATUM_MASK)
#define OBJECT_ADDRESS(object) (DATUM_TO_ADDRESS (OBJECT_DATUM (object)))

#define MAKE_OBJECT(type, datum)					\
  ((((unsigned long) (type)) << DATUM_LENGTH) | (datum))

#define OBJECT_NEW_TYPE(type, datum_object)				\
  (MAKE_OBJECT ((type), (OBJECT_DATUM (datum_object))))

#define OBJECT_NEW_DATUM(type_object, datum)				\
  (MAKE_OBJECT ((OBJECT_TYPE (type_object)), (datum)))

#define MAKE_OBJECT_FROM_OBJECTS(type_object, datum_object)		\
  (MAKE_OBJECT ((OBJECT_TYPE (type_object)), (OBJECT_DATUM (datum_object))))

#define MAKE_POINTER_OBJECT(type, address)				\
  (MAKE_OBJECT ((type), (ADDRESS_TO_DATUM (address))))

#define OBJECT_NEW_ADDRESS(object, address)				\
  (OBJECT_NEW_DATUM ((object), (ADDRESS_TO_DATUM (address))))

/* Machine dependencies */

#ifndef HEAP_MALLOC
#  define HEAP_MALLOC OS_malloc_init
#endif

#ifdef HEAP_IN_LOW_MEMORY	/* Storing absolute addresses */

#define ALLOCATE_HEAP_SPACE(space, low, high) do			\
{									\
  unsigned long _space = (space);					\
  SCHEME_OBJECT * _low							\
    = ((SCHEME_OBJECT *)						\
       (HEAP_MALLOC ((sizeof (SCHEME_OBJECT)) * _space)));		\
									\
  (low) = _low;								\
  (high) = (_low + _space);						\
} while (0)

#ifndef DATUM_TO_ADDRESS
#  define DATUM_TO_ADDRESS(datum) ((SCHEME_OBJECT *) (datum))
#endif

#ifndef ADDRESS_TO_DATUM
#  define ADDRESS_TO_DATUM(address) ((SCHEME_OBJECT) (address))
#endif

#else /* not HEAP_IN_LOW_MEMORY */

extern SCHEME_OBJECT * memory_base;

#define ALLOCATE_HEAP_SPACE(space, low, high) do			\
{									\
  unsigned long _space = (space);					\
  memory_base = ((SCHEME_OBJECT *)					\
		 (HEAP_MALLOC ((sizeof (SCHEME_OBJECT)) * _space)));	\
  (low) = memory_base;							\
  (high) = (memory_base + _space);					\
} while (0)

#define MEMBASE memory_base

/* These use the MEMBASE macro so that C-compiled code can cache
   memory_base locally and use the local version.  */

#ifndef DATUM_TO_ADDRESS
#  define DATUM_TO_ADDRESS(datum) ((SCHEME_OBJECT *) ((datum) + MEMBASE))
#endif

#ifndef ADDRESS_TO_DATUM
#  define ADDRESS_TO_DATUM(address) ((SCHEME_OBJECT) ((address) - MEMBASE))
#endif

#endif /* not HEAP_IN_LOW_MEMORY */

/* Lots of type predicates */

#define FIXNUM_P(object) ((OBJECT_TYPE (object)) == TC_FIXNUM)
#define BIGNUM_P(object) ((OBJECT_TYPE (object)) == TC_BIG_FIXNUM)
#define FLONUM_P(object) ((OBJECT_TYPE (object)) == TC_BIG_FLONUM)
#define COMPLEX_P(object) ((OBJECT_TYPE (object)) == TC_COMPLEX)
#define CHARACTER_P(object) ((OBJECT_TYPE (object)) == TC_CHARACTER)
#define STRING_P(object) ((OBJECT_TYPE (object)) == TC_CHARACTER_STRING)
#define BIT_STRING_P(object) ((OBJECT_TYPE (object)) == TC_BIT_STRING)
#define CELL_P(object) ((OBJECT_TYPE (object)) == TC_CELL)
#define PAIR_P(object) ((OBJECT_TYPE (object)) == TC_LIST)
#define WEAK_PAIR_P(object) ((OBJECT_TYPE (object)) == TC_WEAK_CONS)
#define VECTOR_P(object) ((OBJECT_TYPE (object)) == TC_VECTOR)
#define RECORD_P(object) ((OBJECT_TYPE (object)) == TC_RECORD)
#define BOOLEAN_P(object) (((object) == SHARP_T) || ((object) == SHARP_F))
#define REFERENCE_TRAP_P(object) ((OBJECT_TYPE (object)) == TC_REFERENCE_TRAP)
#define PRIMITIVE_P(object) ((OBJECT_TYPE (object)) == TC_PRIMITIVE)
#define PROMISE_P(object) ((OBJECT_TYPE (object)) == TC_DELAYED)
#define APPARENT_LIST_P(object) ((EMPTY_LIST_P (object)) || (PAIR_P (object)))
#define CONTROL_POINT_P(object) ((OBJECT_TYPE (object)) == TC_CONTROL_POINT)
#define BROKEN_HEART_P(object) ((OBJECT_TYPE (object)) == TC_BROKEN_HEART)
#define RETURN_CODE_P(object) ((OBJECT_TYPE (object)) == TC_RETURN_CODE)
#define EPHEMERON_P(object) ((OBJECT_TYPE (object)) == TC_EPHEMERON)

#define NON_MARKED_VECTOR_P(object)					\
  ((OBJECT_TYPE (object)) == TC_NON_MARKED_VECTOR)

#define SYMBOL_P(object)						\
  ((INTERNED_SYMBOL_P (object)) || (UNINTERNED_SYMBOL_P (object)))

#define INTERNED_SYMBOL_P(object)					\
  ((OBJECT_TYPE (object)) == TC_INTERNED_SYMBOL)

#define UNINTERNED_SYMBOL_P(object)					\
  ((OBJECT_TYPE (object)) == TC_UNINTERNED_SYMBOL)

#define INTEGER_P(object)						\
  (((OBJECT_TYPE (object)) == TC_FIXNUM)				\
   || ((OBJECT_TYPE (object)) == TC_BIG_FIXNUM))

#define REAL_P(object)							\
  (((OBJECT_TYPE (object)) == TC_FIXNUM)				\
   || ((OBJECT_TYPE (object)) == TC_BIG_FIXNUM)				\
   || ((OBJECT_TYPE (object)) == TC_BIG_FLONUM))

#define HUNK3_P(object)							\
  (((OBJECT_TYPE (object)) == TC_HUNK3_A)				\
   || ((OBJECT_TYPE (object)) == TC_HUNK3_B))

#define INTERPRETER_APPLICABLE_P interpreter_applicable_p

#define ENVIRONMENT_P(env)						\
  (((OBJECT_TYPE (env)) == TC_ENVIRONMENT) || (GLOBAL_FRAME_P (env)))

#define EMPTY_LIST_P(object) ((object) == EMPTY_LIST)

/* Memory Operations */

#define MEMORY_REF(obj, i) ((OBJECT_ADDRESS (obj)) [(i)])
#define MEMORY_SET(obj, i, value) ((MEMORY_REF (obj, i)) = (value))
#define MEMORY_LOC(obj, i) (& (MEMORY_REF (obj, i)))

/* Pair Operations */

#define PAIR_CAR_LOC(pair) (MEMORY_LOC ((pair), CONS_CAR))
#define PAIR_CDR_LOC(pair) (MEMORY_LOC ((pair), CONS_CDR))
#define PAIR_CAR(pair) (MEMORY_REF ((pair), CONS_CAR))
#define PAIR_CDR(pair) (MEMORY_REF ((pair), CONS_CDR))
#define SET_PAIR_CAR(pair, car) MEMORY_SET ((pair), CONS_CAR, (car))
#define SET_PAIR_CDR(pair, cdr) MEMORY_SET ((pair), CONS_CDR, (cdr))

/* Vector Operations */

#define VECTOR_LENGTH(v) (OBJECT_DATUM (MEMORY_REF ((v), 0)))

#define SET_VECTOR_LENGTH(v, length)					\
  (MEMORY_SET ((v), 0, (OBJECT_NEW_DATUM ((MEMORY_REF ((v), 0)), (length)))))

#define VECTOR_LOC(v, i) (MEMORY_LOC ((v), ((i) + 1)))
#define VECTOR_REF(v, i) (MEMORY_REF ((v), ((i) + 1)))
#define VECTOR_SET(v, i, object) MEMORY_SET ((v), ((i) + 1), (object))

/* String Operations */

/* Add 1 byte to length to account for '\0' at end of string.
   Add 1 word to length to account for string header word. */
#define STRING_LENGTH_TO_GC_LENGTH(n_chars)				\
  ((BYTES_TO_WORDS ((n_chars) + 1)) + 1)

#define STRING_LENGTH(s)						\
  (OBJECT_DATUM (MEMORY_REF ((s), STRING_LENGTH_INDEX)))

#define SET_STRING_LENGTH(s, n_chars) do				\
{									\
  MEMORY_SET ((s),							\
	      STRING_LENGTH_INDEX,					\
	      (MAKE_OBJECT (0, (n_chars))));				\
  STRING_SET ((s), (n_chars), '\0');					\
} while (0)

/* Subtract 1 to account for the fact that we maintain a '\0'
   at the end of the string. */
#define MAXIMUM_STRING_LENGTH(s)					\
  ((((VECTOR_LENGTH (s)) - 1) * (sizeof (SCHEME_OBJECT))) - 1)

#define SET_MAXIMUM_STRING_LENGTH(s, n_chars)				\
  (SET_VECTOR_LENGTH ((s), (STRING_LENGTH_TO_GC_LENGTH (n_chars))))

#define STRING_LOC(s, i)						\
  (((unsigned char *) (MEMORY_LOC (s, STRING_CHARS))) + (i))

#define STRING_POINTER(s) ((char *) (MEMORY_LOC (s, STRING_CHARS)))
#define STRING_BYTE_PTR(s) ((byte_t *) (MEMORY_LOC (s, STRING_CHARS)))

#define STRING_REF(s, i) (* (STRING_LOC ((s), (i))))
#define STRING_SET(s, i, c) ((* (STRING_LOC ((s), (i)))) = (c))

/* Character Operations */

#define ASCII_LENGTH CHAR_BIT	/* CHAR_BIT in config.h - 8 for unix  */
#define CODE_LENGTH 21
#define BITS_LENGTH 4
#define MIT_ASCII_LENGTH 25

#define CHAR_BITS_META 		0x1
#define CHAR_BITS_CONTROL 	0x2
#define CHAR_BITS_SUPER		0x4
#define CHAR_BITS_HYPER		0x8

#define MAX_ASCII (1UL << ASCII_LENGTH)
#define MAX_CODE (1UL << CODE_LENGTH)
#define MAX_BITS (1UL << BITS_LENGTH)
#define MAX_MIT_ASCII (1UL << MIT_ASCII_LENGTH)

#define MASK_ASCII (MAX_ASCII - 1)
#define CHAR_MASK_CODE (MAX_CODE - 1)
#define CHAR_MASK_BITS (MAX_BITS - 1)
#define MASK_MIT_ASCII (MAX_MIT_ASCII - 1)

#define ASCII_TO_CHAR(ascii) (MAKE_OBJECT (TC_CHARACTER, (ascii)))
#define CHAR_TO_ASCII_P(object) ((OBJECT_DATUM (object)) < MAX_ASCII)
#define CHAR_TO_ASCII(object) ((object) & MASK_ASCII)

#define MAKE_CHAR(bits, code)						\
  (MAKE_OBJECT (TC_CHARACTER,						\
		((((unsigned long) (bits)) << (CODE_LENGTH))		\
		 | ((unsigned long) (code)))))

#define CHAR_BITS(c) (((OBJECT_DATUM (c)) >> CODE_LENGTH) & CHAR_MASK_BITS)
#define CHAR_CODE(c) ((OBJECT_DATUM (c)) & CHAR_MASK_CODE)

/* Fixnum Operations */

#define FIXNUM_ZERO_P(fixnum) ((OBJECT_DATUM (fixnum)) == 0)
#define FIXNUM_NEGATIVE_P(fixnum) (((fixnum) & FIXNUM_SIGN_BIT) != 0)
#define UNSIGNED_FIXNUM_P(x) ((FIXNUM_P (x)) && (!FIXNUM_NEGATIVE_P (x)))
#define FIXNUM_EQUAL_P(x, y) ((OBJECT_DATUM (x)) == (OBJECT_DATUM (y)))
#define FIXNUM_LESS_P(x, y) ((FIXNUM_TO_LONG (x)) < (FIXNUM_TO_LONG (y)))

#define FIXNUM_POSITIVE_P(fixnum)					\
  (! ((FIXNUM_ZERO_P (fixnum)) || (FIXNUM_NEGATIVE_P (fixnum))))

#define UNSIGNED_FIXNUM_TO_LONG(fixnum) ((long) (OBJECT_DATUM (fixnum)))
#define LONG_TO_UNSIGNED_FIXNUM_P(n) ((((unsigned long) (n)) & SIGN_MASK) == 0)

#define LONG_TO_UNSIGNED_FIXNUM(n)					\
  (MAKE_OBJECT (TC_FIXNUM, ((unsigned long) (n))))

#define LONG_TO_FIXNUM_P(n)						\
  (((((unsigned long) (n)) & SIGN_MASK) == 0)				\
   || ((((unsigned long) (n)) & SIGN_MASK) == SIGN_MASK))

#define LONG_TO_FIXNUM(n)						\
  (MAKE_OBJECT (TC_FIXNUM, (((unsigned long) (n)) & DATUM_MASK)))

#define FIXNUM_TO_LONG(fixnum)						\
  ((long)								\
   (((fixnum) ^ FIXNUM_SIGN_BIT)					\
    - ((((unsigned long) TC_FIXNUM) << DATUM_LENGTH) | FIXNUM_SIGN_BIT)))

#define ULONG_TO_FIXNUM_P(n) (((n) & SIGN_MASK) == 0)
#define ULONG_TO_FIXNUM(n) (MAKE_OBJECT (TC_FIXNUM, (n)))
#define FIXNUM_TO_ULONG_P(fixnum) (((OBJECT_DATUM (fixnum)) & SIGN_MASK) == 0)
#define FIXNUM_TO_ULONG(fixnum) (OBJECT_DATUM (fixnum))

#define FIXNUM_TO_DOUBLE(fixnum) ((double) (FIXNUM_TO_LONG (fixnum)))

#define DOUBLE_TO_FIXNUM_P(number)					\
  (((number) > ((double) (SMALLEST_FIXNUM - 1)))			\
   && ((number) < ((double) (BIGGEST_FIXNUM + 1))))

#ifdef HAVE_DOUBLE_TO_LONG_BUG
#  define DOUBLE_TO_FIXNUM double_to_fixnum
#else
#  define DOUBLE_TO_FIXNUM(number) (LONG_TO_FIXNUM ((long) (number)))
#endif

/* Bignum Operations */

#define BIGNUM_ZERO_P(bignum)						\
  ((bignum_test (bignum)) == bignum_comparison_equal)

#define BIGNUM_NEGATIVE_P(bignum)					\
  ((bignum_test (bignum)) == bignum_comparison_less)

#define BIGNUM_POSITIVE_P(bignum)					\
  ((bignum_test (bignum)) == bignum_comparison_greater)

#define BIGNUM_LESS_P(x, y)						\
  ((bignum_compare ((x), (y))) == bignum_comparison_less)

#define BIGNUM_TO_LONG_P(bignum)					\
  (bignum_fits_in_word_p ((bignum), ((sizeof (long)) * CHAR_BIT), 1))

#define BIGNUM_TO_ULONG_P(bignum)					\
  (bignum_fits_in_word_p ((bignum), ((sizeof (unsigned long)) * CHAR_BIT), 0))

#define BIGNUM_TO_INTMAX_P(bignum)					\
  (bignum_fits_in_word_p ((bignum), ((sizeof (intmax_t)) * CHAR_BIT), 1))

#define BIGNUM_TO_UINTMAX_P(bignum)					\
  (bignum_fits_in_word_p ((bignum), ((sizeof (uintmax_t)) * CHAR_BIT), 0))

#define BIGNUM_TO_DOUBLE_P(bignum)					\
  (bignum_fits_in_word_p ((bignum), (DBL_MAX_EXP + 1), 1))

#define LOSSLESS_BIGNUM_TO_DOUBLE_P(bignum)				\
  (bignum_fits_in_word_p ((bignum), (DBL_MANT_DIG + 1), 1))

/* Flonum Operations */

#define FLONUM_SIZE (BYTES_TO_WORDS (sizeof (double)))

#define FLONUM_TO_DOUBLE(object)					\
  (* ((double *) (MEMORY_LOC ((object), 1))))

#define FLOAT_TO_FLONUM(expression)					\
  (double_to_flonum ((double) (expression)))

#define FLONUM_TRUNCATE(object)						\
  (double_to_flonum (double_truncate (FLONUM_TO_DOUBLE (object))))

/* Flonum-vector Operations */

#define FLOATING_VECTOR_LENGTH(vector)					\
  ((VECTOR_LENGTH (vector)) / FLONUM_SIZE)

#define FLOATING_VECTOR_LOC(vector, index)				\
  ((double *) (VECTOR_LOC ((vector), ((index) * FLONUM_SIZE))))

#define FLOATING_VECTOR_REF(vector, index)				\
  (* (FLOATING_VECTOR_LOC ((vector), (index))))

#define FLOATING_VECTOR_SET(vector, index, x)				\
  (* (FLOATING_VECTOR_LOC ((vector), (index)))) = ((double) (x))

/* Numeric Type Conversions */

#define BIGNUM_TO_FIXNUM_P(bignum)					\
  (bignum_fits_in_word_p ((bignum), (FIXNUM_LENGTH + 1), 1))

#define FIXNUM_TO_BIGNUM(fixnum) (long_to_bignum (FIXNUM_TO_LONG (fixnum)))
#define FIXNUM_TO_FLONUM(fixnum) (double_to_flonum (FIXNUM_TO_DOUBLE (fixnum)))
#define BIGNUM_TO_FIXNUM(bignum) (LONG_TO_FIXNUM (bignum_to_long (bignum)))
#define BIGNUM_TO_FLONUM_P BIGNUM_TO_DOUBLE_P
#define BIGNUM_TO_FLONUM(bignum) (double_to_flonum (bignum_to_double (bignum)))
#define FLONUM_TO_BIGNUM(flonum) (double_to_bignum (FLONUM_TO_DOUBLE (flonum)))
#define FLONUM_TO_INTEGER(x) (double_to_integer (FLONUM_TO_DOUBLE (x)))
#define INTEGER_TO_FLONUM_P integer_to_double_p
#define INTEGER_TO_FLONUM(n) (double_to_flonum (integer_to_double (n)))

#define BOOLEAN_TO_OBJECT(expression) ((expression) ? SHARP_T : SHARP_F)
#define OBJECT_TO_BOOLEAN(object) ((object) != SHARP_F)

#define MAKE_BROKEN_HEART(address)					\
  (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, (address)))

#define MAKE_RETURN_CODE(n) (MAKE_OBJECT (TC_RETURN_CODE, (n)))

#define BYTES_TO_WORDS(nbytes)						\
  (((nbytes) + ((sizeof (SCHEME_OBJECT)) - 1)) / (sizeof (SCHEME_OBJECT)))

#define HEAP_ADDRESS_P(address)						\
  (((address) >= heap_start) && ((address) < Free))

#ifndef FLOATING_ALIGNMENT
#  define FLOATING_ALIGNMENT 0
#endif

#define FLOATING_ALIGNED_P(ptr)						\
  ((((unsigned long) ((ptr) + 1)) & FLOATING_ALIGNMENT) == 0)

#define ALIGN_FLOAT(loc) do						\
{									\
  while (!FLOATING_ALIGNED_P (loc))					\
    (*(loc)++) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0));		\
} while (0)

/* Assigned TC_CONSTANT datum values:
   0 #t
   1 unspecific
   2 [non-object]
   3 #!optional
   4 #!rest
   5 #!key
   6 #!eof
   7 #!default
   8 #!aux
   9 '()
 */

#define SHARP_F			MAKE_OBJECT (TC_NULL, 0)
#define SHARP_T			MAKE_OBJECT (TC_CONSTANT, 0)
#define UNSPECIFIC		MAKE_OBJECT (TC_CONSTANT, 1)
#define DEFAULT_OBJECT		MAKE_OBJECT (TC_CONSTANT, 7)
#define EMPTY_LIST		MAKE_OBJECT (TC_CONSTANT, 9)
#define FIXNUM_ZERO		MAKE_OBJECT (TC_FIXNUM, 0)
#define BROKEN_HEART_ZERO	MAKE_OBJECT (TC_BROKEN_HEART, 0)

/* Last immediate reference trap. */
#define TRAP_MAX_IMMEDIATE 9

#endif /* SCM_OBJECT_H */
