/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

#define N_TYPE_CODES (0x40)
#if (SIZEOF_UNSIGNED_LONG == 4) /* 32 bit word versions */
#  define DATUM_LENGTH		(26U)
#  define DATUM_MASK		(0x03FFFFFFL)
#  define TYPE_CODE_MASK	(0XFC000000L)
#  define FIXNUM_LENGTH		(25U) /* doesn't include sign */
#  define FIXNUM_MASK		(0x01FFFFFFL)
#  define FIXNUM_SIGN_BIT	(0x02000000L)
#  define SIGN_MASK		(0xFE000000L)
#  define SMALLEST_FIXNUM	(-33554432L)
#  define BIGGEST_FIXNUM	(33554431L)
#  define HALF_DATUM_LENGTH	(13U)
#  define HALF_DATUM_MASK	(0x00001FFFL)
#endif
#if (SIZEOF_UNSIGNED_LONG == 8) /* 64 bit word versions */
#  define DATUM_LENGTH		(58U)
#  define DATUM_MASK		(0x03FFFFFFFFFFFFFFL)
#  define TYPE_CODE_MASK	(0XFC00000000000000L)
#  define FIXNUM_LENGTH		(57U) /* doesn't include sign */
#  define FIXNUM_MASK		(0x01FFFFFFFFFFFFFFL)
#  define FIXNUM_SIGN_BIT	(0x0200000000000000L)
#  define SIGN_MASK		(0xFE00000000000000L)
#  define SMALLEST_FIXNUM	(-144115188075855872L)
#  define BIGGEST_FIXNUM	(144115188075855871L)
#  define HALF_DATUM_LENGTH	(29U)
#  define HALF_DATUM_MASK	(0x000000001FFFFFFFL)
#endif

/* Machine dependencies */

#ifndef HEAP_MALLOC
#  define HEAP_MALLOC OS_malloc_init
#endif

#ifdef HEAP_IN_LOW_MEMORY	/* Storing absolute addresses */

  static inline void
  allocate_heap_space (size_t size, SCHEME_OBJECT** low_r,
                       SCHEME_OBJECT** high_r)
  {
    SCHEME_OBJECT* low
      = (SCHEME_OBJECT*) HEAP_MALLOC (sizeof (SCHEME_OBJECT) * size);
    *low_r = low;
    *high_r = low + size;
  }

  static inline SCHEME_OBJECT*
  datum_to_address (unsigned long datum)
  {
    return (SCHEME_OBJECT*) datum;
  }

  static inline unsigned long
  address_to_datum (SCHEME_OBJECT* address)
  {
    return (unsigned long) address;
  }

#else

  extern SCHEME_OBJECT* memory_base;

  static inline void
  allocate_heap_space (size_t size, SCHEME_OBJECT** low_r,
                       SCHEME_OBJECT** high_r)
  {
    memory_base = (SCHEME_OBJECT*) HEAP_MALLOC (sizeof (SCHEME_OBJECT) * size);
    *low_r = memory_base;
    *high_r = memory_base + size;
  }

  #define MEMBASE memory_base

  /* These use the MEMBASE macro so that C-compiled code can cache
     memory_base locally and use the local version.  */

  static inline SCHEME_OBJECT*
  datum_to_address (unsigned long datum)
  {
    return MEMBASE + datum;
  }

  static inline unsigned long
  address_to_datum (SCHEME_OBJECT* address)
  {
    return address - MEMBASE;
  }

#endif

/* Basic object structure */

static inline unsigned long
object_type (SCHEME_OBJECT object)
{
  return ((unsigned long) object) >> DATUM_LENGTH;
}

static inline unsigned long
object_datum (SCHEME_OBJECT object)
{
  return ((unsigned long) object) & DATUM_MASK;
}

static inline SCHEME_OBJECT*
object_address (SCHEME_OBJECT object)
{
  return datum_to_address (object_datum (object));
}

static inline SCHEME_OBJECT
make_object (unsigned long type, unsigned long datum)
{
  return type << DATUM_LENGTH | datum;
}

static inline SCHEME_OBJECT
object_new_type (unsigned long type, SCHEME_OBJECT datum_obj)
{
  return make_object (type, object_datum (datum_obj));
}

static inline SCHEME_OBJECT
object_new_datum (SCHEME_OBJECT type_obj, unsigned long datum)
{
  return make_object (object_type (type_obj), datum);
}

static inline SCHEME_OBJECT
make_object_from_objects (SCHEME_OBJECT type_obj, SCHEME_OBJECT datum_obj)
{
  return make_object (object_type (type_obj), object_datum (datum_obj));
}

static inline SCHEME_OBJECT
make_pointer_object (unsigned long type, SCHEME_OBJECT* address)
{
  return make_object (type, address_to_datum (address));
}

static inline SCHEME_OBJECT
object_new_address (SCHEME_OBJECT type_obj, SCHEME_OBJECT* address)
{
  return object_new_datum (type_obj, address_to_datum (address));
}

// Important constant objects

/* Assigned TC_CONSTANT datum values:
   0 #t
   1 unspecific
   2 [non-object]
   3 #!optional
   4 #!rest
   5 #!key
   6 #!eof
   7 #!default
   8 unused (was #!aux)
   9 '()
   10 #!reclaimed
   ...
   0x100 -> 0x1FF reserved for fasdumpable records
 */

#define CONSTANT_OBJECT(tc, datum)                                     \
  (((unsigned long) tc) << DATUM_LENGTH | datum)

#define SHARP_F			CONSTANT_OBJECT (TC_FALSE, 0)
#define SHARP_T			CONSTANT_OBJECT (TC_CONSTANT, 0)
#define UNSPECIFIC		CONSTANT_OBJECT (TC_CONSTANT, 1)
#define OPTIONAL_MARKER		CONSTANT_OBJECT (TC_CONSTANT, 3)
#define REST_MARKER		CONSTANT_OBJECT (TC_CONSTANT, 4)
#define KEY_MARKER		CONSTANT_OBJECT (TC_CONSTANT, 5)
#define EOF_OBJECT		CONSTANT_OBJECT (TC_CONSTANT, 6)
#define DEFAULT_OBJECT		CONSTANT_OBJECT (TC_CONSTANT, 7)
#define EMPTY_LIST		CONSTANT_OBJECT (TC_CONSTANT, 9)
#define GC_RECLAIMED		CONSTANT_OBJECT (TC_CONSTANT, 10)
#define FASDUMP_RECORD_MARKER_START 0x100
#define FASDUMP_RECORD_MARKER_END 0x200
#define BROKEN_HEART_ZERO	CONSTANT_OBJECT (TC_BROKEN_HEART, 0)

/* An environment chain always ends in a pointer with type code
   of TC_GLOBAL_ENV.  This will contain an address part which
   either indicates that the lookup should continue on to the
   true global environment, or terminate at this frame.

   We arrange for the global environment to be the same as #F, and the
   end chain to be different by toggling the lowest bit:  */

#define THE_GLOBAL_ENV		CONSTANT_OBJECT (TC_GLOBAL_ENV, 0)
#define THE_NULL_ENV		CONSTANT_OBJECT (TC_GLOBAL_ENV, 1)

/* Lots of type predicates */

#define DEFINE_SIMPLE_TYPE_PRED(name, tc)                               \
static inline bool                                                      \
name (SCHEME_OBJECT object)                                             \
{                                                                       \
  return object_type (object) == tc;                                    \
}

DEFINE_SIMPLE_TYPE_PRED (BIGNUM_P, TC_BIG_FIXNUM)
DEFINE_SIMPLE_TYPE_PRED (BIT_STRING_P, TC_BIT_STRING)
DEFINE_SIMPLE_TYPE_PRED (BROKEN_HEART_P, TC_BROKEN_HEART)
DEFINE_SIMPLE_TYPE_PRED (BYTEVECTOR_P, TC_BYTEVECTOR)
DEFINE_SIMPLE_TYPE_PRED (CELL_P, TC_CELL)
DEFINE_SIMPLE_TYPE_PRED (CHARACTER_P, TC_CHARACTER)
DEFINE_SIMPLE_TYPE_PRED (COMPLEX_P, TC_COMPLEX)
DEFINE_SIMPLE_TYPE_PRED (CONTROL_POINT_P, TC_CONTROL_POINT)
DEFINE_SIMPLE_TYPE_PRED (EPHEMERON_P, TC_EPHEMERON)
DEFINE_SIMPLE_TYPE_PRED (FIXNUM_P, TC_FIXNUM)
DEFINE_SIMPLE_TYPE_PRED (FLONUM_P, TC_BIG_FLONUM)
DEFINE_SIMPLE_TYPE_PRED (INTERNED_SYMBOL_P, TC_INTERNED_SYMBOL)
DEFINE_SIMPLE_TYPE_PRED (LEGACY_STRING_P, TC_CHARACTER_STRING)
DEFINE_SIMPLE_TYPE_PRED (nmv_header_p, TC_MANIFEST_NM_VECTOR)
DEFINE_SIMPLE_TYPE_PRED (NON_MARKED_VECTOR_P, TC_NON_MARKED_VECTOR)
DEFINE_SIMPLE_TYPE_PRED (PAIR_P, TC_LIST)
DEFINE_SIMPLE_TYPE_PRED (PRIMITIVE_P, TC_PRIMITIVE)
DEFINE_SIMPLE_TYPE_PRED (PROCEDURE_FRAME_P, TC_ENVIRONMENT)
DEFINE_SIMPLE_TYPE_PRED (PROMISE_P, TC_DELAYED)
DEFINE_SIMPLE_TYPE_PRED (RECORD_P, TC_RECORD)
DEFINE_SIMPLE_TYPE_PRED (REFERENCE_TRAP_P, TC_REFERENCE_TRAP)
DEFINE_SIMPLE_TYPE_PRED (RETURN_CODE_P, TC_RETURN_CODE)
DEFINE_SIMPLE_TYPE_PRED (TAGGED_OBJECT_P, TC_TAGGED_OBJECT)
DEFINE_SIMPLE_TYPE_PRED (UNICODE_STRING_P, TC_UNICODE_STRING)
DEFINE_SIMPLE_TYPE_PRED (UNINTERNED_SYMBOL_P, TC_UNINTERNED_SYMBOL)
DEFINE_SIMPLE_TYPE_PRED (vector_header_p, TC_MANIFEST_VECTOR)
DEFINE_SIMPLE_TYPE_PRED (VECTOR_P, TC_VECTOR)
DEFINE_SIMPLE_TYPE_PRED (WEAK_PAIR_P, TC_WEAK_CONS)

#undef DEFINE_SIMPLE_TYPE_PRED

#define STRING_P string_p
extern bool string_p (SCHEME_OBJECT);

static inline bool
BOOLEAN_P (SCHEME_OBJECT object)
{
  return object == SHARP_T || object == SHARP_F;
}

static inline bool
SYMBOL_P (SCHEME_OBJECT object)
{
  return INTERNED_SYMBOL_P (object) || UNINTERNED_SYMBOL_P (object);
}

static inline bool
INTEGER_P (SCHEME_OBJECT object)
{
  return FIXNUM_P (object) || BIGNUM_P (object);
}

static inline bool
REAL_P (SCHEME_OBJECT object)
{
  return INTEGER_P (object) || FLONUM_P (object);
}

static inline bool
HUNK3_P (SCHEME_OBJECT object)
{
  return object_type (object) == TC_HUNK3_A
         || object_type (object) == TC_HUNK3_B;
}

static inline bool
GLOBAL_FRAME_P (SCHEME_OBJECT object)
{
  return object == THE_GLOBAL_ENV;
}

static inline bool
NULL_FRAME_P (SCHEME_OBJECT object)
{
  return object == THE_NULL_ENV;
}

static inline bool
ENVIRONMENT_P (SCHEME_OBJECT object)
{
  return PROCEDURE_FRAME_P (object) || GLOBAL_FRAME_P (object);
}

static inline bool
EMPTY_LIST_P (SCHEME_OBJECT object)
{
  return object == EMPTY_LIST;
}

/* Memory Operations */

static inline SCHEME_OBJECT
memory_ref (SCHEME_OBJECT obj, unsigned long index)
{
  return object_address (obj) [index];
}

static inline SCHEME_OBJECT*
memory_loc (SCHEME_OBJECT obj, unsigned long index)
{
  return object_address (obj) + index;
}

static inline void
memory_set (SCHEME_OBJECT obj, unsigned long index, SCHEME_OBJECT val)
{
  object_address (obj) [index] = val;
}

static inline SCHEME_OBJECT
memory_ref_0 (SCHEME_OBJECT obj)
{
  return memory_ref (obj, 0);
}

static inline SCHEME_OBJECT
memory_ref_1 (SCHEME_OBJECT obj)
{
  return memory_ref (obj, 1);
}

static inline SCHEME_OBJECT
memory_ref_2 (SCHEME_OBJECT obj)
{
  return memory_ref (obj, 2);
}

static inline SCHEME_OBJECT*
memory_loc_0 (SCHEME_OBJECT obj)
{
  return memory_loc (obj, 0);
}

static inline SCHEME_OBJECT*
memory_loc_1 (SCHEME_OBJECT obj)
{
  return memory_loc (obj, 1);
}

static inline SCHEME_OBJECT*
memory_loc_2 (SCHEME_OBJECT obj)
{
  return memory_loc (obj, 2);
}

static inline void
memory_set_0 (SCHEME_OBJECT obj, SCHEME_OBJECT val)
{
  memory_set (obj, 0, val);
}

static inline void
memory_set_1 (SCHEME_OBJECT obj, SCHEME_OBJECT val)
{
  memory_set (obj, 1, val);
}

static inline void
memory_set_2 (SCHEME_OBJECT obj, SCHEME_OBJECT val)
{
  memory_set (obj, 2, val);
}

/* Character Operations */

#define ASCII_LENGTH CHAR_BIT	/* CHAR_BIT in config.h - 8 for unix  */
#define CODE_LENGTH 21
#define BITS_LENGTH 4
#define MIT_ASCII_LENGTH 25

#define CHAR_BITS_META		0x1
#define CHAR_BITS_CONTROL	0x2
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

#define ASCII_TO_CHAR(ascii) (make_object (TC_CHARACTER, (ascii)))
#define CHAR_TO_ASCII_P(object) ((object_datum (object)) < MAX_ASCII)
#define CHAR_TO_ASCII(object) ((object) & MASK_ASCII)

#define MAKE_CHAR(bits, code)						\
  (make_object (TC_CHARACTER,						\
		((((unsigned long) (bits)) << (CODE_LENGTH))		\
		 | ((unsigned long) (code)))))

#define CHAR_BITS(c) (((object_datum (c)) >> CODE_LENGTH) & CHAR_MASK_BITS)
#define CHAR_CODE(c) ((object_datum (c)) & CHAR_MASK_CODE)

/* Fixnum Operations */

#define FIXNUM_ZERO_P(fixnum) ((object_datum (fixnum)) == 0)
#define FIXNUM_NEGATIVE_P(fixnum) (((fixnum) & FIXNUM_SIGN_BIT) != 0)
#define UNSIGNED_FIXNUM_P(x) ((FIXNUM_P (x)) && (!FIXNUM_NEGATIVE_P (x)))
#define FIXNUM_EQUAL_P(x, y) ((object_datum (x)) == (object_datum (y)))
#define FIXNUM_LESS_P(x, y) ((FIXNUM_TO_LONG (x)) < (FIXNUM_TO_LONG (y)))

#define FIXNUM_POSITIVE_P(fixnum)					\
  (! ((FIXNUM_ZERO_P (fixnum)) || (FIXNUM_NEGATIVE_P (fixnum))))

#define UNSIGNED_FIXNUM_TO_LONG(fixnum) ((long) (object_datum (fixnum)))
#define LONG_TO_UNSIGNED_FIXNUM_P(n) ((((unsigned long) (n)) & SIGN_MASK) == 0)

#define LONG_TO_UNSIGNED_FIXNUM(n)					\
  (make_object (TC_FIXNUM, ((unsigned long) (n))))

#define LONG_TO_FIXNUM_P(n)						\
  (((((unsigned long) (n)) & SIGN_MASK) == 0)				\
   || ((((unsigned long) (n)) & SIGN_MASK) == SIGN_MASK))

#define LONG_TO_FIXNUM(n)						\
  (make_object (TC_FIXNUM, (((unsigned long) (n)) & DATUM_MASK)))

#define FIXNUM_TO_LONG(fixnum)						\
  ((long)								\
   (((fixnum) ^ FIXNUM_SIGN_BIT)					\
    - ((((unsigned long) TC_FIXNUM) << DATUM_LENGTH) | FIXNUM_SIGN_BIT)))

#define ULONG_TO_FIXNUM_P(n) (((n) & SIGN_MASK) == 0)
#define ULONG_TO_FIXNUM(n) (make_object (TC_FIXNUM, (n)))
#define FIXNUM_TO_ULONG_P(fixnum) (((object_datum (fixnum)) & SIGN_MASK) == 0)
#define FIXNUM_TO_ULONG(fixnum) (object_datum (fixnum))

#define FIXNUM_ZERO (ULONG_TO_FIXNUM (0))

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
  (* ((double *) (memory_loc ((object), 1))))

#define FLOAT_TO_FLONUM(expression)					\
  (double_to_flonum ((double) (expression)))

#define FLONUM_TRUNCATE(object)						\
  (double_to_flonum (double_truncate (FLONUM_TO_DOUBLE (object))))

/* Flonum-vector Operations */

#define FLOATING_VECTOR_LENGTH(vector)					\
  ((vector_length (vector)) / FLONUM_SIZE)

#define FLOATING_VECTOR_LOC(vector, index)				\
  ((double *) (vector_loc ((vector), ((index) * FLONUM_SIZE))))

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
  (make_pointer_object (TC_BROKEN_HEART, (address)))

#define MAKE_RETURN_CODE(n) (make_object (TC_RETURN_CODE, (n)))

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
    (*(loc)++) = (make_nmv_header (0));		\
} while (0)

/* Last immediate reference trap. */
#define TRAP_MAX_IMMEDIATE 9

#endif /* SCM_OBJECT_H */
