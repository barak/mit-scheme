/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/object.h,v 9.35 1990/09/08 00:10:33 cph Exp $

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

/* This file defines the macros which define and manipulate Scheme
   objects.  This is the lowest level of abstraction in this program. */

/* The value in "Wsize.c" for `TYPE_CODE_LENGTH' must match this!! */
#ifndef TYPE_CODE_LENGTH
#define TYPE_CODE_LENGTH 8
#endif

#ifdef MIN_TYPE_CODE_LENGTH
#if (TYPE_CODE_LENGTH < MIN_TYPE_CODE_LENGTH)
#include ";; inconsistency between object.h and types.h: MIN_TYPE_CODE_LENGTH"
#endif
#endif

#ifdef b32			/* 32 bit word versions */
#if (TYPE_CODE_LENGTH == 8)

#define MAX_TYPE_CODE		0xFF
#define DATUM_LENGTH		24
#define FIXNUM_LENGTH		23
#define FIXNUM_SIGN_BIT		0x00800000
#define SIGN_MASK		0xFF800000
#define SMALLEST_FIXNUM		((long) 0xFF800000)
#define BIGGEST_FIXNUM		((long) 0x007FFFFF)
#define HALF_DATUM_LENGTH	12
#define HALF_DATUM_MASK		0x00000FFF
#define DATUM_MASK		0x00FFFFFF
#define TYPE_CODE_MASK		0xFF000000

#endif /* (TYPE_CODE_LENGTH == 8) */
#if (TYPE_CODE_LENGTH == 6)

#define MAX_TYPE_CODE		0x3F
#define DATUM_LENGTH		26
#define FIXNUM_LENGTH		25
#define FIXNUM_SIGN_BIT		0x02000000
#define SIGN_MASK		0xFE000000
#define SMALLEST_FIXNUM		((long) 0xFE000000)
#define BIGGEST_FIXNUM		((long) 0x01FFFFFF)
#define HALF_DATUM_LENGTH	13
#define HALF_DATUM_MASK		0x00001FFF
#define DATUM_MASK		0x03FFFFFF
#define TYPE_CODE_MASK		0XFC000000

#endif /* (TYPE_CODE_LENGTH == 6) */
#endif /* b32 */
#ifndef DATUM_LENGTH		/* Safe versions */

#define MAX_TYPE_CODE		((1 << TYPE_CODE_LENGTH) - 1)
#define DATUM_LENGTH		(OBJECT_LENGTH - TYPE_CODE_LENGTH)
/* FIXNUM_LENGTH does NOT include the sign bit! */
#define FIXNUM_LENGTH		(DATUM_LENGTH - 1)
#define FIXNUM_SIGN_BIT		(1 << FIXNUM_LENGTH)
#define SIGN_MASK		((long) (-1 << FIXNUM_LENGTH))
#define SMALLEST_FIXNUM		((long) (-1 << FIXNUM_LENGTH))
#define BIGGEST_FIXNUM		((1 << FIXNUM_LENGTH) - 1)
#define HALF_DATUM_LENGTH	(DATUM_LENGTH / 2)
#define HALF_DATUM_MASK		((1 << HALF_DATUM_LENGTH) - 1)
#define DATUM_MASK		((1 << DATUM_LENGTH) - 1)
#define TYPE_CODE_MASK		(~ DATUM_MASK)

#endif /* DATUM_LENGTH */

/* Basic object structure */

#ifndef OBJECT_TYPE
#ifdef UNSIGNED_SHIFT_BUG
/* This fixes bug in some compilers. */
#define OBJECT_TYPE(object) (((object) >> DATUM_LENGTH) & MAX_TYPE_CODE)
#else
/* Faster for logical shifts */
#define OBJECT_TYPE(object) ((object) >> DATUM_LENGTH)
#endif
#endif

#define OBJECT_DATUM(object) ((object) & DATUM_MASK)
#define OBJECT_ADDRESS(object) (DATUM_TO_ADDRESS ((object) & DATUM_MASK))

#define MAKE_OBJECT(type, datum)					\
  ((((unsigned int) (type)) << DATUM_LENGTH) | (datum))

#define OBJECT_NEW_DATUM(type_object, datum)				\
  (((type_object) & TYPE_CODE_MASK) | (datum))

#define OBJECT_NEW_TYPE(type, datum_object)				\
  (MAKE_OBJECT ((type), (OBJECT_DATUM (datum_object))))

#define MAKE_OBJECT_FROM_OBJECTS(type_object, datum_object)		\
  (((type_object) & TYPE_CODE_MASK) | ((datum_object) & DATUM_MASK))

#define MAKE_POINTER_OBJECT(type, address)				\
  (MAKE_OBJECT ((type), (ADDRESS_TO_DATUM (address))))

#define OBJECT_NEW_ADDRESS(object, address)				\
  (OBJECT_NEW_DATUM ((object), (ADDRESS_TO_DATUM (address))))

/* Machine dependencies */

#ifdef HEAP_IN_LOW_MEMORY	/* Storing absolute addresses */

typedef long relocation_type;	/* Used to relocate pointers on fasload */

/* The "-1" in the value returned is a guarantee that there is one
   word reserved exclusively for use by the garbage collector. */
#define ALLOCATE_HEAP_SPACE(space)					\
  (Heap =								\
    ((SCHEME_OBJECT *) (malloc ((sizeof (SCHEME_OBJECT)) * (space)))),	\
   ((Heap + (space)) - 1))

#ifndef DATUM_TO_ADDRESS
#define DATUM_TO_ADDRESS(datum) ((SCHEME_OBJECT *) (datum))
#endif

#ifndef ADDRESS_TO_DATUM
#define ADDRESS_TO_DATUM(address) ((SCHEME_OBJECT) (address))
#endif

#else /* not HEAP_IN_LOW_MEMORY (portable version) */

/* Used to relocate pointers on fasload */

typedef SCHEME_OBJECT * relocation_type;

extern SCHEME_OBJECT * memory_base;

/* The "-1" in the value returned is a guarantee that there is one
   word reserved exclusively for use by the garbage collector. */
#define ALLOCATE_HEAP_SPACE(space)					\
  (memory_base =							\
    ((SCHEME_OBJECT *) (malloc ((sizeof (SCHEME_OBJECT)) * (space)))),	\
   Heap = memory_base,							\
   ((memory_base + (space)) - 1))

#ifndef DATUM_TO_ADDRESS
#define DATUM_TO_ADDRESS(datum) ((SCHEME_OBJECT *) ((datum) + memory_base))
#endif

#ifndef ADDRESS_TO_DATUM
#define ADDRESS_TO_DATUM(address) ((SCHEME_OBJECT) ((address) - memory_base))
#endif

#endif /* HEAP_IN_LOW_MEMORY */

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
#define BOOLEAN_P(object) (((object) == SHARP_T) || ((object) == SHARP_F))
#define REFERENCE_TRAP_P(object) ((OBJECT_TYPE (object)) == TC_REFERENCE_TRAP)
#define PRIMITIVE_P(object) ((OBJECT_TYPE (object)) == TC_PRIMITIVE)
#define FUTURE_P(object) ((OBJECT_TYPE (object)) == TC_FUTURE)
#define PROMISE_P(object) ((OBJECT_TYPE (object)) == TC_DELAYED)
#define APPARENT_LIST_P(object) (((object) == EMPTY_LIST) || (PAIR_P (object)))
#define CONTROL_POINT_P(object) ((OBJECT_TYPE (object)) == TC_CONTROL_POINT)
#define BROKEN_HEART_P(object) ((OBJECT_TYPE (object)) == TC_BROKEN_HEART)
#define GC_NON_POINTER_P(object) ((GC_Type (object)) == GC_Non_Pointer)
#define GC_CELL_P(object) ((GC_Type (object)) == GC_Cell)
#define GC_PAIR_P(object) ((GC_Type (object)) == GC_Pair)
#define GC_TRIPLE_P(object) ((GC_Type (object)) == GC_Triple)
#define GC_QUADRUPLE_P(object) ((GC_Type (object)) == GC_Quadruple)
#define GC_VECTOR_P(object) ((GC_Type (object)) == GC_Vector)

#define COMPILED_CODE_ADDRESS_P(object)					\
  ((OBJECT_TYPE (object)) == TC_COMPILED_ENTRY)

#define STACK_ADDRESS_P(object)						\
  ((OBJECT_TYPE (object)) == TC_STACK_ENVIRONMENT)

#define NON_MARKED_VECTOR_P(object)					\
  ((OBJECT_TYPE (object)) == TC_NON_MARKED_VECTOR)

#define SYMBOL_P(object)						\
  (((OBJECT_TYPE (object)) == TC_INTERNED_SYMBOL) ||			\
   ((OBJECT_TYPE (object)) == TC_UNINTERNED_SYMBOL))

#define INTEGER_P(object)						\
  (((OBJECT_TYPE (object)) == TC_FIXNUM) ||				\
   ((OBJECT_TYPE (object)) == TC_BIG_FIXNUM))

#define REAL_P(object)							\
  (((OBJECT_TYPE (object)) == TC_FIXNUM) ||				\
   ((OBJECT_TYPE (object)) == TC_BIG_FIXNUM) ||				\
   ((OBJECT_TYPE (object)) == TC_BIG_FLONUM))

#define NUMBER_P(object)						\
  (((OBJECT_TYPE (object)) == TC_FIXNUM) ||				\
   ((OBJECT_TYPE (object)) == TC_BIG_FIXNUM) ||				\
   ((OBJECT_TYPE (object)) == TC_BIG_FLONUM)				\
   ((OBJECT_TYPE (object)) == TC_COMPLEX))

#define HUNK3_P(object)							\
  (((OBJECT_TYPE (object)) == TC_HUNK3_A) ||				\
   ((OBJECT_TYPE (object)) == TC_HUNK3_B))

#define INTERPRETER_APPLICABLE_P interpreter_applicable_p

#define ENVIRONMENT_P(env)						\
  ((OBJECT_TYPE (env) == TC_ENVIRONMENT) ||				\
   (OBJECT_TYPE (env) == GLOBAL_ENV))

/* Memory Operations */

/* The FAST_ operations are used only where the object is known to be
   immutable.  On a parallel processor they don't require atomic
   references. */

#define FAST_MEMORY_REF(object, offset)					\
  ((OBJECT_ADDRESS (object)) [(offset)])

#define FAST_MEMORY_SET(object, offset, value)				\
  ((OBJECT_ADDRESS (object)) [(offset)]) = (value)

#define MEMORY_LOC(object, offset)					\
  (& ((OBJECT_ADDRESS (object)) [(offset)]))

/* General case memory access requires atomicity for parallel processors. */

#define MEMORY_REF(object, offset)					\
  (MEMORY_FETCH ((OBJECT_ADDRESS (object)) [(offset)]))

#define MEMORY_SET(object, offset, value)				\
  MEMORY_STORE (((OBJECT_ADDRESS (object)) [(offset)]), (value))

/* Pair Operations */

#define FAST_PAIR_CAR(pair) (FAST_MEMORY_REF ((pair), CONS_CAR))
#define FAST_PAIR_CDR(pair) (FAST_MEMORY_REF ((pair), CONS_CDR))
#define FAST_SET_PAIR_CAR(pair, car) FAST_MEMORY_SET ((pair), CONS_CAR, (car))
#define FAST_SET_PAIR_CDR(pair, cdr) FAST_MEMORY_SET ((pair), CONS_CDR, (cdr))
#define PAIR_CAR_LOC(pair) (MEMORY_LOC ((pair), CONS_CAR))
#define PAIR_CDR_LOC(pair) (MEMORY_LOC ((pair), CONS_CDR))

#define PAIR_CAR(pair) (MEMORY_REF ((pair), CONS_CAR))
#define PAIR_CDR(pair) (MEMORY_REF ((pair), CONS_CDR))
#define SET_PAIR_CAR(pair, car) MEMORY_SET ((pair), CONS_CAR, (car))
#define SET_PAIR_CDR(pair, cdr) MEMORY_SET ((pair), CONS_CDR, (cdr))

/* Vector Operations */

#define VECTOR_LENGTH(vector) (OBJECT_DATUM (FAST_MEMORY_REF ((vector), 0)))

#define SET_VECTOR_LENGTH(vector, length)				\
  FAST_MEMORY_SET							\
    ((vector),								\
     0,									\
     (OBJECT_NEW_DATUM ((FAST_MEMORY_REF ((vector), 0)), (length))));

#define FAST_VECTOR_REF(vector, index)					\
  (FAST_MEMORY_REF ((vector), ((index) + 1)))

#define FAST_VECTOR_SET(vector, index, value)				\
  FAST_MEMORY_SET ((vector), ((index) + 1), (value))

#define VECTOR_LOC(vector, index) (MEMORY_LOC ((vector), ((index) + 1)))
#define VECTOR_REF(vector, index) (MEMORY_REF ((vector), ((index) + 1)))

#define VECTOR_SET(vector, index, value)				\
  MEMORY_SET ((vector), ((index) + 1), (value))

/* String Operations */

/* Add 1 byte to length to account for '\0' at end of string.
   Add 1 word to length to account for string header word. */
#define STRING_LENGTH_TO_GC_LENGTH(length)				\
  ((BYTES_TO_WORDS ((length) + 1)) + 1)

#define STRING_LENGTH(string)						\
  ((long) (MEMORY_REF ((string), STRING_LENGTH_INDEX)))

#define SET_STRING_LENGTH(string, length) do				\
{									\
  MEMORY_SET ((string), STRING_LENGTH_INDEX, (length));			\
  STRING_SET ((string), (length), '\0');				\
} while (0)

/* Subtract 1 to account for the fact that we maintain a '\0'
   at the end of the string. */
#define MAXIMUM_STRING_LENGTH(string)					\
  ((long) ((((VECTOR_LENGTH (string)) - 1) * (sizeof (SCHEME_OBJECT))) - 1))

#define SET_MAXIMUM_STRING_LENGTH(string, length)			\
  SET_VECTOR_LENGTH ((string), (STRING_LENGTH_TO_GC_LENGTH (length)))

#define STRING_LOC(string, index)					\
  (((unsigned char *) (MEMORY_LOC (string, STRING_CHARS))) + (index))

#define STRING_REF(string, index)					\
  ((int) (* (STRING_LOC ((string), (index)))))

#define STRING_SET(string, index, c_char)				\
  (* (STRING_LOC ((string), (index)))) = (c_char)

/* Character Operations */

#define ASCII_LENGTH CHAR_BIT	/* CHAR_BIT in config.h - 8 for unix  */
#define CODE_LENGTH 7
#define BITS_LENGTH 5
#define MIT_ASCII_LENGTH 12

#define CHAR_BITS_META 		01
#define CHAR_BITS_CONTROL 	02
#define CHAR_BITS_CONTROL_META	03

#define MAX_ASCII (1 << ASCII_LENGTH)
#define MAX_CODE (1 << CODE_LENGTH)
#define MAX_BITS (1 << BITS_LENGTH)
#define MAX_MIT_ASCII (1 << MIT_ASCII_LENGTH)

#define MASK_ASCII (MAX_ASCII - 1)
#define CHAR_MASK_CODE (MAX_CODE - 1)
#define CHAR_MASK_BITS (MAX_BITS - 1)
#define MASK_MIT_ASCII (MAX_MIT_ASCII - 1)

#define ASCII_TO_CHAR(ascii) (MAKE_OBJECT (TC_CHARACTER, (ascii)))
#define CHAR_TO_ASCII_P(object) ((OBJECT_DATUM (object)) < MAX_ASCII)
#define CHAR_TO_ASCII(object) ((object) & MASK_ASCII)

#define MAKE_CHAR(bucky_bits, code)					\
  (MAKE_OBJECT								\
   (TC_CHARACTER,							\
    (((unsigned long) (bucky_bits)) << (CODE_LENGTH)) | (code)))

#define CHAR_BITS(chr)						\
  ((((unsigned long) (OBJECT_DATUM (chr))) >> CODE_LENGTH) & CHAR_MASK_BITS)

#define CHAR_CODE(chr) ((OBJECT_DATUM (chr)) & CHAR_MASK_CODE)

/* Fixnum Operations */

#define FIXNUM_ZERO_P(fixnum) ((OBJECT_DATUM (fixnum)) == 0)
#define FIXNUM_NEGATIVE_P(fixnum) (((fixnum) & FIXNUM_SIGN_BIT) != 0)
#define UNSIGNED_FIXNUM_P(x) ((FIXNUM_P (x)) && (! (FIXNUM_NEGATIVE_P (x))))
#define FIXNUM_EQUAL_P(x, y) ((OBJECT_DATUM (x)) == (OBJECT_DATUM (y)))
#define FIXNUM_LESS_P(x, y) ((FIXNUM_TO_LONG (x)) < (FIXNUM_TO_LONG (y)))

#define FIXNUM_POSITIVE_P(fixnum)					\
  (! ((FIXNUM_ZERO_P (fixnum)) || (FIXNUM_NEGATIVE_P (fixnum))))

#define UNSIGNED_FIXNUM_TO_LONG(fixnum) ((long) (OBJECT_DATUM (fixnum)))
#define LONG_TO_UNSIGNED_FIXNUM_P(value) (((value) & SIGN_MASK) == 0)
#define LONG_TO_UNSIGNED_FIXNUM(value) (FIXNUM_ZERO + (value))
#define LONG_TO_FIXNUM(value) (OBJECT_NEW_TYPE (TC_FIXNUM, (value)))

#define LONG_TO_FIXNUM_P(value)						\
  ((((value) & SIGN_MASK) == 0) || (((value) & SIGN_MASK) == SIGN_MASK))

#define FIXNUM_TO_LONG(fixnum)						\
  ((((long) (fixnum)) ^ ((long) FIXNUM_SIGN_BIT))			\
   - ((long) ((TC_FIXNUM << DATUM_LENGTH) | FIXNUM_SIGN_BIT)))

#define FIXNUM_TO_DOUBLE(fixnum) ((double) (FIXNUM_TO_LONG (fixnum)))

#define DOUBLE_TO_FIXNUM_P(number)					\
  (((number) > (((double) SMALLEST_FIXNUM) - 0.5)) &&			\
   ((number) < (((double) BIGGEST_FIXNUM) + 0.5)))

#ifdef HAVE_DOUBLE_TO_LONG_BUG
#define DOUBLE_TO_FIXNUM double_to_fixnum
#else
#define DOUBLE_TO_FIXNUM(number) (LONG_TO_FIXNUM ((long) (number)))
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

/* If precision should not be lost,
   compare to DBL_MANT_DIG instead. */
#define BIGNUM_TO_DOUBLE_P(bignum)					\
  (bignum_fits_in_word_p ((bignum), DBL_MAX_EXP, 0))

/* Flonum Operations */

#define FLONUM_TO_DOUBLE(object)					\
  (* ((double *) (MEMORY_LOC ((object), 1))))

#define FLOAT_TO_FLONUM(expression)					\
  (double_to_flonum ((double) (expression)))

#define FLONUM_TRUNCATE(object)						\
  (double_to_flonum (double_truncate (FLONUM_TO_DOUBLE (object))))

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
  (BROKEN_HEART_ZERO + (ADDRESS_TO_DATUM (address)))

#define BYTES_TO_WORDS(nbytes)						\
  (((nbytes) + ((sizeof (SCHEME_OBJECT)) - 1)) / (sizeof (SCHEME_OBJECT)))

#define ADDRESS_CONSTANT_P(address)					\
  (((address) >= Constant_Space) && ((address) < Free_Constant))

#define ADDRESS_PURE_P(address)						\
  ((ADDRESS_CONSTANT_P (address)) && (Pure_Test (address)))

#define SIDE_EFFECT_IMPURIFY(Old_Pointer, Will_Contain)			\
if ((ADDRESS_CONSTANT_P (OBJECT_ADDRESS (Old_Pointer))) &&		\
    (GC_Type (Will_Contain) != GC_Non_Pointer) &&			\
    (! (ADDRESS_CONSTANT_P (OBJECT_ADDRESS (Will_Contain)))) &&		\
    (Pure_Test (OBJECT_ADDRESS (Old_Pointer))))				\
  signal_error_from_primitive (ERR_WRITE_INTO_PURE_SPACE);		\

#ifdef FLOATING_ALIGNMENT

#define FLOATING_BUFFER_SPACE						\
  ((FLOATING_ALIGNMENT + 1) / (sizeof (SCHEME_OBJECT)))

#define HEAP_BUFFER_SPACE						\
  (TRAP_MAX_IMMEDIATE + 1 + FLOATING_BUFFER_SPACE)

/* The space is there, find the correct position. */

#define INITIAL_ALIGN_FLOAT(Where)					\
{									\
  while ((((long) ((Where) + 1)) & FLOATING_ALIGNMENT) != 0)		\
    Where -= 1;								\
}

#define ALIGN_FLOAT(Where)						\
{									\
  while ((((long) ((Where) + 1)) & FLOATING_ALIGNMENT) != 0)		\
    *Where++ = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0));		\
}

#else not FLOATING_ALIGNMENT

#define HEAP_BUFFER_SPACE		 (TRAP_MAX_IMMEDIATE + 1)

#define INITIAL_ALIGN_FLOAT(Where)
#define ALIGN_FLOAT(Where)

#endif FLOATING_ALIGNMENT
