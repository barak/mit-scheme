/* -*-C-*-

$Id: bitstr.h,v 1.8 1992/08/29 12:57:38 jinx Exp $

Copyright (c) 1987-1992 Massachusetts Institute of Technology

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

/* Bit string macros.  "Little indian" version. */

#define BIT_STRING_LENGTH_OFFSET	1
#define BIT_STRING_FIRST_WORD		2

#define BIT_STRING_LENGTH_TO_GC_LENGTH(bits)				\
  (((bits) + (OBJECT_LENGTH - 1)) / OBJECT_LENGTH)

#define LOW_MASK(nbits) ((1L << (nbits)) - 1)
#define ANY_MASK(nbits, offset) ((LOW_MASK (nbits)) << (offset))

#define BIT_STRING_LENGTH(bit_string)					\
  ((long) (FAST_MEMORY_REF ((bit_string), BIT_STRING_LENGTH_OFFSET)))

#define BIT_STRING_MSW(bit_string)					\
  (BIT_STRING_WORD (BIT_STRING_HIGH_PTR (bit_string)))

#define BIT_STRING_LSW(bit_string)					\
  (BIT_STRING_WORD							\
   (MEMORY_LOC								\
    ((bit_string), (BIT_STRING_INDEX_TO_WORD ((bit_string), 0)))))

/* Byte order dependencies. */

#ifdef VAX_BYTE_ORDER

/*

Memory layout of bit strings:

+-------+-------+-------+-------+
|  NMV	|  GC size (longwords)	| 0
+-------+-------+-------+-------+
|	   Size in bits		| 1
+-------+-------+-------+-------+
|			     LSB| 2
+-------+-------+-------+-------+
|				| 3
+-------+-------+-------+-------+
.				. .
.				. .
.				. .
+-------+-------+-------+-------+
|MSB			        | N
+-------+-------+-------+-------+

The last data word (marked as word "N" above) is where any excess
bits are kept.

The "size in bits" is a C "long" integer.

*/

#define BIT_STRING_HIGH_PTR(bit_string)					\
  (MEMORY_LOC ((bit_string), ((VECTOR_LENGTH (bit_string)) + 1)))

#define BIT_STRING_LOW_PTR(bit_string)					\
  (MEMORY_LOC ((bit_string), BIT_STRING_FIRST_WORD))

#define BIT_STRING_WORD(ptr)		(*((ptr) - 1))
#define DEC_BIT_STRING_PTR(ptr)		(--(ptr))
#define INC_BIT_STRING_PTR(ptr)		((ptr)++)
 
/* This is off by one so BIT_STRING_WORD will get the correct word. */

#define BIT_STRING_INDEX_TO_WORD(bit_string, index)			\
  ((BIT_STRING_FIRST_WORD + 1) + ((index) / OBJECT_LENGTH))

#define BIT_STRING_INDEX_PAIR_TO_INDEX(string, word, bit)		\
  ((((word) - (BIT_STRING_FIRST_WORD + 1)) * OBJECT_LENGTH) + (bit))

#define READ_BITS_PTR(object, offset, end)				\
  (MEMORY_LOC								\
   ((object), (BIT_STRING_LENGTH_TO_GC_LENGTH (((offset) + (end)) - 1))))

#define COMPUTE_READ_BITS_OFFSET(offset, end)				\
{									\
  offset = ((offset + end) % OBJECT_LENGTH);				\
  if (offset != 0)							\
    offset = (OBJECT_LENGTH - offset);					\
}

#else /* not VAX_BYTE_ORDER */

/*

Memory layout of bit strings:

+-------+-------+-------+-------+
|  NMV	|  GC size (longwords)	| 0
+-------+-------+-------+-------+
|	   Size in bits		| 1
+-------+-------+-------+-------+
|MSB				| 2
+-------+-------+-------+-------+
|				| 3
+-------+-------+-------+-------+
.				. .
.				. .
.				. .
+-------+-------+-------+-------+
|			     LSB| N
+-------+-------+-------+-------+

The first data word (marked as word "2" above) is where any excess
bits are kept.

The "size in bits" is a C "long" integer.
*/

#define BIT_STRING_HIGH_PTR(bit_string)					\
  (MEMORY_LOC ((bit_string), BIT_STRING_FIRST_WORD))

#define BIT_STRING_LOW_PTR(bit_string)					\
  (MEMORY_LOC ((bit_string), ((VECTOR_LENGTH (bit_string)) + 1)))

#define BIT_STRING_WORD(ptr)		(*(ptr))
#define DEC_BIT_STRING_PTR(ptr)		((ptr)++)
#define INC_BIT_STRING_PTR(ptr)		(--(ptr))

/* This is especially clever.  To understand it, note that the index
   of the last pointer of a vector is also the GC length of the
   vector, so that all we need do is subtract the zero-based word
   index from the GC length. */
#define BIT_STRING_INDEX_TO_WORD(bit_string, index)			\
  ((VECTOR_LENGTH (bit_string)) - ((index) / OBJECT_LENGTH))

#define BIT_STRING_INDEX_PAIR_TO_INDEX(string, word, bit)		\
  ((((VECTOR_LENGTH (string)) - (word)) * OBJECT_LENGTH) + (bit))

#define READ_BITS_PTR(object, offset, end)				\
  (MEMORY_LOC ((object), ((offset) / OBJECT_LENGTH)))

#define COMPUTE_READ_BITS_OFFSET(offset, end)				\
  (offset) = ((offset) % OBJECT_LENGTH);

#endif /* VAX_BYTE_ORDER */
