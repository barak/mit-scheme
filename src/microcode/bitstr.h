/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

/* Bit string macros.  "Little indian" version. */

#define BIT_STRING_LENGTH_OFFSET	1
#define BIT_STRING_FIRST_WORD		2

#define BIT_STRING_LENGTH_TO_GC_LENGTH(bits)				\
  (((bits) + (OBJECT_LENGTH - 1)) / OBJECT_LENGTH)

#define LOW_MASK(nbits) ((1L << (nbits)) - 1)
#define ANY_MASK(nbits, offset) ((LOW_MASK (nbits)) << (offset))

#define BIT_STRING_LENGTH(bit_string)					\
  ((long) (MEMORY_REF ((bit_string), BIT_STRING_LENGTH_OFFSET)))

#define BIT_STRING_MSW(bit_string)					\
  (BIT_STRING_WORD (BIT_STRING_HIGH_PTR (bit_string)))

#define BIT_STRING_LSW(bit_string)					\
  (BIT_STRING_WORD							\
   (MEMORY_LOC								\
    ((bit_string), (BIT_STRING_INDEX_TO_WORD ((bit_string), 0)))))

/* Byte order dependencies. */

#ifndef WORDS_BIGENDIAN

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

#else /* WORDS_BIGENDIAN */

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

#endif /* WORDS_BIGENDIAN */
