/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/bitstr.h,v 1.4 1987/08/17 19:32:28 jinx Rel $

Copyright (c) 1987 Massachusetts Institute of Technology

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

#define bits_to_pointers(bits)						\
(((bits) + (POINTER_LENGTH - 1)) / POINTER_LENGTH)

#define low_mask(nbits) ((1 << (nbits)) - 1)
#define any_mask(nbits, offset) ((low_mask (nbits)) << (offset))

#define bit_string_length(bit_string)					\
(Fast_Vector_Ref (bit_string, BIT_STRING_LENGTH_OFFSET))

#define bit_string_start_ptr(bit_string)				\
(Nth_Vector_Loc (bit_string, BIT_STRING_FIRST_WORD))

#define bit_string_end_ptr(bit_string)					\
(Nth_Vector_Loc (bit_string, ((Vector_Length (bit_string)) + 1)))

#define bit_string_msw(bit_string)					\
(bit_string_word(bit_string_high_ptr(bit_string)))

#define bit_string_lsw(bit_string)					\
(bit_string_word(Nth_Vector_Loc(bit_string, index_to_word(bit_string, 0))))

#define index_pair_to_bit_fixnum(string, word, bit)			\
(Make_Unsigned_Fixnum (index_pair_to_bit_number (string, word, bit)))

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

#define bit_string_high_ptr		bit_string_end_ptr

#define bit_string_low_ptr		bit_string_start_ptr

#define bit_string_word(ptr)		(*((ptr) - 1))

#define dec_bit_string_ptr(ptr)		(--(ptr))

#define inc_bit_string_ptr(ptr)		((ptr)++)

/* This is off by one so bit_string_word will get the correct word. */

#define index_to_word(bit_string, index)				\
((BIT_STRING_FIRST_WORD + 1) + (index / POINTER_LENGTH))

#define index_pair_to_bit_number(string, word, bit)			\
(((word) * POINTER_LENGTH) + (bit))

#define read_bits_ptr(object, offset, end)				\
(Nth_Vector_Loc(object, bits_to_pointers((offset + end) - 1)))

#define compute_read_bits_offset(offset, end)				\
{									\
  offset = ((offset + end) % POINTER_LENGTH);				\
  if (offset != 0)							\
    offset = (POINTER_LENGTH - offset);					\
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

#define bit_string_high_ptr		bit_string_start_ptr

#define bit_string_low_ptr		bit_string_end_ptr

#define bit_string_word(ptr)		(*(ptr))

#define dec_bit_string_ptr(ptr)		((ptr)++)

#define inc_bit_string_ptr(ptr)		(--(ptr))

/* This is especially clever.  To understand it, note that the index
   of the last pointer of a vector is also the GC length of the
   vector, so that all we need do is subtract the zero-based word
   index from the GC length. */

#define index_to_word(bit_string, index)				\
((Vector_Length (bit_string)) - (index / POINTER_LENGTH))

#define index_pair_to_bit_number(string, word, bit)			\
((((Vector_Length (string)) - (word)) * POINTER_LENGTH) + (bit))

#define read_bits_ptr(object, offset, end)				\
(Nth_Vector_Loc((object), ((offset) / POINTER_LENGTH)))

#define compute_read_bits_offset(offset, end)				\
{									\
  offset = (offset % POINTER_LENGTH);					\
}


#endif /* VAX_BYTE_ORDER */
