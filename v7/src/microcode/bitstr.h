/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/bitstr.h,v 1.1 1987/04/25 20:24:49 cph Exp $

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

/* Bit string macros. */

#define bit_string_p(P) ((pointer_type (P)) == TC_BIT_STRING)

#define guarantee_bit_string_arg_1()				\
if (! (bit_string_p (Arg1))) error_wrong_type_arg_1 ()

#define guarantee_bit_string_arg_2()				\
if (! (bit_string_p (Arg2))) error_wrong_type_arg_2 ()

#define guarantee_bit_string_arg_3()				\
if (! (bit_string_p (Arg3))) error_wrong_type_arg_3 ()

#define guarantee_bit_string_arg_4()				\
if (! (bit_string_p (Arg4))) error_wrong_type_arg_4 ()

#define guarantee_bit_string_arg_5()				\
if (! (bit_string_p (Arg5))) error_wrong_type_arg_5 ()

#define bit_string_length(bit_string)				\
(Fast_Vector_Ref (bit_string, NM_ENTRY_COUNT))

#define bit_string_start_ptr(bit_string)			\
(Nth_Vector_Loc (bit_string, NM_DATA))

#define bit_string_end_ptr(bit_string)				\
(Nth_Vector_Loc (bit_string, ((Vector_Length (bit_string)) + 1)))

#define any_mask(nbits, offset) ((low_mask (nbits)) << (offset))
#define low_mask(nbits) ((1 << (nbits)) - 1)

/* This is especially clever.  To understand it, note that the index
   of the last pointer of a vector is also the GC length of the
   vector, so that all we need do is subtract the zero-based word
   index from the GC length. */

#define index_to_word(bit_string, index)			\
((Vector_Length (bit_string)) - (index / POINTER_LENGTH))

#define bits_to_pointers(bits)					\
(((bits) + (POINTER_LENGTH - 1)) / POINTER_LENGTH)

#define index_pair_to_bit_number(string, word, bit)		\
((((Vector_Length (string)) - (word)) * POINTER_LENGTH) + (bit))

#define index_pair_to_bit_fixnum(string, word, bit)		\
(Make_Unsigned_Fixnum (index_pair_to_bit_number (string, word, bit)))
