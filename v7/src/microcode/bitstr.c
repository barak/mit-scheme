/* -*-C-*-

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/bitstr.c,v 9.36 1987/11/23 05:13:53 cph Rel $

   Bit string primitives. 

    Conversions between nonnegative integers and bit strings are
    implemented here; they use the standard binary encoding, in which
    each index selects the bit corresponding to that power of 2.  Thus
    bit 0 is the LSB.

*/

#include "scheme.h"
#include "primitive.h"
#include "bignum.h"
#include "bitstr.h"

Pointer
allocate_bit_string (length)
     long length;
{
  long total_pointers;
  Pointer result;

  total_pointers = (1 + (bits_to_pointers (length)));
  result = (allocate_non_marked_vector (TC_BIT_STRING, total_pointers, true));
  Fast_Vector_Set (result, BIT_STRING_LENGTH_OFFSET, length);
  return (result);
}

/* (BIT-STRING-ALLOCATE length)
   Returns an uninitialized bit string of the given length. */

DEFINE_PRIMITIVE ("BIT-STRING-ALLOCATE", Prim_bit_string_allocate, 1)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (allocate_bit_string (arg_nonnegative_integer (1)));
}

/* (BIT-STRING? object)
   Returns true iff object is a bit string. */

DEFINE_PRIMITIVE ("BIT-STRING?", Prim_bit_string_p, 1)
{
  fast Pointer object;
  PRIMITIVE_HEADER (1);

  object = (ARG_REF (1));
  Touch_In_Primitive (object, object);
  PRIMITIVE_RETURN ((BIT_STRING_P (object)) ? TRUTH : NIL);
}

void
fill_bit_string (bit_string, sense)
     Pointer bit_string;
     Boolean sense;
{
  Pointer *scanner;
  Pointer filler;
  long i;

  filler = ((Pointer) (sense ? (~ 0) : 0));
  scanner = bit_string_high_ptr (bit_string);
  for (i = bits_to_pointers (bit_string_length (bit_string));
       (i > 0); i -= 1)
    (* (dec_bit_string_ptr (scanner))) = filler;
}

void
clear_bit_string (bit_string)
     Pointer bit_string;
{
  Pointer *scanner;
  long i;

  scanner = bit_string_high_ptr (bit_string);
  for (i = bits_to_pointers (bit_string_length (bit_string));
       (i > 0); i -= 1)
    (* (dec_bit_string_ptr (scanner))) = 0;
}

/* (MAKE-BIT-STRING size initialization)
   Returns a bit string of the specified size with all the bits
   set to zero if the initialization is false, one otherwise. */

DEFINE_PRIMITIVE ("MAKE-BIT-STRING", Prim_make_bit_string, 2)
{
  Pointer result;
  PRIMITIVE_HEADER (2);

  result = allocate_bit_string (arg_nonnegative_integer (1));
  fill_bit_string (result, ((ARG_REF (2)) != NIL));
  PRIMITIVE_RETURN (result);
}

/* (BIT-STRING-FILL! bit-string initialization)
   Fills the bit string with zeros if the initialization is false,
   otherwise fills it with ones. */

DEFINE_PRIMITIVE ("BIT-STRING-FILL!", Prim_bit_string_fill_x, 2)
{
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, BIT_STRING_P);
  fill_bit_string ((ARG_REF (1)), ((ARG_REF (2)) != NIL));
  PRIMITIVE_RETURN (NIL);
}

/* (BIT-STRING-LENGTH bit-string)
   Returns the number of bits in BIT-STRING. */

DEFINE_PRIMITIVE ("BIT-STRING-LENGTH", Prim_bit_string_length, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, BIT_STRING_P);
  PRIMITIVE_RETURN (Make_Unsigned_Fixnum (bit_string_length (ARG_REF (1))));
}

#define REF_INITIALIZATION()						\
  fast Pointer bit_string;						\
  fast long index;							\
  fast Pointer *ptr;							\
  fast long mask;							\
  PRIMITIVE_HEADER (2);							\
									\
  CHECK_ARG (1, BIT_STRING_P);						\
  bit_string = (ARG_REF (1));						\
  index = (arg_nonnegative_integer (2));				\
  if (index >= (bit_string_length (bit_string)))			\
    error_bad_range_arg (1);						\
									\
  ptr =									\
    (Nth_Vector_Loc (bit_string, (index_to_word (bit_string, index))));	\
  mask = (1 << (index % POINTER_LENGTH))

/* (BIT-STRING-REF bit-string index)
   Returns the boolean value of the indexed bit. */

DEFINE_PRIMITIVE ("BIT-STRING-REF", Prim_bit_string_ref, 2)
{
  REF_INITIALIZATION ();

  PRIMITIVE_RETURN ((((bit_string_word (ptr)) & mask) == 0) ? NIL : TRUTH);
}

/* (BIT-STRING-CLEAR! bit-string index)
   Sets the indexed bit to zero, returning its previous value
   as a boolean. */

DEFINE_PRIMITIVE ("BIT-STRING-CLEAR!", Prim_bit_string_clear_x, 2)
{
  REF_INITIALIZATION ();

  if (((bit_string_word (ptr)) & mask) == 0)
    PRIMITIVE_RETURN (NIL);
  (bit_string_word (ptr)) &= ~mask;
  PRIMITIVE_RETURN (TRUTH);
}

/* (BIT-STRING-SET! bit-string index)
   Sets the indexed bit to one, returning its previous value
   as a boolean. */

DEFINE_PRIMITIVE ("BIT-STRING-SET!", Prim_bit_string_set_x, 2)
{
  REF_INITIALIZATION ();

  if (((bit_string_word (ptr)) & mask) != 0)
    PRIMITIVE_RETURN (TRUTH);
  ((bit_string_word (ptr))) |= mask;
  PRIMITIVE_RETURN (NIL);
}

#define ZERO_SECTION_P()						\
{									\
  for (i = (length / POINTER_LENGTH); (i > 0); i -= 1)			\
    if ((* (dec_bit_string_ptr (scan))) != 0)				\
      PRIMITIVE_RETURN (NIL);						\
  PRIMITIVE_RETURN (TRUTH);						\
}

/* (BIT-STRING-ZERO? bit-string)
   Returns true the argument has no "set" bits. */

DEFINE_PRIMITIVE ("BIT-STRING-ZERO?", Prim_bit_string_zero_p, 1)
{
  fast Pointer bit_string;
  fast Pointer *scan;
  fast long i;
  long length, odd_bits;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, BIT_STRING_P);
  bit_string = (ARG_REF (1));
  length = (bit_string_length (bit_string));
  odd_bits = (length % POINTER_LENGTH);
  scan = (bit_string_high_ptr (bit_string));
  if (odd_bits == 0)
    {
      ZERO_SECTION_P ();
    }
  else if (((bit_string_word (scan)) & (low_mask (odd_bits))) != 0)
    PRIMITIVE_RETURN (NIL);
  else
    {
      dec_bit_string_ptr (scan);
      ZERO_SECTION_P ();
    }
}

#define EQUAL_SECTIONS_P()						\
{									\
  for (i = (length / POINTER_LENGTH); (i > 0); i -= 1)			\
    if ((* (dec_bit_string_ptr (scan1))) !=				\
	(* (dec_bit_string_ptr (scan2))))				\
      PRIMITIVE_RETURN (NIL);						\
  PRIMITIVE_RETURN (TRUTH);						\
}

/* (BIT-STRING=? bit-string-1 bit-string-2)
   Returns true iff the two bit strings contain the same bits. */

DEFINE_PRIMITIVE ("BIT-STRING=?", Prim_bit_string_equal_p, 2)
{
  Pointer bit_string_1, bit_string_2;
  long length;
  fast Pointer *scan1, *scan2;
  fast long i;
  long odd_bits;
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, BIT_STRING_P);
  CHECK_ARG (2, BIT_STRING_P);

  bit_string_1 = (ARG_REF (1));
  bit_string_2 = (ARG_REF (2));
  length = bit_string_length (bit_string_1);
  if (length != bit_string_length (bit_string_2))
    PRIMITIVE_RETURN (NIL);

  scan1 = (bit_string_high_ptr (bit_string_1));
  scan2 = (bit_string_high_ptr (bit_string_2));
  odd_bits = (length % POINTER_LENGTH);
  if (odd_bits == 0)
    {
      EQUAL_SECTIONS_P ();
    }
  else
    {
      long mask;

      mask = (low_mask (odd_bits));
      if (((bit_string_msw (bit_string_1)) & mask) !=
	  ((bit_string_msw (bit_string_2)) & mask))
	PRIMITIVE_RETURN (NIL);
      else
	{
	  dec_bit_string_ptr (scan1);
	  dec_bit_string_ptr (scan2);
	  EQUAL_SECTIONS_P ();
	}
    }
}

/* (BIT-STRING-OPERATION! destination source)
   Modifies destination to be the result of using OPERATION bitwise on
   destination and source. */

#define BITWISE_OP(action)						\
  Pointer bit_string_1, bit_string_2;					\
  fast long i;								\
  fast Pointer *scan1, *scan2;						\
  PRIMITIVE_HEADER (2);							\
									\
  bit_string_1 = (ARG_REF (1));						\
  bit_string_2 = (ARG_REF (2));						\
  if ((bit_string_length (bit_string_1)) !=				\
      (bit_string_length (bit_string_2)))				\
    error_bad_range_arg (1);						\
									\
  scan1 = (bit_string_high_ptr (bit_string_1));				\
  scan2 = (bit_string_high_ptr (bit_string_2));				\
  for (i = ((Vector_Length (bit_string_1)) - 1); (i > 0); i -= 1)	\
    (* (dec_bit_string_ptr (scan1))) action()				\
      (* (dec_bit_string_ptr (scan2)));					\
  PRIMITIVE_RETURN (NIL)

#define bit_string_move_x_action()	=
#define bit_string_movec_x_action()	= ~
#define bit_string_or_x_action()	|=
#define bit_string_and_x_action()	&=
#define bit_string_andc_x_action()	&= ~
#define bit_string_xor_x_action()	^=

DEFINE_PRIMITIVE ("BIT-STRING-MOVE!", Prim_bit_string_move_x, 2)
{ BITWISE_OP (bit_string_move_x_action); }

DEFINE_PRIMITIVE ("BIT-STRING-MOVEC!", Prim_bit_string_movec_x, 2)
{ BITWISE_OP (bit_string_movec_x_action); }

DEFINE_PRIMITIVE ("BIT-STRING-OR!", Prim_bit_string_or_x, 2)
{ BITWISE_OP (bit_string_or_x_action); }

DEFINE_PRIMITIVE ("BIT-STRING-AND!", Prim_bit_string_and_x, 2)
{ BITWISE_OP (bit_string_and_x_action); }

DEFINE_PRIMITIVE ("BIT-STRING-ANDC!", Prim_bit_string_andc_x, 2)
{ BITWISE_OP (bit_string_andc_x_action); }

DEFINE_PRIMITIVE ("BIT-STRING-XOR!", Prim_bit_string_xor_x, 2)
{ BITWISE_OP (bit_string_xor_x_action); }

/* (BIT-SUBSTRING-MOVE-RIGHT! source start1 end1 destination start2)
   Destructively copies the substring of SOURCE between START1 and
   END1 into DESTINATION at START2.  The copying is done from the
   MSB to the LSB (which only matters when SOURCE and DESTINATION
   are the same). */

DEFINE_PRIMITIVE ("BIT-SUBSTRING-MOVE-RIGHT!", Prim_bit_substring_move_right_x, 5)
{
  fast Pointer bit_string_1, bit_string_2;
  long start1, end1, start2, end2, nbits;
  long end1_mod, end2_mod;
  void copy_bits();
  PRIMITIVE_HEADER (5);

  CHECK_ARG (1, BIT_STRING_P);
  bit_string_1 = (ARG_REF (1));
  start1 = (arg_nonnegative_integer (2));
  end1 = (arg_nonnegative_integer (3));
  CHECK_ARG (4, BIT_STRING_P); 
  bit_string_2 = (ARG_REF (4));
  start2 = (arg_nonnegative_integer (5));

  nbits = (end1 - start1);
  end2 = (start2 + nbits);

  if ((start1 < 0) || (start1 > end1))
    error_bad_range_arg (2);
  if (end1 > (bit_string_length (bit_string_1)))
    error_bad_range_arg (3);
  if ((start2 < 0) || (end2 > (bit_string_length (bit_string_2))))
    error_bad_range_arg (5);

  end1_mod = (end1 % POINTER_LENGTH);
  end2_mod = (end2 % POINTER_LENGTH);

  /* Using `index_to_word' here with -1 offset will work in every
     case except when the `end' is 0.  In this case the result of
     the expression `(-1 / POINTER_LENGTH)' is either 0 or -1, at
     the discretion of the C compiler being used.  This doesn't
     matter because if `end' is zero, then no bits will be moved. */

  copy_bits ((Nth_Vector_Loc (bit_string_1,
			      (index_to_word (bit_string_1, (end1 - 1))))),
	    ((end1_mod == 0) ? 0 : (POINTER_LENGTH - end1_mod)),
	    (Nth_Vector_Loc (bit_string_2,
			     (index_to_word (bit_string_2, (end2 - 1))))),
	    ((end2_mod == 0) ? 0 : (POINTER_LENGTH - end2_mod)),
	    nbits);
  PRIMITIVE_RETURN (NIL);
}

#define MASKED_TRANSFER(source, destination, nbits, offset) do		\
{									\
  long mask;								\
									\
  mask = (any_mask (nbits, offset));					\
  (bit_string_word (destination)) =					\
    (((bit_string_word (source)) & mask) |				\
     ((bit_string_word (destination)) & ~mask));			\
} while (0)

/* This procedure copies bits from one place to another.
   The offsets are measured from the MSB of the first Pointer of
   each of the arguments SOURCE and DESTINATION.  It copies the bits
   starting with the MSB of a bit string and moving down. */

void
copy_bits (source, source_offset, destination, destination_offset, nbits)
     Pointer *source, *destination;
     long source_offset, destination_offset, nbits;
{

  /* This common case can be done very quickly, by splitting the
     bit string into three parts.  Since the source and destination are
     aligned relative to one another, the main body of bits can be
     transferred as Pointers, and only the `head' and `tail' need be
     treated specially. */

  if (source_offset == destination_offset)
    {
      if (source_offset != 0)
	{
	  long head;

	  head = (POINTER_LENGTH - source_offset);
	  if (nbits <= head)
	    {
	      MASKED_TRANSFER (source, destination, nbits, (head - nbits));
	      nbits = 0;
	    }
	  else
	    {
	      Pointer temp;
	      long mask;

	      mask = (low_mask (head));
	      temp = (bit_string_word (destination));
	      (* (dec_bit_string_ptr (destination))) =
		(((* (dec_bit_string_ptr (source))) & mask) |
		 (temp & (~ mask)));
	      nbits -= head;
	    }
	}
      if (nbits > 0)
	{
	  long nwords, tail;

	  for (nwords = (nbits / POINTER_LENGTH); (nwords > 0); nwords -= 1)
	    (* (dec_bit_string_ptr (destination))) =
	      (* (dec_bit_string_ptr (source)));

	  tail = (nbits % POINTER_LENGTH);
	  if (tail > 0)
	    MASKED_TRANSFER
	      (source, destination, tail, (POINTER_LENGTH - tail));
	}
    }

  else if (source_offset < destination_offset)
    {
      long offset1, offset2, head;

      offset1 = (destination_offset - source_offset);
      offset2 = (POINTER_LENGTH - offset1);
      head = (POINTER_LENGTH - destination_offset);

      if (nbits <= head)
	{
	  long mask;

	  mask = (any_mask (nbits, (head - nbits)));
	  (bit_string_word (destination)) =
	    ((((bit_string_word (source)) >> offset1) & mask) |
	     ((bit_string_word (destination)) & ~mask));
	}
      else
	{
	  long mask1, mask2;

	  { Pointer temp;
	    long mask;

	    mask = (low_mask (head));
	    temp = (bit_string_word (destination));
	    (* (dec_bit_string_ptr (destination))) =
	      ((((bit_string_word (source)) >> offset1) & mask) |
	       (temp & ~mask));
	  }

	  nbits -= head;
	  mask1 = (low_mask (offset1));
	  mask2 = (low_mask (offset2));

	  {
	    long nwords, i;

	    for (nwords = (nbits / POINTER_LENGTH); (nwords > 0); nwords -= 1)
	      {
		i = (((* (dec_bit_string_ptr (source))) & mask1) << offset2);
		(* (dec_bit_string_ptr (destination))) =
		  ((((bit_string_word (source)) >> offset1) & mask2) | i);
	      }
	  }

	  {
	    long tail, dest_tail;

	    tail = (nbits % POINTER_LENGTH);
	    dest_tail =
	      ((bit_string_word (destination)) &
	       (low_mask (POINTER_LENGTH - tail)));
	    if (tail <= offset1)
	      {
		(bit_string_word (destination)) =
		  ((((bit_string_word (source)) &
		     (any_mask (tail, (offset1 - tail))))
		    << offset2)
		   | dest_tail);
	      }
	    else
	      {
		long i, j;

		i = (((* (dec_bit_string_ptr (source))) & mask1) << offset2);
		j = (tail - offset1);
		(bit_string_word (destination)) =
		  ((((bit_string_word (source)) &
		     (any_mask (j, (POINTER_LENGTH - j))))
		    >> offset1)
		   | i | dest_tail);
	      }
	  }
	}
    }

  else				/* if (source_offset > destination_offset) */
    {
      long offset1, offset2, head;

      offset1 = (source_offset - destination_offset);
      offset2 = (POINTER_LENGTH - offset1);
      head = (POINTER_LENGTH - source_offset);

      if (nbits <= head)
	{
	  long mask;

	  mask = (any_mask (nbits, (offset1 + (head - nbits))));
	  (bit_string_word (destination)) =
	    ((((bit_string_word (source)) << offset1) & mask) |
	     ((bit_string_word (destination)) & ~mask));
	}
      else
	{
	  long dest_buffer, mask1, mask2;

	  {
	    long mask;

	    mask = (any_mask (head, offset1));
	    dest_buffer =
	      (((bit_string_word (destination)) & ~mask)
	       | (((* (dec_bit_string_ptr (source))) << offset1) & mask));
	  }
	  nbits -= head;
	  mask1 = (low_mask (offset1));
	  mask2 = (any_mask (offset2, offset1));
	  {
	    long nwords;

	    nwords = (nbits / POINTER_LENGTH);
	    if (nwords > 0)
	      dest_buffer &= mask2;
	    for (; (nwords > 0); nwords -= 1)
	      {
		(* (dec_bit_string_ptr (destination))) =
		  (dest_buffer |
		   (((bit_string_word (source)) >> offset2) & mask1));
		dest_buffer = ((* (dec_bit_string_ptr (source))) << offset1);
	      }
	  }

	  {
	    long tail;

	    tail = (nbits % POINTER_LENGTH);
	    if (tail <= offset1)
	      {
		(bit_string_word (destination)) =
		  (dest_buffer |
		   ((bit_string_word (destination)) &
		    (low_mask (offset1 - tail))) |
		   (((bit_string_word (source)) >> offset2) &
		    (any_mask (tail, (offset1 - tail)))));
	      }
	    else
	      {
		long mask;

		(* (dec_bit_string_ptr (destination))) =
		  (dest_buffer |
		   (((bit_string_word (source)) >> offset2) & mask1));
		mask = (low_mask (POINTER_LENGTH - tail));
		(bit_string_word (destination)) =
		  (((bit_string_word (destination)) & (~ mask)) |
		   (((bit_string_word (source)) << offset1) & mask));
	      }
	  }
	}
    }
}

/* Integer <-> Bit-string Conversions */

long
count_significant_bits (number, start)
     long number, start;
{
  long significant_bits, i;

  significant_bits = start;
  for (i = (1 << (start - 1)); (i >= 0); i >>= 1)
    {
      if (number >= i)
	break;
      significant_bits -= 1;
    }
  return (significant_bits);
}

long
long_significant_bits (number)
     long number;
{
  return
    ((number < 0)
     ? ULONG_SIZE
     : (count_significant_bits (number, (ULONG_SIZE - 1))));
}

Pointer
zero_to_bit_string (length)
     long length;
{
  Pointer result;

  result = (allocate_bit_string (length));
  clear_bit_string (result);
  return (result);
}

Pointer
long_to_bit_string (length, number)
     long length, number;
{
  if (number < 0)
    error_bad_range_arg (2);

  if (number == 0)
    {
      return (zero_to_bit_string (length));
    }
  else
    {
      Pointer result;

      if (length < (long_significant_bits (number)))
	error_bad_range_arg (2);
      result = (zero_to_bit_string (length));
      (bit_string_lsw (result)) = number;
      return (result);
    }
}

/* The bignum <-> bit-string coercion procedures use the following pun:
   inc_bit_string_ptr is being used on a *bigdigit, rather than *Pointer.
*/

Pointer
bignum_to_bit_string (length, bignum)
     long length;
     Pointer bignum;
{
  bigdigit *bigptr;
  long ndigits;

  bigptr = (BIGNUM (Get_Pointer (bignum)));
  if (NEG_BIGNUM (bigptr))
    error_bad_range_arg (2);
  ndigits = (LEN (bigptr));
  if (ndigits == 0)
    zero_to_bit_string (length);
  else
    {
      Pointer result;
      bigdigit *scan1, *scan2;

      if (length <
	  (count_significant_bits ((* (Bignum_Top (bigptr))), SHIFT)
	   + (SHIFT * (ndigits - 1))))
	error_bad_range_arg (2);
      result = (zero_to_bit_string (length));
      scan1 = (Bignum_Bottom (bigptr));
      scan2 = ((bigdigit *) (bit_string_low_ptr (result)));
      for (; (ndigits > 0); ndigits -= 1)
	(* (inc_bit_string_ptr (scan2))) = (*scan1++);
      return (result);
    }
}

Pointer
bit_string_to_bignum (nbits, bitstr)
     long nbits;
     Pointer bitstr;
{
  fast long ndigits;
  long align_ndigits;
  fast bigdigit *scan1, *scan2;
  bigdigit *bignum;

  ndigits = ((nbits + (SHIFT - 1)) / SHIFT);
  align_ndigits = (Align (ndigits));
  Primitive_GC_If_Needed (align_ndigits);
  bignum = (BIGNUM (Free));
  Free += align_ndigits;
  Prepare_Header (bignum, ndigits, POSITIVE);

  scan1 = ((bigdigit *) (bit_string_low_ptr (bitstr)));
  scan2 = (Bignum_Bottom (bignum));
  while ((--ndigits) > 0)
    (*scan2++) = (* (inc_bit_string_ptr (scan1)));
  nbits = (nbits % SHIFT);
  (*scan2) =
    ((nbits == 0)
     ? (* (inc_bit_string_ptr (scan1)))
     : ((* (inc_bit_string_ptr (scan1))) & (low_mask (nbits))));

  return (Make_Pointer (TC_BIG_FIXNUM, ((Pointer *) bignum)));
}

/* (UNSIGNED-INTEGER->BIT-STRING length integer)
   INTEGER, which must be a non-negative integer, is converted to
   a bit-string of length LENGTH.  If INTEGER is too large, an
   error is signalled. */

DEFINE_PRIMITIVE ("UNSIGNED-INTEGER->BIT-STRING", Prim_unsigned_to_bit_string, 2)
{
  fast long length;
  fast Pointer object;
  PRIMITIVE_HEADER (2);

  length = (arg_nonnegative_integer (1));
  object = (ARG_REF (2));

  if (FIXNUM_P (object))
    {
      if (FIXNUM_NEGATIVE_P (object))
	error_bad_range_arg (2);
      PRIMITIVE_RETURN (long_to_bit_string (length,
					    (UNSIGNED_FIXNUM_VALUE (object))));
    }
  if (BIGNUM_P (object))
    PRIMITIVE_RETURN (bignum_to_bit_string (length, object));
  error_wrong_type_arg (2);
}

/* (BIT-STRING->UNSIGNED-INTEGER bit-string)
   BIT-STRING is converted to the appropriate non-negative integer.
   This operation is the inverse of `unsigned-integer->bit-string'. */

DEFINE_PRIMITIVE ("BIT-STRING->UNSIGNED-INTEGER", Prim_bit_string_to_unsigned, 1)
{
  fast Pointer bit_string, *scan;
  long nwords, nbits, word;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, BIT_STRING_P);
  bit_string = (ARG_REF (1));

  /* Count the number of significant bits.*/
  scan = (bit_string_high_ptr (bit_string));
  nbits = ((bit_string_length (bit_string)) % POINTER_LENGTH);
  word =
    ((nbits > 0)
     ? ((* (dec_bit_string_ptr (scan))) & (low_mask (nbits)))
     : (* (dec_bit_string_ptr (scan))));
  for (nwords = ((Vector_Length (bit_string)) - 1); (nwords > 0); nwords -= 1)
    {
      if (word != 0)
	break;
      word = (* (dec_bit_string_ptr (scan)));
    }
  if (nwords == 0)
    PRIMITIVE_RETURN (Make_Unsigned_Fixnum (0));
  nbits = (((nwords - 1) * POINTER_LENGTH) + (long_significant_bits (word)));

  PRIMITIVE_RETURN
    ((nbits < FIXNUM_LENGTH)
     ? (Make_Unsigned_Fixnum (word))
     : (bit_string_to_bignum (nbits, bit_string)));
}

#define READ_BITS_INITIALIZE()						\
  Pointer bit_string;							\
  long end, end_mod, offset;						\
  Pointer *start;							\
  PRIMITIVE_HEADER (3);							\
									\
  CHECK_ARG (3, BIT_STRING_P);						\
  bit_string = (ARG_REF (3));						\
  end = (bit_string_length (bit_string));				\
  end_mod = (end % POINTER_LENGTH);					\
  offset = (arg_nonnegative_integer (2));				\
  start = (read_bits_ptr ((ARG_REF (1)), offset, end));			\
  compute_read_bits_offset (offset, end)


/* (READ-BITS! pointer offset bit-string)
   Read the contents of memory at the address (POINTER,OFFSET)
   into BIT-STRING. */

DEFINE_PRIMITIVE ("READ-BITS!", Prim_read_bits_x, 3)
{
  READ_BITS_INITIALIZE ();

  copy_bits (start,
	     offset,
	     (Nth_Vector_Loc (bit_string,
			      (index_to_word (bit_string, (end - 1))))),
	     ((end_mod == 0) ? 0 : (POINTER_LENGTH - end_mod)),
	     end);
  PRIMITIVE_RETURN (NIL);
}

/* (WRITE-BITS! pointer offset bit-string)
   Write the contents of BIT-STRING in memory at the address
   (POINTER,OFFSET). */

DEFINE_PRIMITIVE ("WRITE-BITS!", Prim_write_bits_x, 3)
{
  READ_BITS_INITIALIZE ();

  copy_bits ((Nth_Vector_Loc (bit_string,
			      (index_to_word (bit_string, (end - 1))))),
	     ((end_mod == 0) ? 0 : (POINTER_LENGTH - end_mod)),
	     start,
	     offset,
	     end);
  PRIMITIVE_RETURN (NIL);
}

/* Search Primitives */

#define SUBSTRING_FIND_INITIALIZE()					\
  Pointer bit_string;							\
  long start, end;							\
  long word, bit, end_word, end_bit, mask;				\
  Pointer *scan;							\
  PRIMITIVE_HEADER (3);							\
									\
  CHECK_ARG (1, BIT_STRING_P);						\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
									\
  if (end > (bit_string_length (bit_string)))				\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2);						\
									\
  if (start == end)							\
    PRIMITIVE_RETURN (NIL)

#define SUBSTRING_FIND_NEXT_INITIALIZE()				\
  SUBSTRING_FIND_INITIALIZE ();						\
  word = (index_to_word (bit_string, start));				\
  bit = (start % POINTER_LENGTH);					\
  end_word = (index_to_word (bit_string, (end - 1)));			\
  end_bit = (((end - 1) % POINTER_LENGTH) + 1);				\
  scan = (Nth_Vector_Loc (bit_string, word))

#define FIND_NEXT_SET_LOOP(init_bit)					\
{									\
  bit = (init_bit);							\
  mask = (1 << (init_bit));						\
  while (true)								\
    {									\
      if (((bit_string_word (scan)) & mask) != 0)			\
	goto win;							\
      bit += 1;								\
      mask <<= 1;							\
    }									\
}

DEFINE_PRIMITIVE ("BIT-SUBSTRING-FIND-NEXT-SET-BIT", Prim_bitstr_find_next_set_bit, 3)
{
  SUBSTRING_FIND_NEXT_INITIALIZE ();

  if (word == end_word)
    {
      if ((((end_bit - bit) == POINTER_LENGTH) &&
	   ((bit_string_word (scan)) != 0)) ||
	  (((bit_string_word (scan)) & (any_mask ((end_bit - bit), bit)))
	   != 0))
	{
	  FIND_NEXT_SET_LOOP (bit);
	}
      PRIMITIVE_RETURN (NIL);
    }
  else if (((bit_string_word (scan)) &
	    ((bit == 0) ? (~ 0) : (any_mask ((POINTER_LENGTH - bit), bit))))
	   != 0)
    {
      FIND_NEXT_SET_LOOP (bit);
    }

  while ((--word) > end_word)
    if ((* (inc_bit_string_ptr (scan))) != 0)
      {
	FIND_NEXT_SET_LOOP (0);
      }

  if (((* (inc_bit_string_ptr (scan))) &
       ((end_bit == POINTER_LENGTH) ? (~ 0) : (low_mask (end_bit))))
      != 0)
    {
      FIND_NEXT_SET_LOOP (0);
    }

  PRIMITIVE_RETURN (NIL);

 win:
  PRIMITIVE_RETURN (index_pair_to_bit_fixnum (bit_string, word, bit));
}
