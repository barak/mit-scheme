/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

/* Bit string primitives.
   Conversions between nonnegative integers and bit strings are
   implemented here; they use the standard binary encoding, in which
   each index selects the bit corresponding to that power of 2.  Thus
   bit 0 is the LSB. */

#include "scheme.h"
#include "prims.h"
#include "bitstr.h"

static void copy_bits
  (SCHEME_OBJECT *, long, SCHEME_OBJECT *, long, long);

SCHEME_OBJECT
allocate_bit_string (unsigned long length)
{
  long total_pointers;
  SCHEME_OBJECT result;

  total_pointers = (1 + (BIT_STRING_LENGTH_TO_GC_LENGTH (length)));
  result = (allocate_non_marked_vector (TC_BIT_STRING, total_pointers, true));
  MEMORY_SET (result, BIT_STRING_LENGTH_OFFSET, length);
  return (result);
}

/* (BIT-STRING-ALLOCATE length)
   Returns an uninitialized bit string of the given length. */

DEFINE_PRIMITIVE ("BIT-STRING-ALLOCATE", Prim_bit_string_allocate, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (allocate_bit_string (arg_ulong_integer (1)));
}

/* (BIT-STRING? object)
   Returns #T iff object is a bit string. */

DEFINE_PRIMITIVE ("BIT-STRING?", Prim_bit_string_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (BIT_STRING_P (ARG_REF (1))));
}

void
fill_bit_string (SCHEME_OBJECT bit_string,
       int sense)
{
  SCHEME_OBJECT *scanner;
  SCHEME_OBJECT filler;
  long i;

  filler = ((SCHEME_OBJECT) (sense ? (~ 0) : 0));
  scanner = BIT_STRING_HIGH_PTR (bit_string);
  for (i = BIT_STRING_LENGTH_TO_GC_LENGTH (BIT_STRING_LENGTH (bit_string));
       (i > 0); i -= 1)
    (* (DEC_BIT_STRING_PTR (scanner))) = filler;
}

void
clear_bit_string (SCHEME_OBJECT bit_string)
{
  SCHEME_OBJECT *scanner;
  long i;

  scanner = BIT_STRING_HIGH_PTR (bit_string);
  for (i = BIT_STRING_LENGTH_TO_GC_LENGTH (BIT_STRING_LENGTH (bit_string));
       (i > 0); i -= 1)
    (* (DEC_BIT_STRING_PTR (scanner))) = 0;
}

DEFINE_PRIMITIVE ("MAKE-BIT-STRING", Prim_make_bit_string, 2, 2,
 "(SIZE INITIALIZATION)\n\
Returns a bit string of the specified size with all the bits\n\
set to zero if the initialization is false, one otherwise.")
{
  SCHEME_OBJECT result;
  PRIMITIVE_HEADER (2);
  result = allocate_bit_string (arg_ulong_integer (1));
  fill_bit_string (result, (OBJECT_TO_BOOLEAN (ARG_REF (2))));
  PRIMITIVE_RETURN (result);
}

DEFINE_PRIMITIVE ("BIT-STRING-FILL!", Prim_bit_string_fill_x, 2, 2,
  "(BIT-STRING INITIALIZATION)\n\
Fills the bit string with zeros if the initialization is false, \
otherwise fills it with ones.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, BIT_STRING_P);
  fill_bit_string ((ARG_REF (1)), (OBJECT_TO_BOOLEAN (ARG_REF (2))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/*  */

DEFINE_PRIMITIVE ("BIT-STRING-LENGTH", Prim_bit_string_length, 1, 1,
  "(BIT-STRING)\n\
Returns the number of bits in BIT-STRING.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, BIT_STRING_P);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (BIT_STRING_LENGTH (ARG_REF (1))));
}

#define REF_INITIALIZATION()						\
  SCHEME_OBJECT bit_string;						\
  long index;								\
  SCHEME_OBJECT *ptr;							\
  long mask;								\
  PRIMITIVE_HEADER (2);							\
									\
  CHECK_ARG (1, BIT_STRING_P);						\
  bit_string = (ARG_REF (1));						\
  index = (arg_nonnegative_integer (2));				\
  if (index >= (BIT_STRING_LENGTH (bit_string)))			\
    error_bad_range_arg (2);						\
									\
  ptr =									\
    (MEMORY_LOC								\
     (bit_string, (BIT_STRING_INDEX_TO_WORD (bit_string, index))));	\
  mask = (1L << (index % OBJECT_LENGTH))

DEFINE_PRIMITIVE ("BIT-STRING-REF", Prim_bit_string_ref, 2, 2,
  "(BIT-STRING INDEX)\n\
Returns the boolean value of the indexed bit.")
{
  REF_INITIALIZATION ();
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (((BIT_STRING_WORD (ptr)) & mask) != 0));
}

/*  */

DEFINE_PRIMITIVE ("BIT-STRING-CLEAR!", Prim_bit_string_clear_x, 2, 2,
  "(BIT-STRING INDEX)\n\
Sets the indexed bit to zero, returning its previous value as a boolean.")
{
  REF_INITIALIZATION ();
  if (((BIT_STRING_WORD (ptr)) & mask) == 0)
    PRIMITIVE_RETURN (SHARP_F);
  (BIT_STRING_WORD (ptr)) &= ~mask;
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIT-STRING-SET!", Prim_bit_string_set_x, 2, 2,
  "(BIT-STRING INDEX)\n\
Sets the indexed bit to one, returning its previous value as a boolean.")
{
  REF_INITIALIZATION ();
  if (((BIT_STRING_WORD (ptr)) & mask) != 0)
    PRIMITIVE_RETURN (SHARP_T);
  (BIT_STRING_WORD (ptr)) |= mask;
  PRIMITIVE_RETURN (SHARP_F);
}

#define ZERO_SECTION_P()						\
{									\
  for (i = (length / OBJECT_LENGTH); (i > 0); i -= 1)			\
    if ((* (DEC_BIT_STRING_PTR (scan))) != 0)				\
      PRIMITIVE_RETURN (SHARP_F);					\
  PRIMITIVE_RETURN (SHARP_T);						\
}

DEFINE_PRIMITIVE ("BIT-STRING-ZERO?", Prim_bit_string_zero_p, 1, 1,
 "(BIT-STRING)\n\
Returns true the argument has no \"set\" bits.")
{
  SCHEME_OBJECT bit_string;
  SCHEME_OBJECT *scan;
  long i;
  long length, odd_bits;
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, BIT_STRING_P);
  bit_string = (ARG_REF (1));
  length = (BIT_STRING_LENGTH (bit_string));
  odd_bits = (length % OBJECT_LENGTH);
  scan = (BIT_STRING_HIGH_PTR (bit_string));
  if (odd_bits == 0)
    {
      ZERO_SECTION_P ();
    }
  else if (((BIT_STRING_WORD (scan)) & (LOW_MASK (odd_bits))) != 0)
    PRIMITIVE_RETURN (SHARP_F);
  else
    {
      DEC_BIT_STRING_PTR (scan);
      ZERO_SECTION_P ();
    }
}

#define EQUAL_SECTIONS_P()						\
{									\
  for (i = (length / OBJECT_LENGTH); (i > 0); i -= 1)			\
    if ((* (DEC_BIT_STRING_PTR (scan1))) !=				\
	(* (DEC_BIT_STRING_PTR (scan2))))				\
      PRIMITIVE_RETURN (SHARP_F);					\
  PRIMITIVE_RETURN (SHARP_T);						\
}

DEFINE_PRIMITIVE ("BIT-STRING=?", Prim_bit_string_equal_p, 2, 2,
  "(BIT-STRING-1 BIT-STRING-2)\n\
Returns true iff the two bit strings contain the same bits.")
{
  SCHEME_OBJECT bit_string_1, bit_string_2;
  long length;
  SCHEME_OBJECT *scan1, *scan2;
  long i;
  long odd_bits;
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, BIT_STRING_P);
  CHECK_ARG (2, BIT_STRING_P);
  bit_string_1 = (ARG_REF (1));
  bit_string_2 = (ARG_REF (2));
  length = BIT_STRING_LENGTH (bit_string_1);
  if (length != BIT_STRING_LENGTH (bit_string_2))
    PRIMITIVE_RETURN (SHARP_F);
  scan1 = (BIT_STRING_HIGH_PTR (bit_string_1));
  scan2 = (BIT_STRING_HIGH_PTR (bit_string_2));
  odd_bits = (length % OBJECT_LENGTH);
  if (odd_bits == 0)
    {
      EQUAL_SECTIONS_P ();
    }
  else
    {
      long mask;

      mask = (LOW_MASK (odd_bits));
      if (((BIT_STRING_MSW (bit_string_1)) & mask) !=
	  ((BIT_STRING_MSW (bit_string_2)) & mask))
	PRIMITIVE_RETURN (SHARP_F);
      else
	{
	  DEC_BIT_STRING_PTR (scan1);
	  DEC_BIT_STRING_PTR (scan2);
	  EQUAL_SECTIONS_P ();
	}
    }
}

/* (BIT-STRING-OPERATION! destination source)
   Modifies destination to be the result of using OPERATION bitwise on
   destination and source. */

#define BITWISE_OP(action)						\
{									\
  SCHEME_OBJECT bit_string_1, bit_string_2;				\
  long i;								\
  SCHEME_OBJECT *scan1, *scan2;						\
  PRIMITIVE_HEADER (2);							\
  bit_string_1 = (ARG_REF (1));						\
  bit_string_2 = (ARG_REF (2));						\
  if ((BIT_STRING_LENGTH (bit_string_1)) !=				\
      (BIT_STRING_LENGTH (bit_string_2)))				\
    error_bad_range_arg (1);						\
  scan1 = (BIT_STRING_HIGH_PTR (bit_string_1));				\
  scan2 = (BIT_STRING_HIGH_PTR (bit_string_2));				\
  for (i = ((VECTOR_LENGTH (bit_string_1)) - 1); (i > 0); i -= 1)	\
    (* (DEC_BIT_STRING_PTR (scan1))) action				\
      (* (DEC_BIT_STRING_PTR (scan2)));					\
  PRIMITIVE_RETURN (UNSPECIFIC);					\
}

DEFINE_PRIMITIVE ("BIT-STRING-MOVE!", Prim_bit_string_move_x, 2, 2, 0)
     BITWISE_OP (=)

DEFINE_PRIMITIVE ("BIT-STRING-MOVEC!", Prim_bit_string_movec_x, 2, 2, 0)
     BITWISE_OP (=~)

DEFINE_PRIMITIVE ("BIT-STRING-OR!", Prim_bit_string_or_x, 2, 2, 0)
     BITWISE_OP (|=)

DEFINE_PRIMITIVE ("BIT-STRING-AND!", Prim_bit_string_and_x, 2, 2, 0)
     BITWISE_OP (&=)

DEFINE_PRIMITIVE ("BIT-STRING-ANDC!", Prim_bit_string_andc_x, 2, 2, 0)
     BITWISE_OP (&=~)

DEFINE_PRIMITIVE ("BIT-STRING-XOR!", Prim_bit_string_xor_x, 2, 2, 0)
     BITWISE_OP (^=)

DEFINE_PRIMITIVE ("BIT-SUBSTRING-MOVE-RIGHT!", Prim_bit_substring_move_right_x, 5, 5,
 "(SOURCE START1 END1 DESTINATION START2)\n\
Destructively copies the substring of SOURCE between START1 and \
END1 into DESTINATION at START2.  The copying is done from the \
MSB to the LSB (which only matters when SOURCE and DESTINATION \
are the same).")
{
  SCHEME_OBJECT bit_string_1, bit_string_2;
  long start1, end1, start2, end2, nbits;
  long end1_mod, end2_mod;
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
  if (end1 > (BIT_STRING_LENGTH (bit_string_1)))
    error_bad_range_arg (3);
  if ((start2 < 0) || (end2 > (BIT_STRING_LENGTH (bit_string_2))))
    error_bad_range_arg (5);
  end1_mod = (end1 % OBJECT_LENGTH);
  end2_mod = (end2 % OBJECT_LENGTH);
  /* Using `BIT_STRING_INDEX_TO_WORD' here with -1 offset will work in every
     case except when the `end' is 0.  In this case the result of
     the expression `(-1 / OBJECT_LENGTH)' is either 0 or -1, at
     the discretion of the C compiler being used.  This doesn't
     matter because if `end' is zero, then no bits will be moved. */
  copy_bits ((MEMORY_LOC
	      (bit_string_1,
	       (BIT_STRING_INDEX_TO_WORD (bit_string_1, (end1 - 1))))),
	    ((end1_mod == 0) ? 0 : (OBJECT_LENGTH - end1_mod)),
	    (MEMORY_LOC
	     (bit_string_2,
	      (BIT_STRING_INDEX_TO_WORD (bit_string_2, (end2 - 1))))),
	    ((end2_mod == 0) ? 0 : (OBJECT_LENGTH - end2_mod)),
	    nbits);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#define MASKED_TRANSFER(source, destination, nbits, offset) do		\
{									\
  long mask = (ANY_MASK (nbits, offset));				\
  (BIT_STRING_WORD (destination))					\
    = (((BIT_STRING_WORD (source)) & mask)				\
       | ((BIT_STRING_WORD (destination)) &~ mask));			\
} while (0)

/* This procedure copies bits from one place to another.
   The offsets are measured from the MSB of the first SCHEME_OBJECT of
   each of the arguments SOURCE and DESTINATION.  It copies the bits
   starting with the MSB of a bit string and moving down. */

static void
copy_bits (SCHEME_OBJECT * source,
       long source_offset,
       SCHEME_OBJECT * destination,
       long destination_offset,
       long nbits)
{

  if (nbits == 0)
    return;

  /* This common case can be done very quickly, by splitting the
     bit string into three parts.  Since the source and destination are
     aligned relative to one another, the main body of bits can be
     transferred as SCHEME_OBJECTs, and only the `head' and `tail' need be
     treated specially.  */
  if (source_offset == destination_offset)
    {
      if (source_offset != 0)
	{
	  long head = (OBJECT_LENGTH - source_offset);
	  if (nbits <= head)
	    {
	      MASKED_TRANSFER (source, destination, nbits, (head - nbits));
	      nbits = 0;
	    }
	  else
	    {
	      long mask = (LOW_MASK (head));
	      SCHEME_OBJECT temp = (BIT_STRING_WORD (destination));
	      (* (DEC_BIT_STRING_PTR (destination)))
		= (((* (DEC_BIT_STRING_PTR (source))) & mask)
		   | (temp &~ mask));
	      nbits -= head;
	    }
	}
      while (nbits >= OBJECT_LENGTH)
	{
	  (* (DEC_BIT_STRING_PTR (destination)))
	    = (* (DEC_BIT_STRING_PTR (source)));
	  nbits -= OBJECT_LENGTH;
	}
      if (nbits > 0)
	MASKED_TRANSFER (source, destination, nbits, (OBJECT_LENGTH - nbits));
    }

  else if (source_offset < destination_offset)
    {
      long offset1 = (destination_offset - source_offset);
      long offset2 = (OBJECT_LENGTH - offset1);
      long head = (OBJECT_LENGTH - destination_offset);
      if (nbits <= head)
	{
	  long mask = (ANY_MASK (nbits, (head - nbits)));
	  (BIT_STRING_WORD (destination))
	    = ((((BIT_STRING_WORD (source)) >> offset1) & mask)
	       | ((BIT_STRING_WORD (destination)) &~ mask));
	}
      else
	{
	  long mask1 = (LOW_MASK (offset1));
	  long mask2 = (LOW_MASK (offset2));
	  {
	    long mask = (LOW_MASK (head));
	    SCHEME_OBJECT temp = (BIT_STRING_WORD (destination));
	    (* (DEC_BIT_STRING_PTR (destination)))
	      = ((((BIT_STRING_WORD (source)) >> offset1) & mask)
		 | (temp &~ mask));
	  }
	  nbits -= head;
	  while (nbits >= OBJECT_LENGTH)
	    {
	      long i
		= (((* (DEC_BIT_STRING_PTR (source))) & mask1) << offset2);
	      (* (DEC_BIT_STRING_PTR (destination)))
		= ((((BIT_STRING_WORD (source)) >> offset1) & mask2) | i);
	      nbits -= OBJECT_LENGTH;
	    }
	  if (nbits > 0)
	    {
	      long dest_tail
		= ((BIT_STRING_WORD (destination))
		   & (LOW_MASK (OBJECT_LENGTH - nbits)));
	      if (nbits <= offset1)
		(BIT_STRING_WORD (destination))
		  = ((((BIT_STRING_WORD (source))
		       & (ANY_MASK (nbits, (offset1 - nbits))))
		      << offset2)
		     | dest_tail);
	      else
		{
		  long i
		    = (((* (DEC_BIT_STRING_PTR (source))) & mask1) << offset2);
		  long j = (nbits - offset1);
		  (BIT_STRING_WORD (destination))
		    = ((((BIT_STRING_WORD (source))
			 &
			 (ANY_MASK (j, (OBJECT_LENGTH - j))))
			>> offset1)
		       | i
		       | dest_tail);
		}
	    }
	}
    }

  else				/* if (source_offset > destination_offset) */
    {
      long offset1 = (source_offset - destination_offset);
      long offset2 = (OBJECT_LENGTH - offset1);
      long head = (OBJECT_LENGTH - source_offset);
      if (nbits <= head)
	{
	  long mask = (ANY_MASK (nbits, (offset1 + (head - nbits))));
	  (BIT_STRING_WORD (destination))
	    = ((((BIT_STRING_WORD (source)) << offset1) & mask)
	       | ((BIT_STRING_WORD (destination)) &~ mask));
	}
      else
	{
	  long mask1 = (LOW_MASK (offset1));
	  long dest_buffer
	    = (((head + offset1) < OBJECT_LENGTH)
	       ? ((BIT_STRING_WORD (destination))
		  &~ (LOW_MASK (head + offset1)))
	       : 0);
	  dest_buffer
	    |= (((* (DEC_BIT_STRING_PTR (source))) & (LOW_MASK (head)))
		<< offset1);
	  nbits -= head;
	  while (nbits >= OBJECT_LENGTH)
	    {
	      (* (DEC_BIT_STRING_PTR (destination)))
		= (dest_buffer
		   | (((BIT_STRING_WORD (source)) >> offset2) & mask1));
	      dest_buffer = ((* (DEC_BIT_STRING_PTR (source))) << offset1);
	      nbits -= OBJECT_LENGTH;
	    }
	  if (nbits <= offset1)
	    (BIT_STRING_WORD (destination))
	      = (dest_buffer
		 | ((BIT_STRING_WORD (destination))
		    & (LOW_MASK (offset1 - nbits)))
		 | (((BIT_STRING_WORD (source)) >> offset2)
		    & (ANY_MASK (nbits, (offset1 - nbits)))));
	  else
	    {
	      (* (DEC_BIT_STRING_PTR (destination)))
		= (dest_buffer
		   | (((BIT_STRING_WORD (source)) >> offset2) & mask1));
	      nbits -= offset1;
	      {
		long mask = (LOW_MASK (OBJECT_LENGTH - nbits));
		(BIT_STRING_WORD (destination))
		  = (((BIT_STRING_WORD (destination)) & mask)
		     | (((BIT_STRING_WORD (source)) << offset1) &~ mask));
	      }
	    }
	}
    }
}

/* Integer <-> Bit-string Conversions */

static unsigned long
ulong_significant_bits (unsigned long number)
{
  unsigned long limit = 1;
  unsigned int nbits = 1;
  while (true)
    {
      if (number <= limit)
	return (nbits);
      limit = ((limit * 2) + 1);
      nbits += 1;
    }
}

static SCHEME_OBJECT
zero_to_bit_string (unsigned long length)
{
  SCHEME_OBJECT result = (allocate_bit_string (length));
  clear_bit_string (result);
  return (result);
}

static SCHEME_OBJECT
ulong_to_bit_string (unsigned long length, unsigned long number)
{
  if (number == 0)
    return (zero_to_bit_string (length));
  if (length < (ulong_significant_bits (number)))
    error_bad_range_arg (2);
  {
    SCHEME_OBJECT result = (zero_to_bit_string (length));
    (BIT_STRING_LSW (result)) = number;
    return (result);
  }
}

static void
btbs_consumer (void * result_ptr,
       long digit)
{
  (* (INC_BIT_STRING_PTR (* ((unsigned char **) result_ptr))))
    = ((unsigned char) digit);
}

static SCHEME_OBJECT
bignum_to_bit_string (unsigned long length, SCHEME_OBJECT bignum)
{
  switch (bignum_test (bignum))
    {
    case bignum_comparison_equal:
      return (zero_to_bit_string (length));
    case bignum_comparison_less:
      error_bad_range_arg (2);
    case bignum_comparison_greater:
      if (! (bignum_fits_in_word_p (bignum, length, 0)))
	error_bad_range_arg (2);
      {
	SCHEME_OBJECT result = (zero_to_bit_string (length));
	unsigned char * result_ptr
	  = ((unsigned char *) (BIT_STRING_LOW_PTR (result)));
	bignum_to_digit_stream
	  (bignum, (1L << CHAR_BIT), btbs_consumer, (&result_ptr));
	return (result);
      }
    default:
      /*NOTREACHED*/
      return (0);
    }
}

struct bitstr_to_bignm_context
{
  unsigned char * source_ptr;
  unsigned int mask;
};

static unsigned int
bstb_producer (void * context)
{
  struct bitstr_to_bignm_context * c = context;
  unsigned int result = (c->mask & (BIT_STRING_WORD (c->source_ptr)));
  c->mask = (LOW_MASK (CHAR_BIT));
  DEC_BIT_STRING_PTR (c->source_ptr);
  return (result);
}

static SCHEME_OBJECT
bit_string_to_bignum (unsigned long nbits, SCHEME_OBJECT bitstr)
{
  unsigned long ndigits = ((nbits + (CHAR_BIT - 1)) / CHAR_BIT);
  struct bitstr_to_bignm_context context;

  (context.mask) = (LOW_MASK (((nbits - 1) % (CHAR_BIT)) + 1));
  (context.source_ptr)
    = ((unsigned char *)
       (MEMORY_LOC (bitstr, (BIT_STRING_INDEX_TO_WORD (bitstr, (nbits - 1))))));

  if (ndigits != 0)
    {
      unsigned long skip
	= ((sizeof (SCHEME_OBJECT))
	   - (((ndigits - 1)
	       % (sizeof (SCHEME_OBJECT)))
	      + 1));
      while (skip > 0)
	{
	  DEC_BIT_STRING_PTR (context.source_ptr);
	  skip -= 1;
	}
    }

  return
    (digit_stream_to_bignum (ndigits, bstb_producer,
			     (&context), (1L << CHAR_BIT),
			     0));
}

DEFINE_PRIMITIVE ("UNSIGNED-INTEGER->BIT-STRING", Prim_unsigned_to_bit_string, 2, 2,
 "(LENGTH INTEGER)\n\
INTEGER, which must be a non-negative integer, is converted to \
a bit-string of length LENGTH.  If INTEGER is too large, an \
error is signalled.")
{
  unsigned long length;
  SCHEME_OBJECT object;
  PRIMITIVE_HEADER (2);

  length = (arg_ulong_integer (1));
  object = (ARG_REF (2));
  if (FIXNUM_P (object))
    {
      if (!FIXNUM_TO_ULONG_P (object))
	error_bad_range_arg (2);
      PRIMITIVE_RETURN
	(ulong_to_bit_string (length, (FIXNUM_TO_ULONG (object))));
    }
  if (BIGNUM_P (object))
    PRIMITIVE_RETURN (bignum_to_bit_string (length, object));
  error_wrong_type_arg (2);
  /*NOTREACHED*/
  return (0);
}

/*  */

DEFINE_PRIMITIVE ("BIT-STRING->UNSIGNED-INTEGER", Prim_bit_string_to_unsigned, 1, 1,
 "(BIT-STRING)\n\
BIT-STRING is converted to the appropriate non-negative integer. \
This operation is the inverse of `unsigned-integer->bit-string'.")
{
  SCHEME_OBJECT bit_string;
  SCHEME_OBJECT * scan;
  unsigned long nwords;
  unsigned long nbits;
  unsigned long word;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, BIT_STRING_P);
  bit_string = (ARG_REF (1));
  /* Count the number of significant bits.*/
  scan = (BIT_STRING_HIGH_PTR (bit_string));
  nbits = ((BIT_STRING_LENGTH (bit_string)) % OBJECT_LENGTH);
  word =
    ((nbits > 0)
     ? ((* (DEC_BIT_STRING_PTR (scan))) & (LOW_MASK (nbits)))
     : (* (DEC_BIT_STRING_PTR (scan))));
  for (nwords = ((VECTOR_LENGTH (bit_string)) - 1); (nwords > 0); nwords -= 1)
    {
      if (word != 0)
	break;
      word = (* (DEC_BIT_STRING_PTR (scan)));
    }
  if (nwords == 0)
    PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (0));
  nbits = (((nwords - 1) * OBJECT_LENGTH) + (ulong_significant_bits (word)));
  PRIMITIVE_RETURN
    ((nbits <= FIXNUM_LENGTH)
     ? (LONG_TO_UNSIGNED_FIXNUM (word))
     : (bit_string_to_bignum (nbits, bit_string)));
}

#define READ_BITS_INITIALIZE()						\
  SCHEME_OBJECT bit_string;						\
  long end, end_mod, offset;						\
  SCHEME_OBJECT *start;							\
  PRIMITIVE_HEADER (3);							\
  CHECK_ARG (3, BIT_STRING_P);						\
  bit_string = (ARG_REF (3));						\
  end = (BIT_STRING_LENGTH (bit_string));				\
  end_mod = (end % OBJECT_LENGTH);					\
  offset = (arg_nonnegative_integer (2));				\
  start = (READ_BITS_PTR ((ARG_REF (1)), offset, end));			\
  COMPUTE_READ_BITS_OFFSET (offset, end)


DEFINE_PRIMITIVE ("READ-BITS!", Prim_read_bits_x, 3, 3,
 "(POINTER OFFSET BIT-STRING)\n\
Read the contents of memory at the address (POINTER,OFFSET) into BIT-STRING.")
{
  READ_BITS_INITIALIZE ();
  copy_bits (start,
	     offset,
	     (MEMORY_LOC
	      (bit_string,
	       (BIT_STRING_INDEX_TO_WORD (bit_string, (end - 1))))),
	     ((end_mod == 0) ? 0 : (OBJECT_LENGTH - end_mod)),
	     end);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("WRITE-BITS!", Prim_write_bits_x, 3, 3,
 "(POINTER OFFSET BIT-STRING)\n\
Write the contents of BIT-STRING in memory at the address (POINTER,OFFSET).")
{
  READ_BITS_INITIALIZE ();
  copy_bits ((MEMORY_LOC
	      (bit_string,
	       (BIT_STRING_INDEX_TO_WORD (bit_string, (end - 1))))),
	     ((end_mod == 0) ? 0 : (OBJECT_LENGTH - end_mod)),
	     start,
	     offset,
	     end);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Search Primitives */

#define SUBSTRING_FIND_INITIALIZE()					\
  SCHEME_OBJECT bit_string;						\
  long start, end;							\
  long word, bit, end_word, end_bit, mask;				\
  SCHEME_OBJECT *scan;							\
  PRIMITIVE_HEADER (3);							\
  CHECK_ARG (1, BIT_STRING_P);						\
  bit_string = (ARG_REF (1));						\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
  if (end > (BIT_STRING_LENGTH (bit_string)))				\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2);						\
  if (start == end)							\
    PRIMITIVE_RETURN (SHARP_F)

#define SUBSTRING_FIND_NEXT_INITIALIZE()				\
  SUBSTRING_FIND_INITIALIZE ();						\
  word = (BIT_STRING_INDEX_TO_WORD (bit_string, start));		\
  bit = (start % OBJECT_LENGTH);					\
  end_word = (BIT_STRING_INDEX_TO_WORD (bit_string, (end - 1)));	\
  end_bit = (((end - 1) % OBJECT_LENGTH) + 1);				\
  scan = (MEMORY_LOC (bit_string, word))

#define FIND_NEXT_SET_LOOP(init_bit)					\
{									\
  bit = (init_bit);							\
  mask = (1L << (init_bit));						\
  while (true)								\
    {									\
      if (((BIT_STRING_WORD (scan)) & mask) != 0)			\
	goto win;							\
      bit += 1;								\
      mask <<= 1;							\
    }									\
}

DEFINE_PRIMITIVE ("BIT-SUBSTRING-FIND-NEXT-SET-BIT", Prim_bitstr_find_next_set_bit, 3, 3,
  "(BIT-STRING START END)")
{
  SUBSTRING_FIND_NEXT_INITIALIZE ();
  if (word == end_word)
    {
      if ((((end_bit - bit) == OBJECT_LENGTH) &&
	   ((BIT_STRING_WORD (scan)) != 0)) ||
	  (((BIT_STRING_WORD (scan)) & (ANY_MASK ((end_bit - bit), bit)))
	   != 0))
	{
	  FIND_NEXT_SET_LOOP (bit);
	}
      PRIMITIVE_RETURN (SHARP_F);
    }
  else if (((BIT_STRING_WORD (scan)) &
	    ((bit == 0) ? (~ 0) : (ANY_MASK ((OBJECT_LENGTH - bit), bit))))
	   != 0)
    {
      FIND_NEXT_SET_LOOP (bit);
    }
  INC_BIT_STRING_PTR (word);
  while (word != end_word)
  {
    if ((* (INC_BIT_STRING_PTR (scan))) != 0)
      {
	FIND_NEXT_SET_LOOP (0);
      }
    INC_BIT_STRING_PTR (word);
  }
  if (((* (INC_BIT_STRING_PTR (scan))) &
       ((end_bit == OBJECT_LENGTH) ? (~ 0) : (LOW_MASK (end_bit))))
      != 0)
    {
      FIND_NEXT_SET_LOOP (0);
    }
  PRIMITIVE_RETURN (SHARP_F);
 win:
  PRIMITIVE_RETURN
    (LONG_TO_UNSIGNED_FIXNUM
     (BIT_STRING_INDEX_PAIR_TO_INDEX (bit_string, word, bit)));
}

void
bit_string_set (SCHEME_OBJECT bitstr, long index, int value)
{
  unsigned long mask;
  SCHEME_OBJECT * ptr;

  ptr = (MEMORY_LOC (bitstr, (BIT_STRING_INDEX_TO_WORD (bitstr, index))));
  mask = (1L << (index % OBJECT_LENGTH));
  if (value == 0)
    (BIT_STRING_WORD (ptr)) &= (~mask);
  else
    (BIT_STRING_WORD (ptr)) |= mask;
  return;
}
