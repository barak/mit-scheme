/* -*-C-*-

Copyright (c) 1987-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* This File contains the code to translate internal format binary
   files to portable format. */

/* IO definitions */

#include "psbmap.h"
#include "limits.h"
#define internal_file input_file
#define portable_file output_file

#undef HEAP_MALLOC
#define HEAP_MALLOC malloc

static long
DEFUN (Load_Data, (Count, To_Where), long Count AND SCHEME_OBJECT *To_Where)
{
  return (fread (((char *) To_Where),
		 (sizeof (SCHEME_OBJECT)),
		 Count,
		 internal_file));
}

#define INHIBIT_FASL_VERSION_CHECK
#define INHIBIT_COMPILED_VERSION_CHECK
#define INHIBIT_CHECKSUMS
#include "load.c"
#include "bltdef.h"
#include "trap.h"

/* Character macros and procedures */

#ifndef _IRIX
extern int strlen ();
#endif

#ifndef isalpha

/* Just in case the stdio library atypically contains the character
   macros, just like the C book claims. */

#include <ctype.h>

#endif /* isalpha */

#ifndef ispunct

/* This is in some libraries but not others */

static char
  punctuation[] = "'\",<.>/?;:{}[]|`~=+-_()*&^%$#@!";

static Boolean
DEFUN (ispunct_local, (c), fast char c)
{
  fast char * s;

  s = &punctuation[0];
  while (*s != '\0')
    if (*s++ == c)
      return (true);
  return (false);
}

#define ispunct ispunct_local

#endif /* ispunct */

/* Needed to upgrade */

#define TC_PRIMITIVE_EXTERNAL	0x10

#define STRING_LENGTH_TO_LONG(value)					\
  ((long) (upgrade_lengths_p ? (OBJECT_DATUM (value)) : (value)))

/* In case there is no compiled code support. */

#ifndef FORMAT_WORD_LOW_BYTE
#define FORMAT_WORD_LOW_BYTE(x) x
#endif

#ifndef FORMAT_WORD_HIGH_BYTE
#define FORMAT_WORD_HIGH_BYTE(x) x
#endif

#ifndef COMPILED_ENTRY_FORMAT_WORD
#define COMPILED_ENTRY_FORMAT_WORD(entry)	0
#endif

#ifndef EXTRACT_EXECUTE_CACHE_ARITY
#define EXTRACT_EXECUTE_CACHE_ARITY(v,a) do { } while (0)
#endif

#if (COMPILER_PROCESSOR_TYPE != COMPILER_LOSING_C_TYPE)

#undef START_CLOSURE_RELOCATION
#undef END_CLOSURE_RELOCATION
#undef EXTRACT_CLOSURE_ENTRY_ADDRESS
#undef STORE_CLOSURE_ENTRY_ADDRESS
#undef EXTRACT_OPERATOR_LINKAGE_ADDRESS
#undef STORE_OPERATOR_LINKAGE_ADDRESS
#undef START_OPERATOR_RELOCATION
#undef END_OPERATOR_RELOCATION

#define START_CLOSURE_RELOCATION(foo) do {} while (0)
#define END_CLOSURE_RELOCATION(foo) do {} while (0)
#define EXTRACT_CLOSURE_ENTRY_ADDRESS(var,addr) do {} while (0)
#define STORE_CLOSURE_ENTRY_ADDRESS(var,addr) do {} while (0)
#define EXTRACT_OPERATOR_LINKAGE_ADDRESS(var,addr) do {} while (0)
#define STORE_OPERATOR_LINKAGE_ADDRESS(var,addr) do {} while (0)
#define START_OPERATOR_RELOCATION(foo) do {} while (0)
#define END_OPERATOR_RELOCATION(foo) do {} while (0)

#endif /* (COMPILER_PROCESSOR_TYPE != COMPILER_LOSING_C_TYPE) */

/* Global data */

static Boolean
  allow_bands_p = false,
  allow_compiled_p = false,
  allow_constant_space_p = false,
  allow_nmv_p = false,
  c_compiled_p = false,
  endian_invert_p = false,
  shuffle_bytes_p = false,
  swap_bytes_p = false,
  upgrade_compiled_p = false,
  upgrade_lengths_p = false,
  upgrade_primitives_p = false,
  upgrade_traps_p = false,
  warn_portable_p = true;

static long
  Heap_Relocation, Constant_Relocation,
  Max_Stack_Offset,
  Scan, Free, Objects,
  Scan_Constant, Free_Constant, Constant_Objects,
  Scan_Pure, Free_Pure, Pure_Objects;

static SCHEME_OBJECT
  * Mem_Base, * Constant_Space, * Constant_Top,
  * Free_Objects, * Free_Cobjects, * Free_Pobjects,
  * compiled_entry_table, * compiled_entry_pointer,
  * compiled_entry_table_end,
  * compiled_block_table, * compiled_block_pointer,
  * compiled_block_table_end,
  * primitive_table, * primitive_table_end,
  * c_code_table, * c_code_table_end;

static long
  NFlonums,
  NIntegers, NBits,
  NBitstrs, NBBits,
  NStrings, NChars,
  NPChars, NCChars;

#define NO_ALIGNMENT(index) do { } while (0)

#ifdef FLOATING_ALIGNMENT
#define INDEX_ALIGN_FLOAT(index) do					\
{									\
  while (((((unsigned long) (& Mem_Base[(index) + 1]))			\
	   - ((unsigned long) (& Mem_Base[0])))				\
	  & FLOATING_ALIGNMENT)						\
	 != 0)								\
    Mem_Base[(index)++] = SHARP_F;					\
} while (0)
#endif /* FLOATING_ALIGNMENT */

#ifndef INDEX_ALIGN_FLOAT
#define INDEX_ALIGN_FLOAT NO_ALIGNMENT
#endif /* INDEX_ALIGN_FLOAT */

#define OUT(s)								\
{									\
  fprintf (portable_file, (s));						\
  break;								\
}

static void
DEFUN (print_a_char, (c, name), fast char c AND char * name)
{
  switch (c)
  {
    case '\n':  OUT ("\\n");
    case '\t':  OUT ("\\t");
    case '\b':  OUT ("\\b");
    case '\r':  OUT ("\\r");
    case '\f':  OUT ("\\f");
    case '\\':  OUT ("\\\\");
    case '\0':  OUT ("\\0");
    case ' ' :  OUT (" ");

    default:
    if ((isascii (c)) && ((isalpha (c)) || (isdigit (c)) || (ispunct (c))))
      putc (c, portable_file);
    else
    {
      unsigned int x = (((int) c) & ((1 << CHAR_BIT) - 1));
      if (warn_portable_p)
      {
	fprintf (stderr,
		 "%s: %s: Warning - file may not be portable: c = 0x%x\n",
		 program_name, name, x);
	warn_portable_p = false;
      }
      /* This does not follow C conventions, but eliminates ambiguity */
      fprintf (portable_file, "\\X%d ", x);
    }
  }
  return;
}

#undef MAKE_BROKEN_HEART
#define MAKE_BROKEN_HEART(offset) (BROKEN_HEART_ZERO + (offset))

#define DO_COMPOUND(Code, Rel, Fre, Scn, Obj, FObj, kernel_code) do	\
{									\
  Old_Address += (Rel);							\
  Old_Contents = (*Old_Address);					\
  if (BROKEN_HEART_P (Old_Contents))					\
    (Mem_Base [(Scn)]) = (OBJECT_NEW_TYPE ((Code), Old_Contents));	\
  else									\
    kernel_code;							\
} while (0)

#define STANDARD_KERNEL(kernel_code, type, Code, Scn, Obj, FObj) do	\
{									\
  (Mem_Base [(Scn)]) = (MAKE_OBJECT ((Code), (Obj)));			\
  {									\
    fast long length = (OBJECT_DATUM (Old_Contents));			\
    kernel_code;							\
    (*Old_Address++) = (MAKE_BROKEN_HEART (Obj));			\
    (Obj) += 1;								\
    (*(FObj)++) = (MAKE_OBJECT ((type), 0));				\
    (*(FObj)++) = Old_Contents;						\
    while ((length--) > 0)						\
      (*(FObj)++) = (*Old_Address++);					\
  }									\
} while (0)

#define DO_STRING_KERNEL() do						\
{									\
  NStrings += 1;							\
  NChars += (pointer_to_char (length - 1));				\
} while (0)

#define DO_BIGNUM_KERNEL() do						\
{									\
  NIntegers += 1;							\
  NBits +=								\
    (((* ((bignum_digit_type *) (Old_Address + 1)))			\
      & BIGNUM_DIGIT_MASK)						\
     * BIGNUM_DIGIT_LENGTH);						\
} while (0)

#define DO_BIT_STRING_KERNEL() do					\
{									\
  NBitstrs += 1;							\
  NBBits += (Old_Address [BIT_STRING_LENGTH_OFFSET]);			\
} while (0)

#define DO_FLONUM_KERNEL(Code, Scn, Obj, FObj) do			\
{									\
  int ctr;								\
  SCHEME_OBJECT * dest;							\
									\
  (Mem_Base [(Scn)]) = (MAKE_OBJECT ((Code), (Obj)));			\
  NFlonums += 1;							\
  (*Old_Address++) = (MAKE_BROKEN_HEART (Obj));				\
  (Obj) += 1;								\
  (*(FObj)++) = (MAKE_OBJECT (TC_BIG_FLONUM, 0));			\
  dest = (FObj);							\
  for (ctr = 0; ctr < float_to_pointer; ctr++)				\
    *dest++ = (*Old_Address++);						\
  (FObj) = dest;							\
} while (0)

#define DO_STRING(Code, Rel, Fre, Scn, Obj, FObj)			\
  DO_COMPOUND (Code, Rel, Fre, Scn, Obj, FObj,				\
	       STANDARD_KERNEL (DO_STRING_KERNEL (),			\
				TC_CHARACTER_STRING,			\
				Code, Scn, Obj, FObj))

#define DO_BIGNUM(Code, Rel, Fre, Scn, Obj, FObj)			\
  DO_COMPOUND (Code, Rel, Fre, Scn, Obj, FObj,				\
	       STANDARD_KERNEL (DO_BIGNUM_KERNEL (), TC_BIG_FIXNUM,	\
				Code, Scn, Obj, FObj))

#define DO_BIT_STRING(Code, Rel, Fre, Scn, Obj, FObj)			\
  DO_COMPOUND (Code, Rel, Fre, Scn, Obj, FObj,				\
	       STANDARD_KERNEL (DO_BIT_STRING_KERNEL (), TC_BIT_STRING,	\
				Code, Scn, Obj, FObj))

#define DO_FLONUM(Code, Rel, Fre, Scn, Obj, FObj)			\
  DO_COMPOUND (Code, Rel, Fre, Scn, Obj, FObj,				\
	       DO_FLONUM_KERNEL (Code, Scn, Obj, FObj))

static void
DEFUN (print_a_fixnum, (val), long val)
{
  fast long size_in_bits;
  fast unsigned long temp;

  temp = ((val < 0) ? -val : val);
  for (size_in_bits = 0; temp != 0; size_in_bits += 1)
    temp = temp >> 1;
  fprintf (portable_file, "%02x %c ", TA_FIXNUM, (val<0) ? '-' : '+');
  if (val == 0)
    fprintf (portable_file, "0\n");
  else
  {
    fprintf (portable_file, "%ld ", size_in_bits);
    temp = ((val < 0) ? -val : val);
    while (temp != 0)
    {
      fprintf (portable_file, "%01lx", (temp & 0xf));
      temp = temp >> 4;
    }
    fprintf (portable_file, "\n");
  }
  return;
}

static void
DEFUN (print_a_string_internal, (len, str), fast long len AND fast char * str)
{
  fprintf (portable_file, "%ld ", len);
  if (shuffle_bytes_p)
  {
    while (len > 0)
    {
      print_a_char (str[3], "print_a_string");
      if (len > 1)
	print_a_char (str[2], "print_a_string");
      if (len > 2)
	print_a_char (str[1], "print_a_string");
      if (len > 3)
	print_a_char (str[0], "print_a_string");
      len -= 4;
      str += 4;
    }
  }
  else
    while (--len >= 0)
      print_a_char (*str++, "print_a_string");
  putc ('\n', portable_file);
  return;
}

static void
DEFUN (print_a_string, (from), SCHEME_OBJECT * from)
{
  long len, maxlen;

  maxlen = ((pointer_to_char ((OBJECT_DATUM (*from++)) - 1)) - 1);
  len = (STRING_LENGTH_TO_LONG (*from++));

  /* If compacting, do not compact strings that have non-default
     maximum lengths.
   */

  fprintf (portable_file,
	   "%02x %ld ",
	   TA_CHARACTER_STRING,
	   ((compact_p
	     && ((BYTES_TO_WORDS (len + 1)) == (BYTES_TO_WORDS (maxlen + 1))))
	    ? len
	    : maxlen));

  print_a_string_internal (len, ((char *) from));
  return;
}

static void
DEFUN (print_a_primitive, (arity, length, name),
       long arity AND long length AND char * name)
{
  fprintf (portable_file, "%ld ", arity);
  print_a_string_internal (length, name);
  return;
}

static void
DEFUN (print_a_c_code_block, (nentries, length, name),
       long nentries AND long length AND char * name)
{
  fprintf (portable_file, "%ld ", nentries);
  print_a_string_internal (length, name);
  return;
}

static long
DEFUN (bignum_length, (bignum), SCHEME_OBJECT bignum)
{
  if (BIGNUM_ZERO_P (bignum))
    return (0);
  {
    bignum_length_type index = ((BIGNUM_LENGTH (bignum)) - 1);
    fast bignum_digit_type digit = (BIGNUM_REF (bignum, index));
    fast long result;
    if (index >= (LONG_MAX / BIGNUM_DIGIT_LENGTH))
      goto loser;
    result = (index * BIGNUM_DIGIT_LENGTH);
    while (digit > 0)
      {
	result += 1;
	if (result >= LONG_MAX)
	  goto loser;
	digit >>= 1;
      }
    return (result);
  }
 loser:
  fprintf (stderr, "%s: Bignum exceeds representable length.\n",
	   program_name);
  quit (1);
  /* NOTREACHED */
}

static void
DEFUN (print_a_bignum, (bignum_ptr), SCHEME_OBJECT * bignum_ptr)
{
  SCHEME_OBJECT bignum;

  bignum = (MAKE_POINTER_OBJECT (TC_BIG_FIXNUM, bignum_ptr));

  if (BIGNUM_ZERO_P (bignum))
  {
    fprintf (portable_file, "%02x + 0\n",
	     (compact_p ? TA_FIXNUM : TA_BIGNUM));
    return;
  }
  {
    int the_type = TA_BIGNUM;
    bignum_digit_type * scan = (BIGNUM_START_PTR (bignum));
    fast long length_in_bits = (bignum_length (bignum));
    fast int bits_in_digit = 0;
    fast bignum_digit_type accumulator;

    /* This attempts to preserve non-canonicalized bignums as such. 
       The test below fails for the most negative fixnum represented
       as a bignum
     */ 

    if (compact_p && (length_in_bits > fixnum_to_bits))
      the_type = TA_FIXNUM;

    fprintf (portable_file, "%02x %c %ld ",
	     the_type,
	     ((BIGNUM_NEGATIVE_P (bignum)) ? '-' : '+'),
	     length_in_bits);
    accumulator = (*scan++);
    bits_in_digit = ((length_in_bits < BIGNUM_DIGIT_LENGTH)
		     ? length_in_bits
		     : BIGNUM_DIGIT_LENGTH);
    while (length_in_bits > 0)
      {
	if (bits_in_digit > 4)
	  {
	    fprintf (portable_file, "%01lx", (accumulator & 0xf));
	    length_in_bits -= 4;
	    accumulator >>= 4;
	    bits_in_digit -= 4;
	  }
	else if (bits_in_digit == 4)
	  {
	    fprintf (portable_file, "%01lx", accumulator);
	    length_in_bits -= 4;
	    if (length_in_bits >= BIGNUM_DIGIT_LENGTH)
	      {
		accumulator = (*scan++);
		bits_in_digit = BIGNUM_DIGIT_LENGTH;
	      }
	    else if (length_in_bits > 0)
	      {
		accumulator = (*scan++);
		bits_in_digit = length_in_bits;
	      }
	    else
	      break;
	  }
	else if (bits_in_digit < length_in_bits)
	  {
	    long carry = accumulator;
	    int diff_bits = (4 - bits_in_digit);
	    accumulator = (*scan++);
	    fprintf (portable_file, "%01lx",
		     (carry
		      | ((accumulator & ((1 << diff_bits) - 1)) <<
			 bits_in_digit)));
	    length_in_bits -= 4;
	    bits_in_digit = (BIGNUM_DIGIT_LENGTH - diff_bits);
	    if (length_in_bits >= bits_in_digit)
	      accumulator >>= diff_bits;
	    else if (length_in_bits > 0)
	      {
		accumulator >>= diff_bits;
		bits_in_digit = length_in_bits;
	      }
	    else
	      break;
	  }
	else
	  {
	    fprintf (portable_file, "%01lx", accumulator);
	    break;
	  }
      }
  }
  fprintf (portable_file, "\n");
  return;
}

/* The following procedure assumes that a C long is at least 4 bits. */

static void
DEFUN (print_a_bit_string, (from), SCHEME_OBJECT * from)
{
  SCHEME_OBJECT the_bit_string;
  fast long bits_remaining, leftover_bits;
  fast SCHEME_OBJECT accumulator, next_word, *scan;

  the_bit_string = (MAKE_POINTER_OBJECT (TC_BIT_STRING, from));
  bits_remaining = (BIT_STRING_LENGTH (the_bit_string));
  fprintf (portable_file, "%02x %ld", TA_VECTOR_1B, bits_remaining);

  if (bits_remaining != 0)
  {
    fprintf (portable_file, " ");
    scan = (BIT_STRING_LOW_PTR (the_bit_string));
    for (leftover_bits = 0;
	 bits_remaining > 0;
	 bits_remaining -= OBJECT_LENGTH)
    {
      next_word = (* (INC_BIT_STRING_PTR (scan)));

      if (bits_remaining < OBJECT_LENGTH)
	next_word &= (LOW_MASK (bits_remaining));

      if (leftover_bits == 0)
	leftover_bits = ((bits_remaining > OBJECT_LENGTH)
			 ? OBJECT_LENGTH
			 : bits_remaining);
      else
      {
	accumulator &= (LOW_MASK (leftover_bits));
	accumulator |=
	  ((next_word & (LOW_MASK (4 - leftover_bits))) << leftover_bits);
	next_word = (next_word >> (4 - leftover_bits));
	leftover_bits += ((bits_remaining > OBJECT_LENGTH)
			  ? (OBJECT_LENGTH - 4)
			  : (bits_remaining - 4));
	fprintf (portable_file, "%01lx", (accumulator & 0xf));
      }

      for (accumulator = next_word; leftover_bits >= 4; leftover_bits -= 4)
      {
	fprintf (portable_file, "%01lx", (accumulator & 0xf));
	accumulator = (accumulator >> 4);
      }
    }
    if (leftover_bits != 0)
      fprintf (portable_file, "%01lx", (accumulator & 0xf));
  }
  fprintf (portable_file, "\n");
  return;
}

union flonum_u
{
  double dval;
  unsigned long lval[float_to_pointer];
};

static void
DEFUN (print_a_flonum, (src), SCHEME_OBJECT * src)
{
  double val;
  union flonum_u utemp;
  fast long size_in_bits;
  fast double mant, temp;
  int expt, ctr;
  extern double EXFUN (frexp, (double, int *));

  for (ctr = 0; ctr < float_to_pointer; ctr++)
    utemp.lval[ctr] = ((unsigned long) src[ctr]);
  val = utemp.dval;

  fprintf (portable_file, "%02x %c ",
	   TA_FLONUM,
	   ((val < 0.0) ? '-' : '+'));
  if (val == 0.0)
  {
    fprintf (portable_file, "0\n");
    return;
  }
  mant = frexp (((val < 0.0) ? -val : val), &expt);
  size_in_bits = 1;

  for (temp = ((mant * 2.0) - 1.0); temp != 0; size_in_bits += 1)
  {
    temp *= 2.0;
    if (temp >= 1.0)
      temp -= 1.0;
  }
  fprintf (portable_file, "%ld %ld ", expt, size_in_bits);

  for (size_in_bits = (hex_digits (size_in_bits));
       size_in_bits > 0;
       size_in_bits -= 1)
  {
    fast unsigned int digit;

    digit = 0;
    for (expt = 4; --expt >= 0;)
    {
      mant *= 2.0;
      digit = digit << 1;
      if (mant >= 1.0)
      {
	mant -= 1.0;
	digit += 1;
      }
    }
    fprintf (portable_file, "%01x", digit);
  }
  putc ('\n', portable_file);
  return;
}

/* Normal Objects */

#define DO_CELL(Code, Rel, Fre, Scn, Obj, FObj) do			\
{									\
  Old_Address += (Rel);							\
  Old_Contents = (*Old_Address);					\
  if (BROKEN_HEART_P (Old_Contents))					\
    (Mem_Base [(Scn)]) =						\
      (MAKE_OBJECT_FROM_OBJECTS (This, Old_Contents));			\
  else									\
    {									\
      (*Old_Address++) = (MAKE_BROKEN_HEART (Fre));			\
      (Mem_Base [(Scn)]) = (OBJECT_NEW_DATUM (This, (Fre)));		\
      (Mem_Base [(Fre)++]) = Old_Contents;				\
    }									\
} while (0)

#define DO_PAIR(Code, Rel, Fre, Scn, Obj, FObj) do			\
{									\
  Old_Address += (Rel);							\
  Old_Contents = (*Old_Address);					\
  if (BROKEN_HEART_P (Old_Contents))					\
    (Mem_Base [(Scn)]) =						\
      (MAKE_OBJECT_FROM_OBJECTS (This, Old_Contents));			\
  else									\
    {									\
      (*Old_Address++) = (MAKE_BROKEN_HEART (Fre));			\
      (Mem_Base [(Scn)]) = (OBJECT_NEW_DATUM (This, (Fre)));		\
      (Mem_Base [(Fre)++]) = Old_Contents;				\
      (Mem_Base [(Fre)++]) = (*Old_Address++);				\
    }									\
} while (0)

#define DO_TRIPLE(Code, Rel, Fre, Scn, Obj, FObj) do			\
{									\
  Old_Address += (Rel);							\
  Old_Contents = (*Old_Address);					\
  if (BROKEN_HEART_P (Old_Contents))					\
    (Mem_Base [(Scn)]) =						\
      (MAKE_OBJECT_FROM_OBJECTS (This, Old_Contents));			\
  else									\
    {									\
      (*Old_Address++) = (MAKE_BROKEN_HEART (Fre));			\
      (Mem_Base [(Scn)]) = (OBJECT_NEW_DATUM (This, (Fre)));		\
      (Mem_Base [(Fre)++]) = Old_Contents;				\
      (Mem_Base [(Fre)++]) = (*Old_Address++);				\
      (Mem_Base [(Fre)++]) = (*Old_Address++);				\
    }									\
} while (0)

#define DO_QUAD(Code, Rel, Fre, Scn, Obj, FObj) do			\
{									\
  Old_Address += (Rel);							\
  Old_Contents = (*Old_Address);					\
  if (BROKEN_HEART_P (Old_Contents))					\
    (Mem_Base [(Scn)]) =						\
      (MAKE_OBJECT_FROM_OBJECTS (This, Old_Contents));			\
  else									\
    {									\
      (*Old_Address++) = (MAKE_BROKEN_HEART (Fre));			\
      (Mem_Base [(Scn)]) = (OBJECT_NEW_DATUM (This, (Fre)));		\
      (Mem_Base [(Fre)++]) = Old_Contents;				\
      (Mem_Base [(Fre)++]) = (*Old_Address++);				\
      (Mem_Base [(Fre)++]) = (*Old_Address++);				\
      (Mem_Base [(Fre)++]) = (*Old_Address++);				\
    }									\
} while (0)

#define DO_RAW_QUAD(Code, Rel, Fre, Scn, Obj, FObj) do			\
{									\
  Old_Address += (Rel);							\
  Old_Contents = (* Old_Address);					\
  if (BROKEN_HEART_P (Old_Contents))					\
    (Mem_Base [(Scn)]) = (OBJECT_DATUM (Old_Contents));			\
  else									\
    {									\
      (*Old_Address++) = (MAKE_BROKEN_HEART (Fre));			\
      (Mem_Base [(Scn)]) = (Fre);					\
      (Mem_Base [(Fre)++]) = Old_Contents;				\
      (Mem_Base [(Fre)++]) = (*Old_Address++);				\
      (Mem_Base [(Fre)++]) = (*Old_Address++);				\
      (Mem_Base [(Fre)++]) = (*Old_Address++);				\
    }									\
} while (0)

#define COPY_VECTOR(Fre) do						\
{									\
  fast long len = (OBJECT_DATUM (Old_Contents));			\
  (*Old_Address++) = (MAKE_BROKEN_HEART (Fre));				\
  if (Old_Contents == SHARP_F)						\
    (Mem_Base [(Fre)++]) = ALIASED_LENGTH_SHARP_F;			\
  else									\
    (Mem_Base [(Fre)++]) = Old_Contents;				\
  while ((len--) > 0)							\
    (Mem_Base [(Fre)++]) = (*Old_Address++);				\
} while (0)

/* This is a hack to get the cross compiler to work
   accross different endianness.
*/

#define COPY_INVERTED_VECTOR(Fre) do					\
{									\
  fast long len1, len2;							\
  SCHEME_OBJECT * Saved;						\
									\
  len1 = (OBJECT_DATUM (Old_Contents));					\
  (*Old_Address++) = (MAKE_BROKEN_HEART (Fre));				\
  (Mem_Base [(Fre)++]) = Old_Contents;					\
  if ((OBJECT_TYPE (* Old_Address)) != TC_MANIFEST_NM_VECTOR)		\
  {									\
    fprintf (stderr, "%s: Bad compiled code block found.\n",		\
	     program_name);						\
    quit (1);								\
  }									\
  len2 = (OBJECT_DATUM (*Old_Address));					\
  (Mem_Base [(Fre)++]) = (*Old_Address++);				\
  Old_Address += len2;							\
  Saved = Old_Address;							\
  len1 -= (len2 + 1);							\
  while ((len2--) > 0)							\
    (Mem_Base [(Fre)++]) = (*--Old_Address);				\
  Old_Address = Saved;							\
  while ((len1--) > 0)							\
    (Mem_Base [(Fre)++]) = (*Old_Address++);				\
} while (0)

#define DO_VECTOR_2(aligner, copier, Code, Rel, Fre, Scn, Obj, FObj) do	\
{									\
  Old_Address += (Rel);							\
  Old_Contents = (*Old_Address);					\
  if (BROKEN_HEART_P (Old_Contents))					\
    (Mem_Base [(Scn)]) =						\
      (MAKE_OBJECT_FROM_OBJECTS (This, Old_Contents));			\
  else									\
    {									\
      aligner (Fre);							\
      (Mem_Base [(Scn)]) = (OBJECT_NEW_DATUM (This, (Fre)));		\
      copier (Fre);							\
    }									\
} while (0)

#define DO_VECTOR(Code, Rel, Fre, Scn, Obj, FObj)			\
  DO_VECTOR_2 (NO_ALIGNMENT, COPY_VECTOR,				\
	       Code, Rel, Fre, Scn, Obj, FObj)

#define DO_INVERTED_BLOCK(Code, Rel, Fre, Scn, Obj, FObj)		\
  DO_VECTOR_2 (NO_ALIGNMENT, COPY_INVERTED_VECTOR,			\
	       Code, Rel, Fre, Scn, Obj, FObj)

#ifdef HAS_COMPILER_SUPPORT

#define CHAR_OFFSET(a,b) (((char *) (a)) - ((char *) (b)))
#define OBJ_OFFSET(a,b)  (((SCHEME_OBJECT *) (a)) - ((SCHEME_OBJECT *) (b)))

#define DO_ENTRY_INTERNAL(sub, copy, Code, Rel, Fre, Scn, Obj, FObj) do	\
{									\
  long offset;								\
  SCHEME_OBJECT * saved;						\
									\
  Old_Address += (Rel);							\
  saved = Old_Address;							\
  Get_Compiled_Block (Old_Address, saved);				\
  Old_Contents = (*Old_Address);					\
  entry_no = (compiled_entry_pointer - compiled_entry_table);		\
  offset = (sub (saved, Old_Address));					\
  (*compiled_entry_pointer++) = (LONG_TO_UNSIGNED_FIXNUM (offset));	\
  if (BROKEN_HEART_P (Old_Contents))					\
    (*compiled_entry_pointer++) =					\
      (MAKE_OBJECT_FROM_OBJECTS (This, Old_Contents));			\
  else									\
  {									\
    INDEX_ALIGN_FLOAT (Fre);						\
    (*compiled_entry_pointer++) =					\
      (MAKE_OBJECT_FROM_OBJECTS (This, (Fre)));				\
    copy (Fre);								\
  }									\
} while (0)

#define DO_COMPILED_BLOCK(Code, Rel, Fre, Scn, Obj, FObj)		\
  DO_VECTOR_2 (INDEX_ALIGN_FLOAT, COPY_VECTOR,				\
	       Code, Rel, Fre, Scn, Obj, FObj)

#define DO_INVERTED_COMPILED_BLOCK(Code, Rel, Fre, Scn, Obj, FObj)	\
  DO_VECTOR_2 (INDEX_ALIGN_FLOAT, COPY_INVERTED_VECTOR,			\
	       Code, Rel, Fre, Scn, Obj, FObj)

#define DO_C_COMPILED_BLOCK(Code, Rel, Fre, Scn, Obj, FObj)		\
  DO_VECTOR_2 (INDEX_ALIGN_FLOAT, COPY_C_COMPILED_BLOCK,		\
	       Code, Rel, Fre, Scn, Obj, FObj)

#define DO_COMPILED_ENTRY(Code, Rel, Fre, Scn, Obj, FObj)		\
  DO_ENTRY_INTERNAL (CHAR_OFFSET, COPY_VECTOR,				\
		     Code, Rel, Fre, Scn, Obj, FObj)

#define DO_C_COMPILED_ENTRY(Code, Rel, Fre, Scn, Obj, FObj)		\
  DO_ENTRY_INTERNAL (OBJ_OFFSET, COPY_C_COMPILED_BLOCK,			\
		     Code, Rel, Fre, Scn, Obj, FObj)

/* This depends on the fact that a compiled code block has an NMV
   header in the first or second words.
 */

long
DEFUN (copy_c_compiled_block, (Fre, Old_Contents, Old_Address),
       long Fre AND SCHEME_OBJECT Old_Contents AND SCHEME_OBJECT * Old_Address)
{
  SCHEME_OBJECT preserved_nmv, preserved_loc;
  SCHEME_OBJECT nmv_replacement
    = (MAKE_OBJECT (TC_BROKEN_HEART,
		    (compiled_block_pointer
		     - compiled_block_table)));
  fast long len = (OBJECT_DATUM (Old_Contents));

  *Old_Address++ = (MAKE_BROKEN_HEART (Fre));
  if ((OBJECT_TYPE (Old_Contents)) != TC_MANIFEST_CLOSURE)
  {
    if ((OBJECT_TYPE (Old_Contents)) == TC_MANIFEST_NM_VECTOR)
    {
      preserved_nmv = Old_Contents;
      preserved_loc = (LONG_TO_UNSIGNED_FIXNUM (Fre));
      Old_Contents = nmv_replacement;
    }
    else if ((OBJECT_TYPE (*Old_Address)) == TC_MANIFEST_NM_VECTOR)
    {
      preserved_nmv = *Old_Address;
      preserved_loc = (LONG_TO_UNSIGNED_FIXNUM ((Fre) + 1));
      *Old_Address = nmv_replacement;
    }
    else
    {
      fprintf (stderr,
	       "%s: Improperly formatted C-compiled code block.\n",
	       program_name);
      quit (1);
    }

    *compiled_block_pointer++ = preserved_loc;
    *compiled_block_pointer++ = preserved_nmv;
  }

  (Mem_Base [(Fre)++]) = Old_Contents;
  while ((len--) > 0)
    (Mem_Base [(Fre)++]) = (*Old_Address++);
  return (Fre);
}

#define COPY_C_COMPILED_BLOCK(Fre) do					\
{									\
  Fre = copy_c_compiled_block (Fre, Old_Contents, Old_Address);		\
} while (0)

#else /* no HAS_COMPILER_SUPPORT */

#define COMPILER_BAD_STMT(name) do					\
{									\
  fprintf (stderr,							\
	   "%s: Invoking %s with no compiler support!\n",		\
	   program_name, name);						\
  quit (1);								\
} while (0)

#define DO_COMPILED_ENTRY(Code, Rel, Fre, Scn, Obj, FObj)		\
  COMPILER_BAD_STMT ("DO_COMPILED_ENTRY")

#define DO_COMPILED_BLOCK(Code, Rel, Fre, Scn, Obj, FObj)		\
  COMPILER_BAD_STMT ("DO_COMPILED_BLOCK")

#define DO_INVERTED_COMPILED_BLOCK(Code, Rel, Fre, Scn, Obj, FObj)	\
  COMPILER_BAD_STMT ("DO_INVERTED_COMPILED_BLOCK")

#define DO_C_COMPILED_ENTRY(Code, Rel, Fre, Scn, Obj, FObj)		\
  COMPILER_BAD_STMT ("DO_C_COMPILED_ENTRY")

#define  DO_C_COMPILED_BLOCK(Code, Rel, Fre, Scn, Obj, FObj)
  COMPILER_BAD_STMT ("DO_C_COMPILED_BLOCK")

#endif /* HAS_COMPILER_SUPPORT */

/* Constant/Pure space utilities */

static SCHEME_OBJECT *
DEFUN (find_constant_top, (constant_space, count),
       SCHEME_OBJECT * constant_space AND unsigned long count)
{
  SCHEME_OBJECT pattern = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0));
  SCHEME_OBJECT * limit = (constant_space + count);

  while (((* (limit - 1)) == pattern)
	 && (limit > constant_space))
    limit -= 1;
  return (limit);
}

static Boolean
DEFUN (address_in_pure_space, (addr), SCHEME_OBJECT * addr)
{
  Boolean result = false;
  SCHEME_OBJECT * where, * low_constant;

  low_constant = Constant_Space;
  where = (Constant_Top - 1);

  while (where >= low_constant)
  {
    where -= (1 + (OBJECT_DATUM (* where)));
    if (where < addr)
    {
      where += 1;		/* block start */
      result = (addr <= (where + (OBJECT_DATUM (* where))));
      break;
    }
  }
  return (result);
}

/* Common Pointer Code */

#define DO_POINTER(Scn, Action) do					\
{									\
  long the_datum;							\
									\
  Old_Address = (OBJECT_ADDRESS (This));				\
  the_datum = (OBJECT_DATUM (This));					\
  if ((the_datum >= Heap_Base) && (the_datum < Dumped_Heap_Top))	\
    Action (HEAP_CODE, Heap_Relocation, Free,				\
	    Scn, Objects, Free_Objects);				\
  else if ((the_datum >= Const_Base)					\
	   && (the_datum < Dumped_Constant_Top))			\
  {									\
    SCHEME_OBJECT * new_addr;						\
									\
    new_addr = (Old_Address + Constant_Relocation);			\
    if (address_in_pure_space (new_addr))				\
      Action (PURE_CODE, Constant_Relocation, Free_Pure,		\
	      Scn, Pure_Objects, Free_Pobjects);			\
    else								\
      Action (CONSTANT_CODE, Constant_Relocation, Free_Constant,	\
	      Scn, Constant_Objects, Free_Cobjects);			\
  }									\
  else									\
    out_of_range_pointer (This);					\
  (Scn) += 1;								\
} while (0)

#define DO_RAW_POINTER(ptr, Scn, Action) do				\
{									\
  long the_datum;							\
									\
  the_datum = (SCHEME_ADDR_TO_OLD_DATUM (ptr));				\
  Old_Address = (DATUM_TO_ADDRESS (the_datum));				\
  if ((the_datum >= Heap_Base) && (the_datum < Dumped_Heap_Top))	\
    Action (HEAP_CODE, Heap_Relocation, Free,				\
	    Scn, Objects, Free_Objects);				\
  else if ((the_datum >= Const_Base)					\
	   && (the_datum < Dumped_Constant_Top))			\
  {									\
    SCHEME_OBJECT * new_addr;						\
									\
    new_addr = (Old_Address + Constant_Relocation);			\
    if (address_in_pure_space (new_addr))				\
      Action (PURE_CODE, Constant_Relocation, Free_Pure,		\
	      Scn, Pure_Objects, Free_Pobjects);			\
    else								\
      Action (CONSTANT_CODE, Constant_Relocation, Free_Constant,	\
	      Scn, Constant_Objects, Free_Cobjects);			\
  }									\
  else									\
    out_of_range_pointer (This);					\
} while (0)

static void
DEFUN (out_of_range_pointer, (ptr), SCHEME_OBJECT ptr)
{
  fprintf (stderr,
	   "%s: The input file is not portable: Out of range pointer.\n",
	   program_name);
  fprintf (stderr, "Heap_Base =  0x%lx;\tHeap_Top = 0x%lx\n",
	   Heap_Base, Dumped_Heap_Top);
  fprintf (stderr, "Const_Base = 0x%lx;\tConst_Top = 0x%lx\n",
	   Const_Base, Dumped_Constant_Top);
  fprintf (stderr, "ptr = 0x%02x|0x%lx\n",
	   (OBJECT_TYPE (ptr)), (OBJECT_DATUM (ptr)));
  quit (1);
}

static SCHEME_OBJECT *
DEFUN (relocate, (object), SCHEME_OBJECT object)
{
  long the_datum;
  SCHEME_OBJECT * result;

  result = (OBJECT_ADDRESS (object));
  the_datum = (OBJECT_DATUM (object));

  if ((the_datum >= Heap_Base) &&
      (the_datum < Dumped_Heap_Top))
    result += Heap_Relocation;
  else if ((the_datum >= Const_Base) &&
	   (the_datum < Dumped_Constant_Top))
      result += Constant_Relocation;
  else
    out_of_range_pointer (object);
  return (result);
}

/* Primitive upgrading code. */

#define PRIMITIVE_UPGRADE_SPACE 2048

static SCHEME_OBJECT
  * internal_renumber_table,
  * external_renumber_table,
  * external_prim_name_table;

static Boolean
  found_ext_prims = false;

static SCHEME_OBJECT
DEFUN (upgrade_primitive, (prim), SCHEME_OBJECT prim)
{
  long the_datum, the_type, new_type, code;
  SCHEME_OBJECT new;

  the_datum = (OBJECT_DATUM (prim));
  the_type = (OBJECT_TYPE (prim));
  if (the_type != TC_PRIMITIVE_EXTERNAL)
  {
    code = the_datum;
    new_type = the_type;
  }
  else
  {
    found_ext_prims = true;
    code = (the_datum + (MAX_BUILTIN_PRIMITIVE + 1));
    new_type = TC_PRIMITIVE;
  }

  new = internal_renumber_table[code];
  if (new != SHARP_F)
    return (OBJECT_NEW_TYPE (new_type, new));
  else
  {
    /*
      This does not need to check for overflow because the worst case
      was checked in setup_primitive_upgrade;
     */

    new = (MAKE_OBJECT (new_type, Primitive_Table_Length));
    internal_renumber_table[code] = new;
    external_renumber_table[Primitive_Table_Length] = prim;
    Primitive_Table_Length += 1;
    if (the_type == TC_PRIMITIVE_EXTERNAL)
      NPChars +=
	STRING_LENGTH_TO_LONG ((((SCHEME_OBJECT *)
				 (external_prim_name_table[the_datum]))
				[STRING_LENGTH_INDEX]));
    else
      NPChars += strlen (builtin_prim_name_table[the_datum]);
    return (new);
  }
}

static SCHEME_OBJECT *
DEFUN (setup_primitive_upgrade, (Heap), SCHEME_OBJECT * Heap)
{
  fast long count, length;
  SCHEME_OBJECT * old_prims_vector;

  internal_renumber_table = &Heap[0];
  external_renumber_table =
    &internal_renumber_table[PRIMITIVE_UPGRADE_SPACE];
  external_prim_name_table =
    &external_renumber_table[PRIMITIVE_UPGRADE_SPACE];

  old_prims_vector = (relocate (Ext_Prim_Vector));
  if (*old_prims_vector == SHARP_F)
    length = 0;
  else
  {
    old_prims_vector = (relocate (*old_prims_vector));
    length = (OBJECT_DATUM (*old_prims_vector));
    old_prims_vector += VECTOR_DATA;
    for (count = 0; count < length; count += 1)
    {
      SCHEME_OBJECT *temp;

      /* symbol */
      temp = (relocate (old_prims_vector[count]));
      /* string */
      temp = (relocate (temp[SYMBOL_NAME]));
      external_prim_name_table[count] = ((SCHEME_OBJECT) temp);
    }
  }
  length += (MAX_BUILTIN_PRIMITIVE + 1);
  if (length > PRIMITIVE_UPGRADE_SPACE)
  {
    fprintf (stderr, "%s: Too many primitives.\n", program_name);
    fprintf (stderr,
	     "Increase PRIMITIVE_UPGRADE_SPACE and recompile %s.\n",
	     program_name);
    quit (1);
  }
  for (count = 0; count < length; count += 1)
    internal_renumber_table[count] = SHARP_F;

  NPChars = 0;
  return (&external_prim_name_table[PRIMITIVE_UPGRADE_SPACE]);
}

/* Processing of a single area */

#define DO_AREA(code, Area, Bound, Obj, FObj)				\
  Process_Area (code, &Area, &Bound, &Obj, &FObj)

static void
DEFUN (Process_Area, (Code, Area, Bound, Obj, FObj),
       int Code
       AND fast long * Area
       AND fast long * Bound
       AND fast long * Obj
       AND fast SCHEME_OBJECT ** FObj)
{
  unsigned long entry_no;
  fast SCHEME_OBJECT This, * Old_Address, Old_Contents;

  while (*Area != *Bound)
  {
    This = Mem_Base[*Area];

#ifdef PRIMITIVE_EXTERNAL_REUSED
    if (upgrade_primitives_p &&
	((OBJECT_TYPE (This)) == TC_PRIMITIVE_EXTERNAL))
    {
      Mem_Base[*Area] = (upgrade_primitive (This));
      *Area += 1;
      continue;
    }
#endif /* PRIMITIVE_EXTERNAL_REUSED */

    Switch_by_GC_Type (This)
    {

#ifndef PRIMITIVE_EXTERNAL_REUSED

      case TC_PRIMITIVE_EXTERNAL:

#endif /* PRIMITIVE_EXTERNAL_REUSED */

      case TC_PRIMITIVE:
      case TC_PCOMB0:
	if (upgrade_primitives_p)
	  Mem_Base[*Area] = (upgrade_primitive (This));
	*Area += 1;
	break;

      case TC_MANIFEST_NM_VECTOR:
	if (This == ALIASED_LENGTH_SHARP_F) /* See psbmap.h */
	{
	    *Area += 1;
	    break;
	}
	nmv_p = true;
        if (null_nmv_p)
	{
	  fast long i;

	  i = (OBJECT_DATUM (This));
	  *Area += 1;
	  for ( ; --i >= 0; *Area += 1)
	    Mem_Base[*Area] = SHARP_F;
	  break;
	}
	else if (!allow_nmv_p)
	{
	  if (((OBJECT_DATUM (This)) != 0) && warn_portable_p)
	  {
	    warn_portable_p = false;
	    fprintf (stderr, "%s: Warning - file is not portable: NMH found\n",
		     program_name);
	  }
	}
	*Area += (1 + (OBJECT_DATUM (This)));
	break;

      case TC_BROKEN_HEART:
      {
	/* [Broken Heart | 0] is the cdr of fasdumped symbols. */
	/* [Broken Heart | x > 0] indicates a C compiled block. */
	unsigned long the_datum = (OBJECT_DATUM (This));

	if (the_datum == 0)
	{
	  *Area += 1;
	  break;
	}
	else if ((! allow_compiled_p)
		 || (! c_compiled_p)
		 || ((OBJECT_DATUM (This))
		     >= (compiled_block_pointer - compiled_block_table))
		 || ((*Area)
		     != (UNSIGNED_FIXNUM_TO_LONG
			 (compiled_block_table [the_datum]))))
	{
	  fprintf (stderr, "%s: Broken Heart found in scan.\n",
		   program_name);
	  quit (1);
	}
	else
	{
	  *Area += (1 + (OBJECT_DATUM (compiled_block_table [1 + the_datum])));
	  break;
	}
      }

      case TC_MANIFEST_CLOSURE:
	if ((! allow_compiled_p) || (! c_compiled_p))
	{
	  fprintf (stderr,
		   "%s: File contains compiled closures.\n",
		   program_name);
	  quit (1);
	}
	else
	{
	  char * word_ptr;
	  long count, address = 0;
	  SCHEME_OBJECT * area_end, * scan, * i_scan;

	  i_scan = (&Mem_Base[*Area]);
	  START_CLOSURE_RELOCATION (i_scan);
	  scan = (i_scan + 1);
	  count = (MANIFEST_CLOSURE_COUNT (scan));
	  word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (scan));
	  area_end = (MANIFEST_CLOSURE_END (scan, count));

	  while ((--count) >= 0)
	  {
	    scan = ((SCHEME_OBJECT *) (word_ptr));
	    word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr));
	    EXTRACT_CLOSURE_ENTRY_ADDRESS (address, scan);
	    DO_RAW_POINTER (address, *Area, DO_C_COMPILED_ENTRY);
	    STORE_CLOSURE_ENTRY_ADDRESS (entry_no, scan);
	  }

	  END_CLOSURE_RELOCATION (area_end);
	  *Area += (1 + (area_end - i_scan));
	  break;
	}

      case TC_LINKAGE_SECTION:
	if ((! allow_compiled_p) || (! c_compiled_p))
	{
	  fprintf (stderr,
		   "%s: File contains linked compiled code.\n",
		   program_name);
	  quit (1);
	}
	else
	{
	  switch (READ_LINKAGE_KIND (This))
	  {
	    case REFERENCE_LINKAGE_KIND:
	    case ASSIGNMENT_LINKAGE_KIND:
	    {
	      long count = (READ_CACHE_LINKAGE_COUNT (This));

	      *Area += 1;
	      while (--count >= 0)
	      {
		DO_RAW_POINTER (Mem_Base[*Area], *Area, DO_RAW_QUAD);
		*Area += 1;
	      }
	      break;
	    }
	    
	    case OPERATOR_LINKAGE_KIND:
	    case GLOBAL_OPERATOR_LINKAGE_KIND:
	    {
	      char * word_ptr;
	      long count, address = 0;
	      SCHEME_OBJECT * area_end, * scan, * i_scan;

	      i_scan = (&Mem_Base[*Area]);
	      START_OPERATOR_RELOCATION (i_scan);
	      count = (READ_OPERATOR_LINKAGE_COUNT (This));
	      word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (i_scan));
	      area_end = (END_OPERATOR_LINKAGE_AREA (i_scan, count));

	      while (--count >= 0)
	      {
		scan = ((SCHEME_OBJECT *) word_ptr);
		word_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
		EXTRACT_OPERATOR_LINKAGE_ADDRESS (address, scan);
		DO_RAW_POINTER (address, *Area, DO_C_COMPILED_ENTRY);
		STORE_OPERATOR_LINKAGE_ADDRESS (entry_no, scan);
	      }
	      END_OPERATOR_RELOCATION (area_end);
	      *Area += (1 + (area_end - i_scan));
	      break;
	    }

	    default:
	    {
	      fprintf (stderr, "%s: Unknown linkage kind.\n",
		       program_name);
	      quit (1);
	    }
	  }
	  break;
	}

      case TC_COMPILED_CODE_BLOCK:
	compiled_p = true;
	if (! allow_compiled_p)
	{
	  fprintf (stderr,
		   "%s: File contains compiled code.\n",
		   program_name);
	  quit (1);
	}
	else if (c_compiled_p)
	  DO_POINTER (*Area, DO_C_COMPILED_BLOCK);
	else if (endian_invert_p)
	  DO_POINTER (*Area, DO_INVERTED_COMPILED_BLOCK);
	else
	  DO_POINTER (*Area, DO_COMPILED_BLOCK);
	break;

      case_compiled_entry_point:
	compiled_p = true;
	if (! allow_compiled_p)
	{
	  fprintf (stderr,
		   "%s: File contains compiled code.\n",
		   program_name);
	  quit (1);
	}
	else if (c_compiled_p)
	  DO_POINTER (*Area, DO_C_COMPILED_ENTRY);
	else
	  DO_POINTER (*Area, DO_COMPILED_ENTRY);
	Mem_Base[*Area - 1] = (MAKE_OBJECT (TC_COMPILED_ENTRY, entry_no));
	break;

      case TC_STACK_ENVIRONMENT:
	if (! allow_bands_p)
	{
	  fprintf (stderr,
		   "%s: File contains stack environments.\n",
		   program_name);
	  quit (1);
	}
	else
	{
	  unsigned long delta;

	  delta = (((SCHEME_OBJECT *) Dumped_Stack_Top)
		   - ((SCHEME_OBJECT *) (OBJECT_DATUM (This))));
	  if (delta > Max_Stack_Offset)
	    Max_Stack_Offset = delta;
	  Mem_Base[*Area] = (MAKE_OBJECT (TC_STACK_ENVIRONMENT, delta));
	  *Area += 1;
	}
	break;

      case TC_POSITIVE_FIXNUM:
#if (TC_POSITIVE_FIXNUM != TC_NEGATIVE_FIXNUM)
      case TC_NEGATIVE_FIXNUM:
#endif
	NIntegers += 1;
	NBits += fixnum_to_bits;
	/* Fall Through */

      case TC_CHARACTER:
      Process_Character:
        Mem_Base[*Area] = (MAKE_OBJECT (Code, *Obj));
        *Obj += 1;
        **FObj = This;
        *FObj += 1;
	/* Fall through */

      case TC_MANIFEST_SPECIAL_NM_VECTOR:
      case_simple_Non_Pointer:
	*Area += 1;
	break;

      case TC_REFERENCE_TRAP:
      {
	long kind;

	kind = (OBJECT_DATUM (This));

	if (upgrade_traps_p)
	{
	  /* It is an old UNASSIGNED object. */
	  if (kind == 0)
	  {
	    Mem_Base[*Area] = UNASSIGNED_OBJECT;
	    *Area += 1;
	    break;
	  }
	  if (kind == 1)
	  {
	    Mem_Base[*Area] = UNBOUND_OBJECT;
	    *Area += 1;
	    break;
	  }
	  fprintf (stderr,
		   "%s: Bad old unassigned object. 0x%x.\n",
		   program_name, This);
	  quit (1);
	}
	if (kind <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */

	  *Area += 1;
	  break;
	}
      }
      /* Fall through */

      case TC_WEAK_CONS:
      case_Pair:
	DO_POINTER (*Area, DO_PAIR);
	break;

      case_Cell:
	DO_POINTER (*Area, DO_CELL);
	break;

      case TC_VARIABLE:
      case_Triple:
	DO_POINTER (*Area, DO_TRIPLE);
	break;

      case_Quadruple:
	DO_POINTER (*Area, DO_QUAD);
	break;

      case TC_BIG_FLONUM:
	DO_POINTER (*Area, DO_FLONUM);
	break;

      case TC_BIG_FIXNUM:
	DO_POINTER (*Area, DO_BIGNUM);
	break;

      case TC_CHARACTER_STRING:
	DO_POINTER (*Area, DO_STRING);
	break;

      case TC_ENVIRONMENT:
	if (upgrade_traps_p)
	{
	  fprintf (stderr,
		   "%s: Cannot upgrade environments.\n",
		   program_name);
	  quit (1);
	}
	/* Fall through */

      case TC_FUTURE:
      case_simple_Vector:
	if (BIT_STRING_P (This))
	  DO_POINTER (*Area, DO_BIT_STRING);
	else
	  DO_POINTER (*Area, DO_VECTOR);
	break;

      default:
      Bad_Type:
	fprintf (stderr, "%s: Unknown Type Code 0x%x found.\n",
		 program_name, (OBJECT_TYPE (This)));
	quit (1);
      }
  }
}

/* Output procedures */

static void
DEFUN (print_binary_objects, (from, count),
       fast SCHEME_OBJECT * from AND fast long count)
{
  while (--count >= 0)
  {
    switch (OBJECT_TYPE (* from))
    { 
      case TC_POSITIVE_FIXNUM:
#if (TC_POSITIVE_FIXNUM != TC_NEGATIVE_FIXNUM)
      case TC_NEGATIVE_FIXNUM:
#endif
	print_a_fixnum (FIXNUM_TO_LONG (*from));
	from += 1;
	break;

      case TC_BIT_STRING:
	print_a_bit_string (++from);
	from += (1 + (OBJECT_DATUM (*from)));
	break;

      case TC_BIG_FIXNUM:
	print_a_bignum (++from);
	from += (1 + (OBJECT_DATUM (*from)));
	break;

      case TC_CHARACTER_STRING:
	print_a_string (++from);
	from += (1 + (OBJECT_DATUM (*from)));
	break;

      case TC_BIG_FLONUM:
	print_a_flonum (from + 1);
	from += (1 + float_to_pointer);
	break;

      case TC_CHARACTER:
	fprintf (portable_file, "%02x %03x\n",
		 TA_CHARACTER, ((*from) & MASK_MIT_ASCII));
	from += 1;
	break;

#ifdef FLOATING_ALIGNMENT

      case TC_MANIFEST_NM_VECTOR:
        if ((OBJECT_DATUM (*from)) == 0)
	{
	    /* used as a word of padding */
	  from += 1;
	  count += 1;
	  break;
	}
        /* fall through */

#endif /* FLOATING_ALIGNMENT */

      default:
	fprintf (stderr,
		 "%s: Bad Binary Object to print %lx\n",
		 program_name, *from);
	quit (1);
    }
  }
  return;
}

static void
DEFUN (print_c_compiled_entries, (entry, count),
       SCHEME_OBJECT * entry AND unsigned long count)
{
  while (count > 0)
  {
    unsigned long entry_index = (* ((unsigned long *) entry));
    unsigned long format = (COMPILED_ENTRY_FORMAT_WORD (entry));
    SCHEME_OBJECT * block;

    Get_Compiled_Block (block, entry);
    fprintf (portable_file, "%02x %lx %ld %ld %lx\n",
	     TA_C_COMPILED_TAG,
	     ((long) C_COMPILED_ENTRY_FORMAT),
	     ((long) (FORMAT_WORD_LOW_BYTE (format))),
	     ((long) (FORMAT_WORD_HIGH_BYTE (format))),
	     ((long) (entry - block)));
    fprintf (portable_file, "%02x %lx %lx\n",
	     TA_C_COMPILED_TAG,
	     ((long) C_COMPILED_ENTRY_CODE),
	     entry_index);
    count -= 1;
    entry += 2;
  }
  return;
}

static void
DEFUN (print_c_closure_entries, (entry, count),
       SCHEME_OBJECT * entry AND unsigned long count)
{
  while (count > 0)
  {
    unsigned long entry_index = (* ((unsigned long *) entry));
    unsigned long format = (COMPILED_ENTRY_FORMAT_WORD (entry));
    SCHEME_OBJECT * block, base;
    unsigned long entry_number = 0;
    long offset;

    EXTRACT_CLOSURE_ENTRY_ADDRESS (entry_number, entry);
    offset = (UNSIGNED_FIXNUM_TO_LONG
	      (compiled_entry_table [entry_number]));
    base = compiled_entry_table[entry_number + 1];

    Get_Compiled_Block (block, entry);
    fprintf (portable_file, "%02x %lx %ld %ld %lx\n",
	     TA_C_COMPILED_TAG,
	     ((long) C_COMPILED_ENTRY_FORMAT),
	     ((long) (FORMAT_WORD_LOW_BYTE (format))),
	     ((long) (FORMAT_WORD_HIGH_BYTE (format))),
	     ((long) (entry - block)));
    fprintf (portable_file, "%02x %lx %lx\n",
	     TA_C_COMPILED_TAG,
	     ((long) C_COMPILED_ENTRY_CODE),
	     entry_index);
    fprintf (portable_file, "%02x %lx %lx %lx\n",
	     TA_C_COMPILED_TAG,
	     ((long) C_COMPILED_EXECUTE_ENTRY),
	     offset,
	     (OBJECT_DATUM (base)));
    count -= 1;
    entry += 3;
  }
  return;
}

#define DIRECT(psbcode)                                          \
   { fprintf (portable_file, "%02x %lx\n", psbcode, the_datum); break; }

#define CONSTANT_TRANSLATION(psbcode) \
   { fprintf (portable_file, "%02x 0\n", psbcode);  goto next_object; }

static void
DEFUN (print_objects, (from, to),
       fast SCHEME_OBJECT * from AND fast SCHEME_OBJECT * to)
{
    SCHEME_OBJECT the_object;
    fast long  the_datum, the_type;

next_object:
  while (from < to)
  {
    the_object = *from;
    from += 1;

    switch (the_object)
    {
      case ALIASED_LENGTH_SHARP_F:
	/* this looked like #F but we knew it was a length so dont translate
	   it */
	from[-1] = SHARP_F;
	the_object = SHARP_F;
	break;
      case EMPTY_LIST_VALUE:  CONSTANT_TRANSLATION(TA_NIL);
#if (SHARP_F != EMPTY_LIST_VALUE)
      case SHARP_F:           CONSTANT_TRANSLATION(TA_FALSE);
#endif
      case SHARP_T:           CONSTANT_TRANSLATION(TA_TRUE);
      case UNSPECIFIC:        CONSTANT_TRANSLATION(TA_UNSPECIFIC);
      default: break;
    }

    the_type = (OBJECT_TYPE (the_object));
    the_datum = (OBJECT_DATUM (the_object));

    switch (the_type)
    {
      case TC_CONSTANT:              DIRECT(TA_CONSTANT);
      case TC_NULL:                  DIRECT(TA_TC_NULL);

      case TC_BIG_FLONUM:            DIRECT(TA_FLONUM);
      case TC_RATNUM:                DIRECT(TA_RATNUM);
      case TC_COMPLEX:               DIRECT(TA_RECNUM);

      case TC_MANIFEST_SPECIAL_NM_VECTOR:DIRECT(TA_MANIFEST_SPECIAL_NM_VECTOR);
      case TC_PRIMITIVE:             DIRECT(TA_PRIMITIVE);

      case TC_REFERENCE_TRAP:        DIRECT(TA_REFERENCE_TRAP);
      case TC_COMPILED_CODE_BLOCK:   DIRECT(TA_COMPILED_CODE_BLOCK);
      case TC_CONTROL_POINT:         DIRECT(TA_CONTROL_POINT);
      case TC_STACK_ENVIRONMENT:     DIRECT(TA_STACK_ENVIRONMENT);

      case TC_CELL:                  DIRECT(TA_CELL);
      case TC_LIST:                  DIRECT(TA_PAIR);
      case TC_WEAK_CONS:             DIRECT(TA_WEAK_CONS);
      case TC_UNINTERNED_SYMBOL:     DIRECT(TA_UNINTERNED_SYMBOL);
      case TC_INTERNED_SYMBOL:       DIRECT(TA_INTERNED_SYMBOL);
      case TC_HUNK3_A:               DIRECT(TA_HUNK3_A);
      case TC_HUNK3_B:               DIRECT(TA_HUNK3_B);
      case TC_QUAD:                  DIRECT(TA_QUAD);

      case TC_NON_MARKED_VECTOR:     DIRECT(TA_NON_MARKED_VECTOR);
      case TC_VECTOR:                DIRECT(TA_VECTOR);
      case TC_RECORD:                DIRECT(TA_RECORD);
      case TC_VECTOR_1B:             DIRECT(TA_VECTOR_1B);
      case TC_CHARACTER_STRING:      DIRECT(TA_CHARACTER_STRING);
      case TC_VECTOR_16B:            DIRECT(TA_VECTOR_16B);

      case TC_ACCESS:                DIRECT(TA_ACCESS);
      case TC_ASSIGNMENT:            DIRECT(TA_ASSIGNMENT);
      case TC_COMBINATION:           DIRECT(TA_COMBINATION);
      case TC_COMBINATION_1:         DIRECT(TA_COMBINATION_1);
      case TC_COMBINATION_2:         DIRECT(TA_COMBINATION_2);
      case TC_COMMENT:               DIRECT(TA_COMMENT);
      case TC_CONDITIONAL:           DIRECT(TA_CONDITIONAL);
      case TC_DEFINITION:            DIRECT(TA_DEFINITION);
      case TC_DELAY:                 DIRECT(TA_DELAY);
      case TC_DELAYED:               DIRECT(TA_PROMISE);
      case TC_DISJUNCTION:           DIRECT(TA_DISJUNCTION);
      case TC_ENTITY:                DIRECT(TA_ENTITY);
      case TC_ENVIRONMENT:           DIRECT(TA_ENVIRONMENT);
      case TC_EXTENDED_LAMBDA:       DIRECT(TA_EXTENDED_LAMBDA);
      case TC_EXTENDED_PROCEDURE:    DIRECT(TA_EXTENDED_PROCEDURE);
      case TC_FUTURE:                DIRECT(TA_FUTURE);
      case TC_IN_PACKAGE:            DIRECT(TA_IN_PACKAGE);
      case TC_LAMBDA:                DIRECT(TA_LAMBDA);
      case TC_LEXPR:                 DIRECT(TA_LEXPR);
      case TC_PCOMB0:                DIRECT(TA_PCOMB0);
      case TC_PCOMB1:                DIRECT(TA_PCOMB1);
      case TC_PCOMB2:                DIRECT(TA_PCOMB2);
      case TC_PCOMB3:                DIRECT(TA_PCOMB3);
      case TC_PROCEDURE:             DIRECT(TA_PROCEDURE);
      case TC_RETURN_CODE:           DIRECT(TA_RETURN_CODE);
      case TC_SCODE_QUOTE:           DIRECT(TA_SCODE_QUOTE);
      case TC_SEQUENCE_2:            DIRECT(TA_SEQUENCE_2);
      case TC_SEQUENCE_3:            DIRECT(TA_SEQUENCE_3);
      case TC_THE_ENVIRONMENT:       DIRECT(TA_THE_ENVIRONMENT);
      case TC_VARIABLE:              DIRECT(TA_VARIABLE);

      /* These account for POSITIVE_FIXNUM, CHARACTER & BIG_FIXNUM: */
      case CONSTANT_CODE:            DIRECT(TA_CONSTANT_CODE);
      case HEAP_CODE:                DIRECT(TA_HEAP_CODE);
      case PURE_CODE:                DIRECT(TA_PURE_CODE);


      case TC_MANIFEST_NM_VECTOR:
      {
	fprintf (portable_file, "%02x %lx\n",
		 TA_MANIFEST_NM_VECTOR, the_datum);
	while (--the_datum >= 0)
	  fprintf (portable_file, "%lx\n", ((unsigned long) *from++));
	break;
      }

      case TC_COMPILED_ENTRY:
      {
	SCHEME_OBJECT base;
	long TC_of_base, TA_of_base;
	long offset;

	offset = (UNSIGNED_FIXNUM_TO_LONG (compiled_entry_table [the_datum]));
	base = compiled_entry_table[the_datum + 1];
	TC_of_base = OBJECT_TYPE(base);
	switch (TC_of_base)  /* translate base type too */
	{
	  case TC_COMPILED_ENTRY:    TA_of_base = TA_COMPILED_ENTRY;  break;
	  default:
	    fprintf(stderr,
		    "%s: Unexpected base type for compiled entry: TC 0x%02x.\n",
		    program_name,
		    TC_of_base);
	    quit(1);
	}
		    
	fprintf (portable_file, "%02x %lx %02x %lx\n",
		 TA_COMPILED_ENTRY, offset,
		 TA_of_base, (OBJECT_DATUM (base)));
	break;
      }

      case TC_LINKAGE_SECTION:
      {
	SCHEME_OBJECT header = (from[-1]);

	switch (READ_LINKAGE_KIND (header))
	{
	  case REFERENCE_LINKAGE_KIND:
	  case ASSIGNMENT_LINKAGE_KIND:
	  {
	    long count = (READ_CACHE_LINKAGE_COUNT (header));

	    fprintf (portable_file, "%02x %lx %lx %lx\n",
		     TA_C_COMPILED_TAG,
		     ((long) C_COMPILED_LINKAGE_HEADER),
		     ((long) (READ_LINKAGE_KIND (header))),
		     ((long) count));
	    while (--count >= 0)
	    {
	      unsigned long the_quad = ((unsigned long) *from++);

	      fprintf (portable_file, "%02x %lx %lx\n",
		       TA_C_COMPILED_TAG,
		       ((long) C_COMPILED_RAW_QUAD),
		       the_quad);
	    }
	    break;
	  }

	  case OPERATOR_LINKAGE_KIND:
	  case GLOBAL_OPERATOR_LINKAGE_KIND:
	  {
	    char * word_ptr;
	    long count, address = 0;
	    SCHEME_OBJECT This, * area_end, * scan, * i_scan;

	    i_scan = (from - 1);
	    This = *i_scan;
	    START_OPERATOR_RELOCATION (i_scan);
	    count = (READ_OPERATOR_LINKAGE_COUNT (This));
	    word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (i_scan));
	    area_end = (END_OPERATOR_LINKAGE_AREA (i_scan, count));

	    fprintf (portable_file, "%02x %lx %lx %lx\n",
		     TA_C_COMPILED_TAG,
		     ((long) C_COMPILED_LINKAGE_HEADER),
		     ((long) (READ_LINKAGE_KIND (header))),
		     ((long) count));

	    while (--count >= 0)
	    {
	      SCHEME_OBJECT base;
	      long arity, offset, address = 0;

	      scan = ((SCHEME_OBJECT *) word_ptr);
	      word_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
	      EXTRACT_OPERATOR_LINKAGE_ADDRESS (address, scan);
	      EXTRACT_EXECUTE_CACHE_ARITY (arity, scan);

	      offset = (UNSIGNED_FIXNUM_TO_LONG
			(compiled_entry_table[address]));
	      base = compiled_entry_table[address + 1];

	      fprintf (portable_file, "%02x %lx %lx %lx\n",
		       TA_C_COMPILED_TAG,
		       ((long) C_COMPILED_EXECUTE_ENTRY),
		       offset,
		       (OBJECT_DATUM (base)));
	      fprintf (portable_file, "%02x %lx %lx\n",
		       TA_C_COMPILED_TAG,
		       ((long) C_COMPILED_EXECUTE_ARITY),
		       arity);
	    }
	    END_OPERATOR_RELOCATION (area_end);
	    from += (area_end - i_scan);
	    break;
	  }

	  default:
	  {
	    fprintf (stderr, "%s: Unknown linkage kind.\n",
		     program_name);
	    quit (1);
	  }
	}
	break;
      }

      case TC_MANIFEST_CLOSURE:
      {
	unsigned long nentries;
	SCHEME_OBJECT * entry, * area_end;

	fprintf (portable_file, "%02x %lx %lx\n",
		 TA_C_COMPILED_TAG,
		 ((long) C_COMPILED_CLOSURE_HEADER),
		 the_datum);

	nentries = (MANIFEST_CLOSURE_COUNT (from));
	entry = ((SCHEME_OBJECT *) (FIRST_MANIFEST_CLOSURE_ENTRY (from)));
	area_end = (MANIFEST_CLOSURE_END (from, nentries));
	
	if (entry != (from + 1))
	  fprintf (portable_file, "%02x %lx %lx\n",
		   TA_C_COMPILED_TAG,
		   ((long) C_COMPILED_MULTI_CLOSURE_HEADER),
		   nentries);

	print_c_closure_entries (entry, nentries);
	from = (area_end + 1);
	break;
      }

      case TC_BROKEN_HEART:
      if (the_datum == 0)
	DIRECT(TA_BROKEN_HEART);
      /* An NMV header fending off C-compiled code descriptors.
	 This knows in detail the format
       */
      
      {
	unsigned long nmv_length;
	SCHEME_OBJECT * entry;

	nmv_length = (OBJECT_DATUM (compiled_block_table [the_datum + 1]));
	fprintf (portable_file, "%02x %lx %lx\n",
		 TA_C_COMPILED_TAG,
		 ((long) C_COMPILED_FAKE_NMV),
		 nmv_length);

	print_c_compiled_entries (from + 1, (nmv_length / 2));
	from += nmv_length;
	break;
      }

      default:
      {
	  fprintf (stderr, "Unknown object kind: 0x%02x | 0x%06x\n",
		   the_type, the_datum);
	  quit(1);
      }
    }
  }
  return;
}

/* Debugging Aids and Consistency Checks */

#define DEBUG 0

#if (DEBUG > 0)

#define WHEN(condition, message)	when(condition, message)

static void
DEFUN (when, (what, message), Boolean what AND char * message)
{
  if (what)
  {
    fprintf (stderr, "%s: Inconsistency: %s!\n",
	     program_name, (message));
    quit (1);
  }
  return;
}

#else /* DEBUG <= 0 */

#define WHEN(what, message) do { } while (0)

#endif /* DEBUG > 0 */

#if (DEBUG > 1)

#define DEBUGGING1(action)		action

#define WRITE_HEADER(name, format, obj) do				\
{									\
  fprintf (portable_file, (format), (obj));				\
  fprintf (portable_file, "\n");					\
  fprintf (stderr, "%s: ", (name));					\
  fprintf (stderr, (format), (obj));					\
  fprintf (stderr, "\n");						\
} while (0)

#else /* DEBUG <= 1 */

#define DEBUGGING1(action) do { } while (0)

#define WRITE_HEADER(name, format, obj) do				\
{									\
  fprintf (portable_file, (format), (obj));				\
  fprintf (portable_file, "\n");					\
} while (0)

#endif /* DEBUG > 1 */

/* The main program */

static void
DEFUN_VOID (do_it)
{
  while (true)
  {
    /* Load the Data */

    SCHEME_OBJECT
      * Heap,
      * Lowest_Allocated_Address, 
      * Highest_Allocated_Address;
    long
      Heap_Start, Heap_Objects_Start,
      Constant_Start, Constant_Objects_Start,
      Pure_Start, Pure_Objects_Start;      

    switch (Read_Header ())
    {
      /* There should really be a difference between no header
	 and a short header.
       */

      case FASL_FILE_TOO_SHORT:
	return;

      case FASL_FILE_FINE:
        break;

      default:
        fprintf (stderr,
		 "%s: Input is not a Scheme binary file.\n",
		 program_name);
	quit (1);
	/* NOTREACHED */
    }

    if (   (Version > FASL_FORMAT_VERSION)
	|| (Version < FASL_OLDEST_VERSION)
	|| (Sub_Version > FASL_SUBVERSION)
	|| (Sub_Version < FASL_OLDEST_SUBVERSION)
	|| ((Machine_Type != FASL_INTERNAL_FORMAT) && (! swap_bytes_p)))
    {
      fprintf (stderr, "%s:\n", program_name);
      fprintf (stderr,
	       "FASL File Version %ld Subversion %ld Machine Type %ld\n",
	       Version, Sub_Version , Machine_Type);
      fprintf (stderr,
	       "Expected: Version %d Subversion %d Machine Type %d\n",
	       FASL_READ_VERSION, FASL_READ_SUBVERSION, FASL_INTERNAL_FORMAT);
      quit (1);
    }

    if ((((compiler_processor_type != COMPILER_NONE_TYPE)
	  && (dumped_processor_type != COMPILER_NONE_TYPE)
	  && (compiler_processor_type != dumped_processor_type))
	 || ((compiler_interface_version != 0)
	     && (dumped_interface_version != 0)
	     && (compiler_interface_version != dumped_interface_version)))
	&& (! upgrade_compiled_p))
    {
      fprintf (stderr, "\nread_file:\n");
      fprintf (stderr,
	       "FASL File: compiled code interface %4d; processor %4d.\n",
	       dumped_interface_version, dumped_processor_type);
      fprintf (stderr,
	       "Expected:  compiled code interface %4d; processor %4d.\n",
	       compiler_interface_version, compiler_processor_type);
      quit (1);
    }
    if (compiler_processor_type != 0)
      dumped_processor_type = compiler_processor_type;
    if (compiler_interface_version != 0)
      dumped_interface_version = compiler_interface_version;
    c_compiled_p = (compiler_processor_type == COMPILER_LOSING_C_TYPE);
    DEBUGGING1 (fprintf (stderr,
			 "compiler_processor_type = %d; c_compiled_p = %s\n",
			 compiler_processor_type,
			 (c_compiled_p ? "true" : "false")));

    if (band_p && (! allow_bands_p))
    {
      fprintf (stderr, "%s: Input file is a band.\n", program_name);
      quit (1);
    }

    if ((Const_Count != 0) && (! allow_constant_space_p))
    {
      fprintf (stderr,
	       "%s: Input file has a constant space area.\n",
	       program_name);
      quit (1);
    }

    shuffle_bytes_p = swap_bytes_p;
    if (Machine_Type == FASL_INTERNAL_FORMAT)
      shuffle_bytes_p = false;

    upgrade_traps_p = (Version == FASL_FORMAT_ADDED_STACK &&
		       Sub_Version < FASL_REFERENCE_TRAP);
    upgrade_primitives_p = (Version == FASL_FORMAT_ADDED_STACK
			    && Sub_Version < FASL_MERGED_PRIMITIVES);
    upgrade_lengths_p = upgrade_primitives_p;

    DEBUGGING1 (fprintf (stderr,
			 "Dumped Heap Base = 0x%08x\n",
			 Heap_Base));

    DEBUGGING1 (fprintf (stderr,
			 "Dumped Constant Base = 0x%08x\n",
			 Const_Base));

    DEBUGGING1 (fprintf (stderr,
			 "Dumped Constant Top = 0x%08x\n",
			 Dumped_Constant_Top));

    DEBUGGING1 (fprintf (stderr,
			 "Heap Count = %6d\n",
			 Heap_Count));

    DEBUGGING1 (fprintf (stderr,
			 "Constant Count = %6d\n",
			 Const_Count));

    {
      long Size;

      /* This is way larger than needed, but... what the hell? */

      Size = (
	      /* All pointers must have datum > TRAP_MAX_IMMEDIATE */
	        (2 * (TRAP_MAX_IMMEDIATE + 1))
	      /* Floating alignment of Heap and Constant Space
		 in incoming image, and of output arenas.
	       */
	      + (5 * ((FLOATING_ALIGNMENT + 1) / (sizeof (SCHEME_OBJECT))))
	      /* Space taken by incoming image. */
	      + (Heap_Count + Const_Count)
	      /* We don't know the partition of the outgoing image,
		 so, make each of the areas large enough:
		 Heap pointers and external heap objects,
		 Constant pointers and external constant objects,
		 Pure pointers and exteranl pure objects
	       */
	      + (2 * (Heap_Count + (2 * Const_Count)))
	      /* Space for the roots */
	      + (NROOTS + 1)
	      /* Space for the primitive table, or space to upgrade */
	      + (upgrade_primitives_p
		 ? (3 * PRIMITIVE_UPGRADE_SPACE)
		 : Primitive_Table_Size)
	      /* Everything might be compiled code blocks, requiring
		 extra tables to map entries to objects, and block alignment
	       */
	      + (allow_compiled_p
		 ? (2 + ((c_compiled_p ? 5 : 3) * (Heap_Count + Const_Count)))
		 : 0)
	      /* C code IDs */
	      + C_Code_Table_Size);

      ALLOCATE_HEAP_SPACE (Size,
			   Lowest_Allocated_Address,
			   Highest_Allocated_Address);

      if (Lowest_Allocated_Address == ((SCHEME_OBJECT *) NULL))
      {
	fprintf (stderr,
		 "%s: Memory Allocation Failed.  Size = %ld Scheme Objects\n",
		 program_name, Size);
	quit (1);
      }
    }

    Heap = (Lowest_Allocated_Address + (TRAP_MAX_IMMEDIATE + 1));
    ALIGN_FLOAT (Heap);
    if ((Load_Data (Heap_Count, Heap)) != Heap_Count)
    {
      fprintf (stderr, "%s: Could not load the heap's contents.\n",
	       program_name);
      quit (1);
    }
    Constant_Space = (Heap + Heap_Count);
    ALIGN_FLOAT (Constant_Space);
    if ((Load_Data (Const_Count, Constant_Space)) != Const_Count)
    {
      fprintf (stderr, "%s: Could not load constant space.\n",
	       program_name);
      quit (1);
    }
    Constant_Top = (find_constant_top (Constant_Space,  Const_Count));
    Heap_Relocation = ((&Heap[0]) - (OBJECT_ADDRESS (Heap_Base)));
    Constant_Relocation = ((&Constant_Space[0]) -
			   (OBJECT_ADDRESS (Const_Base)));
    Max_Stack_Offset = 0;

    /* Setup compiled code and primitive tables. */

    compiled_entry_table = &Constant_Space[Const_Count];
    compiled_entry_pointer = compiled_entry_table;
    compiled_entry_table_end = compiled_entry_pointer;
    if (allow_compiled_p)
      compiled_entry_table_end += (2 * (Heap_Count + Const_Count));

    compiled_block_table = compiled_entry_table_end;
    compiled_block_pointer = &compiled_block_table[2];
    compiled_block_table_end = compiled_block_pointer;
    if (allow_compiled_p && c_compiled_p)
      compiled_block_table_end += (2 *(Heap_Count + Const_Count));

    primitive_table = compiled_block_table_end;
    if (upgrade_primitives_p)
      primitive_table_end = (setup_primitive_upgrade (primitive_table));
    else
    {
      fast SCHEME_OBJECT * table;
      fast long count, char_count;

      if ((Load_Data (Primitive_Table_Size, primitive_table))
	  != Primitive_Table_Size)
      {
	fprintf (stderr, "%s: Could not load the primitive table.\n",
		 program_name);
	quit (1);
      }
      for (char_count = 0,
	   count = Primitive_Table_Length,
	   table = primitive_table;
	   --count >= 0;)
      {
	char_count += (STRING_LENGTH_TO_LONG (table[1 + STRING_LENGTH_INDEX]));
	table += (2 + (OBJECT_DATUM (table[1 + STRING_HEADER])));
      }
      NPChars = char_count;
      primitive_table_end = (&primitive_table[Primitive_Table_Size]);
    }

    c_code_table = primitive_table_end;
    c_code_table_end = &c_code_table[C_Code_Table_Size];
    if (C_Code_Table_Size == 0)
      c_code_table[0] = (LONG_TO_UNSIGNED_FIXNUM (0));
    else
    {
      fast SCHEME_OBJECT * table;
      fast long count, char_count;

      if ((Load_Data (C_Code_Table_Size, c_code_table)) != C_Code_Table_Size)
      {
	fprintf (stderr, "%s: Could not load the C code table.\n",
		 program_name);
	quit (1);
      }
      for (char_count = 0,
	   count = C_Code_Table_Length,
	   table = &c_code_table[1];
	   --count >= 0; )
      {
	long slen;

	slen = (strlen ((char *) (table + 1)));
	table += (1 + (BYTES_TO_WORDS (1 + slen)));
	char_count += slen;
      }
      NCChars = char_count;
    }

    Mem_Base = c_code_table_end;

    /* Reformat the data */

    NFlonums = NIntegers = NStrings = 0;
    NBits = NBBits = NChars = 0;

    Heap_Start = (NROOTS + (TRAP_MAX_IMMEDIATE + 1));
    INDEX_ALIGN_FLOAT (Heap_Start);
    Heap_Objects_Start = (Heap_Start
			  + (allow_compiled_p
			     ? (2 * Heap_Count)
			     : Heap_Count));
    if (! band_p)
      dumped_utilities = SHARP_F;
    Mem_Base[(Heap_Start - NROOTS) + 0] = dumped_utilities;
    if (dumped_utilities != SHARP_F)
    {
      /* This knows the format of the utilities vector. */ 
      SCHEME_OBJECT * uv = (relocate (dumped_utilities));
      unsigned long len = (OBJECT_DATUM (uv[0]));

      uv[len - 1] = ((SCHEME_OBJECT)
		     (((unsigned long) uv[len - 1])
		      / (sizeof (SCHEME_OBJECT))));
      uv[len - 0] = ((SCHEME_OBJECT)
		     (((unsigned long) uv[len - 0])
		      / (sizeof (SCHEME_OBJECT))));
    }
    Mem_Base[(Heap_Start - NROOTS) + 1]
      = (OBJECT_NEW_TYPE (TC_CELL, Dumped_Object));
    Scan = (Heap_Start - NROOTS);
    Free = Heap_Start;
    Free_Objects = &Mem_Base[Heap_Objects_Start];
    Objects = 0;

    Constant_Start = (Heap_Objects_Start + Heap_Count);
    INDEX_ALIGN_FLOAT (Constant_Start);
    Constant_Objects_Start = (Constant_Start
			      + (allow_compiled_p
				 ? (2 * Const_Count)
				 : Const_Count));
    Scan_Constant = Constant_Start;
    Free_Constant = Constant_Start;
    Free_Cobjects = &Mem_Base[Constant_Objects_Start];
    Constant_Objects = 0;

    Pure_Start = (Constant_Objects_Start + Const_Count);
    INDEX_ALIGN_FLOAT (Pure_Start);
    Pure_Objects_Start = (Pure_Start
			  + (allow_compiled_p
			     ? (2 * Const_Count)
			     : Const_Count));
    Scan_Pure = Pure_Start;
    Free_Pure = Pure_Start;
    Free_Pobjects = &Mem_Base[Pure_Objects_Start];
    Pure_Objects = 0;

    if (Const_Count == 0)
      DO_AREA (HEAP_CODE, Scan, Free, Objects, Free_Objects);
    else
      while ((Scan != Free)
	     || (Scan_Constant != Free_Constant)
	     || (Scan_Pure != Free_Pure))
      {
	DO_AREA (HEAP_CODE, Scan, Free,
		 Objects, Free_Objects);
	DO_AREA (CONSTANT_CODE, Scan_Constant, Free_Constant,
		 Constant_Objects, Free_Cobjects);
	DO_AREA (PURE_CODE, Scan_Pure, Free_Pure,
		 Pure_Objects, Free_Pobjects);
      }

    /* Consistency checks */

    WHEN (((Free - Heap_Start) > Heap_Count), "Free overran Heap");

    WHEN (((Free_Objects - &Mem_Base[Heap_Objects_Start])
	   > Heap_Count),
	  "Free_Objects overran Heap Object Space");

    WHEN (((Free_Constant - Constant_Start) > Const_Count),
	  "Free_Constant overran Constant Space");

    WHEN (((Free_Cobjects - &Mem_Base[Constant_Objects_Start])
	   > Const_Count),
	  "Free_Cobjects overran Constant Object Space");

    WHEN (((Free_Pure - Pure_Start) > Const_Count),
	  "Free_Pure overran Pure Space");

    WHEN (((Free_Cobjects - &Mem_Base[Pure_Objects_Start])
	   > Const_Count),
	  "Free_Cobjects overran Pure Object Space");

    /* Output the data */

    if (found_ext_prims)
    {
      fprintf (stderr, "%s:\n", program_name);
      fprintf (stderr, "NOTE: The arity of some primitives is not known.\n");
      fprintf (stderr, "      The portable file has %ld as their arity.\n",
	       UNKNOWN_PRIMITIVE_ARITY);
      fprintf (stderr, "      You may want to fix this by hand.\n");
    }

    if (! compiled_p)
    {
      dumped_processor_type = 0;
      dumped_interface_version = 0;
    }

    /* Header:
			     Portable Version
				      Machine
				      Version
				  Sub Version
					Flags
				   Heap Count
				    Heap Base
				 Heap Objects
			       Constant Count
				Constant Base
			     Constant Objects
				   Pure Count
				    Pure Base
				 Pure Objects
			      & Dumped Object
			 Maximum Stack Offset
			    Number of flonums
			   Number of integers
		   Number of bits in integers
			Number of bit strings
		Number of bits in bit strings
		  Number of character strings
	      Number of characters in strings
			 Number of primitives
	   Number of characters in primitives
				     CPU type
	      Compiled code interface version
		    Compiler utilities vector
		      Number of C code blocks
	Number of characters in C code blocks
		 Number of reserved C entries
     */

    WRITE_HEADER ("Portable Version", "%ld", PORTABLE_VERSION);
    WRITE_HEADER ("Machine", "%ld", FASL_INTERNAL_FORMAT);
    WRITE_HEADER ("Version", "%ld", FASL_FORMAT_VERSION);
    WRITE_HEADER ("Sub Version", "%ld", FASL_SUBVERSION);
    WRITE_HEADER ("Flags", "%ld", (MAKE_FLAGS ()));

    WRITE_HEADER ("Heap Count", "%ld", (Free - Heap_Start));
    WRITE_HEADER ("Heap Base", "%ld", Heap_Start);
    WRITE_HEADER ("Heap Objects", "%ld", Objects);

    WRITE_HEADER ("Constant Count", "%ld", (Free_Constant - Constant_Start));
    WRITE_HEADER ("Constant Base", "%ld", Constant_Start);
    WRITE_HEADER ("Constant Objects", "%ld", Constant_Objects);

    WRITE_HEADER ("Pure Count", "%ld", (Free_Pure - Pure_Start));
    WRITE_HEADER ("Pure Base", "%ld", Pure_Start);
    WRITE_HEADER ("Pure Objects", "%ld", Pure_Objects);

    WRITE_HEADER ("& Dumped Object", "%ld",
		  (OBJECT_DATUM (Mem_Base[(Heap_Start - NROOTS) + 1])));
    WRITE_HEADER ("Maximum Stack Offset", "%ld", Max_Stack_Offset);

    WRITE_HEADER ("Number of flonums", "%ld", NFlonums);
    WRITE_HEADER ("Number of integers", "%ld", NIntegers);
    WRITE_HEADER ("Number of bits in integers", "%ld", NBits);
    WRITE_HEADER ("Number of bit strings", "%ld", NBitstrs);
    WRITE_HEADER ("Number of bits in bit strings", "%ld", NBBits);
    WRITE_HEADER ("Number of character strings", "%ld", NStrings);
    WRITE_HEADER ("Number of characters in strings", "%ld", NChars);

    WRITE_HEADER ("Number of primitives", "%ld", Primitive_Table_Length);
    WRITE_HEADER ("Number of characters in primitives", "%ld", NPChars);

    WRITE_HEADER ("CPU type", "%ld", dumped_processor_type);
    WRITE_HEADER ("Compiled code interface version", "%ld",
		  dumped_interface_version);
    if (allow_bands_p)
      WRITE_HEADER ("Compiler utilities vector", "%ld",
		    (OBJECT_DATUM (Mem_Base[(Heap_Start - NROOTS) + 0])));
    else
      WRITE_HEADER ("Compiler utilities vector", "%ld", 0);

    WRITE_HEADER ("Number of C code blocks", "%ld", C_Code_Table_Length);
    WRITE_HEADER ("Number of characters in C code blocks", "%ld", NCChars);
    WRITE_HEADER ("Number of reserved C entries", "%ld",
		  (OBJECT_DATUM (c_code_table[0])));

    /* Binary Objects */

    print_binary_objects (&Mem_Base[Pure_Objects_Start], Pure_Objects);
    print_binary_objects (&Mem_Base[Constant_Objects_Start], Constant_Objects);
    print_binary_objects (&Mem_Base[Heap_Objects_Start], Objects);

    /* Normal Objects: pointers, simple non-pointers (e.g. SHARP_F) */

    print_objects (&Mem_Base[Pure_Start], &Mem_Base[Free_Pure]);
    print_objects (&Mem_Base[Constant_Start], &Mem_Base[Free_Constant]);
    print_objects (&Mem_Base[Heap_Start], &Mem_Base[Free]);

    /* Primitives */

    if (upgrade_primitives_p)
    {
      SCHEME_OBJECT obj;
      fast SCHEME_OBJECT *table;
      fast long count, the_datum;

      for (count = Primitive_Table_Length,
	   table = external_renumber_table;
	   --count >= 0;)
      {
	obj = *table++;
	the_datum = (OBJECT_DATUM (obj));
	if ((OBJECT_TYPE (obj)) == TC_PRIMITIVE_EXTERNAL)
	{
	  SCHEME_OBJECT *strobj;

	  strobj = ((SCHEME_OBJECT *) (external_prim_name_table[the_datum]));
	  print_a_primitive (((long) UNKNOWN_PRIMITIVE_ARITY),
			     (STRING_LENGTH_TO_LONG
			      (strobj[STRING_LENGTH_INDEX])),
			     ((char *) &strobj[STRING_CHARS]));
	}
	else
	{
	  char *str;

	  str = builtin_prim_name_table[the_datum];
	  print_a_primitive (((long) builtin_prim_arity_table[the_datum]),
			     ((long) strlen(str)),
			     str);
	}
      }
    }
    else
    {
      long count;
      SCHEME_OBJECT * table = primitive_table;

      for (count = Primitive_Table_Length; --count >= 0; )
      {
	long arity = (FIXNUM_TO_LONG (* table));
	table += 1;
	print_a_primitive
	  (arity,
	   (STRING_LENGTH_TO_LONG (table[STRING_LENGTH_INDEX])),
	   ((char *) &table[STRING_CHARS]));
	table += (1 + (OBJECT_DATUM (table[STRING_HEADER])));
      }
    }

    /* C Code block information */

    {
      long count;
      SCHEME_OBJECT * table = &c_code_table[1];

      for (count = C_Code_Table_Length; --count >= 0; )
      {
	char * name;
	long nentries, namelen;

	nentries = (FIXNUM_TO_LONG (* table));
	name = ((char *) (table + 1));
	namelen = (strlen (name));
	print_a_c_code_block (nentries, namelen, name);
	table += (1 + (BYTES_TO_WORDS (namelen + 1)));
      }
    }

    fflush (portable_file);
    free ((char *) Lowest_Allocated_Address);
  }
}

/* Top Level */

static Boolean
  allow_constant_sup_p,
  ci_version_sup_p,
  ci_processor_sup_p,
  help_p = false,
  help_sup_p,
  warn_portable_sup_p;

/* The boolean value here is what value to store when the option is present. */

static struct keyword_struct
  options[] = {
    KEYWORD ("swap_bytes", &swap_bytes_p, BOOLEAN_KYWRD, BFRMT, NULL),
    KEYWORD ("compact", &compact_p, BOOLEAN_KYWRD, BFRMT, NULL),
    KEYWORD ("null_nmv", &null_nmv_p, BOOLEAN_KYWRD, BFRMT, NULL),
    KEYWORD ("allow_nmv", &allow_nmv_p, BOOLEAN_KYWRD, BFRMT, NULL),
    KEYWORD ("allow_cc", &allow_compiled_p, BOOLEAN_KYWRD, BFRMT, NULL),
    KEYWORD ("upgrade_cc", &upgrade_compiled_p, BOOLEAN_KYWRD, BFRMT, NULL),
    KEYWORD ("ci_version", &compiler_interface_version, INT_KYWRD, "%ld",
	     &ci_version_sup_p),
    KEYWORD ("ci_processor", &compiler_processor_type, INT_KYWRD, "%ld",
	     &ci_processor_sup_p),
    KEYWORD ("endian_invert", &endian_invert_p, BOOLEAN_KYWRD, BFRMT, NULL),
    KEYWORD ("allow_bands", &allow_bands_p, BOOLEAN_KYWRD, BFRMT, NULL),
    KEYWORD ("allow_constant_space", &allow_constant_space_p,
	     BOOLEAN_KYWRD, BFRMT, &allow_constant_sup_p),
    KEYWORD ("warn_portable", &warn_portable_p, BOOLEAN_KYWRD, BFRMT,
	     &warn_portable_sup_p),
    KEYWORD ("help", &help_p, BOOLEAN_KYWRD, BFRMT, &help_sup_p),
    OUTPUT_KEYWORD (),
    INPUT_KEYWORD (),
    END_KEYWORD ()
    };

void
DEFUN (main, (argc, argv), int argc AND char **argv)
{
  parse_keywords (argc, argv, options, false);

  if (help_sup_p && help_p)
  {
    print_usage_and_exit(options, 0);
    /*NOTREACHED*/
  }

  upgrade_compiled_p =
    (upgrade_compiled_p || ci_version_sup_p || ci_processor_sup_p);
  allow_compiled_p = (allow_compiled_p || upgrade_compiled_p
		      || c_compiled_p || allow_bands_p);
  allow_nmv_p = (allow_nmv_p || allow_compiled_p || endian_invert_p);
  if (null_nmv_p && allow_nmv_p)
  {
    fprintf (stderr,
	     "%s: NMVs are both allowed and to be nulled out!\n",
	     program_name);
    quit (1);
  }
  if (allow_bands_p && warn_portable_p && (! warn_portable_sup_p))
    warn_portable_p = false;
  if (allow_bands_p && (! allow_constant_space_p) && (! allow_constant_sup_p))
    allow_constant_space_p = true;

  setup_io ("rb", "w");
  do_it ();
  quit (0);
}
