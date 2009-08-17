/* -*-C-*-

$Id: 9a8d1ab5e4d301fc8ab5e92172f1fa49636d6ba9 $

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

/* This file contains the code to translate portable format binary
   files to internal format. */

/* Cheap renames */

#include "psbmap.h"
#include "float.h"
#include "limits.h"

#define portable_file input_file
#define internal_file output_file

#undef HEAP_MALLOC
#define HEAP_MALLOC malloc

static Boolean
  band_p = false,
  allow_compiled_p = false,
  allow_nmv_p = false,
  warn_portable_p = true,
  c_compiled_p = false;

static long
  Dumped_Object_Addr, Dumped_Compiler_Utilities,
  Dumped_Heap_Base, Dumped_Heap_Limit, Heap_Objects, Heap_Count,
  Dumped_Const_Base, Dumped_Const_Limit, Const_Objects, Const_Count,
  Dumped_Pure_Base, Dumped_Pure_Limit, Pure_Objects, Pure_Count,
  Primitive_Table_Length, Max_Stack_Offset,
  C_Code_Table_Length, C_Code_Reserved_Entries;

static SCHEME_OBJECT
  * Heap, * Constant_Space, * Constant_Top, * Stack_Top,
  * Heap_Base, * Heap_Table, * Heap_Object_Limit,
  * Heap_Pointers, * Free,
  * Const_Base, * Const_Table, * Const_Object_Limit,
  * Const_Pointers, * Free_Const,
  * Pure_Base, * Pure_Table, * Pure_Object_Limit,
  * Pure_Pointers, * Free_Pure;

static long
DEFUN (Write_Data, (Count, From_Where),
       long Count AND
       SCHEME_OBJECT *From_Where)
{
  return (fwrite (((char *) From_Where),
		  (sizeof (SCHEME_OBJECT)),
		  Count,
		  internal_file));
}

#include "fasl.h"
#include "dump.c"

#ifndef MAKE_FORMAT_WORD
#define MAKE_FORMAT_WORD(h,l) 0
#endif

#ifndef WRITE_LABEL_DESCRIPTOR
#define WRITE_LABEL_DESCRIPTOR(e,f,o) do { } while (0)
#endif

#ifndef MAKE_LINKAGE_SECTION_HEADER
#define MAKE_LINKAGE_SECTION_HEADER(kind,count)	0
#endif

/*
   The following two lines appears by courtesy of your friendly
   VMS C compiler and runtime library.

   Bug in version 4 VMS scanf.
 */

#ifndef vms

#define VMS_BUG(stmt)

#define read_hex_digit(var)						\
{									\
  VMS_BUG (var = 0);							\
  fscanf (portable_file, "%1lx", &var);					\
}

#else

#define VMS_BUG(stmt)			stmt

#define read_hex_digit (var)						\
{									\
  var = (read_hex_digit_procedure ());					\
}

long
read_hex_digit_procedure ()
{
  long digit;
  int c;

  while ((c = fgetc (portable_file)) == ' ')
  {};
  digit = ((c >= 'a') ? (c - 'a' + 10)
	   : ((c >= 'A') ? (c - 'A' + 10)
	      : ((c >= '0') ? (c - '0')
	         : fprintf (stderr, "Losing big: %d\n", c))));
  return (digit);
}

#endif

static void
DEFUN_VOID (inconsistency)
{
  /* Provide some context (2 lines). */
  char yow[100];

  fgets (&yow[0], 100, portable_file);
  fprintf (stderr, "%s\n", &yow[0]);
  fgets (&yow[0], 100, portable_file);
  fprintf (stderr, "%s\n", &yow[0]);

  quit (1);
  /*NOTREACHED*/
}

#define OUT(c)	return ((long) ((c) & UCHAR_MAX))

static long
DEFUN_VOID (read_a_char)
{
  fast char C;

  C = getc (portable_file);
  if (C != '\\')
    OUT (C);

  C = getc (portable_file);
  switch (C)
  {
    case 'n':  OUT ('\n');
    case 't':  OUT ('\t');
    case 'b':  OUT ('\b');
    case 'r':  OUT ('\r');
    case 'f':  OUT ('\f');
    case '\\': OUT ('\\');
    case '0':  OUT ('\0');
    case 'X':
    {
      long Code;

      if (warn_portable_p)
      {
	warn_portable_p = false;
	fprintf (stderr,
		 "%s: File is not Portable.  Character Code Found.\n",
		 program_name);
      }
      VMS_BUG (Code = 0);
      fscanf (portable_file, "%ld", &Code);
      getc (portable_file);			/* Space */
      OUT (Code);
    }
    default  : OUT (C);
  }
}

static SCHEME_OBJECT *
DEFUN (read_a_char_pointer, (to), SCHEME_OBJECT * to)
{
  long len, maxlen;
  char * str;

  VMS_BUG (len = 0);
  fscanf (portable_file, "%ld", &len);

  maxlen = (len + 1);		/* null terminated */
  str = ((char *) to);
  getc (portable_file);		/* space */

  while (--len >= 0)
    *str++ = ((char) (read_a_char ()));
  *str = '\0';
  return (to + (BYTES_TO_WORDS (maxlen)));
}

static SCHEME_OBJECT *
DEFUN (read_a_string_internal, (To, maxlen),
       SCHEME_OBJECT * To AND long maxlen)
{

  long ilen, Pointer_Count;
  fast char *str;
  fast long len;

  str = ((char *) (&To[STRING_CHARS]));
  VMS_BUG (ilen = 0);
  fscanf (portable_file, "%ld", &ilen);
  len = ilen;

  if (maxlen == -1)
    maxlen = len;

  /* Null terminated */

  maxlen += 1;

  Pointer_Count = (STRING_CHARS + (char_to_pointer (maxlen)));
  To[STRING_HEADER] =
    (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (Pointer_Count - 1)));
  To[STRING_LENGTH_INDEX] = ((SCHEME_OBJECT) len);

  /* Space */

  getc (portable_file);
  while (--len >= 0)
    *str++ = ((char) (read_a_char ()));
  *str = '\0';
  return (To + Pointer_Count);
}

static SCHEME_OBJECT *
DEFUN (read_a_string, (To, Slot),
       SCHEME_OBJECT * To AND SCHEME_OBJECT * Slot)
{
  long maxlen;

  *Slot = (MAKE_POINTER_OBJECT (TC_CHARACTER_STRING, To));
  VMS_BUG (maxlen = 0);
  fscanf (portable_file, "%ld", &maxlen);
  return (read_a_string_internal (To, maxlen));
}

static SCHEME_OBJECT *
DEFUN (read_an_integer, (the_type, To, Slot),
       int the_type AND SCHEME_OBJECT * To AND SCHEME_OBJECT * Slot)
{
  Boolean negative;
  fast long length_in_bits;

  getc (portable_file);				/* Space */
  negative = ((getc (portable_file)) == '-');
  {
    long l;
    VMS_BUG (l = 0);
    fscanf (portable_file, "%ld", (&l));
    length_in_bits = l;
  }
  if ((length_in_bits <= fixnum_to_bits)
      && (the_type == TC_POSITIVE_FIXNUM)) /* Always passed as POSITIVE! */
  {
    /* The most negative fixnum is handled in the bignum case */
    fast long Value = 0;
    fast int Normalization;
    fast long ndigits;
    long digit;

    if (length_in_bits != 0)
    {
      for (Normalization = 0,
	  ndigits = hex_digits (length_in_bits);
	  --ndigits >= 0;
	  Normalization += 4)
      {
	read_hex_digit (digit);
	Value += (digit << Normalization);
      }
    }
    if (negative)
      Value = -Value;

    *Slot = (LONG_TO_FIXNUM (Value));
    return (To);
  }
  else if (length_in_bits == 0)
    {
      SCHEME_OBJECT bignum = (MAKE_POINTER_OBJECT (TC_BIG_FIXNUM, To));
      long gc_length = (BIGNUM_LENGTH_TO_GC_LENGTH (0));
      (*To) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, gc_length));
      BIGNUM_SET_HEADER (bignum, 0, 0);
      (*Slot) = bignum;
      return (To + gc_length + 1);
    }
  else
    {
      SCHEME_OBJECT bignum = (MAKE_POINTER_OBJECT (TC_BIG_FIXNUM, To));
      bignum_length_type length = (BIGNUM_BITS_TO_DIGITS (length_in_bits));
      long gc_length = (BIGNUM_LENGTH_TO_GC_LENGTH (length));
      bignum_digit_type * scan = (BIGNUM_START_PTR (bignum));
      fast bignum_digit_type accumulator = 0;
      fast int bits_in_digit =
	((length_in_bits < BIGNUM_DIGIT_LENGTH)
	 ? length_in_bits
	 : BIGNUM_DIGIT_LENGTH);
      fast int position = 0;
      long original_length_in_bits = length_in_bits;
      long hex_digit, low_digit;
     
      while (length_in_bits > 0)
	{
	  read_hex_digit (hex_digit);
	  if (bits_in_digit > 4)
	    {
	      accumulator |= (hex_digit << position);
	      length_in_bits -= 4;
	      position += 4;
	      bits_in_digit -= 4;
	    }
	  else if (bits_in_digit == 4)
	    {
	      (*scan++) = (accumulator | (hex_digit << position));
	      accumulator = 0;
	      position = 0;
	      length_in_bits -= 4;
	      bits_in_digit =
		((length_in_bits < BIGNUM_DIGIT_LENGTH)
		 ? length_in_bits
		 : BIGNUM_DIGIT_LENGTH);
	    }
	  else
	    {
	      (*scan++) =
		(accumulator |
		 ((hex_digit & ((1 << bits_in_digit) - 1)) << position));
	      accumulator = (hex_digit >> bits_in_digit);
	      position = (4 - bits_in_digit);
	      length_in_bits -= 4;
	      if (length_in_bits <= 0)
	      {
		(*scan) = accumulator;
		break;
	      }
	      else if (length_in_bits >= BIGNUM_DIGIT_LENGTH)
		bits_in_digit = BIGNUM_DIGIT_LENGTH;
	      else
		bits_in_digit = length_in_bits;
	    }
	}
      (*To) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, gc_length));
      BIGNUM_SET_HEADER (bignum, length, negative);

      /* The following test depends on BIGNUM_DIGITs being long */

      low_digit = (- (BIGNUM_REF (bignum, 0)));
      if (negative
	  && (the_type == TC_POSITIVE_FIXNUM) /* Always passed as POSITIVE! */
	  && (original_length_in_bits == (fixnum_to_bits + 1))
	  && (LONG_TO_FIXNUM_P (low_digit)))
      {
	*Slot = (LONG_TO_FIXNUM (low_digit));
	return (To);
      }
      else
      {
	*Slot = bignum;
	return (To + gc_length + 1);
      }
    }
}

SCHEME_OBJECT *
DEFUN (read_a_bignum, (the_type, To, Slot),
       int the_type AND SCHEME_OBJECT * To AND SCHEME_OBJECT * Slot)
{
  return (read_an_integer (the_type, To, Slot));
}

static SCHEME_OBJECT *
DEFUN (read_a_bit_string, (To, Slot),
       SCHEME_OBJECT * To AND SCHEME_OBJECT * Slot)
{
  long size_in_bits, size_in_words;
  SCHEME_OBJECT the_bit_string;

  VMS_BUG (size_in_bits = 0);
  fscanf (portable_file, "%ld", &size_in_bits);
  size_in_words = (1 + (BIT_STRING_LENGTH_TO_GC_LENGTH (size_in_bits)));

  the_bit_string = (MAKE_POINTER_OBJECT (TC_BIT_STRING, To));
  *To++ = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, size_in_words));
  *To = size_in_bits;
  To += size_in_words;

  if (size_in_bits != 0)
  {
    unsigned long temp;
    fast SCHEME_OBJECT *scan;
    fast long bits_remaining, bits_accumulated;
    fast SCHEME_OBJECT accumulator, next_word;

    accumulator = 0;
    bits_accumulated = 0;
    scan = (BIT_STRING_LOW_PTR (the_bit_string));
    for (bits_remaining = size_in_bits;
	bits_remaining > 0;
	bits_remaining -= 4)
    {
      read_hex_digit (temp);
      if ((bits_accumulated + 4) > OBJECT_LENGTH)
      {
	accumulator |=
	  ((temp & LOW_MASK (OBJECT_LENGTH - bits_accumulated)) <<
	   bits_accumulated);
	*(INC_BIT_STRING_PTR (scan)) = accumulator;
	accumulator = (temp >> (OBJECT_LENGTH - bits_accumulated));
	bits_accumulated -= (OBJECT_LENGTH - 4);
	temp &= LOW_MASK (bits_accumulated);
      }
      else
      {
	accumulator |= (temp << bits_accumulated);
	bits_accumulated += 4;
      }
    }
    if (bits_accumulated != 0)
    {
      *(INC_BIT_STRING_PTR (scan)) = accumulator;
    }
  }
  *Slot = the_bit_string;
  return (To);
}

/* Underflow and Overflow */

/* dflmax and dflmin exist in the Berserkely FORTRAN library */

static double the_max = 0.0;

#define dflmin()	0.0	/* Cop out */
#define dflmax()	((the_max == 0.0) ? (compute_max ()) : the_max)

static double
DEFUN_VOID (compute_max)
{
  fast double Result;
  fast int expt;

  Result = 0.0;
  for (expt = DBL_MAX_EXP; expt != 0; expt >>= 1)
    Result += (ldexp (1.0, expt));
  the_max = Result;
  return (Result);
}

static long
DEFUN (read_signed_decimal, (stream), fast FILE * stream)
{
  fast int c = (getc (stream));
  fast long result = (-1);
  int negative_p = 0;
  while (c == ' ')
    c = (getc (stream));

  if (c == '+')
    c = (getc (stream));
  else if (c == '-')
  {
    negative_p = 1;
    c = (getc (stream));
  }

  if ((c >= '0') && (c <= '9'))
  {
    result = (c - '0');
    c = (getc (stream));
    while ((c >= '0') && (c <= '9'))
    {
      result = ((result * 10) + (c - '0'));
      c = (getc (stream));
    }
  }
  if (c != EOF)
    ungetc (c, stream);

  if (result == (-1))
  {
    fprintf (stderr, "%s: Unable to read expected decimal integer\n",
	     program_name);
    inconsistency ();
  }
  return (negative_p ? (-result) : result);
}

static double
DEFUN_VOID (read_a_flonum)
{
  Boolean negative;
  long exponent;
  long size_in_bits;
  fast double Result;

  getc (portable_file);				/* Space */
  negative = ((getc (portable_file)) == '-');
  /* Hair here because portable file format incorrect for flonum 0. */
  exponent = (read_signed_decimal (portable_file));
  if (exponent == 0)
  {
    int c = (getc (portable_file));
    if (c == '\n')
      return (0);
    ungetc (c, portable_file);
  }
  size_in_bits = (read_signed_decimal (portable_file));
  if (size_in_bits == 0)
    return (0);

  if ((exponent > DBL_MAX_EXP) || (exponent < DBL_MIN_EXP))
  {
    /* Skip over mantissa */

    while ((getc (portable_file)) != '\n')
      ;
    fprintf (stderr,
	     "%s: Floating point exponent too %s!\n",
	     program_name,
	     ((exponent < 0) ? "small" : "large"));
    Result = ((exponent < 0) ? (dflmin ()) : (dflmax ()));
  }
  else
  {
    fast long ndigits;
    fast double Normalization;
    long digit;

    if (size_in_bits > DBL_MANT_DIG)
      fprintf (stderr,
	       "%s: Some precision may be lost.",
	       program_name);
    getc (portable_file);			/* Space */
    for (ndigits = (hex_digits (size_in_bits)),
	 Result = 0.0,
	 Normalization = (1.0 / 16.0);
	 --ndigits >= 0;
	 Normalization /= 16.0)
    {
      read_hex_digit (digit);
      Result += (((double ) digit) * Normalization);
    }
    Result = (ldexp (Result, ((int) exponent)));
  }
  if (negative)
    Result = -Result;

  return (Result);
}

static SCHEME_OBJECT *
DEFUN (Read_External, (N, Table, To),
       long N
       AND fast SCHEME_OBJECT * Table
       AND SCHEME_OBJECT * To)
{
  fast SCHEME_OBJECT *Until = &Table[N];
  int the_type;

  while (Table < Until)
  {
    VMS_BUG (the_type = 0);
    fscanf (portable_file, "%2x", &the_type);
    switch (the_type)
    { 
      case TA_CHARACTER_STRING:
        To = (read_a_string (To, Table++));
	continue;

      case TA_VECTOR_1B:
	To = (read_a_bit_string (To, Table++));
	continue;

      case TA_FIXNUM:
	/* Choice of POSITIVE/NEGATIVE for output is independent of    */
	/* value on input, since the sign is indicated separately (for */
	/* largely historical reasons) in the input file. The type     */
	/* here is used to determine whether a FIXNUM or BIGNUM result */
	/* is required.                                                */
	To = (read_an_integer (TC_POSITIVE_FIXNUM, To, Table++));
	continue;
	
      case TA_BIGNUM:
	To = (read_a_bignum (TC_BIG_FIXNUM, To, Table++));
	continue;

      case TA_CHARACTER:
      {
	long the_char_code;

	getc (portable_file);	/* Space */
	VMS_BUG (the_char_code = 0);
	fscanf (portable_file, "%3lx", &the_char_code);
	*Table++ = (MAKE_OBJECT (TC_CHARACTER, the_char_code));
	continue;
      }

      case TA_FLONUM:
      {
	double The_Flonum = (read_a_flonum ());

	ALIGN_FLOAT (To);
	*Table++ = (MAKE_POINTER_OBJECT (TC_BIG_FLONUM, To));
	*To++ = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (float_to_pointer)));
	*((double *) To) = The_Flonum;
	To += float_to_pointer;
	continue;
      }

      default:
	fprintf (stderr,
		 "%s: Unknown external object found; Type = 0x%02x\n",
		 program_name, the_type);
	inconsistency ();
	/*NOTREACHED*/
    }
  }
  return (To);
}

#define DEBUG 0

#if (DEBUG > 2)

static void
DEFUN (print_external_objects, (area_name, Table, N),
       char * area_name
       AND fast SCHEME_OBJECT * Table
       AND fast long N)
{
  fast SCHEME_OBJECT * Table_End = &Table[N];

  fprintf (stderr, "%s External Objects:\n", area_name);
  fprintf (stderr, "Table = 0x%x; N = %d\n", Table, N);

  for ( ; Table < Table_End; Table++)
  {
    switch (OBJECT_TYPE (*Table))
    {
#if (TC_NEGATIVE_FIXNUM != TC_POSITIVE_FIXNUM)	
      case TC_NEGATIVE_FIXNUM:
#endif
      case TC_POSITIVE_FIXNUM:
      { fprintf (stderr,
		 "Table[%6d] = Fixnum %d\n",
		 (N - (Table_End - Table)),
		 (FIXNUM_TO_LONG (*Table)));
	break;
      }
      case TC_CHARACTER:
        fprintf (stderr,
		 "Table[%6d] = Character %c = 0x%02x\n",
		 (N - (Table_End - Table)),
		 (OBJECT_DATUM (*Table)),
		 (OBJECT_DATUM (*Table)));
	break;

      case TC_CHARACTER_STRING:
        fprintf (stderr,
		 "Table[%6d] = string \"%s\"\n",
		 (N - (Table_End - Table)),
		 ((char *) MEMORY_LOC (*Table, STRING_CHARS)));
	break;

      case TC_BIG_FIXNUM:
	fprintf (stderr,
		 "Table[%6d] = Bignum\n",
		 (N - (Table_End - Table)));
	break;

      case TC_BIG_FLONUM:
	fprintf (stderr,
		 "Table[%6d] = Flonum %lf\n",
		 (N - (Table_End - Table)),
		 (* ((double *) MEMORY_LOC (*Table, 1))));
	break;

      default:
        fprintf (stderr,
		 "Table[%6d] = Unknown External Object 0x%8x\n",
		 (N - (Table_End - Table)),
		 *Table);
	break;
    }
  }
  return;
}

#endif /* DEBUG > 1 */

#if (DEBUG > 0)

#define WHEN(condition, message)	when (condition, message)

static void
DEFUN (when, (what, message), Boolean what AND char * message)
{
  if (what)
  {
    fprintf (stderr, "%s: Inconsistency: %s!\n",
	     program_name, (message));
    inconsistency ();
  }
  return;
}

#else /* DEBUG <= 0 */

#define WHEN(what, message) do { } while (0)

#endif /* DEBUG > 0 */

#if (DEBUG > 1)

#define DEBUGGING(action)		action

#define READ_HEADER_FAILURE(string) do					\
{									\
  fprintf (stderr, "Unable to read header field \"%s\".\n", (string));	\
} while (0)

#define READ_HEADER_SUCCESS(string, format, value) do			\
{									\
  fprintf (stderr, "%s: ", (string));					\
  fprintf (stderr, (format), (value));					\
  fprintf (stderr, "\n");						\
} while (0)

#else /* DEBUG <= 1 */

#define DEBUGGING(action) do { } while (0)

#define READ_HEADER_FAILURE(s) do { } while (0)
#define READ_HEADER_SUCCESS(s,f,v) do { } while (0)

#endif /* DEBUG > 0 */

#if (DEBUG > 2)

#define XDEBUGGING(action) DEBUGGING(action)

#else /* DEBUG <= 2 */

#define XDEBUGGING(action) do { } while (0)

#endif /* DEBUG > 2 */

void
DEFUN (relocation_error, (addr), long addr)
{
  fprintf (stderr, "%s: Out of range address %d.\n",
	   program_name, addr);
  inconsistency ();
  /*NOTREACHED*/
}

#define Relocate_Into(Where, Addr) do					\
{									\
  long _addr = (Addr);							\
									\
  if ((_addr >= Dumped_Heap_Base) && (_addr < Dumped_Heap_Limit))	\
    (Where) = &Heap_Pointers[_addr - Dumped_Heap_Base];			\
  else if ((_addr >= Dumped_Const_Base)					\
	   && (_addr < Dumped_Const_Limit))				\
    (Where) = &Const_Pointers[_addr - Dumped_Const_Base];		\
  else if ((_addr >= Dumped_Pure_Base)					\
	   && (_addr < Dumped_Pure_Limit))				\
    (Where) = &Pure_Pointers[_addr - Dumped_Pure_Base];			\
  else									\
    (void) relocation_error (_addr);					\
} while (0)

#ifndef Conditional_Bug

#define Relocate(Addr)							\
((((Addr) >= Dumped_Heap_Base) && ((Addr) < Dumped_Heap_Limit))		\
 ? &Heap_Pointers[(Addr) - Dumped_Heap_Base]				\
 : ((((Addr) >= Dumped_Const_Base) && ((Addr) < Dumped_Const_Limit))	\
    ? &Const_Pointers[(Addr) - Dumped_Const_Base]			\
    : ((((Addr) >= Dumped_Pure_Base) && ((Addr) < Dumped_Pure_Limit))	\
       ? &Pure_Pointers[(Addr) - Dumped_Pure_Base]			\
       : ((relocation_error (Addr)), ((SCHEME_OBJECT *) NULL)))))

#else

static SCHEME_OBJECT * Relocate_Temp;

#define Relocate(Addr)							\
  (Relocate_Into (Relocate_Temp, Addr), Relocate_Temp)

#endif

#define TRANSLATE_CONSTANT(value) {                                     \
    *to++ = value;                                                      \
    continue;                                                           \
}

#define TRANSLATE_NONPOINTER(typecode)   {                              \
  *to++ = (MAKE_OBJECT(typecode, the_datum));                           \
  continue;                                                             \
}

#define TRANSLATE_POINTER(typecode)   {                                 \
  *to++ = (MAKE_POINTER_OBJECT(typecode, Relocate(the_datum)));         \
  continue;                                                             \
}

static SCHEME_OBJECT *
DEFUN (Read_Pointers_and_Relocate, (how_many, to),
       fast long how_many AND fast SCHEME_OBJECT * to)
{
  int the_type;
  long the_datum;

  while ((--how_many) >= 0)
  {
    VMS_BUG (the_type = 0);
    VMS_BUG (the_datum = 0);
    fscanf (portable_file, "%2x %lx", &the_type, &the_datum);

    switch (the_type)
    {
      case TA_FALSE:      TRANSLATE_CONSTANT(SHARP_F);
      case TA_NIL:        TRANSLATE_CONSTANT(EMPTY_LIST_VALUE);
      case TA_TRUE:       TRANSLATE_CONSTANT(SHARP_T);
      case TA_UNSPECIFIC: TRANSLATE_CONSTANT(UNSPECIFIC);

      case TA_CONSTANT_CODE:
        WHEN (((the_datum < 0) || (the_datum >= Const_Objects)),
	      "CONSTANT_CODE too large");
	*to++ = Const_Table[the_datum];
	continue;

      case TA_HEAP_CODE:
        WHEN (((the_datum < 0) || (the_datum >= Heap_Objects)),
	      "HEAP_CODE too large");
	*to++ = Heap_Table[the_datum];
	continue;
	
      case TA_PURE_CODE:
        WHEN (((the_datum < 0) || (the_datum >= Pure_Objects)),
	      "PURE_CODE too large");
	*to++ = Pure_Table[the_datum];
	continue;

      case TA_CHARACTER_STRING:
      case TA_FIXNUM:
      case TA_BIGNUM:
      case TA_FLONUM:
      case TA_CHARACTER:
	fprintf (stderr,
		 "%s: Unexpected external constant in pointer area: 0x%02x\n.",
		 program_name, the_type);
	inconsistency ();
	continue;

      case TA_MANIFEST_NM_VECTOR:
	*to++ = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, the_datum));
        {
	  fast long count;

	  count = the_datum;
	  how_many = how_many - count;
	  while (--count >= 0)
	  {
	    VMS_BUG (*to = 0);
	    fscanf (portable_file, "%lx", to++);
	  }
	}
	continue;

      case TA_BROKEN_HEART:
	if (the_datum != 0)
	{
	  fprintf (stderr, "%s: Broken Heart found.\n", program_name);
	  inconsistency ();
	}
	TRANSLATE_NONPOINTER(TC_BROKEN_HEART);

      case TA_PCOMB0:               TRANSLATE_NONPOINTER(TC_PCOMB0);
      case TA_PRIMITIVE:            TRANSLATE_NONPOINTER(TC_PRIMITIVE);
      case TA_MANIFEST_SPECIAL_NM_VECTOR:
                           TRANSLATE_NONPOINTER(TC_MANIFEST_SPECIAL_NM_VECTOR);
      case TA_THE_ENVIRONMENT:      TRANSLATE_NONPOINTER(TC_THE_ENVIRONMENT);
      case TA_RETURN_CODE:          TRANSLATE_NONPOINTER(TC_RETURN_CODE);
      case TA_TC_NULL:              TRANSLATE_NONPOINTER(TC_NULL);
      case TA_CONSTANT:             TRANSLATE_NONPOINTER(TC_CONSTANT);

      case TA_COMPILED_ENTRY:
      {
	SCHEME_OBJECT * temp, * entry_addr;
	long TA_of_base, TC_of_base, base_datum;

	VMS_BUG (TA_of_base = 0);
	VMS_BUG (base_datum = 0);
	fscanf (portable_file, "%02x %lx", &TA_of_base, &base_datum);
	temp = (Relocate (base_datum));
	if (c_compiled_p)
	  entry_addr = &temp[the_datum];
	else
	  entry_addr = ((SCHEME_OBJECT *) (&(((char *) temp) [the_datum])));
	switch (TA_of_base)  /* translate base object type */
	{
	  case TA_COMPILED_ENTRY:  TC_of_base = TC_COMPILED_ENTRY;  break;
	  default:
	    fprintf(stderr,
		    "%s: Unexpected base type for compiled entry: TA 0x%02x.\n",
		    program_name,
		    TA_of_base);
	    inconsistency();
	}
	*to++ = (MAKE_POINTER_OBJECT (TC_of_base, entry_addr));
	continue;
      }

      case TA_C_COMPILED_TAG:
      {
	if (! c_compiled_p)
	{
	  fprintf (stderr, "%s: C-compiled code descriptors found.\n",
		   program_name);
	  inconsistency ();
	}
	switch (the_datum)
	{
	  case C_COMPILED_FAKE_NMV:
	  {
	    long nmv_length;

	    VMS_BUG (nmv_length = 0);
	    fscanf (portable_file, "%lx", &nmv_length);
	    *to++ = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, nmv_length));
	    continue;
	  }

	  case C_COMPILED_ENTRY_FORMAT:
	  {
	    long low_byte, high_byte, offset, format;

	    VMS_BUG (low_byte = 0);
	    VMS_BUG (high_byte = 0);
	    VMS_BUG (offset = 0);
	    fscanf (portable_file, "%ld %ld %lx",
		    &low_byte, &high_byte, &offset);
	    format = (MAKE_FORMAT_WORD (high_byte, low_byte));
	    to += 1;
	    WRITE_LABEL_DESCRIPTOR (to, format, offset);
	    continue;
	  }

	  case C_COMPILED_ENTRY_CODE:
	  {
	    long entry_number;

	    VMS_BUG (entry_number = 0);
	    fscanf (portable_file, "%lx", &entry_number);
	    *to++ = ((SCHEME_OBJECT) entry_number);
	    continue;
	  }

	  case C_COMPILED_CLOSURE_HEADER:
	  {
	    long header_datum;

	    VMS_BUG (header_datum = 0);
	    fscanf (portable_file, "%lx", &header_datum);
	    *to++ = (MAKE_OBJECT (TC_MANIFEST_CLOSURE, header_datum));
	    continue;
	  }

	  case C_COMPILED_MULTI_CLOSURE_HEADER:
	  {
	    long nentries;

	    VMS_BUG (nentries = 0);
	    fscanf (portable_file, "%lx", &nentries);
	    to += 1;
	    WRITE_LABEL_DESCRIPTOR (to, nentries, 0);
	    continue;
	  }

	  case C_COMPILED_LINKAGE_HEADER:
	  {
	    long kind, count;

	    VMS_BUG (kind = 0);
	    VMS_BUG (count = 0);
	    fscanf (portable_file, "%lx %lx", &kind, &count);
	    *to++ = (MAKE_LINKAGE_SECTION_HEADER (kind, count));
	    continue;
	  }

	  case C_COMPILED_RAW_QUAD:
	  {
	    long quad_datum;

	    VMS_BUG (quad_datum = 0);
	    fscanf (portable_file, "%lx", &quad_datum);
	    *to++ = (ADDR_TO_SCHEME_ADDR (Relocate (quad_datum)));
	    continue;
	  }

	  case C_COMPILED_EXECUTE_ENTRY:
	  {
	    long offset, block_base;
	    SCHEME_OBJECT * temp;

	    VMS_BUG (offset = 0);
	    VMS_BUG (block_base = 0);
	    fscanf (portable_file, "%lx %lx", &offset, &block_base);
	    temp = (Relocate (block_base));
	    *to++ = (ADDR_TO_SCHEME_ADDR (&temp[offset]));
	    continue;
	  }

	  case C_COMPILED_EXECUTE_ARITY:
	  {
	    long arity;

	    VMS_BUG (arity = 0);
	    fscanf (portable_file, "%lx", &arity);
	    *to++ = ((SCHEME_OBJECT) arity);
	    continue;
	  }

	  default:
	  {
	    fprintf (stderr, "%s: Unknown C compiled tag found.\n",
		     program_name);
	    inconsistency ();
	  }
	}
	continue;
      }

      case TA_STACK_ENVIRONMENT:
	*to++ = (MAKE_POINTER_OBJECT (TC_STACK_ENVIRONMENT, (Stack_Top - the_datum)));
	continue;
	
      case TA_REFERENCE_TRAP:
	if (the_datum <= TRAP_MAX_IMMEDIATE)
	{ *to++ = (MAKE_OBJECT (TC_REFERENCE_TRAP, the_datum));
	  continue;
	}
	*to++ = MAKE_POINTER_OBJECT (TC_REFERENCE_TRAP, Relocate (the_datum));
	continue;

      case TA_RATNUM:        TRANSLATE_POINTER(TC_RATNUM);
      case TA_RECNUM:        TRANSLATE_POINTER(TC_COMPLEX);

      case TA_MANIFEST_CLOSURE:    TRANSLATE_POINTER(TC_MANIFEST_CLOSURE);
      case TA_COMPILED_CODE_BLOCK: TRANSLATE_POINTER(TC_COMPILED_CODE_BLOCK);
      case TA_LINKAGE_SECTION:     TRANSLATE_POINTER(TC_LINKAGE_SECTION);
      case TA_CONTROL_POINT:       TRANSLATE_POINTER(TC_CONTROL_POINT);

      case TA_CELL:                TRANSLATE_POINTER(TC_CELL);
      case TA_PAIR:                TRANSLATE_POINTER(TC_LIST);
      case TA_WEAK_CONS:           TRANSLATE_POINTER(TC_WEAK_CONS);
      case TA_UNINTERNED_SYMBOL:   TRANSLATE_POINTER(TC_UNINTERNED_SYMBOL);
      case TA_INTERNED_SYMBOL:     TRANSLATE_POINTER(TC_INTERNED_SYMBOL);
      case TA_HUNK3_A:             TRANSLATE_POINTER(TC_HUNK3_A);
      case TA_HUNK3_B:             TRANSLATE_POINTER(TC_HUNK3_B);
      case TA_QUAD:                TRANSLATE_POINTER(TC_QUAD);

      case TA_NON_MARKED_VECTOR:   TRANSLATE_POINTER(TC_NON_MARKED_VECTOR);
      case TA_VECTOR:              TRANSLATE_POINTER(TC_VECTOR);
      case TA_RECORD:              TRANSLATE_POINTER(TC_RECORD);
      case TA_VECTOR_1B:           TRANSLATE_POINTER(TC_VECTOR_1B);
      case TA_VECTOR_16B:          TRANSLATE_POINTER(TC_VECTOR_16B);

      case TA_PROCEDURE:           TRANSLATE_POINTER(TC_PROCEDURE);
      case TA_EXTENDED_PROCEDURE:  TRANSLATE_POINTER(TC_EXTENDED_PROCEDURE);
      case TA_LEXPR:               TRANSLATE_POINTER(TC_LEXPR);
      case TA_ENTITY:              TRANSLATE_POINTER(TC_ENTITY);
      case TA_ENVIRONMENT:         TRANSLATE_POINTER(TC_ENVIRONMENT);
      case TA_PROMISE:             TRANSLATE_POINTER(TC_DELAYED);
      case TA_FUTURE:              TRANSLATE_POINTER(TC_FUTURE);
      case TA_IN_PACKAGE:          TRANSLATE_POINTER(TC_IN_PACKAGE);
      case TA_COMMENT:             TRANSLATE_POINTER(TC_COMMENT);
      case TA_SCODE_QUOTE:         TRANSLATE_POINTER(TC_SCODE_QUOTE);
      case TA_VARIABLE:            TRANSLATE_POINTER(TC_VARIABLE);
      case TA_ACCESS:              TRANSLATE_POINTER(TC_ACCESS);
      case TA_LAMBDA:              TRANSLATE_POINTER(TC_LAMBDA);
      case TA_EXTENDED_LAMBDA:     TRANSLATE_POINTER(TC_EXTENDED_LAMBDA);
      case TA_SEQUENCE_2:          TRANSLATE_POINTER(TC_SEQUENCE_2);
      case TA_SEQUENCE_3:          TRANSLATE_POINTER(TC_SEQUENCE_3);
      case TA_CONDITIONAL:         TRANSLATE_POINTER(TC_CONDITIONAL);
      case TA_DISJUNCTION:         TRANSLATE_POINTER(TC_DISJUNCTION);
      case TA_COMBINATION:         TRANSLATE_POINTER(TC_COMBINATION);
      case TA_COMBINATION_1:       TRANSLATE_POINTER(TC_COMBINATION_1);
      case TA_COMBINATION_2:       TRANSLATE_POINTER(TC_COMBINATION_2);
      case TA_PCOMB1:              TRANSLATE_POINTER(TC_PCOMB1);
      case TA_PCOMB2:              TRANSLATE_POINTER(TC_PCOMB2);
      case TA_PCOMB3:              TRANSLATE_POINTER(TC_PCOMB3);
      case TA_DEFINITION:          TRANSLATE_POINTER(TC_DEFINITION);
      case TA_DELAY:               TRANSLATE_POINTER(TC_DELAY);
      case TA_ASSIGNMENT:          TRANSLATE_POINTER(TC_ASSIGNMENT);

      default:
	fprintf(stderr,"Unknown abstract tag (TA_* value): 0x%02x\n", the_type);
	inconsistency();
    }
  }
  return  to;
}

static Boolean primitive_warn = false;

static SCHEME_OBJECT *
DEFUN (read_primitives, (how_many, where),
       fast long how_many
       AND fast SCHEME_OBJECT * where)
{
  long arity;

  while (--how_many >= 0)
  {
    VMS_BUG (arity = 0);
    fscanf (portable_file, "%ld", &arity);
    if (arity == ((long) UNKNOWN_PRIMITIVE_ARITY))
      primitive_warn = true;
    *where++ = (LONG_TO_FIXNUM (arity));
    where = (read_a_string_internal (where, ((long) -1)));
  }
  return (where);
}

static SCHEME_OBJECT *
DEFUN (read_c_code_blocks, (nreserved, length, area),
       long nreserved AND long length AND SCHEME_OBJECT * area)
{
  if (length != 0)
  {
    *area++ = (LONG_TO_FIXNUM (nreserved));
    while (--length >= 0)
    {
      long nentries;

      VMS_BUG (nentries = 0);
      fscanf (portable_file, "%ld", &nentries);
      *area++ = (LONG_TO_FIXNUM (nentries));
      area = (read_a_char_pointer (area));
    }
  }
  return (area);
}

#define READ_HEADER_NO_ERROR(string, format, value, flag) do		\
{									\
  VMS_BUG (value = 0);							\
  if (fscanf (portable_file, format, &(value)) == EOF)			\
  {									\
    (flag) = (false);							\
    READ_HEADER_FAILURE (string);					\
  }									\
  else									\
  {									\
    (flag) = (true);							\
    READ_HEADER_SUCCESS (string, format, value);			\
  }									\
} while (0)

#define READ_HEADER(string, format, value) do				\
{									\
  VMS_BUG (value = 0);							\
  if (fscanf (portable_file, format, &(value)) == EOF)			\
  {									\
    READ_HEADER_FAILURE (string);					\
    short_header_read ();						\
  }									\
  else									\
    READ_HEADER_SUCCESS (string, format, value);			\
} while (0)

static void
DEFUN_VOID (short_header_read)
{
  fprintf (stderr, "%s: Header is not complete!\n", program_name);
  quit (1);
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

static SCHEME_OBJECT * Lowest_Allocated_Address, * Highest_Allocated_Address;

static long
DEFUN_VOID (Read_Header_and_Allocate)
{
  Boolean ok;

  long
    Portable_Version, Machine,
    Version, Sub_Version, Flags,
    NFlonums, NIntegers, NBits,
    NBitstrs, NBBits, NStrings, NChars,
    NPChars, NCChars, Size, initial_delta;

  /* We don't use READ_HEADER here because it is not an error if
     there is no first word.
     .bin (and .psb) files can contain multiple objects.
   */

  compiler_utilities = SHARP_F;
  READ_HEADER_NO_ERROR ("Portable Version", "%ld", Portable_Version, ok);
  if (! ok)
    return (-1);

  if (Portable_Version != PORTABLE_VERSION)
  {
    fprintf (stderr, "%s: Portable version mismatch:\n", program_name);
    fprintf (stderr, "Portable File Version %4d\n", Portable_Version);
    fprintf (stderr, "Expected:     Version %4d\n", PORTABLE_VERSION);
    quit (1);
  }

  READ_HEADER ("Machine", "%ld", Machine);
  READ_HEADER ("Version", "%ld", Version);
  READ_HEADER ("Sub Version", "%ld", Sub_Version);

  if ((Version != FASL_FORMAT_VERSION)		||
      (Sub_Version != FASL_SUBVERSION))
  {
    fprintf (stderr, "%s: Binary version mismatch:\n", program_name);
    fprintf (stderr,
	     "Portable File Version %4d; Binary Version %4d; Subversion %4d\n",
	     Portable_Version, Version, Sub_Version);
    fprintf (stderr,
	     "Expected:     Version %4d; Binary Version %4d; Subversion %4d\n",
	     PORTABLE_VERSION, FASL_FORMAT_VERSION, FASL_SUBVERSION);
    quit (1);
  }

  READ_HEADER ("Flags", "%ld", Flags);
  READ_FLAGS (Flags);

  if (band_p)
    allow_nmv_p = true;
  if ((Machine != FASL_INTERNAL_FORMAT)
      && ((nmv_p && (! allow_nmv_p))
	  || (compiled_p && (! allow_compiled_p) && (! c_compiled_p))))
  {
    if (compiled_p)
      fprintf (stderr, "%s: %s\n", program_name,
	       "Portable file contains \"non-portable\" compiled code.");
    else
      fprintf (stderr, "%s: %s\n", program_name,
	       "Portable file contains \"unexpected\" non-marked vectors.");
    fprintf (stderr, "Machine specified in the portable file: %4d\n",
	     Machine);
    fprintf (stderr, "Machine Expected:                       %4d\n",
	     FASL_INTERNAL_FORMAT);
    quit (1);
  }

  if (compiled_p
      && c_compiled_p
      && (COMPILER_PROCESSOR_TYPE != COMPILER_LOSING_C_TYPE))
  {
    fprintf (stderr,
	     "Portable file contains descriptors for code compiled to C.\n");
    fprintf (stderr,
	     "The microcode is not configured to handle such code.\n");
    quit (1);
  }

  READ_HEADER ("Heap Count", "%ld", Heap_Count);
  READ_HEADER ("Dumped Heap Base", "%ld", Dumped_Heap_Base);
  READ_HEADER ("Heap Objects", "%ld", Heap_Objects);

  READ_HEADER ("Constant Count", "%ld", Const_Count);
  READ_HEADER ("Dumped Constant Base", "%ld", Dumped_Const_Base);
  READ_HEADER ("Constant Objects", "%ld", Const_Objects);

  READ_HEADER ("Pure Count", "%ld", Pure_Count);
  READ_HEADER ("Dumped Pure Base", "%ld", Dumped_Pure_Base);
  READ_HEADER ("Pure Objects", "%ld", Pure_Objects);

  READ_HEADER ("& Dumped Object", "%ld", Dumped_Object_Addr);
  READ_HEADER ("Max Stack Offset", "%ld", Max_Stack_Offset);

  READ_HEADER ("Number of flonums", "%ld", NFlonums);
  READ_HEADER ("Number of integers", "%ld", NIntegers);
  READ_HEADER ("Number of bits in integers", "%ld", NBits);
  READ_HEADER ("Number of bit strings", "%ld", NBitstrs);
  READ_HEADER ("Number of bits in bit strings", "%ld", NBBits);
  READ_HEADER ("Number of character strings", "%ld", NStrings);
  READ_HEADER ("Number of characters in strings", "%ld", NChars);

  READ_HEADER ("Primitive Table Length", "%ld", Primitive_Table_Length);
  READ_HEADER ("Number of characters in primitives", "%ld", NPChars);

  READ_HEADER ("CPU type", "%ld", compiler_processor_type);
  READ_HEADER ("Compiled code interface version", "%ld",
	       compiler_interface_version);
  READ_HEADER ("Compiler utilities vector", "%ld", Dumped_Compiler_Utilities);

  READ_HEADER ("Number of C code blocks", "%ld", C_Code_Table_Length);
  READ_HEADER ("Number of characters in C code blocks", "%ld", NCChars);
  READ_HEADER ("Number of reserved C entries", "%ld", C_Code_Reserved_Entries);

  Dumped_Heap_Limit = Dumped_Heap_Base + Heap_Count;
  Dumped_Const_Limit = Dumped_Const_Base + Const_Count;
  Dumped_Pure_Limit = Dumped_Pure_Base + Pure_Count;

  initial_delta = (TRAP_MAX_IMMEDIATE + 1);
  if (Max_Stack_Offset > initial_delta)
    initial_delta = Max_Stack_Offset;

  Size = (
	  /* SNMV headers for constant and pure space */
	  6
	  /* Float alignment of the different arenas */
	  + (5 * ((FLOATING_ALIGNMENT + 1) / (sizeof (SCHEME_OBJECT))))
	  /* All pointers must have datum greater than this */
	  + initial_delta
	  /* Incoming heap */
	  + (Heap_Count + Heap_Objects)
	  /* Incoming constant space */
	  + (Const_Count + Const_Objects)
	  /* Incoming pure space */
	  + (Pure_Count + Pure_Objects)
	  /* Maximum space taken up by flonums */
	  + (flonum_to_pointer (NFlonums))
	  /* Maximum space taken up by integers */
	  + ((NIntegers * (2 + (BYTES_TO_WORDS (sizeof (bignum_digit_type)))))
	     + (BYTES_TO_WORDS (BIGNUM_BITS_TO_DIGITS (NBits))))
	  /* Maximum space taken up by strings */
	  + ((NStrings * (1 + STRING_CHARS))
	     + (char_to_pointer (NChars)))
	  /* Maximum space taken up by bit strings */
	  + ((NBitstrs * (1 + BIT_STRING_FIRST_WORD))
	     + (BIT_STRING_LENGTH_TO_GC_LENGTH (NBBits)))
	  /* space taken by the primitive table */
	  + ((Primitive_Table_Length * (2 + STRING_CHARS))
	     + (char_to_pointer (NPChars)))
	  /* Space taken up by the C code block IDs */
	  + (1 + (2 * C_Code_Table_Length) + (char_to_pointer (NCChars))));

  ALLOCATE_HEAP_SPACE (Size,
		       Lowest_Allocated_Address,
		       Highest_Allocated_Address);
  if (Lowest_Allocated_Address == NULL)
  {
    fprintf (stderr,
	     "%s: Memory Allocation Failed.  Size = %ld Scheme Objects\n",
	     program_name, Size);
    quit (1);
  }
  Heap = (Lowest_Allocated_Address + initial_delta);
  return (Size - initial_delta);
}

static void
DEFUN_VOID (do_it)
{
  while (1)
  {
    SCHEME_OBJECT
      * primitive_table, * primitive_table_end,
      * c_code_table, * c_code_table_end,
      * Dumped_Object;
    Boolean result;
    long Size;

    Size = (Read_Header_and_Allocate ());
    if (Size < 0)
      return;

    if (band_p)
      warn_portable_p = false;
    Stack_Top = Heap;
    DEBUGGING (fprintf (stderr, "Stack_Top: 0x%x\n", Stack_Top));

    Heap_Table = &Heap[Size - Heap_Objects];
    Const_Table = &Heap_Table[- Const_Objects];
    Pure_Table = &Const_Table[- Pure_Objects];

    /* The various 2s below are for SNMV headers in constant/pure markers. */

    Constant_Space = &Heap[0];
    ALIGN_FLOAT (Constant_Space);
    
    Pure_Base = &Constant_Space[2];
    Pure_Object_Limit
      = (Read_External (Pure_Objects, Pure_Table, Pure_Base));
    Pure_Pointers = Pure_Object_Limit;
    ALIGN_FLOAT (Pure_Pointers);

    XDEBUGGING (print_external_objects ("Pure", Pure_Table, Pure_Objects));
    DEBUGGING (fprintf (stderr, "Pure_Base: 0x%x\n", Pure_Base));
    DEBUGGING (fprintf (stderr, "Pure_Pointers: 0x%x\n", Pure_Pointers));

    Const_Base = &Pure_Pointers[Pure_Count + 2];
    Const_Object_Limit
      = (Read_External (Const_Objects, Const_Table, Const_Base));
    Const_Pointers = Const_Object_Limit;
    ALIGN_FLOAT (Const_Pointers);

    XDEBUGGING (print_external_objects ("Constant", Const_Table,
					Const_Objects));
    DEBUGGING (fprintf (stderr, "Const_Base: 0x%x\n", Const_Base));
    DEBUGGING (fprintf (stderr, "Const_Pointers: 0x%x\n", Const_Pointers));

    Constant_Top = &Const_Pointers[Const_Count + 2];

    Heap_Base = Constant_Top;
    ALIGN_FLOAT (Heap_Base);
    Heap_Object_Limit
      = (Read_External (Heap_Objects, Heap_Table, Heap_Base));
    Heap_Pointers = Heap_Object_Limit;
    ALIGN_FLOAT (Heap_Pointers);

    XDEBUGGING (print_external_objects ("Heap", Heap_Table, Heap_Objects));
    DEBUGGING (fprintf (stderr, "Heap_Base: 0x%x\n", Heap_Base));
    DEBUGGING (fprintf (stderr, "Heap_Pointers: 0x%x\n", Heap_Pointers));

    primitive_table = &Heap_Pointers[Heap_Count];

    WHEN ((primitive_table > &Heap[Size]), "primitive_table overran memory.");

    /* Read the normal objects */

    Free_Pure = (Read_Pointers_and_Relocate (Pure_Count, Pure_Pointers));
    WHEN ((Free_Pure > (Const_Base - 2)),
	  "Free_Pure overran Const_Base");
    WHEN ((Free_Pure < (Const_Base - 2)),
	  "Free_Pure did not reach Const_Base");

    Free_Const = (Read_Pointers_and_Relocate (Const_Count, Const_Pointers));
    WHEN ((Free_Const > (Constant_Top - 2)),
	  "Free_Const overran Constant_Top");
    WHEN ((Free_Const < (Constant_Top - 2)),
	  "Free_Const did not reach Constant_Top");

    Free = (Read_Pointers_and_Relocate (Heap_Count, Heap_Pointers));

    WHEN ((Free > primitive_table), "Free overran primitive_table");
    WHEN ((Free < primitive_table), "Free did not reach primitive_table");

    primitive_table_end
      = (read_primitives (Primitive_Table_Length, primitive_table));

    if (primitive_warn)
    {
      fprintf (stderr, "%s:\n", program_name);
      fprintf
	(stderr,
	 "NOTE: The binary file contains primitives with unknown arity.\n");
    }

    c_code_table = primitive_table_end;
    c_code_table_end
      = (read_c_code_blocks (C_Code_Reserved_Entries,
			     C_Code_Table_Length,
			     c_code_table));

    WHEN ((c_code_table_end > Pure_Table),
	  "c_code_table_end overran Pure_Table");
    /*
      c_code_table_end can be well below Pure_Table, since
      the memory allocation is conservative (it rounds up), and all
      the slack ends up between them.
      */

    /* Dump the objects */

    Relocate_Into (Dumped_Object, Dumped_Object_Addr);

    DEBUGGING (fprintf (stderr, "Dumping:\n"));
    DEBUGGING (fprintf (stderr,
			"Heap = 0x%x; Heap Count = %d\n",
			Heap_Base, (Free - Heap_Base)));
    DEBUGGING (fprintf (stderr,
			"Pure Space = 0x%x; Pure Count = %d\n",
			Pure_Base, (Free_Pure - Pure_Base)));
    DEBUGGING (fprintf (stderr,
			"Constant Space = 0x%x; Constant Count = %d\n",
			Const_Base, (Free_Const - Const_Base)));
    DEBUGGING (fprintf (stderr,
			"& Dumped Object = 0x%x; Dumped Object = 0x%x\n",
			Dumped_Object, * Dumped_Object));
    DEBUGGING (fprintf (stderr, "Primitive_Table_Length = %ld; ",
			Primitive_Table_Length));
    DEBUGGING (fprintf (stderr, "Primitive_Table_Size = %ld\n",
			(primitive_table_end - primitive_table)));

    if (Dumped_Compiler_Utilities != 0)
    {
      /* This knows the format of the utilities vector. */ 
      SCHEME_OBJECT * uv = (Relocate (Dumped_Compiler_Utilities));
      unsigned long len = uv[0];

      uv[len - 1] = ((SCHEME_OBJECT)
		     (((unsigned long) uv[len - 1])
		      * (sizeof (SCHEME_OBJECT))));
      uv[len - 0] = ((SCHEME_OBJECT)
		     (((unsigned long) uv[len - 0])
		      * (sizeof (SCHEME_OBJECT))));
      compiler_utilities = (MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, uv));
    }

    /* Is there a Pure/Constant block? */

    if ((Const_Objects == 0) && (Const_Count == 0)
	&& (Pure_Objects == 0) && (Pure_Count == 0))
      result = (Write_File (Dumped_Object,
			    (Free - Heap_Base), Heap_Base,
			    0, Stack_Top,
			    primitive_table, Primitive_Table_Length,
			    ((long) (primitive_table_end - primitive_table)),
			    c_code_table, C_Code_Table_Length,
			    ((long) (c_code_table_end - c_code_table)),
			    compiled_p, band_p));
    else
    {
      long Pure_Length, Total_Length;

      Pure_Length = ((Const_Base - Pure_Base) + 1);
      Total_Length = ((Constant_Top - Pure_Base) + 1);
      Pure_Base[-2] = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR,
				    Pure_Length));
      Pure_Base[-1] = (MAKE_OBJECT (PURE_PART, Total_Length));
      Const_Base[-2] = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
      Const_Base[-1] = (MAKE_OBJECT (CONSTANT_PART, Pure_Length));
      Free_Const[0] = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
      Free_Const[1] = (MAKE_OBJECT (END_OF_BLOCK, Total_Length));

      result = (Write_File (Dumped_Object,
			    (Free - Heap_Base), Heap_Base,
			    (Total_Length + 1), (Pure_Base - 2),
			    primitive_table, Primitive_Table_Length,
			    ((long) (primitive_table_end - primitive_table)),
			    c_code_table, C_Code_Table_Length,
			    ((long) (c_code_table_end - c_code_table)),
			    compiled_p, band_p));
    }

    if (!result)
    {
      fprintf (stderr, "%s: Error writing the output file.\n", program_name);
      quit (1);
    }
    free ((char *) Lowest_Allocated_Address);
  }
}

/* Top level */

static Boolean
  help_p = false,
  help_sup_p;

static struct keyword_struct
  options[] = {
    KEYWORD ("allow_nmv", &allow_nmv_p, BOOLEAN_KYWRD, BFRMT, NULL),
    KEYWORD ("allow_cc", &allow_compiled_p, BOOLEAN_KYWRD, BFRMT, NULL),
    KEYWORD ("help", &help_p, BOOLEAN_KYWRD, BFRMT, &help_sup_p),
    OUTPUT_KEYWORD (),
    INPUT_KEYWORD (),
    END_KEYWORD ()
    };

DEFUN (main, (argc, argv),
       int argc AND
       char **argv)
{
  parse_keywords (argc, argv, options, false);
  if (help_sup_p && help_p)
    print_usage_and_exit (options, 0);
    /*NOTREACHED*/

  allow_nmv_p = (allow_nmv_p || allow_compiled_p);

  setup_io ("r", "wb");
  do_it ();
  quit (0);
}
