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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/char.c,v 5.3 1987/01/13 19:33:40 cph Exp $ */

/* Character primitives. */

#include "scheme.h"
#include "primitive.h"
#include "character.h"
#include <ctype.h>

#define define_ascii_character_guarantee(procedure_name, wta, bra) \
long								\
procedure_name (argument)					\
     Pointer argument;						\
{								\
  fast long ascii;						\
								\
  if (! (character_p (argument)))				\
    wta ();							\
  ascii = (scheme_char_to_c_char (argument));			\
  if (ascii == NOT_ASCII)					\
    bra ();							\
  return (ascii);						\
}

define_ascii_character_guarantee (guarantee_ascii_character_arg_1,
				  error_wrong_type_arg_1,
				  error_bad_range_arg_1)

define_ascii_character_guarantee (guarantee_ascii_character_arg_2,
				  error_wrong_type_arg_2,
				  error_bad_range_arg_2)

define_ascii_character_guarantee (guarantee_ascii_character_arg_3,
				  error_wrong_type_arg_3,
				  error_bad_range_arg_3)

define_ascii_character_guarantee (guarantee_ascii_character_arg_4,
				  error_wrong_type_arg_4,
				  error_bad_range_arg_4)

define_ascii_character_guarantee (guarantee_ascii_character_arg_5,
				  error_wrong_type_arg_5,
				  error_bad_range_arg_5)

define_ascii_character_guarantee (guarantee_ascii_character_arg_6,
				  error_wrong_type_arg_6,
				  error_bad_range_arg_6)

define_ascii_character_guarantee (guarantee_ascii_character_arg_7,
				  error_wrong_type_arg_7,
				  error_bad_range_arg_7)

define_ascii_character_guarantee (guarantee_ascii_character_arg_8,
				  error_wrong_type_arg_8,
				  error_bad_range_arg_8)

define_ascii_character_guarantee (guarantee_ascii_character_arg_9,
				  error_wrong_type_arg_9,
				  error_bad_range_arg_9)

define_ascii_character_guarantee (guarantee_ascii_character_arg_10,
				  error_wrong_type_arg_10,
				  error_bad_range_arg_10)

#define define_ascii_integer_guarantee(procedure_name, wta, bra) \
long								\
procedure_name (argument)					\
     Pointer argument;						\
{								\
  fast long ascii;						\
								\
  if (! (fixnum_p (argument))) wta ();				\
  if (fixnum_negative_p (argument)) bra ();			\
  ascii = (pointer_datum (argument));				\
  if (ascii >= MAX_ASCII) bra ();				\
  return (ascii);						\
}

define_ascii_integer_guarantee (guarantee_ascii_integer_arg_1,
				error_wrong_type_arg_1,
				error_bad_range_arg_1)

define_ascii_integer_guarantee (guarantee_ascii_integer_arg_2,
				error_wrong_type_arg_2,
				error_bad_range_arg_2)

define_ascii_integer_guarantee (guarantee_ascii_integer_arg_3,
				error_wrong_type_arg_3,
				error_bad_range_arg_3)

define_ascii_integer_guarantee (guarantee_ascii_integer_arg_4,
				error_wrong_type_arg_4,
				error_bad_range_arg_4)

define_ascii_integer_guarantee (guarantee_ascii_integer_arg_5,
				error_wrong_type_arg_5,
				error_bad_range_arg_5)

define_ascii_integer_guarantee (guarantee_ascii_integer_arg_6,
				error_wrong_type_arg_6,
				error_bad_range_arg_6)

define_ascii_integer_guarantee (guarantee_ascii_integer_arg_7,
				error_wrong_type_arg_7,
				error_bad_range_arg_7)

define_ascii_integer_guarantee (guarantee_ascii_integer_arg_8,
				error_wrong_type_arg_8,
				error_bad_range_arg_8)

define_ascii_integer_guarantee (guarantee_ascii_integer_arg_9,
				error_wrong_type_arg_9,
				error_bad_range_arg_9)

define_ascii_integer_guarantee (guarantee_ascii_integer_arg_10,
				error_wrong_type_arg_10,
				error_bad_range_arg_10)

Built_In_Primitive (Prim_Make_Char, 2, "MAKE-CHAR")
{
  long bucky_bits, code;
  Primitive_2_Args ();

  code = (guarantee_index_arg_1 (Arg1, MAX_CODE));
  bucky_bits = (guarantee_index_arg_2 (Arg2, MAX_BITS));
  return (make_char (bucky_bits, code));
}

Built_In_Primitive (Prim_Char_Bits, 1, "CHAR-BITS")
{
  Primitive_1_Arg ();

  guarantee_character_arg_1 ();
  return (Make_Unsigned_Fixnum (char_bits (Arg1)));
}

Built_In_Primitive (Prim_Char_Code, 1, "CHAR-CODE")
{
  Primitive_1_Arg ();

  guarantee_character_arg_1 ();
  return (Make_Unsigned_Fixnum (char_code (Arg1)));
}

Built_In_Primitive (Prim_Char_To_Integer, 1, "CHAR->INTEGER")
{
  Primitive_1_Arg ();

  guarantee_character_arg_1 ();
  return (Make_Unsigned_Fixnum (Arg1 & MASK_EXTNDD_CHAR));
}

Built_In_Primitive (Prim_Integer_To_Char, 1, "INTEGER->CHAR")
{
  Primitive_1_Arg ();

  return
    (Make_Non_Pointer (TC_CHARACTER,
		       (guarantee_index_arg_1 (Arg1, MAX_EXTNDD_CHAR))));
}

long
char_downcase (c)
     long c;
{
  c = (char_to_long (c));
  return ((isupper (c)) ? ((c - 'A') + 'a') : c);
}

long
char_upcase (c)
     long c;
{
  c = (char_to_long (c));
  return ((islower (c)) ? ((c - 'a') + 'A') : c);
}

Built_In_Primitive (Prim_Char_Downcase, 1, "CHAR-DOWNCASE")
{
  Primitive_1_Arg ();

  guarantee_character_arg_1 ();
  return (make_char ((char_bits (Arg1)), (char_downcase (char_code (Arg1)))));
}

Built_In_Primitive (Prim_Char_Upcase, 1, "CHAR-UPCASE")
{
  Primitive_1_Arg ();

  guarantee_character_arg_1 ();
  return (make_char ((char_bits (Arg1)), (char_upcase (char_code (Arg1)))));
}

Built_In_Primitive (Prim_Ascii_To_Char, 1, "ASCII->CHAR")
{
  Primitive_1_Arg ();

  return (c_char_to_scheme_char (guarantee_ascii_integer_arg_1 (Arg1)));
}

Built_In_Primitive (Prim_Char_To_Ascii, 1, "CHAR->ASCII")
{
  Primitive_1_Arg ();

  return (Make_Unsigned_Fixnum (guarantee_ascii_character_arg_1 (Arg1)));
}

Built_In_Primitive (Prim_Char_Ascii_P, 1, "CHAR-ASCII?")
{
  long ascii;
  Primitive_1_Arg ();

  guarantee_character_arg_1 ();
  ascii = (scheme_char_to_c_char (Arg1));
  return ((ascii == NOT_ASCII) ? NIL : (Make_Unsigned_Fixnum (ascii)));
}

forward Boolean ascii_control_p();

long
ascii_to_mit_ascii (ascii)
     long ascii;
{
  long bucky_bits, code;

  bucky_bits = (((ascii & 0200) != 0) ? CHAR_BITS_META : 0);
  code = (ascii & 0177);
  if (ascii_control_p (code))
    {
      code |= 0100;		/* Convert to non-control code. */
      bucky_bits |= CHAR_BITS_CONTROL;
    }
  return ((bucky_bits << CODE_LENGTH) | code);
}

long
mit_ascii_to_ascii (mit_ascii)
     long mit_ascii;
{
  long bucky_bits, code;

  bucky_bits = ((mit_ascii >> CODE_LENGTH) & CHAR_MASK_BITS);
  code = (mit_ascii & CHAR_MASK_CODE);
  if ((bucky_bits & (~ CHAR_BITS_CONTROL_META)) != 0)
    return (NOT_ASCII);
  else
    {
      if ((bucky_bits & CHAR_BITS_CONTROL) != 0)
	{
	  code = (char_upcase (code) & (~ 0100));
	  if (!ascii_control_p (code))
	    return (NOT_ASCII);
	}
      else
	{
	  if (ascii_control_p (code))
	    return (NOT_ASCII);
	}
      return (((bucky_bits & CHAR_BITS_META) != 0) ? (code | 0200) : code);
    }
}

Boolean
ascii_control_p (code)
     int code;
{
  switch (code)
    {
    case 000:
    case 001:
    case 002:
    case 003:
    case 004:
    case 005:
    case 006:
    case 007:
    case 016:
    case 017:
    case 020:
    case 021:
    case 022:
    case 023:
    case 024:
    case 025:
    case 026:
    case 027:
    case 030:
    case 031:
    case 034:
    case 035:
    case 036:
      return (true);

    default:
      return (false);
    }
}
