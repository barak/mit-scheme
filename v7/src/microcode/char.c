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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/char.c,v 9.24 1987/11/23 05:16:52 cph Rel $ */

/* Character primitives. */

#include "scheme.h"
#include "primitive.h"
#include "char.h"
#include <ctype.h>

long
arg_ascii_char (n)
     int n;
{
  fast long ascii;

  CHECK_ARG (n, CHARACTER_P);
  ascii = (scheme_char_to_c_char (ARG_REF (n)));
  if (ascii == NOT_ASCII)
    error_bad_range_arg (n);
  return (ascii);
}

long
arg_ascii_integer (n)
     int n;
{
  fast Pointer arg;
  fast long ascii;

  CHECK_ARG (n, FIXNUM_P);
  arg = (ARG_REF (n));
  if (FIXNUM_NEGATIVE_P (arg))
    error_bad_range_arg (n);
  FIXNUM_VALUE (arg, ascii);
  if (ascii >= MAX_ASCII)
    error_bad_range_arg (n);
  return (ascii);
}

DEFINE_PRIMITIVE ("MAKE-CHAR", Prim_Make_Char, 2)
{
  long bucky_bits, code;
  PRIMITIVE_HEADER (2);

  code = (arg_index_integer (1, MAX_CODE));
  bucky_bits = (arg_index_integer (2, MAX_BITS));
  PRIMITIVE_RETURN (make_char (bucky_bits, code));
}

DEFINE_PRIMITIVE ("CHAR-BITS", Prim_Char_Bits, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, CHARACTER_P);
  PRIMITIVE_RETURN (MAKE_UNSIGNED_FIXNUM (char_bits (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("CHAR-CODE", Prim_Char_Code, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, CHARACTER_P);
  PRIMITIVE_RETURN (MAKE_UNSIGNED_FIXNUM (char_code (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("CHAR->INTEGER", Prim_Char_To_Integer, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, CHARACTER_P);
  PRIMITIVE_RETURN (MAKE_UNSIGNED_FIXNUM ((ARG_REF (1)) & MASK_EXTNDD_CHAR));
}

DEFINE_PRIMITIVE ("INTEGER->CHAR", Prim_Integer_To_Char, 1)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN
    (Make_Non_Pointer (TC_CHARACTER,
		       (arg_index_integer (1, MAX_EXTNDD_CHAR))));
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

DEFINE_PRIMITIVE ("CHAR-DOWNCASE", Prim_Char_Downcase, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, CHARACTER_P);
  PRIMITIVE_RETURN
    (make_char ((char_bits (ARG_REF (1))),
		(char_downcase (char_code (ARG_REF (1))))));
}

DEFINE_PRIMITIVE ("CHAR-UPCASE", Prim_Char_Upcase, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, CHARACTER_P);
  PRIMITIVE_RETURN
    (make_char ((char_bits (ARG_REF (1))),
		(char_upcase (char_code (ARG_REF (1))))));
}

DEFINE_PRIMITIVE ("ASCII->CHAR", Prim_Ascii_To_Char, 1)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (c_char_to_scheme_char (arg_ascii_integer (1)));
}

DEFINE_PRIMITIVE ("CHAR->ASCII", Prim_Char_To_Ascii, 1)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (MAKE_UNSIGNED_FIXNUM (arg_ascii_char (1)));
}

DEFINE_PRIMITIVE ("CHAR-ASCII?", Prim_Char_Ascii_P, 1)
{
  long ascii;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, CHARACTER_P);
  ascii = (scheme_char_to_c_char (ARG_REF (1)));
  PRIMITIVE_RETURN
    ((ascii == NOT_ASCII) ? NIL : (MAKE_UNSIGNED_FIXNUM (ascii)));
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
