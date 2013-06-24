/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

/* Character primitives. */

#include "scheme.h"
#include "prims.h"
#include <ctype.h>

long
arg_ascii_char (int n)
{
  CHECK_ARG (n, CHARACTER_P);
  {
    SCHEME_OBJECT object = (ARG_REF (n));
    if (! (CHAR_TO_ASCII_P (object)))
      error_bad_range_arg (n);
    return (CHAR_TO_ASCII (object));
  }
}

long
arg_ascii_integer (int n)
{
  return (arg_index_integer (n, MAX_ASCII));
}

DEFINE_PRIMITIVE ("CHAR?", Prim_char_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (CHARACTER_P (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("MAKE-CHAR", Prim_make_char, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (MAKE_CHAR ((arg_index_integer (2, MAX_BITS)),
		(arg_index_integer (1, MAX_CODE))));
}

DEFINE_PRIMITIVE ("CHAR-BITS", Prim_char_bits, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, CHARACTER_P);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (CHAR_BITS (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("CHAR-CODE", Prim_char_code, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, CHARACTER_P);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (CHAR_CODE (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("CHAR->INTEGER", Prim_char_to_integer, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, CHARACTER_P);
  PRIMITIVE_RETURN
    (LONG_TO_UNSIGNED_FIXNUM ((ARG_REF (1)) & MASK_MIT_ASCII));
}

DEFINE_PRIMITIVE ("INTEGER->CHAR", Prim_integer_to_char, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (MAKE_OBJECT (TC_CHARACTER, (arg_index_integer (1, MAX_MIT_ASCII))));
}

long
char_downcase (long c)
{
  return ((isupper (c)) ? ((c - 'A') + 'a') : c);
}

long
char_upcase (long c)
{
  return ((islower (c)) ? ((c - 'a') + 'A') : c);
}

DEFINE_PRIMITIVE ("CHAR-DOWNCASE", Prim_char_downcase, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, CHARACTER_P);
  PRIMITIVE_RETURN
    (MAKE_CHAR ((CHAR_BITS (ARG_REF (1))),
		(char_downcase (CHAR_CODE (ARG_REF (1))))));
}

DEFINE_PRIMITIVE ("CHAR-UPCASE", Prim_char_upcase, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, CHARACTER_P);
  PRIMITIVE_RETURN
    (MAKE_CHAR ((CHAR_BITS (ARG_REF (1))),
		(char_upcase (CHAR_CODE (ARG_REF (1))))));
}

DEFINE_PRIMITIVE ("ASCII->CHAR", Prim_ascii_to_char, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ASCII_TO_CHAR (arg_index_integer (1, MAX_ASCII)));
}

DEFINE_PRIMITIVE ("CHAR->ASCII", Prim_char_to_ascii, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (arg_ascii_char (1)));
}

DEFINE_PRIMITIVE ("CHAR-ASCII?", Prim_char_ascii_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, CHARACTER_P);
  {
    SCHEME_OBJECT character = ARG_REF (1);
    PRIMITIVE_RETURN
      (((OBJECT_DATUM (character)) >= MAX_ASCII) ?
       SHARP_F :
       (LONG_TO_UNSIGNED_FIXNUM (CHAR_TO_ASCII (character))));
  }
}
