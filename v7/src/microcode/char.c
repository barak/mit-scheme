/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/char.c,v 9.29 1991/10/29 22:55:11 jinx Exp $

Copyright (c) 1987-1991 Massachusetts Institute of Technology

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

/* Character primitives. */

#include "scheme.h"
#include "prims.h"
#include <ctype.h>

long
DEFUN (arg_ascii_char, (n), int n)
{
  CHECK_ARG (n, CHARACTER_P);
  {
    fast SCHEME_OBJECT object = (ARG_REF (n));
    if (! (CHAR_TO_ASCII_P (object)))
      error_bad_range_arg (n);
    return (CHAR_TO_ASCII (object));
  }
}

long
DEFUN (arg_ascii_integer, (n), int n)
{
  return (arg_index_integer (n, MAX_ASCII));
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
DEFUN (char_downcase, (c), fast long c)
{
  return ((isupper (c)) ? ((c - 'A') + 'a') : c);
}

long
DEFUN (char_upcase, (c), fast long c)
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
    fast SCHEME_OBJECT character = ARG_REF (1);
    PRIMITIVE_RETURN
      (((OBJECT_DATUM (character)) >= MAX_ASCII) ?
       SHARP_F :
       (LONG_TO_UNSIGNED_FIXNUM (CHAR_TO_ASCII (character))));
  }
}
