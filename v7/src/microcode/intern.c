/* -*-C-*-

$Id: intern.c,v 9.58 2002/11/20 19:46:09 cph Exp $

Copyright (c) 1987-2000 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* String hash functions and interning of symbols. */

#include "scheme.h"
#include "prims.h"
#include "trap.h"

#ifdef STDC_HEADERS
#  include <string.h>
#else
   extern int EXFUN (strlen, (const char *));
#endif

/* These are exported to other parts of the system. */

extern SCHEME_OBJECT EXFUN (string_to_symbol, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (char_pointer_to_symbol, (unsigned char *));
extern SCHEME_OBJECT EXFUN (memory_to_symbol, (long, unsigned char *));
extern SCHEME_OBJECT EXFUN (find_symbol, (long, unsigned char *));

/* Hashing strings */

#define STRING_HASH_BITS 16

static unsigned int
DEFUN (string_hash, (length, string),
       long length AND unsigned char * string)
{
  fast unsigned char * scan = string;
  fast unsigned char * end = (scan + length);
  fast unsigned int result = 0;
  while (scan < end)
  {
    result <<= 1;
    result |= (result >> STRING_HASH_BITS);
    result ^= (*scan++);
    result &= ((1 << STRING_HASH_BITS) - 1);
  }
  return (result);
}

static Boolean
DEFUN (string_equal, (length1, string1, length2, string2),
       long length1 AND unsigned char * string1
       AND long length2 AND unsigned char * string2)
{
  fast unsigned char * scan1 = string1;
  fast unsigned char * scan2 = string2;
  fast long length = length1;
  fast unsigned char * end1 = (scan1 + length);
  if (scan1 == scan2)
    return (true);
  if (length != length2)
    return (false);
  while (scan1 < end1)
    if ((*scan1++) != (*scan2++))
      return (false);
  return (true);
}

static SCHEME_OBJECT *
DEFUN (find_symbol_internal, (length, string),
       long length AND unsigned char * string)
{
  fast SCHEME_OBJECT * bucket;
  {
    fast SCHEME_OBJECT obarray = (Get_Fixed_Obj_Slot (OBArray));
    bucket =
      (MEMORY_LOC (obarray,
		   (((string_hash (length, string))
		     % (VECTOR_LENGTH (obarray)))
		    + 1)));
  }
  while ((*bucket) != EMPTY_LIST)
    {
      fast SCHEME_OBJECT symbol = (PAIR_CAR (*bucket));
      fast SCHEME_OBJECT name = (FAST_MEMORY_REF (symbol, SYMBOL_NAME));
      if (string_equal (length, string,
			(STRING_LENGTH (name)), (STRING_LOC (name, 0))))
	return (PAIR_CAR_LOC (*bucket));
      bucket = (PAIR_CDR_LOC (*bucket));
    }
  return (bucket);
}

/* Set this to be informed of symbols as they are interned. */
void EXFUN ((*intern_symbol_hook), (SCHEME_OBJECT)) = 0;

static SCHEME_OBJECT
DEFUN (link_new_symbol, (symbol, cell),
       SCHEME_OBJECT symbol
       AND SCHEME_OBJECT * cell)
{
  /* `symbol' does not exist yet in obarray.  `cell' points to the
     cell containing the final '() in the list.  Replace this
     with a cons of the new symbol and '() (i.e. extend the
     list in the bucket by 1 new element). */

  fast SCHEME_OBJECT result = (OBJECT_NEW_TYPE (TC_INTERNED_SYMBOL, symbol));
  (*cell) = (cons (result, EMPTY_LIST));
  if (intern_symbol_hook != ((void (*) ()) 0))
    (*intern_symbol_hook) (result);
  return (result);
}

SCHEME_OBJECT
DEFUN (find_symbol, (length, string), long length AND unsigned char * string)
{
  SCHEME_OBJECT result = (* (find_symbol_internal (length, string)));
  return ((result == EMPTY_LIST) ? SHARP_F : result);
}

static SCHEME_OBJECT
DEFUN (make_symbol, (string, cell),
       SCHEME_OBJECT string AND
       SCHEME_OBJECT * cell)
{
  Primitive_GC_If_Needed (2);
  {
    SCHEME_OBJECT symbol = (MAKE_POINTER_OBJECT (TC_UNINTERNED_SYMBOL, Free));
    (Free [SYMBOL_NAME]) = string;
    (Free [SYMBOL_GLOBAL_VALUE]) = UNBOUND_OBJECT;
    Free += 2;
    return (link_new_symbol (symbol, cell));
  }
}

SCHEME_OBJECT
DEFUN (memory_to_symbol, (length, string),
       long length AND
       unsigned char * string)
{
  SCHEME_OBJECT * cell = (find_symbol_internal (length, string));
  return
    (((*cell) == EMPTY_LIST)
     ? (make_symbol ((memory_to_string (length, string)), cell))
     : (*cell));
}

SCHEME_OBJECT
DEFUN (char_pointer_to_symbol, (string), unsigned char * string)
{
  return (memory_to_symbol ((strlen (string)), string));
}

SCHEME_OBJECT
DEFUN (string_to_symbol, (string), SCHEME_OBJECT string)
{
  SCHEME_OBJECT * cell =
    (find_symbol_internal ((STRING_LENGTH (string)),
			   (STRING_LOC (string, 0))));
  return (((*cell) == EMPTY_LIST) ? (make_symbol (string, cell)) : (*cell));
}

SCHEME_OBJECT
DEFUN (intern_symbol, (symbol), SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT name = (FAST_MEMORY_REF (symbol, SYMBOL_NAME));
  SCHEME_OBJECT * cell =
    (find_symbol_internal ((STRING_LENGTH (name)), (STRING_LOC (name, 0))));
  return (((*cell) == EMPTY_LIST)
	  ? (link_new_symbol (symbol, cell))
	  : (*cell));
}

DEFINE_PRIMITIVE ("FIND-SYMBOL", Prim_find_symbol, 1, 1,
  "(FIND-SYMBOL STRING)\n\
Returns the symbol whose name is STRING, or #F if no such symbol exists.")
{
  SCHEME_OBJECT string;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  string = (ARG_REF (1));
  PRIMITIVE_RETURN
    (find_symbol ((STRING_LENGTH (string)), (STRING_LOC (string, 0))));
}

DEFINE_PRIMITIVE ("STRING->SYMBOL", Prim_string_to_symbol, 1, 1,
  "(STRING->SYMBOL STRING)\n\
Returns the symbol whose name is STRING, constructing a new symbol if needed.")
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (string_to_symbol (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("STRING-HASH", Prim_string_hash, 1, 1,
  "(STRING-HASH STRING)\n\
Return a hash value for a string.  This uses the hashing\n\
algorithm used for interning symbols.  It is intended for use by\n\
the reader in creating interned symbols.")
{
  SCHEME_OBJECT string;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  string = (ARG_REF (1));
  PRIMITIVE_RETURN
    (LONG_TO_UNSIGNED_FIXNUM (string_hash ((STRING_LENGTH (string)),
					   (STRING_LOC (string, 0)))));
}

DEFINE_PRIMITIVE ("STRING-HASH-MOD", Prim_string_hash_mod, 2, 2,
  "(STRING-HASH-MOD STRING DENOMINATOR)\n\
DENOMINATOR must be a nonnegative integer.\n\
Equivalent to (MOD (STRING-HASH STRING) DENOMINATOR).")
{
  SCHEME_OBJECT string;
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, STRING_P);
  string = (ARG_REF (1));
  PRIMITIVE_RETURN
    (LONG_TO_UNSIGNED_FIXNUM
     ((string_hash ((STRING_LENGTH (string)),
		    (STRING_LOC (string, 0))))
      % (arg_nonnegative_integer (2))));
}
