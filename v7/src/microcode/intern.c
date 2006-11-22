/* -*-C-*-

$Id: intern.c,v 9.63 2006/11/22 04:36:35 cph Exp $

Copyright 1987,1988,1989,1992,1994,1996 Massachusetts Institute of Technology
Copyright 2000,2004,2005,2006 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

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

/* Hashing strings */

/* The FNV hash, short for Fowler/Noll/Vo in honor of its creators.  */

static unsigned int
DEFUN (string_hash, (length, string),
       unsigned long length AND
       CONST char * string)
{
  CONST unsigned char * scan = string;
  CONST unsigned char * end = (scan + length);
  unsigned int result = 0x811c9dc5;
  while (scan < end)
    result = ((result * 0x1000193) + (*scan++));
  return (result & ((unsigned int) BIGGEST_FIXNUM));
}

static SCHEME_OBJECT *
DEFUN (find_symbol_internal, (length, string),
       unsigned long length AND
       CONST char * string)
{
  SCHEME_OBJECT obarray = (Get_Fixed_Obj_Slot (OBArray));
  SCHEME_OBJECT * bucket
    = (MEMORY_LOC (obarray,
		   (((string_hash (length, string))
		     % (VECTOR_LENGTH (obarray)))
		    + 1)));
  while (!EMPTY_LIST_P (*bucket))
    {
      SCHEME_OBJECT symbol = (PAIR_CAR (*bucket));
      SCHEME_OBJECT name = (FAST_MEMORY_REF (symbol, SYMBOL_NAME));
      if (((STRING_LENGTH (name)) == length)
	  && ((memcmp ((STRING_LOC (name, 0)), string, length)) == 0))
	return (PAIR_CAR_LOC (*bucket));
      bucket = (PAIR_CDR_LOC (*bucket));
    }
  return (bucket);
}

CONST char *
DEFUN (arg_symbol, (n), int n)
{
  CHECK_ARG (n, SYMBOL_P);
  return (STRING_LOC ((FAST_MEMORY_REF ((ARG_REF (n)), SYMBOL_NAME)), 0));
}

CONST char *
DEFUN (arg_interned_symbol, (n), int n)
{
  CHECK_ARG (n, SYMBOL_P);
  return (STRING_LOC ((FAST_MEMORY_REF ((ARG_REF (n)), SYMBOL_NAME)), 0));
}

SCHEME_OBJECT
DEFUN (find_symbol, (length, string),
       unsigned long length AND
       CONST char * string)
{
  SCHEME_OBJECT result = (* (find_symbol_internal (length, string)));
  return ((EMPTY_LIST_P (result)) ? SHARP_F : result);
}

static SCHEME_OBJECT
DEFUN (make_symbol, (string, cell),
       SCHEME_OBJECT string AND
       SCHEME_OBJECT * cell)
{
  Primitive_GC_If_Needed (4);
  {
    SCHEME_OBJECT symbol = (MAKE_POINTER_OBJECT (TC_INTERNED_SYMBOL, Free));
    (Free[SYMBOL_NAME]) = string;
    (Free[SYMBOL_GLOBAL_VALUE]) = UNBOUND_OBJECT;
    Free += 2;
    (*cell) = (cons (symbol, EMPTY_LIST));
    return (symbol);
  }
}

SCHEME_OBJECT
DEFUN (memory_to_symbol, (length, string),
       unsigned long length AND
       CONST char * string)
{
  SCHEME_OBJECT * cell = (find_symbol_internal (length, string));
  return
    ((EMPTY_LIST_P (*cell))
     ? (make_symbol ((memory_to_string (length, string)), cell))
     : (*cell));
}

SCHEME_OBJECT
DEFUN (char_pointer_to_symbol, (string), CONST char * string)
{
  return (memory_to_symbol ((strlen (string)), string));
}

SCHEME_OBJECT
DEFUN (string_to_symbol, (string), SCHEME_OBJECT string)
{
  SCHEME_OBJECT * cell
    = (find_symbol_internal ((STRING_LENGTH (string)),
			     (STRING_LOC (string, 0))));
  return ((EMPTY_LIST_P (*cell)) ? (make_symbol (string, cell)) : (*cell));
}

SCHEME_OBJECT
DEFUN (intern_symbol, (symbol), SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT name = (FAST_MEMORY_REF (symbol, SYMBOL_NAME));
  SCHEME_OBJECT * cell
    = (find_symbol_internal ((STRING_LENGTH (name)), (STRING_LOC (name, 0))));
  if (!EMPTY_LIST_P (*cell))
    return (*cell);
  else
    {
      SCHEME_OBJECT result = (OBJECT_NEW_TYPE (TC_INTERNED_SYMBOL, symbol));
      (*cell) = (cons (result, EMPTY_LIST));
      return (result);
    }
}

DEFINE_PRIMITIVE ("FIND-SYMBOL", Prim_find_symbol, 1, 1,
  "(FIND-SYMBOL STRING)\n\
Returns the symbol whose name is STRING, or #F if no such symbol exists.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (1));
    PRIMITIVE_RETURN
      (find_symbol ((STRING_LENGTH (string)), (STRING_LOC (string, 0))));
  }
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
algorithm for interning symbols.  It is intended for use by\n\
the reader in creating interned symbols.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (1));
    PRIMITIVE_RETURN
      (LONG_TO_UNSIGNED_FIXNUM (string_hash ((STRING_LENGTH (string)),
					     (STRING_LOC (string, 0)))));
  }
}

DEFINE_PRIMITIVE ("STRING-HASH-MOD", Prim_string_hash_mod, 2, 2,
  "(STRING-HASH-MOD STRING DENOMINATOR)\n\
DENOMINATOR must be a nonnegative integer.\n\
Equivalent to (MODULO (STRING-HASH STRING) DENOMINATOR).")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (1));
    PRIMITIVE_RETURN
      (LONG_TO_UNSIGNED_FIXNUM
       ((string_hash ((STRING_LENGTH (string)),
		      (STRING_LOC (string, 0))))
	% (arg_ulong_integer (2))));
  }
}
