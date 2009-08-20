/* -*-C-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

/* String hash functions and interning of symbols. */

#include "scheme.h"
#include "prims.h"
#include "trap.h"

/* The FNV hash, short for Fowler/Noll/Vo in honor of its creators.  */

static uint32_t
string_hash (uint32_t length, const char * string)
{
  const unsigned char * scan = ((const unsigned char *) string);
  const unsigned char * end = (scan + length);
  uint32_t result = 2166136261U;
  while (scan < end)
    result = ((result * 16777619U) + (*scan++));
#if (BIGGEST_FIXNUM >= 0xFFFFFFFF)
  return (result);
#else
  return (result & ((uint32_t) BIGGEST_FIXNUM));
#endif
}

static SCHEME_OBJECT *
find_symbol_internal (unsigned long length, const char * string)
{
  SCHEME_OBJECT obarray = (VECTOR_REF (fixed_objects, OBARRAY));
  SCHEME_OBJECT * bucket
    = (VECTOR_LOC (obarray,
		   ((string_hash (length, string))
		    % (VECTOR_LENGTH (obarray)))));
  while (true)
    {
      SCHEME_OBJECT list = (*bucket);
      if (PAIR_P (list))
	{
	  SCHEME_OBJECT symbol = (PAIR_CAR (list));
	  SCHEME_OBJECT name = (MEMORY_REF (symbol, SYMBOL_NAME));
	  if (((STRING_LENGTH (name)) == length)
	      && ((memcmp ((STRING_POINTER (name)), string, length)) == 0))
	    return (PAIR_CAR_LOC (list));
	}
      else
	return (bucket);
      bucket = (PAIR_CDR_LOC (list));
    }
}

static SCHEME_OBJECT
make_symbol (SCHEME_OBJECT string, SCHEME_OBJECT * cell)
{
  Primitive_GC_If_Needed (4);
  {
    SCHEME_OBJECT symbol = (MAKE_POINTER_OBJECT (TC_INTERNED_SYMBOL, Free));
    Free += 2;
    MEMORY_SET (symbol, SYMBOL_NAME, string);
    MEMORY_SET (symbol, SYMBOL_GLOBAL_VALUE, UNBOUND_OBJECT);
    (*cell) = (cons (symbol, EMPTY_LIST));
    return (symbol);
  }
}

SCHEME_OBJECT
find_symbol (unsigned long length, const char * string)
{
  SCHEME_OBJECT * cell = (find_symbol_internal (length, string));
  return ((INTERNED_SYMBOL_P (*cell)) ? (*cell) : SHARP_F);
}

SCHEME_OBJECT
memory_to_symbol (unsigned long length, const void * string)
{
  SCHEME_OBJECT * cell = (find_symbol_internal (length, string));
  return
    ((INTERNED_SYMBOL_P (*cell))
     ? (*cell)
     : (make_symbol ((memory_to_string (length, string)), cell)));
}

SCHEME_OBJECT
char_pointer_to_symbol (const char * string)
{
  return (memory_to_symbol ((strlen (string)), string));
}

SCHEME_OBJECT
string_to_symbol (SCHEME_OBJECT string)
{
  SCHEME_OBJECT * cell
    = (find_symbol_internal ((STRING_LENGTH (string)),
			     (STRING_POINTER (string))));
  return ((INTERNED_SYMBOL_P (*cell))
	  ? (*cell)
	  : (make_symbol (string, cell)));
}

SCHEME_OBJECT
intern_symbol (SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT name = (MEMORY_REF (symbol, SYMBOL_NAME));
  SCHEME_OBJECT * cell
    = (find_symbol_internal ((STRING_LENGTH (name)),
			     (STRING_POINTER (name))));
  if (INTERNED_SYMBOL_P (*cell))
    return (*cell);
  else
    {
      SCHEME_OBJECT result = (OBJECT_NEW_TYPE (TC_INTERNED_SYMBOL, symbol));
      (*cell) = (cons (result, EMPTY_LIST));
      return (result);
    }
}

const char *
arg_symbol (int n)
{
  CHECK_ARG (n, SYMBOL_P);
  return (STRING_POINTER (MEMORY_REF ((ARG_REF (n)), SYMBOL_NAME)));
}

const char *
arg_interned_symbol (int n)
{
  CHECK_ARG (n, SYMBOL_P);
  return (STRING_POINTER (MEMORY_REF ((ARG_REF (n)), SYMBOL_NAME)));
}

DEFINE_PRIMITIVE ("FIND-SYMBOL", Prim_find_symbol, 1, 1,
  "(STRING)\n\
Returns the symbol named STRING, or #F if no such symbol exists.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (1));
    PRIMITIVE_RETURN (find_symbol ((STRING_LENGTH (string)),
				   (STRING_POINTER (string))));
  }
}

DEFINE_PRIMITIVE ("STRING->SYMBOL", Prim_string_to_symbol, 1, 1,
  "(STRING)\n\
Returns the interned symbol named STRING, constructing a new symbol\n\
if needed.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (string_to_symbol (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("STRING-HASH", Prim_string_hash, 1, 1,
  "(STRING)\n\
Returns the hash value for STRING, using the hashing algorithm for\n\
interning symbols.")

{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (1));
    PRIMITIVE_RETURN
      (ULONG_TO_FIXNUM (string_hash ((STRING_LENGTH (string)),
				     (STRING_POINTER (string)))));
  }
}

DEFINE_PRIMITIVE ("STRING-HASH-MOD", Prim_string_hash_mod, 2, 2,
  "(STRING DENOMINATOR)\n\
DENOMINATOR must be a nonnegative integer.\n\
Equivalent to (MODULO (STRING-HASH STRING) DENOMINATOR).")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (1));
    PRIMITIVE_RETURN
      (ULONG_TO_FIXNUM ((string_hash ((STRING_LENGTH (string)),
				      (STRING_POINTER (string))))
			% (arg_ulong_integer (2))));
  }
}
