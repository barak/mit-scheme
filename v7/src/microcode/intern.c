/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/intern.c,v 9.51 1989/09/20 23:09:28 cph Rel $

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

/* String hash functions and interning of symbols. */

#include "scheme.h"
#include "prims.h"
#include "trap.h"

/* Hashing strings */

#define STRING_HASH_BITS 16

static unsigned int
string_hash (string)
     SCHEME_OBJECT string;
{
  fast unsigned char * scan = (STRING_LOC (string, 0));
  fast unsigned char * end = (scan + (STRING_LENGTH (string)));
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
string_equal (string1, string2)
     SCHEME_OBJECT string1, string2;
{
  fast unsigned char * scan1 = (STRING_LOC (string1, 0));
  fast unsigned char * scan2 = (STRING_LOC (string2, 0));
  fast long length = (STRING_LENGTH (string1));
  fast unsigned char * end1 = (scan1 + length);
  if (scan1 == scan2)
    return (true);
  if (length != (STRING_LENGTH (string2)))
    return (false);
  while (scan1 < end1)
    if ((*scan1++) != (*scan2++))
      return (false);
  return (true);
}

static SCHEME_OBJECT *
find_symbol_internal (string)
     SCHEME_OBJECT string;
{
  fast SCHEME_OBJECT * bucket;
  {
    fast SCHEME_OBJECT obarray = (Get_Fixed_Obj_Slot (OBArray));
    bucket =
      (MEMORY_LOC (obarray,
		   (((string_hash (string)) % (VECTOR_LENGTH (obarray)))
		    + 1)));
  }
  while ((*bucket) != EMPTY_LIST)
    {
      fast SCHEME_OBJECT symbol = (PAIR_CAR (*bucket));
      if (string_equal (string, (FAST_MEMORY_REF (symbol, SYMBOL_NAME))))
	return (PAIR_CAR_LOC (*bucket));
      bucket = (PAIR_CDR_LOC (*bucket));
    }
  return (bucket);
}

/* Set this to be informed of symbols as they are interned. */
void (*intern_symbol_hook) () = ((void (*) ()) 0);

static SCHEME_OBJECT
link_new_symbol (symbol, cell)
     SCHEME_OBJECT symbol;
     SCHEME_OBJECT * cell;
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
find_symbol (string)
     SCHEME_OBJECT string;
{
  fast SCHEME_OBJECT result = (* (find_symbol_internal (string)));
  return ((result == EMPTY_LIST) ? SHARP_F : result);
}

SCHEME_OBJECT
string_to_symbol (string)
     SCHEME_OBJECT string;
{
  fast SCHEME_OBJECT * cell = (find_symbol_internal (string));
  if ((*cell) != EMPTY_LIST)
    return (*cell);
  Primitive_GC_If_Needed (2);
  {
    fast SCHEME_OBJECT symbol =
      (MAKE_POINTER_OBJECT (TC_UNINTERNED_SYMBOL, Free));
    (Free [SYMBOL_NAME]) = string;
    (Free [SYMBOL_GLOBAL_VALUE]) = UNBOUND_OBJECT;
    Free += 2;
    return (link_new_symbol (symbol, cell));
  }
}

SCHEME_OBJECT
intern_symbol (symbol)
     SCHEME_OBJECT symbol;
{
  fast SCHEME_OBJECT * cell =
    (find_symbol_internal (FAST_MEMORY_REF (symbol, SYMBOL_NAME)));
  return
    (((*cell) != EMPTY_LIST)
     ? (*cell)
     : (link_new_symbol (symbol, cell)));
}

DEFINE_PRIMITIVE ("FIND-SYMBOL", Prim_find_symbol, 1, 1,
  "(FIND-SYMBOL STRING)\n\
Returns the symbol whose name is STRING, or #F if no such symbol exists.")
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (find_symbol (ARG_REF (1)));
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
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (string_hash (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("STRING-HASH-MOD", Prim_string_hash_mod, 2, 2,
  "(STRING-HASH-MOD STRING DENOMINATOR)\n\
DENOMINATOR must be a nonnegative integer.\n\
Equivalent to (MOD (STRING-HASH STRING) DENOMINATOR).")
{
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN
    (LONG_TO_UNSIGNED_FIXNUM
     ((string_hash (ARG_REF (1))) % (arg_nonnegative_integer (2))));
}
