/* -*-C-*-

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/intern.c,v 9.47 1989/06/16 11:15:04 cph Exp $ */

/* String hash functions and interning of symbols. */

#include "scheme.h"
#include "prims.h"
#include "trap.h"
#include "string.h"

/* Hashing strings */

static long
string_hash (string)
     Pointer string;
{
  fast unsigned char * scan;
  fast unsigned char * end;
  fast long result;

  scan = ((unsigned char *) (string_pointer (string, 0)));
  end = (scan + (string_length (string)));
  result = 0;
  while (scan < end)
    result = ((((result & 0x1fff) << 1) | (result >> 13)) ^ (*scan++));
  return (result);
}

static Boolean
string_equal (string1, string2)
     Pointer string1, string2;
{
  fast char * scan1;
  fast char * scan2;
  fast long length;
  fast char * end1;

  scan1 = (string_pointer (string1, 0));
  scan2 = (string_pointer (string2, 0));
  if (scan1 == scan2)
    return (true);
  length = (string_length (string1));
  if (length != (string_length (string2)))
    return (false);
  end1 = (scan1 + length);

  while (scan1 < end1)
    if ((*scan1++) != (*scan2++))
      return (false);
  return (true);
}

static Pointer *
find_symbol_internal (string)
     Pointer string;
{
  fast Pointer * bucket;
  {
    fast Pointer obarray = (Get_Fixed_Obj_Slot (OBArray));
    bucket =
      (Nth_Vector_Loc (obarray,
		       (((string_hash (string)) % (Vector_Length (obarray)))
			+ 1)));
  }
  while ((*bucket) != EMPTY_LIST)
    {
      fast Pointer symbol = (Vector_Ref ((*bucket), CONS_CAR));
      if (string_equal (string, (Fast_Vector_Ref (symbol, SYMBOL_NAME))))
	return (Nth_Vector_Loc ((*bucket), CONS_CAR));
      bucket = (Nth_Vector_Loc ((*bucket), CONS_CDR));
    }
  return (bucket);
}

/* Set this to be informed of symbols as they are interned. */
void (*intern_symbol_hook) () = ((void (*) ()) 0);

static Pointer
link_new_symbol (symbol, cell)
     Pointer symbol;
     Pointer * cell;
{
  /* `symbol' does not exist yet in obarray.  `cell' points to the
     cell containing the final '() in the list.  Replace this
     with a cons of the new symbol and '() (i.e. extend the
     list in the bucket by 1 new element). */

  fast Pointer result =
    (Make_Object (TC_INTERNED_SYMBOL, (OBJECT_DATUM (symbol))));
  Primitive_GC_If_Needed (2);
  (*cell) = (Make_Pointer (TC_LIST, Free));
  (Free [CONS_CAR]) = result;
  (Free [CONS_CDR]) = EMPTY_LIST;
  Free += 2;
  if (intern_symbol_hook != ((void (*) ()) 0))
    (*intern_symbol_hook) (result);
  return (result);
}

Pointer
find_symbol (string)
     Pointer string;
{
  fast Pointer result = (* (find_symbol_internal (string)));
  return ((result == EMPTY_LIST) ? SHARP_F : result);
}

Pointer 
string_to_symbol (string)
     Pointer string;
{
  fast Pointer * cell = (find_symbol_internal (string));
  if ((*cell) != EMPTY_LIST)
    return (*cell);
  Primitive_GC_If_Needed (2);
  {
    fast Pointer symbol = (Make_Pointer (TC_UNINTERNED_SYMBOL, Free));
    (Free [SYMBOL_NAME]) = string;
    (Free [SYMBOL_GLOBAL_VALUE]) = UNBOUND_OBJECT;
    Free += 2;
    return (link_new_symbol (symbol, cell));
  }
}

Pointer
intern_symbol (symbol)
     Pointer symbol;
{
  fast Pointer * cell =
    (find_symbol_internal (Fast_Vector_Ref (symbol, SYMBOL_NAME)));
  return
    (((*cell) != EMPTY_LIST)
     ? (*cell)
     : (link_new_symbol (symbol, cell)));
}

DEFINE_PRIMITIVE ("FIND-SYMBOL", Prim_find_symbol, 1, 1,
  "(FIND-SYMBOL STRING)
Returns the symbol whose name is STRING, or #F if no such symbol exists.")
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (find_symbol (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("STRING->SYMBOL", Prim_string_to_symbol, 1, 1,
  "(STRING->SYMBOL STRING)
Returns the symbol whose name is STRING, constructing a new symbol if needed.")
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (string_to_symbol (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("STRING-HASH", Prim_string_hash, 1, 1,
  "(STRING-HASH STRING)
Return a hash value for a string.  This uses the hashing
algorithm used for interning symbols.  It is intended for use by
the reader in creating interned symbols.")
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (MAKE_UNSIGNED_FIXNUM (string_hash (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("STRING-HASH-MOD", Prim_string_hash_mod, 2, 2,
  "(STRING-HASH-MOD STRING DENOMINATOR)
DENOMINATOR must be a nonnegative integer.
Equivalent to (MOD (STRING-HASH STRING) DENOMINATOR).")
{
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN
    (MAKE_UNSIGNED_FIXNUM
     ((string_hash (ARG_REF (1))) % (arg_nonnegative_integer (2))));
}
