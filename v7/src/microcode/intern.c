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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/intern.c,v 9.44 1987/11/23 05:17:30 cph Rel $ */

/* Utilities for manipulating symbols. */

#include "scheme.h"
#include "primitive.h"
#include "trap.h"
#include "string.h"

/* Hashing strings and character lists. */

long
Do_Hash (String_Ptr, String_Length)
     char *String_Ptr;
     long String_Length;
{
  fast long i, Value, End_Count;

  Value = (LENGTH_MULTIPLIER * String_Length);
  End_Count = ((String_Length > MAX_HASH_CHARS) ?
	       MAX_HASH_CHARS :
	       String_Length);
  for (i = 0; i < End_Count; i++)
    Value = ((Value << SHIFT_AMOUNT) + (MAX_CHAR & String_Ptr[i]));
  return (Value);
}

long
scheme_string_hash (string)
     Pointer string;
{
  return
    (Do_Hash ((Scheme_String_To_C_String (string)),
	      (string_length (string))));
}

Pointer
Hash (string)
     Pointer string;
{
  return (MAKE_SIGNED_FIXNUM (scheme_string_hash (string)));
}

Boolean
string_equal (String1, String2)
     Pointer String1, String2;
{
  fast char *S1, *S2;
  fast long i, Length1, Length2;

  if (Address(String1) == Address(String2))
    return true;
  Length1 = ((long) (Fast_Vector_Ref(String1, STRING_LENGTH)));
  Length2 = ((long) (Fast_Vector_Ref(String2, STRING_LENGTH)));
  if (Length1 != Length2)
  {
    return false;
  }

  S1 = ((char *) Nth_Vector_Loc(String1, STRING_CHARS));
  S2 = ((char *) Nth_Vector_Loc(String2, STRING_CHARS));
  for (i = 0; i < Length1; i++)
  {
    if (*S1++ != *S2++)
    {
      return (false);
    }
  }
  return (true);
}

/* Interning involves hashing the input string and either returning
   an existing symbol with that name from the ObArray or creating a
   new symbol and installing it in the ObArray. The resulting interned
   symbol is stored in *Un_Interned. */

/* Set this to be informed of symbols as they are interned. */
void (*intern_symbol_hook) ();

extern void Intern();

void
Intern (Un_Interned)
     Pointer *Un_Interned;
{
  fast Pointer string, *bucket, symbol;
  Pointer Ob_Array;

  string = (Fast_Vector_Ref (*Un_Interned, SYMBOL_NAME));
  Ob_Array = (Get_Fixed_Obj_Slot (OBArray));
  bucket =
    (Nth_Vector_Loc (Ob_Array,
		     (((scheme_string_hash (string)) %
		       (Vector_Length (Ob_Array)))
		      + 1)));

  while (*bucket != NIL)
    {
      symbol = (Vector_Ref (*bucket, CONS_CAR));
      if (string_equal(string, (Fast_Vector_Ref (symbol, SYMBOL_NAME))))
	{
	  *Un_Interned = symbol;
	  return;
	}
      bucket = (Nth_Vector_Loc (*bucket, CONS_CDR));
    }

  /* Symbol does not exist yet in obarray.  bucket points to the
     cell containing the final '() in the list.  Replace this
     with the CONS of the new symbol and '() (i.e. extend the
     list in the bucket by 1 new element). */

  Store_Type_Code (*Un_Interned, TC_INTERNED_SYMBOL);
  *bucket = (Make_Pointer (TC_LIST, Free));
  Free[CONS_CAR] = *Un_Interned;
  Free[CONS_CDR] = NIL;
  Free += 2;
  if (intern_symbol_hook)
    (*intern_symbol_hook) (*Un_Interned);
  return;
}

Pointer 
string_to_symbol (String)
     Pointer String;
{
  Pointer New_Symbol, Interned_Symbol, *Orig_Free;

  Orig_Free = Free;
  New_Symbol = Make_Pointer(TC_UNINTERNED_SYMBOL, Free);
  Free[SYMBOL_NAME] = String;
  Free[SYMBOL_GLOBAL_VALUE] = UNBOUND_OBJECT;
  Free += 2;
  Interned_Symbol = New_Symbol;

  /* The work is done by Intern which returns in Interned_Symbol
     either the same symbol we gave it (in which case we need to check
     for GC) or an existing symbol (in which case we have to release
     the heap space acquired to hold New_Symbol).
  */

  Intern(&Interned_Symbol);
  if (Address(Interned_Symbol) == Address(New_Symbol))
  {
    Primitive_GC_If_Needed(0);	
  }
  else
    Free = Orig_Free;
  return Interned_Symbol;
}

/* For debugging, given a String, return either a "not interned"
 * message or the address of the symbol and its global value.
 */

void 
Find_Symbol (scheme_string)
     Pointer scheme_string;
{
  Pointer the_obarray, symbol, *bucket;
  long hash_of_string;

  hash_of_string = scheme_string_hash(scheme_string);
  the_obarray = Get_Fixed_Obj_Slot(OBArray);
  hash_of_string %= Vector_Length(the_obarray);
  bucket = Nth_Vector_Loc(the_obarray, hash_of_string);
  while (*bucket != NIL)
  {
    if (string_equal(scheme_string,
                     Vector_Ref(Vector_Ref(*bucket, CONS_CAR),
                                SYMBOL_NAME)))
    {
      symbol = Vector_Ref(*bucket, CONS_CAR);
      printf("\nInterned Symbol: 0x%x", symbol);
      Print_Expression(Vector_Ref(symbol, SYMBOL_GLOBAL_VALUE),
                       "Value");
      printf("\n");
      return;
    }
    bucket = Nth_Vector_Loc(*bucket, CONS_CDR);
  }
  printf("\nNot interned.\n");
  return;
}

/* (STRING->SYMBOL STRING)
   Similar to INTERN-CHARACTER-LIST, except this one takes a string
   instead of a list of ascii values as argument.  */

DEFINE_PRIMITIVE ("STRING->SYMBOL", Prim_String_To_Symbol, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (string_to_symbol (ARG_REF (1)));
}

/* (INTERN-CHARACTER-LIST LIST)
   LIST should consist of the ASCII codes for characters.  Returns
   a new (interned) symbol made out of these characters.  Notice
   that this is a fairly low-level primitive, and no checking is
   done on the characters except that they are in the range 0 to
   255.  Thus non-printing, lower-case, and special characters can
   be put into symbols this way.  */

DEFINE_PRIMITIVE ("INTERN-CHARACTER-LIST", Prim_Intern_Character_List, 1)
{
  extern Pointer list_to_string();
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (string_to_symbol (list_to_string (ARG_REF (1))));
}

/* (STRING-HASH STRING)
   Return a hash value for a string.  This uses the hashing
   algorithm used for interning symbols.  It is intended for use by
   the reader in creating interned symbols.  */

DEFINE_PRIMITIVE ("STRING-HASH", Prim_String_Hash, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (Hash (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("STRING-HASH-MOD", Prim_string_hash_mod, 2)
{
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN
    (MAKE_UNSIGNED_FIXNUM
     ((scheme_string_hash (ARG_REF (1))) %
      (arg_nonnegative_integer (2))));
}

/* (CHARACTER-LIST-HASH LIST)
   Takes a list of ASCII codes for characters and returns a hash
   code for them.  This uses the hashing function used to intern
   symbols in Fasload, and is really intended only for that
   purpose.  */

DEFINE_PRIMITIVE ("CHARACTER-LIST-HASH", Prim_Character_List_Hash, 1)
{
  fast Pointer char_list;
  long Length;
  Pointer This_Char;
  char String[MAX_HASH_CHARS];
  PRIMITIVE_HEADER (1);

  char_list = (ARG_REF (1));
  Touch_In_Primitive (char_list, char_list);
  for (Length = 0; (PAIR_P (char_list)); Length++)
    {
      if (Length < MAX_HASH_CHARS)
	{
	  Touch_In_Primitive
	    ((Vector_Ref (char_list, CONS_CAR)), This_Char);
	  if (! (CHARACTER_P (This_Char)))
	    error_wrong_type_arg (1);
	  Range_Check((String [Length]), This_Char,
		      '\0', ((char) MAX_CHAR),
		      ERR_ARG_1_WRONG_TYPE);
	  Touch_In_Primitive
	    ((Vector_Ref (char_list, CONS_CDR)), char_list);
	}
    }
  if (char_list != NIL)
    error_wrong_type_arg (1);
  PRIMITIVE_RETURN (MAKE_SIGNED_FIXNUM (Do_Hash (String, Length)));
}
