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


/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/primutl.c,v 9.40 1987/04/16 14:34:28 jinx Rel $
 *
 * This file contains the support routines for mapping primitive names
 * to numbers within the microcode.  This mechanism is only used by
 * the runtime system on "external" primitives.  "Built-in" primitives
 * must match their position in utabmd.scm.  Eventually both
 * mechanisms will be merged.  External primitives are written in C
 * and available in Scheme, but not always present in all versions of
 * the interpreter.  Thus, these objects are always referenced
 * externally by name and converted to numeric references only for the
 * duration of a single Scheme session.
 */

#include "scheme.h"
#include "primitive.h"

/* Common utilities. */

/* In the following two procedures, size is really 1 less than size.
   It is really the index of the last valid entry.
 */

long
primitive_name_to_code(name, table, size)
     char *name;
     char *table[];
     long size;
{
  fast long i;

  for (i = size; i >= 0; i -= 1)
  {
    fast char *s1, *s2;

    s1 = name;
    s2 = table[i];

    while (*s1++ == *s2)
      if (*s2++ == '\0')
	return i;
      
  }
  return -1;
}

char *
primitive_code_to_name(code, table, size)
     long code;
     char *table[];
     long size;
{
  if ((code > size) || (code < 0))
    return ((char *) NULL);
  else
    return table[code];
}

int
primitive_code_to_arity(code, table, size)
     long code;
     int table[];
     long size;
{
  if ((code > size) || (code < 0))
    return -1;
  else
    return table[code];
}

/* Utilities exclusively for built-in primitives. */

extern Pointer make_primitive();

Pointer
make_primitive(name)
     char *name;
{
  long code;

  code = primitive_name_to_code(name,
				&Primitive_Name_Table[0],
				MAX_PRIMITIVE);
  if (code == -1)
    return NIL;
  return
    Make_Non_Pointer(TC_PRIMITIVE, code);
}

extern long primitive_to_arity();

long
primitive_to_arity(code)
     int code;
{
  return
    primitive_code_to_arity(code,
			    &Primitive_Arity_Table[0],
			    MAX_PRIMITIVE);
}

extern char *primitive_to_name();

char *
primitive_to_name(code)
     int code;
{
  return
    primitive_code_to_name(code,
			   &Primitive_Name_Table[0],
			   MAX_PRIMITIVE);
}

/* Utilities exclusively for external primitives. */

Pointer Undefined_Externals = NIL;

Pointer
external_primitive_name(code)
     long code;
{
  extern Pointer string_to_symbol();

  return
    string_to_symbol(C_String_To_Scheme_String(External_Name_Table[code]));
}

extern long make_external_primitive();

long
make_external_primitive(Symbol, Intern_It)
     Pointer Symbol, Intern_It;
{
  extern Boolean string_equal();
  Pointer *Next, Name;
  long i, Max;

  Name = Fast_Vector_Ref(Symbol, SYMBOL_NAME);

  i = primitive_name_to_code(Scheme_String_To_C_String(Name),
			     &External_Name_Table[0],
			     MAX_EXTERNAL_PRIMITIVE);
  if (i != -1)
    return Make_Non_Pointer(TC_PRIMITIVE_EXTERNAL, i);
  else if (Intern_It == NIL)
    return NIL;

  Max = NUndefined();
  if (Max > 0)
    Next = Nth_Vector_Loc(Undefined_Externals, 2);

  for (i = 1; i <= Max; i++)
  {
    if (string_equal(Name, Fast_Vector_Ref(*Next++, SYMBOL_NAME)))
      return Make_Non_Pointer(TC_PRIMITIVE_EXTERNAL,
			      (MAX_EXTERNAL_PRIMITIVE + i));
  }
  if (Intern_It != TRUTH)
    return NIL;

  /* Intern the primitive name by adding it to the vector of
     undefined primitives */

  if ((Max % CHUNK_SIZE) == 0)
  {
    Primitive_GC_If_Needed(Max + CHUNK_SIZE + 2);
    if (Max > 0) Next =
      Nth_Vector_Loc(Undefined_Externals, 2);
    Undefined_Externals = Make_Pointer(TC_VECTOR, Free);
    *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, (Max + CHUNK_SIZE + 1));
    *Free++ = Make_Unsigned_Fixnum(Max + 1);
    for (i = 0; i < Max; i++)
      *Free++ = Fetch(*Next++);
    *Free++ = Symbol;
    for (i = 1; i < CHUNK_SIZE; i++)
      *Free++ = NIL;
  }
  else
  {
    User_Vector_Set(Undefined_Externals, (Max + 1), Symbol);
    User_Vector_Set(Undefined_Externals, 0, Make_Unsigned_Fixnum(Max + 1));
  }
  return
    Make_Non_Pointer(TC_PRIMITIVE_EXTERNAL,
		     (MAX_EXTERNAL_PRIMITIVE + Max + 1));
}

extern long external_primitive_to_arity();

long
external_primitive_to_arity(code)
     int code;
{
  return
    primitive_code_to_arity(code,
			    &External_Arity_Table[0],
			    MAX_EXTERNAL_PRIMITIVE);
}

extern Pointer Make_Prim_Exts();

/*
   Used to create a vector with symbols for each of the external
   primitives known to the system.
*/

Pointer 
Make_Prim_Exts()
{
  fast Pointer Result, *scan;
  fast long i, Max, Count;

  Max = NUndefined();
  Count = (MAX_EXTERNAL_PRIMITIVE + Max + 1);
  Primitive_GC_If_Needed(Count + 1);
  Result = Make_Pointer(TC_VECTOR, Free);
  scan = Free;
  Free += Count + 1;

  *scan++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, Count);
  for (i = 0; i <= MAX_EXTERNAL_PRIMITIVE; i++)
  {
    *scan++ = external_primitive_name(i);
  }
  for (i = 1; i <= Max; i++)
  {
    *scan++ = User_Vector_Ref(Undefined_Externals, i);
  }
  return Result;
}
