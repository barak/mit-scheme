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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/extern.c,v 9.21 1987/01/22 14:23:45 jinx Exp $
 *
 * This file contains the support routines for externally supplied
 * procedure -- that is, primitives written in C and available
 * in Scheme, but not always present in all versions of the interpreter.
 * Thus, these objects are always referenced externally by name and
 * converted to numeric references only for the duration of a single
 * Scheme session.
 */

#include "scheme.h"
#include "primitive.h"

Pointer Undefined_Externals = NIL;

#define NUndefined()	\
((Undefined_Externals==NIL) ? \
 0 : Get_Integer(User_Vector_Ref(Undefined_Externals, 0)))

#define CHUNK_SIZE	20	/* Grow undefined vector by this much */

/* (GET-EXTERNALS-COUNT)
   [Primitive number 0x101]
   Returns a CONS of the number of external primitives defined in this
   interpreter and the number of external primitives referenced but
   not defined.
*/

Built_In_Primitive(Prim_Get_External_Count, 0, "GET-EXTERNALS-COUNT")
{ Primitive_0_Args();
  *Free++ = FIXNUM_0 + (MAX_EXTERNAL_PRIMITIVE + 1);
  *Free++ = FIXNUM_0 + NUndefined();
  return Make_Pointer(TC_LIST, Free-2);
}

Pointer Get_Name_Of_Impl_External(Number)
long Number;
{ Pointer Result;
  Pointer *Orig_Result, *Orig_Free = Free;

  Result = C_String_To_Scheme_String(Ext_Prim_Desc[Number].name);
  Free[SYMBOL_NAME] = Result;
  Free[SYMBOL_GLOBAL_VALUE] = NIL;
  Result = Make_Pointer(TC_UNINTERNED_SYMBOL, Free);
  Orig_Result = Free;
  Free += 2;
  Intern(&Result);
  if (Get_Pointer(Result) != Orig_Result) Free = Orig_Free;
  return Result;
}

/* (GET-EXTERNAL-NAME n)
   [Primitive number 0x102]
   Given a number, return the string for the name of the corresponding
   external primitive.  An error if the number is out of range.
   External primitives start at 0.
*/

Built_In_Primitive(Prim_Get_Ext_Name, 1, "GET-EXTERNAL-NAME")
{ long Number, TC;
  Primitive_1_Arg();

  TC = Type_Code(Arg1);
  if ((TC != TC_FIXNUM) && (TC != TC_PRIMITIVE_EXTERNAL))
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Range_Check(Number, Arg1, 0, MAX_EXTERNAL_PRIMITIVE+NUndefined(),
              ERR_ARG_1_BAD_RANGE);
  if (Number <= MAX_EXTERNAL_PRIMITIVE)
    return Get_Name_Of_Impl_External(Number);
  else return User_Vector_Ref(Undefined_Externals,
                              Number-MAX_EXTERNAL_PRIMITIVE);
}

Boolean PGEN_Compare(C_String, S_String)
char *C_String;
Pointer S_String;
{ char *S = (char *) Nth_Vector_Loc(S_String, STRING_CHARS);
  long N = Get_Integer(Fast_Vector_Ref(S_String, STRING_LENGTH));
  long i;
  for (i=0; i < N; i++) if (*S++ != *C_String++) return false;
  return (*C_String == 0);
}

long Get_Ext_Number(Symbol, Intern_It)
Pointer Symbol, Intern_It;
{ Pointer *Next, Name = Fast_Vector_Ref(Symbol, SYMBOL_NAME);
  long i, Max;

  for (i=0; i <= MAX_EXTERNAL_PRIMITIVE; i++)
    if (PGEN_Compare(Ext_Prim_Desc[i].name, Name)) return i;
  if (Intern_It == NIL) return -1;
  Max = NUndefined();
  if (Max > 0) Next = Nth_Vector_Loc(Undefined_Externals, 2);
  for (i=1; i <= Max; i++)
    if (String_Equal(Name, Fast_Vector_Ref(*Next++, SYMBOL_NAME)))
      return MAX_EXTERNAL_PRIMITIVE+i;
  if (Intern_It != TRUTH) return -1;
  /* Intern the primitive name by adding it to the vector of
     undefined primitives */
  if ((Max % CHUNK_SIZE) == 0)
  { Primitive_GC_If_Needed(Max+CHUNK_SIZE+2);
    if (Max > 0) Next = Nth_Vector_Loc(Undefined_Externals, 2);
    Undefined_Externals = Make_Pointer(TC_VECTOR, Free);
    *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, Max+CHUNK_SIZE+1);
    *Free++ = FIXNUM_0 + Max + 1;
    for (i=0; i < Max; i++) *Free++ = Fetch(*Next++);
    *Free++ = Symbol;
    for (i=1; i < CHUNK_SIZE; i++) *Free++ = NIL;
  }
  else
  { User_Vector_Set(Undefined_Externals, Max+1, Symbol);
    User_Vector_Set(Undefined_Externals, 0, FIXNUM_0+Max+1);
  }
  return MAX_EXTERNAL_PRIMITIVE+Max+1;
}

/* (GET-EXTERNAL-NUMBER name intern?)
   [Primitive number 0x103]
   Given a symbol (name), return the external primitive object
   corresponding to this name.  
   If intern? is true, then an external object is created if one
   didn't exist before.
   If intern? is false, NIL is returned if the primitive is not
   implemented even if the name alredy exists.
   Otherwise, NIL is returned if the primitive does not exist and
   the name does not exist either.
*/

Built_In_Primitive(Prim_Get_Ext_Number, 2, "GET-EXTERNAL-NUMBER")
{ long Answer;
  Primitive_2_Args();

  Arg_1_Type(TC_INTERNED_SYMBOL);
  Touch_In_Primitive(Arg2, Arg2);
  Answer = Get_Ext_Number(Arg1, Arg2);
  return ((Answer == -1) ?
	  NIL : Make_Non_Pointer(TC_PRIMITIVE_EXTERNAL, Answer));
}

/* Called from FASDUMP and BAND_DUMP to create a vector with
   symbols for each of the external primitives known to the system.
*/

Pointer Make_Prim_Exts()
{ Pointer Result = Make_Pointer(TC_VECTOR, Free), *Orig_Free=Free;
  long i, Max=NUndefined(), Count;

  Count = MAX_EXTERNAL_PRIMITIVE + Max + 1;
  Primitive_GC_If_Needed(Count+1);
  Free += Count+1;
  *Orig_Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, Count);
  for (i=0; i <= MAX_EXTERNAL_PRIMITIVE; i++)
    *Orig_Free++ = Get_Name_Of_Impl_External(i);
  for (i=1; i <= Max; i++)
    *Orig_Free++ = User_Vector_Ref(Undefined_Externals, i);
  return Result;
}
