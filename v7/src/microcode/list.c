/* -*-C-*-

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/list.c,v 9.26 1988/08/15 20:50:44 cph Rel $
 *
 * List creation and manipulation primitives.
 */

#include "scheme.h"
#include "prims.h"

/* (CONS LEFT RIGHT)
   Creates a pair with left component LEFT and right component
   RIGHT.
*/
DEFINE_PRIMITIVE ("CONS", Prim_cons, 2, 2, 0)
{
  Primitive_2_Args();

  Primitive_GC_If_Needed(2);
  *Free++ = Arg1;
  *Free++ = Arg2;
  return Make_Pointer(TC_LIST, Free-2);
}

/* (CDR PAIR)
   Returns the second element in the pair.
*/
DEFINE_PRIMITIVE ("CDR", Prim_cdr, 1, 1, 0)
{
  Primitive_1_Arg();

  Arg_1_Type(TC_LIST);
  return Vector_Ref(Arg1, CONS_CDR);
}
      
/* (CAR PAIR)
   Returns the first element in the pair.
*/
DEFINE_PRIMITIVE ("CAR", Prim_car, 1, 1, 0)
{
  Primitive_1_Arg();

  Arg_1_Type(TC_LIST);
  return Vector_Ref(Arg1, CONS_CAR);
}

/* (GENERAL-CAR-CDR LIST DIRECTIONS)
   DIRECTIONS encodes a string of CAR and CDR operations to be
   performed on LIST as follows:
     1   = NOP	101 = CDAR
     10  = CDR	110 = CADR
     11  = CAR	111 = CAAR
     100 = CDDR	...
*/
DEFINE_PRIMITIVE ("GENERAL-CAR-CDR", Prim_general_car_cdr, 2, 2, 0)
{
  fast long CAR_CDR_Pattern;
  Primitive_2_Args();

  Arg_2_Type(TC_FIXNUM);
  CAR_CDR_Pattern = Get_Integer(Arg2);
  while (CAR_CDR_Pattern > 1)
  {
    Touch_In_Primitive(Arg1, Arg1);
    if (Type_Code(Arg1) != TC_LIST)
      Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    Arg1 = 
      Vector_Ref(Arg1,
                 ((CAR_CDR_Pattern & 1) == 0) ? CONS_CDR : CONS_CAR);
    CAR_CDR_Pattern >>= 1;
  }
  return Arg1;
}

/* (ASSQ ITEM A-LIST)
   Searches the association list A-LIST for ITEM, using EQ? for
   testing equality.  Returns NIL if ITEM is not found, or the tail
   of the list whose CAAR is ITEM.
*/
DEFINE_PRIMITIVE ("ASSQ", Prim_assq, 2, 2, 0)
{
  Pointer This_Assoc_Pair, Key;
  Primitive_2_Args();

  Touch_In_Primitive(Arg1, Arg1);
  Touch_In_Primitive(Arg2, Arg2);
  while (Type_Code(Arg2) == TC_LIST)
  {
    Touch_In_Primitive(Vector_Ref(Arg2, CONS_CAR), This_Assoc_Pair);
    if (Type_Code(This_Assoc_Pair) != TC_LIST)
      Primitive_Error(ERR_ARG_2_WRONG_TYPE);
    Touch_In_Primitive(Vector_Ref(This_Assoc_Pair, CONS_CAR), Key);
    if (Key == Arg1)
      return This_Assoc_Pair;
    Touch_In_Primitive(Vector_Ref(Arg2, CONS_CDR), Arg2);
  }
  if (Arg2 != NIL)
    Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  return NIL;
}

/* (LENGTH LIST)
   Returns the number of items in the list.
   LENGTH will loop forever if given a circular structure.
*/
DEFINE_PRIMITIVE ("LENGTH", Prim_length, 1, 1, 0)
{
  fast long i;
  Primitive_1_Arg();

  i = 0;
  Touch_In_Primitive(Arg1, Arg1);
  while (Type_Code(Arg1) == TC_LIST)
  {
    i += 1;
    Touch_In_Primitive(Vector_Ref(Arg1, CONS_CDR), Arg1);
  }
  if (Arg1 != NIL)
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  return Make_Unsigned_Fixnum(i);
}

/* (MEMQ ITEM LIST)
   Searches LIST for ITEM, using EQ? as a test.  Returns NIL if it
   is not found, or the sublist of LIST whose CAR is ITEM.
*/
DEFINE_PRIMITIVE ("MEMQ", Prim_memq, 2, 2, 0)
{
  fast Pointer Key;
  Primitive_2_Args();

  Touch_In_Primitive(Arg1, Arg1);
  Touch_In_Primitive(Arg2, Arg2);
  while (Type_Code(Arg2) == TC_LIST)
  {
    Touch_In_Primitive(Vector_Ref(Arg2, CONS_CAR), Key);
    if (Arg1 == Key)
      return Arg2;
    else
      Touch_In_Primitive(Vector_Ref(Arg2, CONS_CDR), Arg2);
  }
  if (Arg2 != NIL)
    Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  return NIL;
}   

/* (SET-CAR! PAIR VALUE)
   Stores VALUE in the CAR of PAIR.  Returns the previous CAR of PAIR.
*/
DEFINE_PRIMITIVE ("SET-CAR!", Prim_set_car, 2, 2, 0)
{
  Primitive_2_Args();

  Arg_1_Type(TC_LIST);
  Side_Effect_Impurify(Arg1, Arg2);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, CONS_CAR), Arg2);
}

/* (SET-CDR! PAIR VALUE)
   Stores VALUE in the CDR of PAIR.  Returns the previous CDR of PAIR.
*/
DEFINE_PRIMITIVE ("SET-CDR!", Prim_set_cdr, 2, 2, 0)
{
  Primitive_2_Args();

  Arg_1_Type(TC_LIST);
  Side_Effect_Impurify(Arg1, Arg2);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, CONS_CDR), Arg2);
}

/* (PAIR? OBJECT)
   Returns #!TRUE if OBJECT has the type-code LIST (ie if it was
   created by CONS).  Returns NIL otherwise.
*/
DEFINE_PRIMITIVE ("PAIR?", Prim_pair, 1, 1, 0)
{
  Primitive_1_Arg();

  Touch_In_Primitive(Arg1, Arg1);
  if (Type_Code(Arg1) == TC_LIST)
    return SHARP_T;
  else
    return NIL;
}

/* (SYSTEM-PAIR? OBJECT)
   Returns #!TRUE if the garbage collector type of OBJECT is PAIR.
*/
DEFINE_PRIMITIVE ("SYSTEM-PAIR?", Prim_sys_pair, 1, 1, 0)
{
  Primitive_1_Arg();

  Touch_In_Primitive(Arg1, Arg1);
  if (GC_Type_List(Arg1))
    return SHARP_T;
  else
    return NIL;
}

/* (SYSTEM-PAIR-CAR GC-PAIR)
   Same as CAR, but for anything of GC type PAIR.
*/
DEFINE_PRIMITIVE ("SYSTEM-PAIR-CAR", Prim_sys_pair_car, 1, 1, 0)
{
  Primitive_1_Arg();

  Arg_1_GC_Type(GC_Pair);
  return Vector_Ref(Arg1, CONS_CAR);
}

/* (SYSTEM-PAIR-CDR GC-PAIR)
   Same as CDR, but for anything of GC type PAIR.
*/
DEFINE_PRIMITIVE ("SYSTEM-PAIR-CDR", Prim_sys_pair_cdr, 1, 1, 0)
{
  Primitive_1_Arg();

  Arg_1_GC_Type(GC_Pair);
  return Vector_Ref(Arg1, CONS_CDR);
}

/* (SYSTEM-PAIR-CONS TYPE-CODE OBJECT-1 OBJECT-2)
   Like CONS, but returns an object with the specified type code
   (not limited to type code LIST).
*/
DEFINE_PRIMITIVE ("SYSTEM-PAIR-CONS", Prim_sys_pair_cons, 3, 3, 0)
{
  long Type;
  Primitive_3_Args();

  Arg_1_Type(TC_FIXNUM);
  Range_Check(Type, Arg1, 0, MAX_TYPE_CODE,
              ERR_ARG_1_BAD_RANGE);
  if (GC_Type_Code(Type) == GC_Pair)
  {
    Primitive_GC_If_Needed(2);
    *Free++ = Arg2;
    *Free++ = Arg3;
    return Make_Pointer(Type, Free-2);
  }
  else
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  /*NOTREACHED*/
}


/* (SYSTEM-PAIR-SET-CAR! GC-PAIR NEW_CAR)
   Same as SET-CAR!, but for anything of GC type PAIR.
*/
DEFINE_PRIMITIVE ("SYSTEM-PAIR-SET-CAR!", Prim_sys_set_car, 2, 2, 0)
{
  Primitive_2_Args();

  Arg_1_GC_Type(GC_Pair);
  Side_Effect_Impurify(Arg1, Arg2);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, CONS_CAR), Arg2);
}

/* (SYSTEM-PAIR-SET-CDR! GC-PAIR NEW_CDR)
   Same as SET-CDR!, but for anything of GC type PAIR.
*/
DEFINE_PRIMITIVE ("SYSTEM-PAIR-SET-CDR!", Prim_sys_set_cdr, 2, 2, 0)
{
  Primitive_2_Args();

  Arg_1_GC_Type(GC_Pair);
  Side_Effect_Impurify(Arg1, Arg2);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, CONS_CDR), Arg2);
}

