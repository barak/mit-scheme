/*          Hey EMACS, this is -*- C -*- code!                 */

/****************************************************************
*                                                               *
*                         Copyright (c) 1984                    *
*               Massachusetts Institute of Technology           *
*                                                               *
* This material was developed by the Scheme project at the      *
* Massachusetts Institute of Technology, Department of          *
* Electrical Engineering and Computer Science.  Permission to   *
* copy this software, to redistribute it, and to use it for any *
* purpose is granted, subject to the following restrictions and *
* understandings.                                               *
*                                                               *
* 1. Any copy made of this software must include this copyright *
* notice in full.                                               *
*                                                               *
* 2. Users of this software agree to make their best efforts (a)*
* to return to the MIT Scheme project any improvements or       *
* extensions that they make, so that these may be included in   *
* future releases; and (b) to inform MIT of noteworthy uses of  *
* this software.                                                *
*                                                               *
* 3.  All materials developed as a consequence of the use of    *
* this software shall duly acknowledge such use, in accordance  *
* with the usual standards of acknowledging credit in academic  *
* research.                                                     *
*                                                               *
* 4. MIT has made no warrantee or representation that the       *
* operation of this software will be error-free, and MIT is     *
* under no obligation to provide any services, by way of        *
* maintenance, update, or otherwise.                            *
*                                                               *
* 5.  In conjunction with products arising from the use of this *
* material, there shall be no use of the name of the            *
* Massachusetts Institute of Technology nor of any adaptation   *
* thereof in any advertising, promotional, or sales literature  *
* without prior written consent from MIT in each case.          *
*                                                               *
****************************************************************/

/* File: LIST.C
 *
 * List creation and manipulation primitives.
 */

#include "scheme.h"
#include "primitive.h"

/* (CONS LEFT RIGHT)
      [Primitive number 0x20]
      Creates a pair with left component LEFT and right component
      RIGHT.
*/
Built_In_Primitive(Prim_Cons, 2, "CONS")
{ Primitive_2_Args();
  Primitive_GC_If_Needed(2);
  *Free++ = Arg1;
  *Free++ = Arg2;
  return Make_Pointer(TC_LIST, Free-2);
}

/* (CDR PAIR)
      [Primitive number 0x22]
      Returns the second element in the pair.  By convention, (CAR
      NIL) is NIL.
*/
Built_In_Primitive(Prim_Cdr, 1, "CDR")
{ Primitive_1_Arg();
 if (Arg1 == NIL) return NIL;
 Arg_1_Type(TC_LIST);
 return Vector_Ref(Arg1, CONS_CDR);
}
      
/* (CAR PAIR)
      [Primitive number 0x21]
      Returns the first element in the pair.  By convention, (CAR NIL)
      is NIL.
*/
Built_In_Primitive(Prim_Car, 1, "CAR")
{ Primitive_1_Arg();
  if (Arg1 == NIL) return NIL;
  Arg_1_Type(TC_LIST);
  return Vector_Ref(Arg1, CONS_CAR);
}

/* (GENERAL_CAR_CDR LIST DIRECTIONS)
      [Primitive number 0x27]
      DIRECTIONS encodes a string of CAR and CDR operations to be
      performed on LIST as follows:
        1   = NOP	101 = CDAR
        10  = CDR	110 = CADR
        11  = CAR	111 = CAAR
        100 = CDDR	...
*/
Built_In_Primitive(Prim_General_Car_Cdr, 2, "GENERAL-CAR-CDR")
{ fast long CAR_CDR_Pattern;
  Primitive_2_Args();
  Arg_2_Type(TC_FIXNUM);
  CAR_CDR_Pattern = Get_Integer(Arg2);
  while (CAR_CDR_Pattern > 1)
  { Touch_In_Primitive(Arg1, Arg1);
    if (Arg1 == NIL) return NIL;
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
Built_In_Primitive(Prim_Assq, 2, "ASSQ")
{ Pointer This_Assoc_Pair, Key;
  Primitive_2_Args();

  Touch_In_Primitive(Arg1, Arg1);
  Touch_In_Primitive(Arg2, Arg2);
  while (Type_Code(Arg2) == TC_LIST)
  { Touch_In_Primitive(Vector_Ref(Arg2, CONS_CAR), This_Assoc_Pair);
    if (Type_Code(This_Assoc_Pair) != TC_LIST)
      Primitive_Error(ERR_ARG_2_WRONG_TYPE);
    Touch_In_Primitive(Vector_Ref(This_Assoc_Pair, CONS_CAR), Key);
    if (Key == Arg1) return This_Assoc_Pair;
    Touch_In_Primitive(Vector_Ref(Arg2, CONS_CDR), Arg2);
  }
  if (Arg2 != NIL) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  return NIL;
}

/* (LENGTH LIST)
      [Primitive number 0x5D]
      Returns the number of items in the list.  By convention, (LENGTH
      NIL) is 0.  LENGTH will loop forever if given a circular
      structure.
*/
Built_In_Primitive(Prim_Length, 1, "LENGTH")
{ fast long i;
  Primitive_1_Arg();
  i = 0;
  Touch_In_Primitive(Arg1, Arg1);
  while (Type_Code(Arg1) == TC_LIST)
  { i += 1;
    Touch_In_Primitive(Vector_Ref(Arg1, CONS_CDR), Arg1);
  }
  if (Arg1 != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  return FIXNUM_0+i;
}

/* (MEMQ ITEM LIST)
      [Primitive number 0x1C]
      Searches LIST for ITEM, using EQ? as a test.  Returns NIL if it
      is not found, or the [first] tail of LIST whose CAR is ITEM.
*/
Built_In_Primitive(Prim_Memq, 2, "MEMQ")
{ fast Pointer Key;
  Primitive_2_Args();
  Touch_In_Primitive(Arg1, Arg1);
  Touch_In_Primitive(Arg2, Arg2);
  while (Type_Code(Arg2) == TC_LIST)
  { Touch_In_Primitive(Vector_Ref(Arg2, CONS_CAR), Key);
    if (Arg1 == Key) return Arg2;
    else Touch_In_Primitive(Vector_Ref(Arg2, CONS_CDR), Arg2);
  }
  if (Arg2 != NIL) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  return NIL;
}   

/* (SET_CAR PAIR VALUE)
      [Primitive number 0x23]
      Stores VALUE in the CAR of PAIR.  Returns (bad style to count on
      this) the previous CAR of PAIR.
*/
Built_In_Primitive(Prim_Set_Car, 2, "SET-CAR!")
{ Primitive_2_Args();
  Arg_1_Type(TC_LIST);
  Side_Effect_Impurify(Arg1, Arg2);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, CONS_CAR), Arg2);
}

/* (SET_CDR PAIR VALUE)
      [Primitive number 0x24]
      Stores VALUE in the CDR of PAIR.  Returns (bad style to count on
      this) the previous CDR of PAIR.
*/
Built_In_Primitive(Prim_Set_Cdr, 2, "SET-CDR!")
{ Primitive_2_Args();
  Arg_1_Type(TC_LIST);
  Side_Effect_Impurify(Arg1, Arg2);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, CONS_CDR), Arg2);
}

/* (PAIR OBJECT)
      [Primitive number 0x7E]
      Returns #!TRUE if OBJECT has the type-code LIST (ie if it was
      created by CONS). Return NIL otherwise.
*/
Built_In_Primitive(Prim_Pair, 1, "PAIR?")
{ Primitive_1_Arg();
  Touch_In_Primitive(Arg1, Arg1);
  if (Type_Code(Arg1) == TC_LIST) return TRUTH;
  else return NIL;
}

/* (SYS_PAIR OBJECT)
      [Primitive number 0x85]
      Returns #!TRUE if the garbage collector type of OBJECT is PAIR.
*/
Built_In_Primitive(Prim_Sys_Pair, 1, "SYSTEM-PAIR?")
{ Primitive_1_Arg();
  Touch_In_Primitive(Arg1, Arg1);
  if (GC_Type_List(Arg1)) return TRUTH;
  else return NIL;
}

/* (SYS_PAIR_CAR GC-PAIR)
      [Primitive number 0x86]
      Same as CAR, but for anything of GC type PAIR.
*/
Built_In_Primitive(Prim_Sys_Pair_Car, 1, "SYSTEM-PAIR-CAR")
{ Primitive_1_Arg();
  Arg_1_GC_Type(GC_Pair);
  return Vector_Ref(Arg1, CONS_CAR);
}

/* (SYS_PAIR_CDR GC-PAIR)
      [Primitive number 0x87]
      Same as CDR, but for anything of GC type PAIR.
*/
Built_In_Primitive(Prim_Sys_Pair_Cdr, 1, "SYSTEM-PAIR-CDR")
{ Primitive_1_Arg();
  Arg_1_GC_Type(GC_Pair);
  return Vector_Ref(Arg1, CONS_CDR);
}

/* (SYS_PAIR_CONS TYPE-CODE OBJECT-1 OBJECT-2)
      [Primitive number 0x84]
      Like CONS, but returns an object with the specified type code
      (not limited to type code LIST).
*/
Built_In_Primitive(Prim_Sys_Pair_Cons, 3, "SYSTEM-PAIR-CONS")
{ long Type;
  Primitive_3_Args();
  Arg_1_Type(TC_FIXNUM);
  Range_Check(Type, Arg1, 0, MAX_SAFE_TYPE,
              ERR_ARG_1_BAD_RANGE);
  if (GC_Type_Code(Type) == GC_Pair)
  { Primitive_GC_If_Needed(2);
    *Free++ = Arg2;
    *Free++ = Arg3;
    return Make_Pointer(Type, Free-2);
  }
  else Primitive_Error(ERR_ARG_1_BAD_RANGE); /*NOTREACHED*/
}


/* (SYS_SET_CAR GC-PAIR NEW_CAR)
      [Primitive number 0x88]
      Same as SET_CAR, but for anything of GC type PAIR.
*/
Built_In_Primitive(Prim_Sys_Set_Car, 2, "SYSTEM-PAIR-SET-CAR!")
{ Primitive_2_Args();
  Arg_1_GC_Type(GC_Pair);
  Side_Effect_Impurify(Arg1, Arg2);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, CONS_CAR), Arg2);
}

/* (SYS_SET_CDR GC-PAIR NEW_CDR)
      [Primitive number 0x89]
      Same as SET_CDR, but for anything of GC type PAIR.
*/
Built_In_Primitive(Prim_Sys_Set_Cdr, 2, "SYSTEM-PAIR-SET-CDR!")
{ Primitive_2_Args();
  Arg_1_GC_Type(GC_Pair);
  Side_Effect_Impurify(Arg1, Arg2);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, CONS_CDR), Arg2);
}

