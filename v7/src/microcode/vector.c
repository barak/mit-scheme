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

/* File: VECTOR.C
 *
 * This file contains procedures for handling vectors and conversion
 * back and forth to lists.
 */

#include "scheme.h"
#include "primitive.h"

                       /*********************/
                       /* VECTORS <-> LISTS */
                       /*********************/

/* Subvector_To_List is a utility routine used by both
   SUBVECTOR_TO_LIST and SYS_SUBVECTOR_TO_LIST.  It copies the entries
   in a vector (first argument) starting with the entry specified by
   argument 2 and ending at the one specified by argument 3.  The copy
   includes the starting entry but does NOT include the ending entry.
   Thus the entire vector is converted to a list by setting argument 2
   to 0 and argument 3 to the length of the vector.
*/

Pointer Subvector_To_List()
{ Pointer *From, Result;
  long Length, Start, End, Count, i;
  Primitive_3_Args();
  if (Type_Code(Arg2) != TC_FIXNUM) Primitive_Error(ERR_ARG_2_WRONG_TYPE); 
  if (Type_Code(Arg3) != TC_FIXNUM) Primitive_Error(ERR_ARG_3_WRONG_TYPE); 
  if (Type_Code(Vector_Ref(Arg1, VECTOR_TYPE)) != TC_MANIFEST_VECTOR)
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Length = Vector_Length(Arg1);
  Start = Get_Integer(Arg2);
  End = Get_Integer(Arg3);
  if (End > Length) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Start > End) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Start == End) return NIL;
  Primitive_GC_If_Needed(2*(End-Start));
  Result = Make_Pointer(TC_LIST, Free);
  From = Nth_Vector_Loc(Arg1, Start+1);
  Count = End-Start;
  for (i=0; i < Count; i++)
  { *Free++ = Fetch(*From++);
    *Free = Make_Pointer(TC_LIST, Free+1);
     Free += 1;
  }
  Free[-1] = NIL;
  return Result;
}

/* Called by the primitives LIST_TO_VECTOR and SYS_LIST_TO_VECTOR.
   This utility routine converts a list into a vector.
*/

Pointer L_To_V(Result_Type, List)
long Result_Type;
fast Pointer List;
{ Pointer *Orig_Free;
  long Count;
  Touch_In_Primitive(List, List);
  Count = 0;
  Orig_Free = Free++;
  while (Type_Code(List) == TC_LIST)
  { Primitive_GC_If_Needed(0);
    Count += 1;
    *Free++ = Vector_Ref(List, CONS_CAR);
    Touch_In_Primitive(Vector_Ref(List, CONS_CDR), List);
  }
  if (List != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  *Orig_Free = Make_Non_Pointer(TC_MANIFEST_VECTOR, Count);
  return Make_Pointer(Result_Type, Orig_Free);
}

/* (LIST_TO_VECTOR LIST)
      [Primitive number 0x7C]
      Returns a vector made from the items in LIST.
*/

Built_In_Primitive(Prim_List_To_Vector, 1, "LIST->VECTOR")
{ Primitive_1_Arg();
  return L_To_V(TC_VECTOR, Arg1);
}

/* (SUBVECTOR_TO_LIST VECTOR FROM TO)
      [Primitive number 0x7D]
      Returns a list of the FROMth through TO-1st items in the vector.
      Thus (SUBVECTOR_TO_LIST V 0 (VECTOR_LENGTH V)) returns a list of
      all the items in V.
*/
Built_In_Primitive(Prim_Subvector_To_List, 3, "SUBVECTOR->LIST")
{ Primitive_3_Args();
  Arg_1_Type(TC_VECTOR);
  /* The work is done by Subvector_To_List, in PRIMSUBR.C */
  return Subvector_To_List();
}

/* (VECTOR_CONS LENGTH CONTENTS)
      [Primitive number 0x2C]
      Create a new vector to hold LENGTH entries, all of which are
      initialized to CONTENTS.
*/
Built_In_Primitive(Prim_Vector_Cons, 2, "VECTOR-CONS")
{ long Length, i;
  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Length = Get_Integer(Arg1);
  Primitive_GC_If_Needed(Length+1);
  *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, Length);
  for (i=0; i < Length; i++) *Free++ = Arg2;
  return Make_Pointer(TC_VECTOR, Free-(Length+1));
}

/* (VECTOR_REF VECTOR OFFSET)
      [Primitive number 0x2E]
      Return the OFFSETth entry in VECTOR.  Entries are numbered from
      0.
*/
Built_In_Primitive(Prim_Vector_Ref, 2, "VECTOR-REF")
{ long Offset;
  Primitive_2_Args();
  Arg_1_Type(TC_VECTOR);
  Arg_2_Type(TC_FIXNUM);
  Range_Check(Offset, Arg2,
              0, Vector_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
  return User_Vector_Ref(Arg1, Offset);
}

/* (VECTOR_SET VECTOR OFFSET VALUE)
      [Primitive number 0x30]
      Store VALUE as the OFFSETth entry in VECTOR.  Entries are
      numbered from 0.  Returns (bad style to rely on this) the
      previous value of the entry.
*/
Built_In_Primitive(Prim_Vector_Set, 3, "VECTOR-SET!")
{ long Offset;
  Primitive_3_Args();

  Arg_1_Type(TC_VECTOR);
  Arg_2_Type(TC_FIXNUM);
  Range_Check(Offset, Arg2,
              0, Vector_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
  Side_Effect_Impurify(Arg1, Arg3);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, Offset+1), Arg3);
}

/* (VECTOR_SIZE VECTOR)
      [Primitive number 0x2D]
      Returns the number of entries in VECTOR.
*/
Built_In_Primitive(Prim_Vector_Size, 1, "VECTOR-SIZE")
{ Primitive_1_Arg();
  Arg_1_Type(TC_VECTOR);
  return FIXNUM_0+Vector_Length(Arg1);
}

/* (SYS_LIST_TO_VECTOR GC-LIST)
      [Primitive number 0x97]
      Same as LIST_TO_VECTOR except that the resulting vector has the
      specified type code.  This can be used, for example, to create
      an environment from a list of values.
*/
Built_In_Primitive(Prim_Sys_List_To_Vector, 2, "SYSTEM-LIST->VECTOR")
{ long Type;
  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Range_Check(Type, Arg1, 0, MAX_TYPE_CODE, ERR_ARG_1_BAD_RANGE);
  if (GC_Type_Code(Type) == GC_Vector) return L_To_V(Type, Arg2);
  else Primitive_Error(ERR_ARG_1_BAD_RANGE); /*NOTREACHED*/
}

/* (SYS_SUBVECTOR_TO_LIST GC-VECTOR FROM TO)
      [Primitive number 0x98]
      Same as SUBVECTOR_TO_LIST, but accepts anything with a GC type
      of VECTOR.  Most useful for accessing values from environments.
*/
Built_In_Primitive(Prim_Sys_Subvector_To_List, 3,
		 "SYSTEM-SUBVECTOR->LIST")
{ Primitive_3_Args();
  Touch_In_Primitive(Arg1, Arg1);
  Arg_1_GC_Type(GC_Vector);
  /* The work is done by Subvector_To_List, in PRIMSUBR.C */
  return Subvector_To_List();
}

/* (SYS_VECTOR OBJECT)
      [Primitive number 0x99]
      Returns #!TRUE if OBJECT is of GC type VECTOR.  Otherwise
      returns NIL.
*/
Built_In_Primitive(Prim_Sys_Vector, 1, "SYSTEM-VECTOR")
{ Primitive_1_Arg();
  Touch_In_Primitive(Arg1, Arg1);
  if (GC_Type_Vector(Arg1)) return TRUTH; else return NIL;
}

/* (SYS_VECTOR_REF GC-VECTOR OFFSET)
      [Primitive number 0x9A]
      Like VECTOR_REF, but for anything of GC type VECTOR (eg.
      environments)
*/
Built_In_Primitive(Prim_Sys_Vector_Ref, 2, "SYSTEM-VECTOR-REF")
{ long Offset;
  Primitive_2_Args();
  Touch_In_Primitive(Arg1, Arg1);
  Arg_1_GC_Type(GC_Vector);
  Range_Check(Offset, Arg2, 0,
	      Vector_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
  return User_Vector_Ref(Arg1, Offset);
}

/* (SYS_VECTOR_SET GC-VECTOR OFFSET VALUE)
      [Primitive number 0x9B]
      Like VECTOR_SET, but for anything of GC type VECTOR (eg.
      environments)
*/
Built_In_Primitive(Prim_Sys_Vec_Set, 3, "SYSTEM-VECTOR-SET!")
{ long Offset;
  Primitive_3_Args();
  Touch_In_Primitive(Arg1, Arg1);
  Arg_1_GC_Type(GC_Vector);
  Range_Check(Offset, Arg2, 0,
	      Vector_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
  Side_Effect_Impurify(Arg1, Arg3);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, Offset+1), Arg3);
}

/* (SYS_VECTOR_SIZE GC-VECTOR)
      [Primitive number 0xAE]
      Like VECTOR_SIZE, but for anything of GC type VECTOR (eg.
      environments)
*/
Built_In_Primitive(Prim_Sys_Vec_Size, 1, "SYSTEM-VECTOR-SIZE")
{ Primitive_1_Arg();
  Touch_In_Primitive(Arg1, Arg1);
  Arg_1_GC_Type(GC_Vector);
  return FIXNUM_0+Vector_Length(Arg1);
}

