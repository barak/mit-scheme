/* -*-C-*-

Copyright (c) 1986 Massachusetts Institute of Technology

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

/* File: prim.c
 *
 * The leftovers ... primitives that don't seem to belong elsewhere
 *
 */

#include "scheme.h"
#include "primitive.h"
#include "prims.h"

/* Random predicates: */

/* (NULL OBJECT)
      [Primitive number 0x0C]
      Returns #!TRUE if OBJECT is NIL.  Otherwise returns NIL.  This is
      the primitive known as NOT, NIL?, and NULL? in Scheme.
*/
Built_In_Primitive(Prim_Null, 1, "NULL?")
{ Primitive_1_Arg();
  Touch_In_Primitive(Arg1, Arg1);
  return (Arg1 == NIL) ? TRUTH : NIL;
}

/* (EQ? OBJECT-1 OBJECT-2)
      [Primitive number 0x0D]
      Returns #!TRUE if the two objects have the same type code,
      address portion, and danger bit.  Returns NIL otherwise.
*/
Built_In_Primitive(Prim_Eq, 2, "EQ?")
{ Primitive_2_Args();
  if (Arg1 == Arg2) return TRUTH;
  Touch_In_Primitive(Arg1, Arg1);
  Touch_In_Primitive(Arg2, Arg2);
  return (Arg1 == Arg2) ? TRUTH : NIL;
}

/* Pointer manipulation */

/* (MAKE_NON_POINTER NUMBER)
      [Primitive number 0xB1]
      Returns an (extended) fixnum with the same value as NUMBER.  In
      the CScheme interpreter this is basically a no-op, since fixnums
      already store 24 bits.
*/
Built_In_Primitive(Prim_Make_Non_Pointer, 1, "MAKE-NON-POINTER")
{ Primitive_1_Arg();
  Arg_1_Type(TC_FIXNUM);
  return Arg1;
}

/* (PRIMITIVE-TYPE OBJECT)
      [Primitive number 0x10]
      Returns the type code of OBJECT as a number.  This includes the
      danger bit, if it is set.  THE OBJECT IS TOUCHED FIRST.
*/
Built_In_Primitive(Prim_Prim_Type, 1, "PRIMITIVE-TYPE")
{ Primitive_1_Arg();
  Touch_In_Primitive(Arg1, Arg1);
  return FIXNUM_0+Type_Code(Arg1);
}

/* (PRIMITIVE_DATUM OBJECT)
      [Primitive number 0xB0]
      Returns the address part of OBJECT.
*/
Built_In_Primitive(Prim_Primitive_Datum, 1, "PRIMITIVE-DATUM")
{ Primitive_1_Arg();
  return Make_New_Pointer(TC_ADDRESS, Arg1);
}

/* (PRIMITIVE-TYPE? TYPE-CODE OBJECT)
      [Primitive number 0x0F]
      Return #!TRUE if the type code of OBJECT is TYPE-CODE, NIL
      otherwise.  The check includes the danger bit of OBJECT.
      THE OBJECT IS TOUCHED FIRST.
*/
Built_In_Primitive(Prim_Prim_Type_QM, 2, "PRIMITIVE-TYPE?")
{ Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Touch_In_Primitive(Arg2, Arg2);
  if (Type_Code(Arg2) == Get_Integer(Arg1)) return TRUTH;
  else return NIL;
}

/* (PRIMITIVE-SET-TYPE TYPE-CODE OBJECT)
      [Primitive number 0x11]

      Returns a new object with TYPE-CODE and the address part of
      OBJECT.  TOUCHES ITS SECOND ARGUMENT (for completeness sake).
      This is a "gc-safe" (paranoid) operation.
*/

Built_In_Primitive(Prim_Primitive_Set_Type, 2, "PRIMITIVE-SET-TYPE")
{ long New_GC_Type, New_Type;
  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Range_Check(New_Type, Arg1, 0, MAX_SAFE_TYPE, ERR_ARG_1_BAD_RANGE);
  Touch_In_Primitive(Arg2, Arg2);
  New_GC_Type = GC_Type_Code(New_Type);
  if ((GC_Type(Arg2) == New_GC_Type) ||
      (New_GC_Type == GC_Non_Pointer))
    return Make_New_Pointer(New_Type, Arg2);
  else Primitive_Error(ERR_ARG_1_BAD_RANGE); /*NOTREACHED*/
}

/* (&MAKE-OBJECT TYPE-CODE OBJECT)
      [Primitive number 0x8D]

      Makes a Scheme object whose datum field is the datum field of
      OBJECT, and whose type code is TYPE-CODE.  It does not touch,
      and is not "gc-safe": You can screw yourself royally by using
      this.
*/

Built_In_Primitive(Prim_And_Make_Object, 2, "&MAKE-OBJECT")
{ long New_Type;
  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Range_Check(New_Type, Arg1, 0, MAX_SAFE_TYPE, ERR_ARG_1_BAD_RANGE);
  return Make_New_Pointer(New_Type, Arg2);
}

/* Playing with the danger bit */

/* (DANGEROUS? OBJECT)
      [Primitive number 0x49]
      Returns #!TRUE if OBJECT has the danger bit set, NIL otherwise.
*/
Built_In_Primitive(Prim_Dangerous_QM, 1, "DANGEROUS?")
{ Primitive_1_Arg();
  return (Dangerous(Arg1)) ? TRUTH : NIL;
}

/* (DANGERIZE OBJECT)
      [Primitive number 0x48]
      Returns OBJECT, but with the danger bit set.
*/
Built_In_Primitive(Prim_Dangerize, 1, "DANGERIZE")
{ Primitive_1_Arg();
  return Set_Danger_Bit(Arg1);
}

/* (UNDANGERIZE OBJECT)
      [Primitive number 0x47]
      Returns OBJECT with the danger bit cleared.  This does not
      side-effect the object, it merely returns a new (non-dangerous)
      pointer to the same item.
*/
Built_In_Primitive(Prim_Undangerize, 1, "UNDANGERIZE")
{ Primitive_1_Arg();
  return Clear_Danger_Bit(Arg1);
}

/* Mapping between the internal and external representations of
   primitives, return addresses, external primitives, etc.
 */

/* (MAP_CODE_TO_ADDRESS TYPE-CODE VALUE-CODE)
      [Primitive number 0x93]
      For return codes and primitives, this returns the internal
      representation of the return address or primitive address given
      the external representation.  Currently in CScheme these two are
      the same.  In the 68000 assembly version the internal
      representation is an actual address in memory.
*/
Built_In_Primitive(Prim_Map_Code_To_Address, 2, "MAP-CODE-TO-ADDRESS")
{ long Code, Offset;
  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  Code = Get_Integer(Arg1);
  Offset = Get_Integer(Arg2);
  switch (Code)
  { case TC_RETURN_CODE:
      if (Offset > MAX_RETURN_CODE) Primitive_Error(ERR_ARG_2_BAD_RANGE);
      break;

    case TC_PRIMITIVE:
      if (Offset > MAX_PRIMITIVE) Primitive_Error(ERR_ARG_2_BAD_RANGE);
      break;

    default: Primitive_Error(ERR_ARG_1_BAD_RANGE);
  }
  return Make_Non_Pointer(Code, Offset);
}

/* (MAP_ADDRESS_TO_CODE TYPE-CODE ADDRESS)
      [Primitive number 0x90]
      This is the inverse operation for MAP_CODE_TO_ADDRESS.
      Given a machine ADDRESS and a TYPE-CODE (either return code or
      primitive) it finds the number for the external representation
      for the internal address.
*/
Built_In_Primitive(Prim_Map_Address_To_Code, 2, "MAP-ADDRESS-TO-CODE")
{ long Code, Offset;
  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Code = Get_Integer(Arg1);
  Arg_2_Type(Code);
  Offset = Get_Integer(Arg2);
  switch (Code)
  { case TC_RETURN_CODE:
      if (Offset > MAX_RETURN_CODE)
        Primitive_Error(ERR_ARG_2_BAD_RANGE);
      break;

    case TC_PRIMITIVE:
      if (Offset > MAX_PRIMITIVE)
        Primitive_Error(ERR_ARG_2_BAD_RANGE);
      break;

    default: 
      Primitive_Error(ERR_ARG_1_BAD_RANGE);
  }
  return FIXNUM_0+Offset;
}

/* (MAP_PRIM_ADDRESS_TO_ARITY INTERNAL-PRIMITIVE)
      [Primitive number 0x96]
      Given the internal representation of a primitive (in CScheme the
      internal and external representations are the same), return the
      number of arguments it requires.
*/
Built_In_Primitive(Prim_Map_Prim_Address_To_Arity, 1,
		 "PRIMITIVE-PROCEDURE-ARITY")
{ long Prim_Num;
  Primitive_1_Arg();
  if (Type_Code(Arg1) != TC_PRIMITIVE_EXTERNAL)
  { Arg_1_Type(TC_PRIMITIVE);
    Range_Check(Prim_Num, Arg1, 0, MAX_PRIMITIVE, ERR_ARG_1_BAD_RANGE);
    return FIXNUM_0 + (int) Arg_Count_Table[Prim_Num];
  }
  /* External primitives here */
  Prim_Num = Get_Integer(Arg1);
  if (Prim_Num <= MAX_EXTERNAL_PRIMITIVE)
    return FIXNUM_0 + Ext_Prim_Desc[Prim_Num].arity;
  if (Undefined_Externals==NIL) Primitive_Error(ERR_ARG_1_BAD_RANGE);
  if (Prim_Num > (MAX_EXTERNAL_PRIMITIVE+
                  Get_Integer(User_Vector_Ref(Undefined_Externals, 0))))
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  return NIL;
}

/* Playing with non marked vectors. */

/* (NON_MARKED_VECTOR_CONS LENGTH)
      [Primitive number 0x31]
      Creates a non-marked vector of the specified LENGTH.  The
      contents of such a vector are not seen by the garbage collector.
      There are no ordinary operations which can be performed on
      non-marked vectors, but the SYS_VECTOR operations can be used
      with care. [This primitive appears to be a relic of days gone
      by.]
*/
Built_In_Primitive(Prim_Non_Marked_Vector_Cons, 1, "NON-MARKED-VECTOR-CONS")
{ long Length;
  Primitive_1_Arg();
  Arg_1_Type(TC_FIXNUM);
  Length = Get_Integer(Arg1);
  Primitive_GC_If_Needed(Length+1);
  *Free = Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, Length);
  Free += Length+1;
  return Make_Pointer(TC_NON_MARKED_VECTOR, Free-(Length+1));
}

/* (INSERT_NON_MARKED_VECTOR TO-GC-VECTOR N FROM-GC-VECTOR)
      [Primitive number 0x19]
      This primitive performs a side-effect on the TO-GC-VECTOR.  Both
      TO- and FROM-GC-VECTOR must be of the garbage collector type
      vector (i.e. vectors, strings, non-marked vectors, bignums,
      etc.).  The FROM-GC-VECTOR is inserted in the middle of
      TO-GC-VECTOR, preceded by a non-marked vector header.  The
      insertion begins at the Nth position of the vector with the
      non-marked header.  Notice that this is really an "overwrite"
      rather than an insertion, since the length of the TO-GC-VECTOR
      does not change and the data which was formerly in the part of
      the vector now occupied by FROM-GC-VECTOR and its header has
      been lost.  This primitive was added for the use of certain
      parts of the compiler and runtime system which need to make
      objects that have an internal part which is "hidden" from the
      garbage collector. The value returned is TO-GC-VECTOR.
*/
Built_In_Primitive(Prim_Insert_Non_Marked_Vector, 3,
		 "INSERT-NON-MARKED-VECTOR!")
{ Pointer *To,*From;
  long Index,NM_Length,Length,i;
  Primitive_3_Args();
  Arg_1_GC_Type(GC_Vector);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_GC_Type(GC_Vector);
  Length = Vector_Length(Arg1);
  NM_Length = Vector_Length(Arg3);
  Range_Check(Index, Arg2, 0, Length-1, ERR_ARG_2_BAD_RANGE);
  if (Length-Index <= NM_Length)
    Primitive_Error(ERR_ARG_3_BAD_RANGE);
  From = Nth_Vector_Loc(Arg3, VECTOR_TYPE);
  To = Nth_Vector_Loc(Arg1, VECTOR_DATA+Index);
  for (i=0; i<=NM_Length; i++)
  *To++ = *From++;
  return Arg1;
}

/* Cells */

/* (MAKE-CELL CONTENTS)
	[Primitive number 0x61]
	Creates a cell with contents CONTENTS.
*/
Built_In_Primitive(Prim_Make_Cell, 1, "MAKE-CELL")
{ Primitive_1_Arg();
  Primitive_GC_If_Needed(1);
  *Free++ = Arg1;
  return Make_Pointer(TC_CELL, Free-1);
}

/* (CONTENTS CELL)
        [Primitive number 0x62]
	Returns the contents of the cell CELL.
*/
Built_In_Primitive(Prim_Cell_Contents, 1, "CONTENTS")
{ Primitive_1_Arg();
  Arg_1_Type(TC_CELL);
  return(Vector_Ref(Arg1, CELL_CONTENTS));
}

/* (CELL? OBJECT)
      [Primitive number 0x63]
      Returns #!TRUE if OBJECT has type-code CELL, otherwise returns
      NIL.
*/
Built_In_Primitive(Prim_Cell, 1,"CELL?")
{ Primitive_1_Arg();
  Touch_In_Primitive(Arg1,Arg1);
  return (Type_Code(Arg1 == TC_CELL)) ? TRUTH : NIL;
}

/* (SET-CONTENTS! CELL VALUE)
        [Primitive number 0x8C]
	Stores VALUE as contents of CELL.  Returns (bad style to count
	on this) the previous contents of CELL.
*/
Built_In_Primitive(Prim_Set_Cell_Contents, 2, "SET-CONTENTS!")
{ Primitive_2_Args();
  Arg_1_Type(TC_CELL);
  Side_Effect_Impurify(Arg1, Arg2);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, CELL_CONTENTS), Arg2);
}

/* Multiprocessor scheduling primitive */

#ifndef butterfly
#ifdef COMPILE_FUTURES
Built_In_Primitive(Prim_Get_Work, 1, "GET-WORK")
{ Pointer The_Queue, Queue_Head, Result;
  Primitive_1_Arg();

  The_Queue = Get_Fixed_Obj_Slot(The_Work_Queue);
  if (The_Queue != NIL) Queue_Head = Vector_Ref(The_Queue, CONS_CAR);
  if ((The_Queue==NIL) || (Queue_Head==NIL))
    if (Arg1 == NIL)
    { printf("\nNo work available, but some has been requested!\n");
      Microcode_Termination(TERM_EXIT);
    }
    else
    { Pop_Primitive_Frame(1);
     Will_Push(2*(STACK_ENV_EXTRA_SLOTS+1) + 1 + CONTINUATION_SIZE);
      Push(NIL);	/* Upon return, no hope if there is no work */
      Push(Make_Non_Pointer(TC_PRIMITIVE, PC_GET_WORK));
      Push(STACK_FRAME_HEADER+1);
      Store_Expression(NIL);
      Store_Return(RC_INTERNAL_APPLY);
      Save_Cont();
      Push(Arg1);
      Push(STACK_FRAME_HEADER);
     Pushed();
      longjmp(*Back_To_Eval, PRIM_APPLY);
  }
  Result = Vector_Ref(Queue_Head, CONS_CAR);
  Queue_Head = Vector_Ref(Queue_Head, CONS_CDR);
  Vector_Set(The_Queue, CONS_CAR, Queue_Head);
  if (Queue_Head==NIL) Vector_Set(The_Queue, CONS_CDR, NIL);
  return Result;
}
#else /* #ifdef COMPILE_FUTURES */
Built_In_Primitive(Prim_Get_Work, 1, "GET-WORK")
{ Primitive_1_Arg();
  Primitive_Error(ERR_UNDEFINED_PRIMITIVE);
}
#endif /* #ifdef COMPILE_FUTURES */
#endif /* #ifndef butterfly */
