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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/purutl.c,v 9.30 1987/07/22 21:54:46 jinx Exp $ */

/* Pure/Constant space utilities. */

#include "scheme.h"
#include "primitive.h"
#include "gccode.h"
#include "zones.h"

void
Update(From, To, Was, Will_Be)
     fast Pointer *From, *To, *Was, *Will_Be;
{
  for (; From < To; From++)
  {
    if (GC_Type_Special(*From))
    {
      if (Safe_Type_Code(*From) == TC_MANIFEST_NM_VECTOR)
        From += Get_Integer(*From);
      continue;
    }
    if (GC_Type_Non_Pointer(*From))
      continue;
    if (Get_Pointer(*From) == Was)
      *From = Make_Pointer(Type_Code(*From), Will_Be);
  }
  return;
}

Pointer
Make_Impure(Object)
     Pointer Object;
{
  Pointer *New_Address, *End_Of_Area;
  fast Pointer *Obj_Address, *Constant_Address;
  long Length, Block_Length;
  fast long i;

  /* Calculate size of object to be "impurified".
     Note that this depends on the fact that Compiled Entries CANNOT
     be pure.
   */

  Switch_by_GC_Type(Object)
  {
    case TC_BROKEN_HEART:
    case TC_MANIFEST_NM_VECTOR:
    case TC_MANIFEST_SPECIAL_NM_VECTOR:
    case_Non_Pointer:
      fprintf(stderr, "\nImpurify Non-Pointer.\n");
      Microcode_Termination(TERM_NON_POINTER_RELOCATION);
  
    case TC_BIG_FLONUM:
    case TC_FUTURE:
    case_Vector:
      Length = Vector_Length(Object) + 1;
      break;

    case_Quadruple:
      Length = 4;
      break;

    case TC_VARIABLE:
    case_Triple:
      Length = 3;
      break;

    case TC_WEAK_CONS:
    case_Pair:
      Length = 2;
      break;

    case_Cell:
      Length = 1;
      break;

    default:
      fprintf(stderr, "\nImpurify: Bad type code = 0x%02x\n",
	      Type_Code(Object));
      Invalid_Type_Code();
  }

  /* Add a copy of the object to the last constant block in memory.
   */

  Constant_Address = Free_Constant;

  Obj_Address = Get_Pointer(Object);
  if (!Test_Pure_Space_Top(Constant_Address + Length))
    return NIL;
  Block_Length = Get_Integer(*(Constant_Address-1));
  Constant_Address -= 2;
  New_Address = Constant_Address;

#ifdef FLOATING_ALIGNMENT
  /* This should be done more cleanly, always align before doing a
     block, or something like it. -- JINX
   */

  if (Type_Code(Object) == TC_BIG_FLONUM)
  {
    Pointer *Start;

    Start = Constant_Address;
    Align_Float(Constant_Address);
    for (i = 0; i < Length; i++)
      *Constant_Address++ = *Obj_Address++;
    Length = Constant_Address - Start;
  }
  else
#endif
    for (i = Length; --i >= 0; )
    {
      *Constant_Address++ = *Obj_Address;
      *Obj_Address++ = Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, i);
    }
  *Constant_Address++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
  *Constant_Address++ = Make_Non_Pointer(END_OF_BLOCK, Block_Length + Length);
  *(New_Address + 2 - Block_Length) =
    Make_Non_Pointer(PURE_PART, Block_Length + Length);
  Obj_Address -= Length;
  Free_Constant = Constant_Address;

  /* Run through memory relocating pointers to this object, including
   * those in pure areas.
   */

  Set_Pure_Top();
  Terminate_Old_Stacklet();
  Terminate_Constant_Space(End_Of_Area);
  Update(Heap_Bottom, Free, Obj_Address, New_Address);
  Update(Constant_Space, End_Of_Area, Obj_Address, New_Address);
  return Make_Pointer(Type_Code(Object), New_Address);
}

/* (PRIMITIVE-IMPURIFY OBJECT)
   Remove an object from pure space so it can be side effected.
   The object is placed in constant space instead.
*/
Built_In_Primitive(Prim_Impurify, 1, "PRIMITIVE-IMPURIFY", 0xBD)
{
  Pointer Result;
  Primitive_1_Arg();

  Touch_In_Primitive(Arg1, Arg1);
  Result = Make_Impure(Arg1);
  if (Result != NIL)
    return Result;
  Primitive_Error(ERR_IMPURIFY_OUT_OF_SPACE);
  /*NOTREACHED*/
}

Boolean
Pure_Test(Obj_Address)
     fast Pointer *Obj_Address;
{
  fast Pointer *Where;
#ifdef FLOATING_ALIGNMENT
  fast Pointer Float_Align_Value;

  Float_Align_Value = Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, 0);
#endif

  Where = Free_Constant-1;
  while (Where >= Constant_Space)
  {
#ifdef FLOATING_ALIGNMENT
    while (*Where == Float_Align_Value)
      Where -= 1;
#endif
    Where -= 1 + Get_Integer(*Where);
    if (Where <= Obj_Address)
      return
	((Boolean) (Obj_Address <= (Where + 1 + Get_Integer(*(Where + 1)))));
  }
  return ((Boolean) false);
}

/* (PURE? OBJECT)
   Returns #!TRUE if the object is pure (ie it doesn't point to any
   other object, or it is in a pure section of the constant space).
*/
Built_In_Primitive(Prim_Pure_P, 1, "PURE?", 0xBB)
{
  Primitive_1_Arg();

  if ((GC_Type_Non_Pointer(Arg1)) ||
      (GC_Type_Special(Arg1)))
    return TRUTH;
  Touch_In_Primitive(Arg1, Arg1);
  {
    extern Pointer *compiled_entry_to_block_address();
    Pointer *Obj_Address;

    Obj_Address =
      ((GC_Type_Compiled(Arg1))
       ? (compiled_entry_to_block_address(Arg1))
       : (Get_Pointer(Arg1)));
    if (Is_Pure(Obj_Address))
      return TRUTH;
  }
  return NIL;
}

/* (CONSTANT? OBJECT)
   Returns #!TRUE if the object is in constant space or isn't a
   pointer.
*/
Built_In_Primitive(Prim_Constant_P, 1, "CONSTANT?", 0xBA)
{
  Primitive_1_Arg();

  Touch_In_Primitive(Arg1, Arg1);
  return ((GC_Type_Non_Pointer(Arg1)) ||
	  (GC_Type_Special(Arg1)) ||
	  (Is_Constant(Get_Pointer(Arg1)))) ?
         TRUTH : NIL;
}

/* (GET-NEXT-CONSTANT)
   Returns the next free address in constant space.
*/
Built_In_Primitive(Prim_Get_Next_Constant, 0, "GET-NEXT-CONSTANT", 0xE4)
{
  Pointer *Next_Address;

  Next_Address = Free_Constant + 1;
  Primitive_0_Args();
  return Make_Pointer(TC_ADDRESS, Next_Address);
}

/* copy_to_constant_space is a microcode utility procedure.
   It takes care of making legal constant space blocks.
   The microcode kills itself if there is not enough constant
   space left.
 */

extern Pointer *copy_to_constant_space();

Pointer *
copy_to_constant_space(source, nobjects)
     fast Pointer *source;
     long nobjects;
{
  fast Pointer *dest;
  fast long i;
  Pointer *result;

  dest = Free_Constant;
  if (!Test_Pure_Space_Top(dest + nobjects + 6))
  {
    fprintf(stderr,
	    "copy_to_constant_space: Not enough constant space!\n");
    Microcode_Termination(TERM_NO_SPACE);
  }
  *dest++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 3);
  *dest++ = Make_Non_Pointer(PURE_PART, nobjects + 5);
  *dest++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
  *dest++ = Make_Non_Pointer(CONSTANT_PART, 3);
  result = dest;
  for (i = nobjects; --i >= 0; )
  {
    *dest++ = *source++;
  }
  *dest++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
  *dest++ = Make_Non_Pointer(END_OF_BLOCK, nobjects + 5);
  Free_Constant = dest;

  return result;
}
