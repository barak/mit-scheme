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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/step.c,v 9.22 1987/04/16 02:29:36 jinx Rel $
 *
 * Support for the stepper
 */

#include "scheme.h"
#include "primitive.h"

                 /**********************************/
                 /* Support of stepping primitives */
                 /**********************************/

long Install_Traps(Hunk3, Return_Hook_Too)
/* UGLY ... this knows (a) that it is called with the primitive frame
   already popped off the stack; and (b) the order in which Save_Cont
   stores things on the stack.
*/
Pointer Hunk3;
Boolean Return_Hook_Too;
{ Pointer Eval_Hook, Apply_Hook, Return_Hook;
  Stop_Trapping();
  Eval_Hook = Vector_Ref(Hunk3, HUNK_CXR0);
  Apply_Hook = Vector_Ref(Hunk3, HUNK_CXR1);
  Return_Hook = Vector_Ref(Hunk3, HUNK_CXR2);
  Set_Fixed_Obj_Slot(Stepper_State, Hunk3);
  Trapping = (Eval_Hook != NIL) | (Apply_Hook != NIL);
  if (Microcode_Does_Stepping && Return_Hook_Too && (Return_Hook != NIL))
  { /* Here it is ... gross and ugly.  We know that the top of stack
       has the existing return code to be clobbered, since it was put
       there by Save_Cont.
    */
    Return_Hook_Address = &Top_Of_Stack();
    Old_Return_Code = Top_Of_Stack();
    *Return_Hook_Address = Make_Non_Pointer(TC_RETURN_CODE,
                                            RC_RETURN_TRAP_POINT);
  }
}

/* (PRIMITIVE-EVAL-STEP EXPRESSION ENV HUNK3)
   Evaluates EXPRESSION in ENV and intalls the eval-trap,
   apply-trap, and return-trap from HUNK3.  If any
   trap is '(), it is a null trap that does a normal EVAL,
   APPLY or return.
*/

Built_In_Primitive(Prim_Eval_Step, 3, "PRIMITIVE-EVAL-STEP", 0xCA)
{
  Primitive_3_Args();

  Install_Traps(Arg3, false);
  Pop_Primitive_Frame(3);
  Store_Expression(Arg1);
  Store_Env(Arg2);
  longjmp(*Back_To_Eval, PRIM_NO_TRAP_EVAL);
  /*NOTREACHED*/
}

/* (PRIMITIVE-APPLY-STEP OPERATOR OPERANDS HUNK3)
   Applies OPERATOR to OPERANDS and intalls the eval-trap,
   apply-trap, and return-trap from HUNK3.  If any
   trap is '(), it is a null trap that does a normal EVAL,
   APPLY or return.

   Mostly a copy of Prim_Apply, since this, too, must count the space
   required before actually building a frame
*/

Built_In_Primitive(Prim_Apply_Step, 3, "PRIMITIVE-APPLY-STEP", 0xCB)
{
  Pointer Next_From_Slot, *Next_To_Slot;
  long Number_Of_Args, i;
  Primitive_3_Args();

  Arg_3_Type(TC_HUNK3);
  Number_Of_Args = 0;
  Next_From_Slot = Arg2;
  while (Type_Code(Next_From_Slot) == TC_LIST)
  {
    Number_Of_Args += 1;
    Next_From_Slot = Vector_Ref(Next_From_Slot, CONS_CDR);
  }
  if (Next_From_Slot != NIL)
    Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  Install_Traps(Arg3, true);
  Pop_Primitive_Frame(3);
  Next_From_Slot = Arg2;
  Next_To_Slot = Stack_Pointer - Number_Of_Args;
 Will_Push(Number_Of_Args + STACK_ENV_EXTRA_SLOTS + 1);
  Stack_Pointer = Next_To_Slot;

  for (i = 0; i < Number_Of_Args; i++)
  {
    *Next_To_Slot++ = Vector_Ref(Next_From_Slot, CONS_CAR);
    Next_From_Slot = Vector_Ref(Next_From_Slot, CONS_CDR);
  }
  Push(Arg1);		/* The function */
  Push(STACK_FRAME_HEADER + Number_Of_Args);
 Pushed();
  longjmp(*Back_To_Eval, PRIM_NO_TRAP_APPLY);
  /*NOTREACHED*/
}

/* (PRIMITIVE-RETURN-STEP VALUE HUNK3)
   Returns VALUE and intalls the eval-trap, apply-trap, and
   return-trap from HUNK3.  If any trap is '(), it is a null trap
   that does a normal EVAL, APPLY or return.

   UGLY ... currently assumes that it is illegal to set a return trap
   this way, so that we don't run into stack parsing problems.  If
   this is ever changed, be sure to check for COMPILE_STEPPER flag!
*/

Built_In_Primitive(Prim_Return_Step, 2, "PRIMITIVE-RETURN-STEP", 0xCC)
{
  Pointer Return_Hook;
  Primitive_2_Args();

  Return_Hook = Vector_Ref(Arg2, HUNK_CXR2);
  if (Return_Hook != NIL)
    Primitive_Error(ERR_ARG_2_BAD_RANGE);
  Install_Traps(Arg2, false);
  return Arg1;
}
