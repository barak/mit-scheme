/*          Hey EMACS, this is -*- C -*- code!                 */

/****************************************************************
*                                                               *
*                         Copyright (c) 1986                    *
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

/* File: HOOKS.C
 *
 * This file contains various hooks and handles which connect the
 * primitives with the main interpreter.
 */

#include "scheme.h"
#include "primitive.h"
#include "winder.h"

/* (APPLY FN LIST-OF-ARGUMENTS)
   Calls the function FN on the arguments specified in the list
   LIST-OF-ARGUMENTS. FN must be a primitive procedure, compound
   procedure, or control point. */

Built_In_Primitive( Prim_Apply, 2, "APPLY")
{
  fast Pointer scan_list, *scan_stack;
  fast long number_of_args, i;
#ifdef butterfly
  Pointer saved_stack_pointer;
#endif
  Primitive_2_Args();

  /* Since this primitive must pop its own frame off and push a new
     frame on the stack, it has to be careful.  Its own stack frame is
     needed if an error or GC is required.  So these checks are done
     first (at the cost of traversing the argument list twice), then
     the primitive's frame is popped, and finally the new frame is
     constructed.

     Originally this code tried to be clever by copying the argument
     list into a linear (vector-like) form, so as to avoid the
     overhead of traversing the list twice.  Unfortunately, the
     overhead of maintaining this other form (e.g. PRIMITIVE_GC_If_Needed)
     is sufficiently high that it probably makes up for the time saved. */

  Touch_In_Primitive( Arg2, scan_list);
  number_of_args = 0;
  while (Type_Code( scan_list) == TC_LIST)
    {
      number_of_args += 1;
      Touch_In_Primitive( Vector_Ref( scan_list, CONS_CDR), scan_list);
    }
  if (scan_list != NIL)
    Primitive_Error( ERR_ARG_2_WRONG_TYPE);
#ifdef USE_STACKLETS
  /* This is conservative: if the number of arguments is large enough
     the Will_Push below may try to allocate space on the heap for the
     stack frame. */
  Primitive_GC_If_Needed(New_Stacklet_Size(number_of_args +
					   STACK_ENV_EXTRA_SLOTS + 1));
#endif
  Pop_Primitive_Frame( 2);

 Will_Push( (number_of_args + STACK_ENV_EXTRA_SLOTS + 1));
#ifdef butterfly
  saved_stack_pointer = Stack_Pointer;
#endif
  scan_stack = Simulate_Pushing( number_of_args);
  Stack_Pointer = scan_stack;
  i = number_of_args;
  Touch_In_Primitive( Arg2, scan_list);
  while (i > 0)
    {
#ifdef butterfly
      /* Check for abominable case of someone bashing the arg list. */
      if (Type_Code( scan_list) != TC_LIST)
	{
	  Stack_Pointer = saved_stack_pointer;
	  Primitive_Error( ERR_ARG_2_BAD_RANGE);
	}
#endif
      *scan_stack++ = Vector_Ref( scan_list, CONS_CAR);
      Touch_In_Primitive( Vector_Ref( scan_list, CONS_CDR), scan_list);
      i -= 1;
    }
  Push( Arg1);			/* The procedure */
  Push( (STACK_FRAME_HEADER + number_of_args));
 Pushed();
  longjmp( *Back_To_Eval, PRIM_APPLY);
}

/* This code used to be in the middle of Make_Control_Point, replaced
 * by CWCC below.  Preprocessor conditionals do not work in macros.
 */

#define CWCC(Return_Code) 						\
  fast Pointer *From_Where; 						\
  Primitive_1_Arg();							\
  CWCC_1();								\
  /* Implementation detail: in addition to setting aside the old	\
     stacklet on a catch, the new stacklet is cleared and a return	\
     code is placed at the base of the (now clear) stack indicating	\
     that a return back through here requires restoring the stacklet.	\
     The current enabled interrupts are also saved in the old stacklet.	\
									\
     >>> Temporarily (maybe) the act of doing a CATCH will disable any	\
     >>> return hook that may be in the stack.				\
									\
     >>> Don't even think about adding COMPILER to this stuff!		\
  */ 									\
  Pop_Primitive_Frame(1);						\
  if (Return_Hook_Address != NULL)					\
  { *Return_Hook_Address = Old_Return_Code;				\
    Return_Hook_Address = NULL;						\
  }									\
/* Put down frames to restore history and interrupts so that these 	\
 * operations will be performed on a throw.				\
 */									\
  Will_Push(CONTINUATION_SIZE + HISTORY_SIZE);				\
    Save_History(Return_Code);						\
    Store_Expression(Make_Non_Pointer(TC_FIXNUM, IntEnb));		\
    Store_Return(RC_RESTORE_INT_MASK);					\
    Save_Cont();							\
  Pushed();								\
/* There is no history to use since the last control point was formed.	\
 */									\
  Previous_Restore_History_Stacklet = NULL;				\
  Previous_Restore_History_Offset = 0;					\
  CWCC_2();								\
/* Will_Push(3); -- we just cleared the stack so there MUST be room */	\
  Push(Control_Point);							\
  Push(Arg1);	/* Function */						\
  Push(STACK_FRAME_HEADER+1);
/*  Pushed(); */

#ifdef USE_STACKLETS
#define CWCC_1()							\
  Primitive_GC_If_Needed(2*Default_Stacklet_Size)

#define CWCC_2()							\
  Control_Point = Get_Current_Stacklet();				\
  Allocate_New_Stacklet(3)

#else	/* Not using stacklets, so full copy must be made */
#define CWCC_1()							\
  Primitive_GC_If_Needed((Stack_Top-Stack_Pointer) +			\
			 STACKLET_HEADER_SIZE - 1 +			\
			 CONTINUATION_SIZE +			 	\
                         HISTORY_SIZE)

#define CWCC_2()						\
{ fast long i;							\
  fast long Stack_Cells = (Stack_Top-Stack_Pointer);		\
  Control_Point = Make_Pointer(TC_CONTROL_POINT, Free);		\
  Free[STACKLET_LENGTH] =					\
    Make_Non_Pointer(TC_MANIFEST_VECTOR,			\
		     Stack_Cells + STACKLET_HEADER_SIZE - 1);	\
  Free[STACKLET_UNUSED_LENGTH] =				\
    Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, 0);			\
  Free += STACKLET_HEADER_SIZE;					\
  for (i=0; i < Stack_Cells; i++) *Free++ = Pop();		\
  if (Consistency_Check)					\
    if (Stack_Pointer != Stack_Top)				\
      Microcode_Termination(TERM_BAD_STACK);			\
 Will_Push(CONTINUATION_SIZE);					\
  Store_Return(RC_JOIN_STACKLETS);				\
  Store_Expression(Control_Point);				\
  Save_Cont();							\
 Pushed();							\
}
#endif

/* (CATCH PROCEDURE)
      [Primitive number 0x03]

      Creates a control point (a pointer to the current stack) and
      passes it to PROCEDURE as its only argument.  The inverse
      operation, typically called THROW, is performed by using the
      control point as you would a procedure.  A control point accepts
      one argument which is then returned as the value of the CATCH
      which created the control point.  If the dangerous bit of the
      unused length word in the stacklet is clear then the control
      point may be reused as often as desired since the stack will be
      copied on every throw.  The user level CATCH is built on this
      primitive but is not the same, since it handles dynamic-wind
      while the primitive does not; it assumes that the microcode
      sets and clears the appropriate danger bits for copying.
*/

Built_In_Primitive(Prim_Catch, 1, "CALL-WITH-CURRENT-CONTINUATION")
{ fast Pointer Control_Point;
  CWCC(RC_RESTORE_HISTORY);
  Clear_Danger_Bit((Get_Pointer(Control_Point))[STACKLET_UNUSED_LENGTH]);
  longjmp(*Back_To_Eval, PRIM_APPLY); 
}

#ifdef USE_STACKLETS
Built_In_Primitive(Prim_Non_Reentrant_Catch, 1, "FAST-CALL-WITH-CURRENT-CONTINUATION")
{ Pointer Control_Point;
  CWCC(RC_RESTORE_DONT_COPY_HISTORY);
  longjmp(*Back_To_Eval, PRIM_APPLY);
}

#else	/* Without stacklets, the two catches are identical */

Built_In_Primitive(Prim_Non_Reentrant_Catch, 1, "FAST-CALL-WITH-CURRENT-CONTINUATION")
{ Pointer Control_Point;
  CWCC(RC_RESTORE_HISTORY);
  Clear_Danger_Bit((Get_Pointer(Control_Point))[STACKLET_UNUSED_LENGTH]);
  longjmp(*Back_To_Eval, PRIM_APPLY); 
}
#endif

/* (ENABLE-INTERRUPTS! INTERRUPTS)
      [Primitive number 0x06]
      Changes the enabled interrupt bits to bitwise-or of INTERRUPTS
      and previous value of interrupts.  Returns the previous value.
      See MASK_INTERRUPT_ENABLES for more information on interrupts.
*/
Built_In_Primitive(Prim_Enable_Interrupts, 1, "ENABLE-INTERRUPTS!")
{ Pointer Result;
  Primitive_1_Arg();
  Arg_1_Type(TC_FIXNUM);
  Result = Make_Non_Pointer(TC_FIXNUM, IntEnb);
  IntEnb = Get_Integer(Arg1) | INT_Mask;
  New_Compiler_MemTop();
  return Result;
}

/* (ERROR-PROCEDURE arg1 arg2 arg3)
     Passes its arguments along to the appropriate Scheme error handler
     after turning off history, etc.
*/
Built_In_Primitive(Prim_Error_Procedure, 3, "ERROR-PROCEDURE")
{ Primitive_3_Args();
 Will_Push(CONTINUATION_SIZE+HISTORY_SIZE+STACK_ENV_EXTRA_SLOTS+4);
  Back_Out_Of_Primitive();
  Save_Cont();
  Stop_History();
 /* Stepping should be cleared here! */
  Push(Arg3);
  Push(Arg2);
  Push(Arg1);
  Push(Get_Fixed_Obj_Slot(Error_Procedure));
  Push(STACK_FRAME_HEADER+3);
 Pushed();
  longjmp(*Back_To_Eval, PRIM_APPLY);
}

/* (GET_FIXED_OBJECTS_VECTOR)
      [Primitive number 0x7A]
      Returns the current fixed objects vector.  This vector is used
      for communication between the interpreter and the runtime
      system.  See the file UTABCSCM.SCM in the runtime system for the
      names of the slots in the vector.
*/
Built_In_Primitive(Prim_Get_Fixed_Objects_Vector, 0,
		 "GET-FIXED-OBJECTS-VECTOR")
{ Primitive_0_Args();
  if (Valid_Fixed_Obj_Vector())
    return Get_Fixed_Obj_Slot(Me_Myself);
  else return NIL;
}

/* (FORCE DELAYED-OBJECT)
      [Primitive number 0xAF]
      Returns the memoized value of the DELAYED-OBJECT (created by a
      DELAY special form) if it has already been calculated.
      Otherwise, it calculates the value and memoizes it for future
      use.
*/
Built_In_Primitive(Prim_Force, 1, "FORCE")
{ Primitive_1_Arg();
  Arg_1_Type(TC_DELAYED);
  if (Vector_Ref(Arg1, THUNK_SNAPPED) == TRUTH)
    return Vector_Ref(Arg1, THUNK_VALUE);
  Pop_Primitive_Frame(1);
 Will_Push(CONTINUATION_SIZE);
  Store_Return(RC_SNAP_NEED_THUNK);
  Store_Expression(Arg1);
  Save_Cont();
 Pushed();
  Store_Env(Fast_Vector_Ref(Arg1, THUNK_ENVIRONMENT));
  Store_Expression(Fast_Vector_Ref(Arg1, THUNK_PROCEDURE));
  longjmp(*Back_To_Eval, PRIM_DO_EXPRESSION); /*NOTREACHED*/
}

/* (EXECUTE_AT_NEW_POINT SPACE BEFORE DURING AFTER)
      [Primitive number 0xE2]
      Create a new state point in the specified state SPACE.  To enter
      the new point you must execute the BEFORE thunk.  On the way out,
      the AFTER thunk is executed.  If SPACE is NIL, then the microcode
      variable Current_State_Point is used to find the current state
      point and no state space is side-effected as the code runs.
*/
Built_In_Primitive(Prim_Execute_At_New_Point, 4, "EXECUTE-AT-NEW-POINT")
{ Pointer New_Point, Old_Point;
  Primitive_4_Args();
  guarantee_state_point();
  if (Arg1 == NIL) Old_Point = Current_State_Point;
  else
  { Arg_1_Type(TC_VECTOR);
    if (Vector_Ref(Arg1, STATE_SPACE_TAG) !=
        Get_Fixed_Obj_Slot(State_Space_Tag))
      Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    Old_Point = Fast_Vector_Ref(Arg1, STATE_SPACE_NEAREST_POINT);
  }
  Primitive_GC_If_Needed(STATE_POINT_SIZE);
  Pop_Primitive_Frame(4);
  New_Point = Make_Pointer(TC_VECTOR, Free);
  Free[STATE_POINT_HEADER] =
    Make_Non_Pointer(TC_MANIFEST_VECTOR, STATE_POINT_SIZE-1);
  Free[STATE_POINT_TAG] = Get_Fixed_Obj_Slot(State_Point_Tag);
  Free[STATE_POINT_BEFORE_THUNK] = Arg2;
  Free[STATE_POINT_AFTER_THUNK] = Arg4;
  Free[STATE_POINT_NEARER_POINT] = Old_Point;
  Free[STATE_POINT_DISTANCE_TO_ROOT] =
    1 + Fast_Vector_Ref(Old_Point, STATE_POINT_DISTANCE_TO_ROOT);
  Free += STATE_POINT_SIZE;
 Will_Push(2*CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS+1));
  /* Push a continuation to go back to the current state after the
     body is evaluated */
  Store_Expression(Old_Point);
  Store_Return(RC_RESTORE_TO_STATE_POINT);
  Save_Cont();
  /* Push a stack frame which will call the body after we have moved
     into the new state point */
  Push(Arg3);
  Push(STACK_FRAME_HEADER);
  /* Push the continuation to go with the stack frame */
  Store_Expression(NIL);
  Store_Return(RC_INTERNAL_APPLY);
  Save_Cont();
 Pushed();
  Translate_To_Point(New_Point);
}

/* (MAKE_STATE_SPACE MUTABLE?)
      [Primitive number 0xE1]
      Creates a new state space for the dynamic winder.  Used only
      internally to the dynamic wind operations.  If the arugment
      is #!TRUE, then a real, mutable state space is created.
      Otherwise a (actually, THE) immutable space is created and
      the microcode will track motions in this space.
*/
Built_In_Primitive(Prim_Make_State_Space, 1, "MAKE-STATE-SPACE")
{ Pointer New_Point;
  Primitive_1_Arg();
  Primitive_GC_If_Needed(STATE_POINT_SIZE+STATE_SPACE_SIZE);
  New_Point = Make_Pointer(TC_VECTOR, Free);
  Free[STATE_POINT_HEADER] =
    Make_Non_Pointer(TC_MANIFEST_VECTOR, STATE_POINT_SIZE-1);
  Free[STATE_POINT_TAG] = Get_Fixed_Obj_Slot(State_Point_Tag);
  Free[STATE_POINT_BEFORE_THUNK] = NIL;
  Free[STATE_POINT_AFTER_THUNK] = NIL;
  Free[STATE_POINT_NEARER_POINT] = NIL;
  Free[STATE_POINT_DISTANCE_TO_ROOT] = FIXNUM_0;
  Free += STATE_POINT_SIZE;
  if (Arg1 == NIL)
  { Current_State_Point = New_Point;
    return NIL;
  }
  else
  { Pointer New_Space = Make_Pointer(TC_VECTOR, Free);
    Free[STATE_SPACE_HEADER] =
      Make_Non_Pointer(TC_MANIFEST_VECTOR, STATE_SPACE_SIZE-1);
    Free[STATE_SPACE_TAG] = Get_Fixed_Obj_Slot(State_Space_Tag);
    Free[STATE_SPACE_NEAREST_POINT] = New_Point;
    Free += STATE_SPACE_SIZE;
    Fast_Vector_Set(New_Point, STATE_POINT_NEARER_POINT, New_Space);
    return New_Space;
  }
}

Built_In_Primitive(Prim_Current_Dynamic_State, 1, "CURRENT-DYNAMIC-STATE")
{ Primitive_1_Arg();
  guarantee_state_point();
  if (Arg1 == NIL) return Current_State_Point;
  Arg_1_Type(TC_VECTOR);
  if (Fast_Vector_Ref(Arg1, STATE_SPACE_TAG) !=
      Get_Fixed_Obj_Slot(State_Space_Tag))
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  return Vector_Ref(Arg1, STATE_SPACE_NEAREST_POINT);
}

Built_In_Primitive(Prim_Set_Dynamic_State, 1, "SET-CURRENT-DYNAMIC-STATE!")
{ Pointer State_Space, Result;
  Primitive_1_Arg();
  Arg_1_Type(TC_VECTOR);
  if (Fast_Vector_Ref(Arg1, STATE_POINT_TAG) !=
      Get_Fixed_Obj_Slot(State_Point_Tag))
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  State_Space = Find_State_Space(Arg1);
  if (State_Space==NIL)
  { guarantee_state_point();
    Result = Current_State_Point;
    Current_State_Point = Arg1;
  }
  else
  { Result = Vector_Ref(State_Space, STATE_SPACE_NEAREST_POINT);
    Vector_Set(State_Space, STATE_SPACE_NEAREST_POINT, Arg1);
  }
  return Result;
}

/* (SCODE_EVAL SCODE-EXPRESSION ENVIRONMENT)
      [Primitive number 0x04]
      Evaluate the piece of SCode (SCODE-EXPRESSION) in the
      ENVIRONMENT. This is like Eval, except that it expects its input
      to be syntaxed into SCode rather than just a list.
*/
Built_In_Primitive(Prim_Scode_Eval, 2, "SCODE-EVAL")
{ Primitive_2_Args();
  if (Type_Code(Arg2) != GLOBAL_ENV) Arg_2_Type(TC_ENVIRONMENT);
  Pop_Primitive_Frame(2);
  Store_Env(Arg2);
  Store_Expression(Arg1);
  longjmp(*Back_To_Eval, PRIM_DO_EXPRESSION);
}

/* (SET_INTERRUPT_ENABLES NEW-INT-ENABLES)
      [Primitive number 0x06]
      Changes the enabled interrupt bits to NEW-INT-ENABLES and
      returns the previous value.  See MASK_INTERRUPT_ENABLES for more
      information on interrupts.
*/
Built_In_Primitive(Prim_Set_Interrupt_Enables, 1, "SET-INTERRUPT-ENABLES!")
{ Pointer Result;
  Primitive_1_Arg();
  Arg_1_Type(TC_FIXNUM);
  Result = FIXNUM_0+IntEnb;
  IntEnb = Get_Integer(Arg1) & INT_Mask;
  New_Compiler_MemTop();
  return Result;
}

/* (SET_CURRENT_HISTORY TRIPLE)
      [Primitive number 0x2F]
      Begins recording history into TRIPLE.  The history structure is
      somewhat complex and should be understood before trying to use
      this primitive.  It is used in the Read-Eval-Print loop in the
      Scheme runtime system.
*/
Built_In_Primitive(Prim_Set_Current_History, 1, "SET-CURRENT-HISTORY")
{ Pointer Result;
  Primitive_1_Arg();
  Arg_1_Type(TC_HUNK3);
  Result = *History;
#ifdef COMPILE_HISTORY
  History = Get_Pointer(Arg1);
#else
  History = Get_Pointer(Get_Fixed_Obj_Slot(Dummy_History));
#endif
  return Result;
}

/* (SET_FIXED_OBJECTS_VECTOR VECTOR)
      [Primitive number 0x7B]
      Replace the current fixed objects vector with VECTOR.  The fixed
      objects vector is used for communication between the Scheme
      runtime system and the interpreter.  The file UTABCSCM.SCM
      contains the names of the slots in the vector.  Returns (bad
      style to depend on this) the previous fixed objects vector.
*/
Built_In_Primitive(Prim_Set_Fixed_Objects_Vector, 1,
		   "SET-FIXED-OBJECTS-VECTOR!")
{ Pointer Result;
  Primitive_1_Arg();
  Arg_1_Type(TC_VECTOR);

  if (Valid_Fixed_Obj_Vector())
    Result = Get_Fixed_Obj_Slot(Me_Myself);
  else Result = NIL;
  Set_Fixed_Obj_Hook(Arg1);
  Set_Fixed_Obj_Slot(Me_Myself, Arg1);
  return Result;
}

/* (TRANSLATE_TO_STATE_POINT STATE_POINT)
      [Primitive number 0xE3]
      Move to a new dynamic wind environment by performing all of the
      necessary enter and exit forms to get from the current state to
      the new state as specified by STATE_POINT.
*/
Built_In_Primitive(Prim_Translate_To_Point, 1, "TRANSLATE-TO-STATE-POINT")
{ Primitive_1_Arg();
  Arg_1_Type(TC_VECTOR);
  if (Vector_Ref(Arg1, STATE_POINT_TAG) != Get_Fixed_Obj_Slot(State_Point_Tag))
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Pop_Primitive_Frame(1);
  Translate_To_Point(Arg1);
  /* This ends by longjmp-ing back to the interpreter */
}

/* (WITH_HISTORY_DISABLED THUNK)
      [Primitive number 0x9C]
      THUNK must be a procedure or primitive procedure which takes no
      arguments.  Turns off the history collection mechanism.  Removes
      the most recent reduction (the expression which called the
      primitive) from the current history and saves the history.  Then
      it calls the THUNK.  When (if) the THUNK returns, the history is
      restored back and collection resumes.  The net result is that the
      THUNK is called with history collection turned off.
*/
Built_In_Primitive(Prim_With_History_Disabled, 1, "WITH-HISTORY-DISABLED")
{ Pointer *First_Rib, *Rib, *Second_Rib;
  Primitive_1_Arg();
  /* Remove one reduction from the history before saving it */
  First_Rib = Get_Pointer(History[HIST_RIB]);
  Second_Rib = Get_Pointer(First_Rib[RIB_NEXT_REDUCTION]);
  if (!((Dangerous(First_Rib[RIB_MARK])) ||
       (First_Rib == Second_Rib)))
  { Set_Danger_Bit(Second_Rib[RIB_MARK]);
    for (Rib = First_Rib;
         Get_Pointer(Rib[RIB_NEXT_REDUCTION]) != First_Rib;
         Rib = Get_Pointer(Rib[RIB_NEXT_REDUCTION]))
    { /* Look for one that points to the first rib */ }
    History[HIST_RIB] = Make_Pointer(Type_Code(History[HIST_RIB]), Rib);
  }
  Pop_Primitive_Frame(1);
  Stop_History();
 Will_Push(STACK_ENV_EXTRA_SLOTS+1);
  Push(Arg1);
  Push(STACK_FRAME_HEADER);
 Pushed();
  longjmp(*Back_To_Eval, PRIM_APPLY);
}

/* Called with a mask and a thunk */

Built_In_Primitive(Prim_With_Interrupt_Mask, 2, "WITH-INTERRUPT-MASK")
{ Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Pop_Primitive_Frame(2);
 Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS+2));
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(FIXNUM_0+IntEnb);
  Save_Cont();
  Push(FIXNUM_0 + IntEnb);	/* Current interrupt mask */
  Push(Arg2);			/* Function to call */
  Push(STACK_FRAME_HEADER+1);
 Pushed();
  IntEnb = INT_Mask & Get_Integer(Arg1);
  longjmp(*Back_To_Eval, PRIM_APPLY);
}

/* Called with a mask and a thunk */

Built_In_Primitive(Prim_With_Interrupts_Reduced, 2, "WITH-INTERRUPTS-REDUCED")
{
  long new_interrupt_mask;
  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Pop_Primitive_Frame(2);
 Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS+2));
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(FIXNUM_0+IntEnb);
  Save_Cont();
  Push(FIXNUM_0 + IntEnb);	/* Current interrupt mask */
  Push(Arg2);			/* Function to call */
  Push(STACK_FRAME_HEADER+1);
 Pushed();
  new_interrupt_mask = (INT_Mask & Get_Integer( Arg1));
  if (new_interrupt_mask > IntEnb)
    IntEnb = new_interrupt_mask;
  else
    IntEnb = (new_interrupt_mask & IntEnb);
  longjmp(*Back_To_Eval, PRIM_APPLY);
}

/* (WITHIN_CONTROL_POINT CONTROL-POINT THUNK)
      [Primitive number 0xBF]
      THUNK must be a procedure or primitive procedure which takes no
      arguments.  Restores the state of the machine from the control
      point, and then calls the THUNK in this new state.
*/
Built_In_Primitive(Prim_Within_Control_Point, 2, "WITHIN-CONTROL-POINT")
{ Primitive_2_Args();
  Arg_1_Type(TC_CONTROL_POINT);
  Our_Throw(false, Arg1);
  Within_Stacklet_Backout();
  Our_Throw_Part_2();
 Will_Push(STACK_ENV_EXTRA_SLOTS+1);
  Push(Arg2);
  Push(STACK_FRAME_HEADER);
 Pushed();
  longjmp(*Back_To_Eval, PRIM_APPLY);
}
/* (WITH_THREADED_STACK PROCEDURE THUNK)
      [Primitive number 0xBE]
      THUNK must be a procedure or primitive procedure which takes no
      arguments.  PROCEDURE must expect one argument.  Basically this
      primitive does (PROCEDURE (THUNK)) ... it calls the THUNK and
      passes the result on as an argument to PROCEDURE.  However, it
      leaves a "well-known continuation code" on the stack for use by
      the continuation parser in the Scheme runtime system.
*/
Built_In_Primitive(Prim_With_Threaded_Stack, 2, "WITH-THREADED-STACK")
{ Primitive_2_Args();
  Pop_Primitive_Frame(2);
 Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS+1));
  Store_Expression(Arg1);	/* Save procedure to call later */
  Store_Return(RC_INVOKE_STACK_THREAD);
  Save_Cont();
  Push(Arg2);	/* Function to call now */
  Push(STACK_FRAME_HEADER);
 Pushed();
  longjmp(*Back_To_Eval, PRIM_APPLY);
}

