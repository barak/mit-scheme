/* -*-C-*-

Copyright (c) 1988 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/hooks.c,v 9.33 1988/10/21 00:12:37 cph Exp $
 *
 * This file contains various hooks and handles which connect the
 * primitives with the main interpreter.
 */

#include "scheme.h"
#include "prims.h"
#include "winder.h"
#include "history.h"

/* (APPLY FN LIST-OF-ARGUMENTS)
   Calls the function FN to the arguments specified in the list
   LIST-OF-ARGUMENTS. FN must be a primitive procedure, compound
   procedure, or control point. */

DEFINE_PRIMITIVE ("APPLY", Prim_apply, 2, 2, 0)
{
  fast Pointer scan_list, *scan_stack;
  fast long number_of_args, i;
#ifdef butterfly
  Pointer *saved_stack_pointer;
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
  {
    signal_error_from_primitive( ERR_ARG_2_WRONG_TYPE);
  }
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
  scan_stack = Simulate_Pushing (number_of_args);
  Stack_Pointer = scan_stack;
  i = number_of_args;
  Touch_In_Primitive (Arg2, scan_list);
  while (i > 0)
  {
#ifdef butterfly
    /* Check for abominable case of someone bashing the arg list. */
    if (Type_Code( scan_list) != TC_LIST)
    {
      Stack_Pointer = saved_stack_pointer;
      signal_error_from_primitive (ERR_ARG_2_BAD_RANGE);
    }
#endif
    *scan_stack++ = Vector_Ref( scan_list, CONS_CAR);
    Touch_In_Primitive ((Vector_Ref (scan_list, CONS_CDR)), scan_list);
    i -= 1;
  }
  Push (Arg1);			/* The procedure */
  Push ((STACK_FRAME_HEADER + number_of_args));
 Pushed ();
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
}

/* Implementation detail: in addition to setting aside the old
   stacklet on a catch, the new stacklet is cleared and a return
   code is placed at the base of the (now clear) stack indicating
   that a return back through here requires restoring the stacklet.
   The current enabled interrupts are also saved in the old stacklet.

   >>> Temporarily (maybe) the act of doing a CATCH will disable any
   >>> return hook that may be in the stack.
*/

#define CWCC(Return_Code)						\
{									\
  fast Pointer *From_Where;						\
									\
  CWCC_1();								\
  Pop_Primitive_Frame(1);						\
  if (Return_Hook_Address != NULL)					\
  {									\
    *Return_Hook_Address = Old_Return_Code;				\
    Return_Hook_Address = NULL;						\
  }									\
  /*									\
    Put down frames to restore history and interrupts so that these	\
    operations will be performed on a throw.				\
   */									\
  Will_Push(CONTINUATION_SIZE + HISTORY_SIZE);				\
    Save_History(Return_Code);						\
    Store_Expression(MAKE_SIGNED_FIXNUM(FETCH_INTERRUPT_MASK()));	\
    Store_Return(RC_RESTORE_INT_MASK);					\
    Save_Cont();							\
  Pushed();								\
  /*									\
    There is no history to use since the last control point was formed.	\
   */									\
  Prev_Restore_History_Stacklet = NULL;					\
  Prev_Restore_History_Offset = 0;					\
  CWCC_2();								\
  /* we just cleared the stack so there MUST be room */			\
  /* Will_Push(3); */							\
  Push(Control_Point);							\
  Push(Arg1);	/* Function */						\
  Push(STACK_FRAME_HEADER + 1);						\
  /*  Pushed(); */							\
}

#ifdef USE_STACKLETS

#define CWCC_1()							\
{									\
  Primitive_GC_If_Needed(2 * Default_Stacklet_Size);			\
}

#define CWCC_2()							\
{									\
  Control_Point = Get_Current_Stacklet();				\
  Allocate_New_Stacklet(3);						\
}

#else /* not USE_STACKLETS */

#define CWCC_1()							\
{									\
  Primitive_GC_If_Needed((Stack_Top - Stack_Pointer) +			\
			 STACKLET_HEADER_SIZE +				\
			 CONTINUATION_SIZE +				\
                         HISTORY_SIZE);					\
}

#define CWCC_2()							\
{									\
  fast long i, Stack_Cells;						\
									\
  Stack_Cells = (Stack_Top - Stack_Pointer);				\
  Control_Point = Make_Pointer(TC_CONTROL_POINT, Free);			\
  Free[STACKLET_LENGTH] =						\
    Make_Non_Pointer(TC_MANIFEST_VECTOR,				\
		     (Stack_Cells + (STACKLET_HEADER_SIZE - 1)));	\
  Free[STACKLET_REUSE_FLAG] = SHARP_T;					\
  Free[STACKLET_UNUSED_LENGTH] =					\
    Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, 0);				\
  Free += STACKLET_HEADER_SIZE;						\
  for (i = Stack_Cells; --i >= 0; )					\
  {									\
    *Free++ = Pop();							\
  }									\
  if (Consistency_Check)						\
  {									\
    if (Stack_Pointer != Stack_Top)					\
    {									\
      Microcode_Termination(TERM_BAD_STACK);				\
    }									\
  }									\
 Will_Push(CONTINUATION_SIZE);						\
  Store_Return(RC_JOIN_STACKLETS);					\
  Store_Expression(Control_Point);					\
  Save_Cont();								\
 Pushed();								\
}

#endif /* USE_STACKLETS */

/* (CALL-WITH-CURRENT-CONTINUATION PROCEDURE)

   Creates a control point (a pointer to the current stack) and passes
   it to PROCEDURE as its only argument.  The inverse operation,
   typically called THROW, is performed by using the control point as
   you would a procedure.  A control point accepts one argument which
   is then returned as the value of the CATCH which created the
   control point.  If the reuse flag of the stacklet is clear then the
   control point may be reused as often as desired since the stack
   will be copied on every throw.  The user level CATCH is built on
   this primitive but is not the same, since it handles dynamic state
   while the primitive does not; it assumes that the microcode sets
   and clears the appropriate reuse flags for copying.
*/

DEFINE_PRIMITIVE ("CALL-WITH-CURRENT-CONTINUATION", Prim_catch, 1, 1, 0)
{
  Pointer Control_Point;
  Primitive_1_Arg ();

  CWCC (RC_RESTORE_HISTORY);
  Vector_Set (Control_Point, STACKLET_REUSE_FLAG, NIL);
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("NON-REENTRANT-CALL-WITH-CURRENT-CONTINUATION", Prim_non_reentrant_catch, 1, 1, 0)
{
  Pointer Control_Point;
  Primitive_1_Arg ();

#ifdef USE_STACKLETS

  CWCC (RC_RESTORE_DONT_COPY_HISTORY);

#else
  /* When there are no stacklets, it is identical to the reentrant version. */

  CWCC (RC_RESTORE_HISTORY);
  Vector_Set (Control_Point, STACKLET_REUSE_FLAG, NIL);

#endif

  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
}

/* (ENABLE-INTERRUPTS! INTERRUPTS)
   Changes the enabled interrupt bits to bitwise-or of INTERRUPTS
   and previous value of interrupts.  Returns the previous value.
   See MASK_INTERRUPT_ENABLES for more information on interrupts.
*/
DEFINE_PRIMITIVE ("ENABLE-INTERRUPTS!", Prim_enable_interrupts, 1, 1, 0)
{
  long previous;
  Primitive_1_Arg ();

  Arg_1_Type (TC_FIXNUM);
  previous = (FETCH_INTERRUPT_MASK ());
  SET_INTERRUPT_MASK (((Get_Integer (Arg1)) & INT_Mask) | previous);
  PRIMITIVE_RETURN (MAKE_SIGNED_FIXNUM (previous));
}

/* (ERROR-PROCEDURE arg1 arg2 arg3)
   Passes its arguments along to the appropriate Scheme error handler
   after turning off history, etc.
*/
DEFINE_PRIMITIVE ("ERROR-PROCEDURE", Prim_error_procedure, 3, 3, 0)
{
  Primitive_3_Args();

  /*
    This is done outside the Will_Push because the space for it
    is guaranteed by the interpreter before it gets here.
    If done inside, this could break when using stacklets.
   */
  Back_Out_Of_Primitive();
  Save_Cont();
 Will_Push(HISTORY_SIZE+STACK_ENV_EXTRA_SLOTS+4);
  Stop_History();
 /* Stepping should be cleared here! */
  Push(Arg3);
  Push(Arg2);
  Push(Arg1);
  Push(Get_Fixed_Obj_Slot(Error_Procedure));
  Push(STACK_FRAME_HEADER+3);
 Pushed();
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
}

/* (GET-FIXED-OBJECTS-VECTOR)
   Returns the current fixed objects vector.  This vector is used
   for communication between the interpreter and the runtime
   system.  See the file UTABCSCM.SCM in the runtime system for the
   names of the slots in the vector.
*/
DEFINE_PRIMITIVE ("GET-FIXED-OBJECTS-VECTOR", Prim_get_fixed_objects_vector, 0, 0, 0)
{
  Primitive_0_Args ();

  if (Valid_Fixed_Obj_Vector ())
    PRIMITIVE_RETURN (Get_Fixed_Obj_Slot (Me_Myself));
  else
    PRIMITIVE_RETURN (NIL);
}

/* (FORCE DELAYED-OBJECT)
   Returns the memoized value of the DELAYED-OBJECT (created by a
   DELAY special form) if it has already been calculated.
   Otherwise, it calculates the value and memoizes it for future
   use. */

#define DELAYED_P(object) ((OBJECT_TYPE (object)) == TC_DELAYED)

DEFINE_PRIMITIVE ("FORCE", Prim_force, 1, 1, 0)
{
  fast Pointer thunk;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, DELAYED_P);
  thunk = (ARG_REF (1));
  switch (Vector_Ref (thunk, THUNK_SNAPPED))
    {
    case SHARP_T:
      PRIMITIVE_RETURN (Vector_Ref (thunk, THUNK_VALUE));

    case FIXNUM_ZERO:
      {
	/* New-style thunk used by compiled code. */
	Pop_Primitive_Frame (1);
       Will_Push (CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS + 1);
	Store_Return (RC_SNAP_NEED_THUNK);
	Store_Expression (thunk);
	Save_Cont ();
	Push (Vector_Ref (thunk, THUNK_VALUE));
	Push (STACK_FRAME_HEADER);
       Pushed ();
	PRIMITIVE_ABORT (PRIM_APPLY);
	/*NOTREACHED*/
      }

    default:
      {
	/* Old-style thunk used by interpreted code. */
	Pop_Primitive_Frame (1);
       Will_Push (CONTINUATION_SIZE);
	Store_Return (RC_SNAP_NEED_THUNK);
	Store_Expression (thunk);
	Save_Cont ();
       Pushed ();
	Store_Env (Fast_Vector_Ref (thunk, THUNK_ENVIRONMENT));
	Store_Expression (Fast_Vector_Ref (thunk, THUNK_PROCEDURE));
	PRIMITIVE_ABORT (PRIM_DO_EXPRESSION);
	/*NOTREACHED*/
      }
    }
}

/* (EXECUTE-AT-NEW-STATE-POINT SPACE BEFORE DURING AFTER)
   Create a new state point in the specified state SPACE.  To enter
   the new point you must execute the BEFORE thunk.  On the way out,
   the AFTER thunk is executed.  If SPACE is NIL, then the microcode
   variable Current_State_Point is used to find the current state
   point and no state space is side-effected as the code runs.
*/
DEFINE_PRIMITIVE ("EXECUTE-AT-NEW-STATE-POINT", Prim_execute_at_new_point, 4, 4, 0)
{
  Pointer New_Point, Old_Point;
  Primitive_4_Args();

  guarantee_state_point();
  if (Arg1 == NIL)
    Old_Point = Current_State_Point;
  else
  {
    Arg_1_Type(TC_VECTOR);
    if (Vector_Ref(Arg1, STATE_SPACE_TAG) !=
        Get_Fixed_Obj_Slot(State_Space_Tag))
    {
      signal_error_from_primitive(ERR_ARG_1_WRONG_TYPE);
    }
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
  /*NOTREACHED*/
}

/* (MAKE-STATE-SPACE MUTABLE?)
   Creates a new state space for the dynamic winder.  Used only
   internally to the dynamic wind operations.  If the arugment
   is #!TRUE, then a real, mutable state space is created.
   Otherwise a (actually, THE) immutable space is created and
   the microcode will track motions in this space.
*/
DEFINE_PRIMITIVE ("MAKE-STATE-SPACE", Prim_make_state_space, 1, 1, 0)
{
  Pointer New_Point;
  Primitive_1_Arg();

  Primitive_GC_If_Needed(STATE_POINT_SIZE+STATE_SPACE_SIZE);
  New_Point = Make_Pointer(TC_VECTOR, Free);
  Free[STATE_POINT_HEADER] =
    Make_Non_Pointer(TC_MANIFEST_VECTOR, STATE_POINT_SIZE-1);
  Free[STATE_POINT_TAG] = Get_Fixed_Obj_Slot(State_Point_Tag);
  Free[STATE_POINT_BEFORE_THUNK] = NIL;
  Free[STATE_POINT_AFTER_THUNK] = NIL;
  Free[STATE_POINT_NEARER_POINT] = NIL;
  Free[STATE_POINT_DISTANCE_TO_ROOT] = Make_Unsigned_Fixnum(0);
  Free += STATE_POINT_SIZE;
  if (Arg1 == NIL)
  {
    Current_State_Point = New_Point;
    PRIMITIVE_RETURN( NIL);
  }
  else
  {
    Pointer New_Space;

    New_Space = Make_Pointer(TC_VECTOR, Free);
    Free[STATE_SPACE_HEADER] =
      Make_Non_Pointer(TC_MANIFEST_VECTOR, STATE_SPACE_SIZE-1);
    Free[STATE_SPACE_TAG] = Get_Fixed_Obj_Slot(State_Space_Tag);
    Free[STATE_SPACE_NEAREST_POINT] = New_Point;
    Free += STATE_SPACE_SIZE;
    Fast_Vector_Set(New_Point, STATE_POINT_NEARER_POINT, New_Space);
    PRIMITIVE_RETURN( New_Space);
  }
}

DEFINE_PRIMITIVE ("CURRENT-DYNAMIC-STATE", Prim_current_dynamic_state, 1, 1, 0)
{
  Primitive_1_Arg();

  guarantee_state_point();
  if (Arg1 == NIL)
  {
    PRIMITIVE_RETURN( Current_State_Point);
  }
  Arg_1_Type(TC_VECTOR);
  if (Fast_Vector_Ref(Arg1, STATE_SPACE_TAG) !=
      Get_Fixed_Obj_Slot(State_Space_Tag))
  {
    signal_error_from_primitive(ERR_ARG_1_WRONG_TYPE);
  }
  PRIMITIVE_RETURN( Vector_Ref(Arg1, STATE_SPACE_NEAREST_POINT));
}

DEFINE_PRIMITIVE ("SET-CURRENT-DYNAMIC-STATE!", Prim_set_dynamic_state, 1, 1, 0)
{
  Pointer State_Space, Result;
  Primitive_1_Arg();

  Arg_1_Type(TC_VECTOR);
  if (Fast_Vector_Ref(Arg1, STATE_POINT_TAG) !=
      Get_Fixed_Obj_Slot(State_Point_Tag))
    signal_error_from_primitive(ERR_ARG_1_WRONG_TYPE);
  State_Space = Find_State_Space(Arg1);
  if (State_Space == NIL)
  {
    guarantee_state_point();
    Result = Current_State_Point;
    Current_State_Point = Arg1;
  }
  else
  {
    Result = Vector_Ref(State_Space, STATE_SPACE_NEAREST_POINT);
    Vector_Set(State_Space, STATE_SPACE_NEAREST_POINT, Arg1);
  }
  PRIMITIVE_RETURN( Result);
}

/* (SCODE-EVAL SCODE-EXPRESSION ENVIRONMENT)
   Evaluate the piece of SCode (SCODE-EXPRESSION) in the
   ENVIRONMENT. This is like Eval, except that it expects its input
   to be syntaxed into SCode rather than just a list.
*/
DEFINE_PRIMITIVE ("SCODE-EVAL", Prim_scode_eval, 2, 2, 0)
{
  Primitive_2_Args();

  if (Type_Code(Arg2) != GLOBAL_ENV)
    Arg_2_Type(TC_ENVIRONMENT);
  Pop_Primitive_Frame(2);
  Store_Env(Arg2);
  Store_Expression(Arg1);
  PRIMITIVE_ABORT( PRIM_DO_EXPRESSION);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("GET-INTERRUPT-ENABLES", Prim_get_interrupt_enables, 0, 0,
  "Returns the current interrupt mask.")
{
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN (MAKE_UNSIGNED_FIXNUM (FETCH_INTERRUPT_MASK ()));
}

DEFINE_PRIMITIVE ("SET-INTERRUPT-ENABLES!", Prim_set_interrupt_enables, 1, 1,
  "Sets the interrupt mask to NEW-INT-ENABLES; returns previous mask value.\n\
See `mask_interrupt_enables' for more information on interrupts.")
{
  long previous;
  PRIMITIVE_HEADER (1);

  previous = (FETCH_INTERRUPT_MASK ());
  SET_INTERRUPT_MASK ((FIXNUM_ARG (1)) & INT_Mask);
  PRIMITIVE_RETURN (MAKE_UNSIGNED_FIXNUM (previous));
}

DEFINE_PRIMITIVE ("CLEAR-INTERRUPTS!", Prim_clear_interrupts, 1, 1, 
  "Clears the interrupt bits in the MASK argument.
The bits in MASK are interpreted as for `get-interrupt-enables'.")
{
  PRIMITIVE_HEADER (1);

  CLEAR_INTERRUPT ((FIXNUM_ARG (1)) & INT_Mask);
  PRIMITIVE_RETURN (SHARP_F);
}

/* (GET-FLUID-BINDINGS)
   Gets the microcode fluid-bindings variable.  */

DEFINE_PRIMITIVE ("GET-FLUID-BINDINGS", Prim_get_fluid_bindings, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN (Fluid_Bindings);
}

/* (SET-FLUID-BINDINGS! NEW-BINDINGS)
   Sets the microcode fluid-bindings variable.
   Returns the previous value.  */

DEFINE_PRIMITIVE ("SET-FLUID-BINDINGS!", Prim_set_fluid_bindings, 1, 1, 0)
{
  Pointer new_bindings;
  Pointer old_bindings;
  PRIMITIVE_HEADER (1);

  new_bindings = (ARG_REF (1));
  if (! ((new_bindings == NIL) || (PAIR_P (new_bindings))))
    error_wrong_type_arg (1);
  old_bindings = Fluid_Bindings;
  Fluid_Bindings = new_bindings;
  PRIMITIVE_RETURN (old_bindings);
}

/* (SET-CURRENT-HISTORY! TRIPLE)
   Begins recording history into TRIPLE.  The history structure is
   somewhat complex and should be understood before trying to use
   this primitive.  It is used in the Read-Eval-Print loop in the
   Scheme runtime system.

   This primitive pops its own frame and escapes back to the interpreter
   because it modifies one of the registers that the interpreter caches
   (History).

   The longjmp forces the interpreter to recache.  */

DEFINE_PRIMITIVE ("SET-CURRENT-HISTORY!", Prim_set_current_history, 1, 1, 0)
{
  Primitive_1_Arg();

  if (!(HUNK3_P(Arg1)))
    error_wrong_type_arg (1);

  Val = *History;
#ifdef COMPILE_HISTORY
  History = Get_Pointer(Arg1);
#else
  History = Get_Pointer(Get_Fixed_Obj_Slot(Dummy_History));
#endif
  Pop_Primitive_Frame( 1);
  PRIMITIVE_ABORT( PRIM_POP_RETURN);
  /*NOTREACHED*/
}

/* (SET-FIXED-OBJECTS-VECTOR! VECTOR)
   Replace the current fixed objects vector with VECTOR.  The fixed
   objects vector is used for communication between the Scheme
   runtime system and the interpreter.  The file UTABCSCM.SCM
   contains the names of the slots in the vector.  Returns (bad
   style to depend on this) the previous fixed objects vector.
*/
DEFINE_PRIMITIVE ("SET-FIXED-OBJECTS-VECTOR!", Prim_set_fixed_objects_vector, 1, 1, 0)
{
  Pointer Result;
  Primitive_1_Arg();

  Arg_1_Type(TC_VECTOR);
  if (Vector_Length(Arg1) < NFixed_Objects)
  {
    signal_error_from_primitive (ERR_ARG_1_BAD_RANGE);
  }

  if (Valid_Fixed_Obj_Vector())
  {
    Result = Get_Fixed_Obj_Slot(Me_Myself);
  }
  else
  {
    Result = NIL;
  }
  Set_Fixed_Obj_Hook(Arg1);
  Set_Fixed_Obj_Slot(Me_Myself, Arg1);
  PRIMITIVE_RETURN( Result);
}

/* (TRANSLATE-TO-STATE-POINT STATE_POINT)
   Move to a new dynamic wind environment by performing all of the
   necessary enter and exit forms to get from the current state to
   the new state as specified by STATE_POINT.
*/
DEFINE_PRIMITIVE ("TRANSLATE-TO-STATE-POINT", Prim_translate_to_point, 1, 1, 0)
{
  Primitive_1_Arg();

  Arg_1_Type(TC_VECTOR);
  if (Vector_Ref(Arg1, STATE_POINT_TAG) != Get_Fixed_Obj_Slot(State_Point_Tag))
    signal_error_from_primitive(ERR_ARG_1_WRONG_TYPE);
  Pop_Primitive_Frame(1);
  Translate_To_Point(Arg1);
  /*NOTREACHED*/
}

/* (WITH-HISTORY-DISABLED THUNK)
   THUNK must be a procedure or primitive procedure which takes no
   arguments.  Turns off the history collection mechanism.  Removes
   the most recent reduction (the expression which called the
   primitive) from the current history and saves the history.  Then
   it calls the THUNK.  When (if) the THUNK returns, the history is
   restored back and collection resumes.  The net result is that the
   THUNK is called with history collection turned off.
*/
DEFINE_PRIMITIVE ("WITH-HISTORY-DISABLED", Prim_with_history_disabled, 1, 1, 0)
{
  Pointer *First_Rib, *Rib, *Second_Rib;
  Primitive_1_Arg();

  /* Remove one reduction from the history before saving it */
  First_Rib = Get_Pointer(History[HIST_RIB]);
  Second_Rib = Get_Pointer(First_Rib[RIB_NEXT_REDUCTION]);
  if (!((HISTORY_MARKED_P(First_Rib[RIB_MARK])) ||
       (First_Rib == Second_Rib)))
  {
    HISTORY_MARK(Second_Rib[RIB_MARK]);
    for (Rib = First_Rib;
         Get_Pointer(Rib[RIB_NEXT_REDUCTION]) != First_Rib;
         Rib = Get_Pointer(Rib[RIB_NEXT_REDUCTION]))
    {
      /* Look for one that points to the first rib */
    }
    /* This maintains the mark in History[HIST_RIB] */
    History[HIST_RIB] = Make_Pointer(OBJECT_TYPE(History[HIST_RIB]), Rib);
  }
  Pop_Primitive_Frame(1);
  Stop_History();
 Will_Push(STACK_ENV_EXTRA_SLOTS+1);
  Push(Arg1);
  Push(STACK_FRAME_HEADER);
 Pushed();
  PRIMITIVE_ABORT( PRIM_APPLY);
  /*NOTREACHED*/
}

/* Called with a mask and a thunk */

DEFINE_PRIMITIVE ("WITH-INTERRUPT-MASK", Prim_with_interrupt_mask, 2, 2, 0)
{
  Pointer mask;
  Primitive_2_Args();

  Arg_1_Type(TC_FIXNUM);
  Pop_Primitive_Frame(2);
  mask = MAKE_SIGNED_FIXNUM(FETCH_INTERRUPT_MASK());
 Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS+2));
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(mask);
  Save_Cont();

  Push(mask);		/* Current interrupt mask */
  Push(Arg2);		/* Function to call */
  Push(STACK_FRAME_HEADER+1);
 Pushed();
  SET_INTERRUPT_MASK(INT_Mask & Get_Integer(Arg1));
  PRIMITIVE_ABORT( PRIM_APPLY);
  /*NOTREACHED*/
}

/* Called with a mask and a thunk */

DEFINE_PRIMITIVE ("WITH-INTERRUPTS-REDUCED", Prim_with_interrupts_reduced, 2, 2, 0)
{
  Pointer mask;
  long new_interrupt_mask, old_interrupt_mask;
  Primitive_2_Args();

  Arg_1_Type(TC_FIXNUM);
  Pop_Primitive_Frame(2);
  mask = MAKE_SIGNED_FIXNUM(FETCH_INTERRUPT_MASK());

 Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS+2));
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(mask);
  Save_Cont();

  Push(mask);		/* Current interrupt mask */
  Push(Arg2);		/* Function to call */
  Push(STACK_FRAME_HEADER+1);
 Pushed();
  new_interrupt_mask = (INT_Mask & Get_Integer( Arg1));
  old_interrupt_mask = FETCH_INTERRUPT_MASK();
  if (new_interrupt_mask > old_interrupt_mask)
  {
    SET_INTERRUPT_MASK(new_interrupt_mask);
  }
  else
  {
    SET_INTERRUPT_MASK(new_interrupt_mask & old_interrupt_mask);
  }
  PRIMITIVE_ABORT( PRIM_APPLY);
  /*NOTREACHED*/
}

/* (WITHIN-CONTROL-POINT CONTROL-POINT THUNK)
   THUNK must be a procedure or primitive procedure which takes no
   arguments.  Restores the state of the machine from the control
   point, and then calls the THUNK in this new state.
*/
DEFINE_PRIMITIVE ("WITHIN-CONTROL-POINT", Prim_within_control_point, 2, 2, 0)
{
  Primitive_2_Args();

  Arg_1_Type(TC_CONTROL_POINT);
  Our_Throw(false, Arg1);
  Within_Stacklet_Backout();
  Our_Throw_Part_2();
 Will_Push(STACK_ENV_EXTRA_SLOTS+1);
  Push(Arg2);
  Push(STACK_FRAME_HEADER);
 Pushed();
  PRIMITIVE_ABORT( PRIM_APPLY);
  /*NOTREACHED*/
}
