/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/hooks.c,v 9.39 1990/06/20 17:40:58 cph Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

/* This file contains various hooks and handles that connect the
   primitives with the main interpreter. */

#include "scheme.h"
#include "prims.h"
#include "winder.h"
#include "history.h"

DEFINE_PRIMITIVE ("APPLY", Prim_apply, 2, 2, 0)
{
  SCHEME_OBJECT procedure;
  SCHEME_OBJECT argument_list;
  fast long number_of_args;
#ifdef LOSING_PARALLEL_PROCESSOR
  SCHEME_OBJECT * saved_stack_pointer;
#endif
  PRIMITIVE_HEADER (2);
  procedure = (ARG_REF (1));
  argument_list = (ARG_REF (2));
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
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  {
    fast SCHEME_OBJECT scan_list;
    TOUCH_IN_PRIMITIVE (argument_list, scan_list);
    number_of_args = 0;
    while (PAIR_P (scan_list))
      {
	number_of_args += 1;
	TOUCH_IN_PRIMITIVE ((PAIR_CDR (scan_list)), scan_list);
      }
    if (scan_list != EMPTY_LIST)
      error_wrong_type_arg (2);
  }
#ifdef USE_STACKLETS
  /* This is conservative: if the number of arguments is large enough
     the Will_Push below may try to allocate space on the heap for the
     stack frame. */
  Primitive_GC_If_Needed
    (New_Stacklet_Size (number_of_args + STACK_ENV_EXTRA_SLOTS + 1));
#endif
  POP_PRIMITIVE_FRAME (2);
 Will_Push (number_of_args + STACK_ENV_EXTRA_SLOTS + 1);
#ifdef LOSING_PARALLEL_PROCESSOR
  saved_stack_pointer = Stack_Pointer;
#endif
  {
    fast long i;
    fast SCHEME_OBJECT * scan_stack = (STACK_LOC (- number_of_args));
    fast SCHEME_OBJECT scan_list;
    Stack_Pointer = scan_stack;
    TOUCH_IN_PRIMITIVE (argument_list, scan_list);
    for (i = number_of_args; (i > 0); i -= 1)
      {
#ifdef LOSING_PARALLEL_PROCESSOR
	/* This half-measure should be replaced by some kind of lock
	   or something else that guarantees that the code will win.  */
	/* Check for abominable case of someone bashing the arg list. */
	if (! (PAIR_P (scan_list)))
	  {
	    Stack_Pointer = saved_stack_pointer;
	    error_bad_range_arg (2);
	  }
#endif
	(*scan_stack++) = (PAIR_CAR (scan_list));
	TOUCH_IN_PRIMITIVE ((PAIR_CDR (scan_list)), scan_list);
      }
  }
  STACK_PUSH (procedure);
  STACK_PUSH (STACK_FRAME_HEADER + number_of_args);
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
   >>> return hook that may be in the stack. */

#define CWCC(return_code, reuse_flag, receiver_expression)		\
{									\
  SCHEME_OBJECT receiver = (receiver_expression);			\
  CWCC_1 ();								\
  POP_PRIMITIVE_FRAME (1);						\
  if (Return_Hook_Address != NULL)					\
    {									\
      (* Return_Hook_Address) = Old_Return_Code;			\
      Return_Hook_Address = NULL;					\
    }									\
  /* Put down frames to restore history and interrupts so that these	\
     operations will be performed on a throw. */			\
  Will_Push (CONTINUATION_SIZE + HISTORY_SIZE);				\
    Save_History (return_code);						\
    Store_Expression (LONG_TO_FIXNUM (FETCH_INTERRUPT_MASK()));		\
    Store_Return (RC_RESTORE_INT_MASK);					\
    Save_Cont ();							\
  Pushed ();								\
  /* There is no history to use since the				\
     last control point was formed. */					\
  Prev_Restore_History_Stacklet = NULL;					\
  Prev_Restore_History_Offset = 0;					\
  {									\
    SCHEME_OBJECT control_point;					\
    CWCC_2 (control_point, reuse_flag);					\
    /* we just cleared the stack so there MUST be room */		\
    /* Will_Push(3); */							\
    STACK_PUSH (control_point);						\
    STACK_PUSH (receiver);						\
    STACK_PUSH (STACK_FRAME_HEADER + 1);				\
    /*  Pushed(); */							\
  }									\
}

#ifdef USE_STACKLETS

#define CWCC_1()							\
{									\
  Primitive_GC_If_Needed (2 * Default_Stacklet_Size);			\
}

#define CWCC_2(target, reuse_flag)					\
{									\
  (target) = (Get_Current_Stacklet ());					\
  Allocate_New_Stacklet (3);						\
}

#else /* not USE_STACKLETS */

#define CWCC_1()							\
{									\
  Primitive_GC_If_Needed						\
    ((Stack_Top - Stack_Pointer) +					\
     STACKLET_HEADER_SIZE +						\
     CONTINUATION_SIZE +						\
     HISTORY_SIZE);							\
}

#define CWCC_2(target, reuse_flag)					\
{									\
  fast long n_words = (Stack_Top - Stack_Pointer);			\
  (target) =								\
    (allocate_marked_vector						\
     (TC_CONTROL_POINT,							\
      (n_words + (STACKLET_HEADER_SIZE - 1)),				\
      false));								\
  FAST_MEMORY_SET ((target), STACKLET_REUSE_FLAG, (reuse_flag));	\
  FAST_MEMORY_SET							\
    ((target),								\
     STACKLET_UNUSED_LENGTH,						\
     (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0)));				\
  {									\
    fast SCHEME_OBJECT * scan =						\
      (MEMORY_LOC ((target), STACKLET_HEADER_SIZE));			\
    while ((n_words--) > 0)						\
      (*scan++) = (STACK_POP ());					\
  }									\
  if (Consistency_Check && (Stack_Pointer != Stack_Top))		\
    Microcode_Termination (TERM_BAD_STACK);				\
 Will_Push (CONTINUATION_SIZE);						\
  Store_Return (RC_JOIN_STACKLETS);					\
  Store_Expression (target);						\
  Save_Cont ();								\
 Pushed ();								\
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
   and clears the appropriate reuse flags for copying. */

DEFINE_PRIMITIVE ("CALL-WITH-CURRENT-CONTINUATION", Prim_catch, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  CWCC (RC_RESTORE_HISTORY, SHARP_F, (ARG_REF (1)));
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("NON-REENTRANT-CALL-WITH-CURRENT-CONTINUATION", Prim_non_reentrant_catch, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT();
#ifdef USE_STACKLETS
  CWCC (RC_RESTORE_DONT_COPY_HISTORY, SHARP_T, (ARG_REF (1)));
#else
  /* When there are no stacklets, it is identical to the reentrant version. */
  CWCC (RC_RESTORE_HISTORY, SHARP_F, (ARG_REF (1)));
#endif
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("WITHIN-CONTROL-POINT", Prim_within_control_point, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_CANONICALIZE_CONTEXT();
  CHECK_ARG (1, CONTROL_POINT_P);
  {
    fast SCHEME_OBJECT control_point = (ARG_REF (1));
    SCHEME_OBJECT thunk = (ARG_REF (2));
    Our_Throw (false, control_point);
    Within_Stacklet_Backout ();
    Our_Throw_Part_2 ();
  Will_Push (STACK_ENV_EXTRA_SLOTS + 1);
    STACK_PUSH (thunk);
    STACK_PUSH (STACK_FRAME_HEADER);
  Pushed ();
  }
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("ERROR-PROCEDURE", Prim_error_procedure, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  {
    fast SCHEME_OBJECT message = (ARG_REF (1));
    fast SCHEME_OBJECT irritants = (ARG_REF (2));
    fast SCHEME_OBJECT environment = (ARG_REF (3));
    /* This is done outside the Will_Push because the space for it
       is guaranteed by the interpreter before it gets here.
       If done inside, this could break when using stacklets. */
    Back_Out_Of_Primitive ();
    Save_Cont ();
  Will_Push (HISTORY_SIZE + STACK_ENV_EXTRA_SLOTS + 4);
    Stop_History ();
    /* Stepping should be cleared here! */
    STACK_PUSH (environment);
    STACK_PUSH (irritants);
    STACK_PUSH (message);
    STACK_PUSH (Get_Fixed_Obj_Slot (Error_Procedure));
    STACK_PUSH (STACK_FRAME_HEADER + 3);
  Pushed ();
    PRIMITIVE_ABORT (PRIM_APPLY);
    /*NOTREACHED*/
  }
}

DEFINE_PRIMITIVE ("SCODE-EVAL", Prim_scode_eval, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  CHECK_ARG (2, ENVIRONMENT_P);
  {
    fast SCHEME_OBJECT expression = (ARG_REF (1));
    fast SCHEME_OBJECT environment = (ARG_REF (2));
    POP_PRIMITIVE_FRAME (2);
    Store_Env (environment);
    Store_Expression (expression);
  }
  PRIMITIVE_ABORT (PRIM_DO_EXPRESSION);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("FORCE", Prim_force, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, PROMISE_P);
  {
    fast SCHEME_OBJECT thunk = (ARG_REF (1));
    switch (MEMORY_REF (thunk, THUNK_SNAPPED))
      {
      case SHARP_T:
	PRIMITIVE_RETURN (MEMORY_REF (thunk, THUNK_VALUE));

      case FIXNUM_ZERO:
	{
	  /* New-style thunk used by compiled code. */
	  PRIMITIVE_CANONICALIZE_CONTEXT();
	  POP_PRIMITIVE_FRAME (1);
	Will_Push (CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS + 1);
	  Store_Return (RC_SNAP_NEED_THUNK);
	  Store_Expression (thunk);
	  Save_Cont ();
	  STACK_PUSH (MEMORY_REF (thunk, THUNK_VALUE));
	  STACK_PUSH (STACK_FRAME_HEADER);
	Pushed ();
	  PRIMITIVE_ABORT (PRIM_APPLY);
	  /*NOTREACHED*/
	}

      default:
	{
	  /* Old-style thunk used by interpreted code. */
	  PRIMITIVE_CANONICALIZE_CONTEXT();
	  POP_PRIMITIVE_FRAME (1);
	Will_Push (CONTINUATION_SIZE);
	  Store_Return (RC_SNAP_NEED_THUNK);
	  Store_Expression (thunk);
	  Save_Cont ();
	Pushed ();
	  Store_Env (FAST_MEMORY_REF (thunk, THUNK_ENVIRONMENT));
	  Store_Expression (FAST_MEMORY_REF (thunk, THUNK_PROCEDURE));
	  PRIMITIVE_ABORT (PRIM_DO_EXPRESSION);
	  /*NOTREACHED*/
	}
      }
  }
}

/* State Space Implementation */

DEFINE_PRIMITIVE ("EXECUTE-AT-NEW-STATE-POINT", Prim_execute_at_new_point, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);

  PRIMITIVE_CANONICALIZE_CONTEXT ();
  guarantee_state_point ();
  {
    SCHEME_OBJECT old_point;
    if ((ARG_REF (1)) == SHARP_F)
      old_point = Current_State_Point;
    else
      {
	CHECK_ARG (1, STATE_SPACE_P);
	old_point =
	  (FAST_MEMORY_REF ((ARG_REF (1)), STATE_SPACE_NEAREST_POINT));
      }
    {
      SCHEME_OBJECT new_point =
	(allocate_marked_vector (TC_VECTOR, STATE_POINT_LENGTH, true));
      SCHEME_OBJECT during_thunk = (ARG_REF (3));
      FAST_MEMORY_SET
	(new_point, STATE_POINT_TAG, (Get_Fixed_Obj_Slot (State_Point_Tag)));
      FAST_MEMORY_SET (new_point, STATE_POINT_BEFORE_THUNK, (ARG_REF (2)));
      FAST_MEMORY_SET (new_point, STATE_POINT_AFTER_THUNK, (ARG_REF (4)));
      FAST_MEMORY_SET (new_point, STATE_POINT_NEARER_POINT, old_point);
      FAST_MEMORY_SET
	(new_point,
	 STATE_POINT_DISTANCE_TO_ROOT,
	 (1 + (FAST_MEMORY_REF (old_point, STATE_POINT_DISTANCE_TO_ROOT))));

      POP_PRIMITIVE_FRAME (4);
    Will_Push((2 * CONTINUATION_SIZE) + (STACK_ENV_EXTRA_SLOTS + 1));
      /* Push a continuation to go back to the current state after the
	 body is evaluated */
      Store_Expression (old_point);
      Store_Return (RC_RESTORE_TO_STATE_POINT);
      Save_Cont ();
      /* Push a stack frame which will call the body after we have moved
	 into the new state point */
      STACK_PUSH (during_thunk);
      STACK_PUSH (STACK_FRAME_HEADER);
      /* Push the continuation to go with the stack frame */
      Store_Expression (SHARP_F);
      Store_Return (RC_INTERNAL_APPLY);
      Save_Cont ();
    Pushed ();
      Translate_To_Point (new_point);
      /*NOTREACHED*/
    }
  }
}

DEFINE_PRIMITIVE ("TRANSLATE-TO-STATE-POINT", Prim_translate_to_point, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  CHECK_ARG (1, STATE_POINT_P);
  {
    SCHEME_OBJECT state_point = (ARG_REF (1));
    POP_PRIMITIVE_FRAME (1);
    Translate_To_Point (state_point);
    /*NOTREACHED*/
  }
}

DEFINE_PRIMITIVE ("MAKE-STATE-SPACE", Prim_make_state_space, 1, 1,
  "Return a newly-allocated state-space.\n\
Argument MUTABLE?, if not #F, means return a mutable state-space.\n\
Otherwise, -the- immutable state-space is saved internally.")
{
  PRIMITIVE_HEADER (1);
  {
    fast SCHEME_OBJECT new_point =
      (allocate_marked_vector (TC_VECTOR, STATE_POINT_LENGTH, true));
    FAST_MEMORY_SET
      (new_point, STATE_POINT_TAG, (Get_Fixed_Obj_Slot (State_Point_Tag)));
    FAST_MEMORY_SET (new_point, STATE_POINT_BEFORE_THUNK, SHARP_F);
    FAST_MEMORY_SET (new_point, STATE_POINT_AFTER_THUNK, SHARP_F);
    FAST_MEMORY_SET
      (new_point, STATE_POINT_DISTANCE_TO_ROOT, (LONG_TO_UNSIGNED_FIXNUM (0)));
    if ((ARG_REF (1)) == SHARP_F)
      {
	FAST_MEMORY_SET (new_point, STATE_POINT_NEARER_POINT, SHARP_F);
	Current_State_Point = new_point;
	PRIMITIVE_RETURN (SHARP_F);
      }
    else
      {
	fast SCHEME_OBJECT new_space =
	  (allocate_marked_vector (TC_VECTOR, STATE_SPACE_LENGTH, true));
	FAST_MEMORY_SET
	  (new_space, STATE_SPACE_TAG, (Get_Fixed_Obj_Slot (State_Space_Tag)));
	FAST_MEMORY_SET (new_space, STATE_SPACE_NEAREST_POINT, new_point);
	FAST_MEMORY_SET (new_point, STATE_POINT_NEARER_POINT, new_space);
	PRIMITIVE_RETURN (new_space);
      }
  }
}

DEFINE_PRIMITIVE ("CURRENT-DYNAMIC-STATE", Prim_current_dynamic_state, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  guarantee_state_point ();
  if ((ARG_REF (1)) == SHARP_F)
    PRIMITIVE_RETURN (Current_State_Point);
  CHECK_ARG (1, STATE_SPACE_P);
  PRIMITIVE_RETURN (MEMORY_REF ((ARG_REF (1)), STATE_SPACE_NEAREST_POINT));
}

DEFINE_PRIMITIVE ("SET-CURRENT-DYNAMIC-STATE!", Prim_set_dynamic_state, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STATE_POINT_P);
  {
    fast SCHEME_OBJECT state_point = (ARG_REF (1));
    fast SCHEME_OBJECT state_space = (Find_State_Space (state_point));
    fast SCHEME_OBJECT result;
    if (state_space == SHARP_F)
      {
	guarantee_state_point ();
	result = Current_State_Point;
	Current_State_Point = state_point;
      }
    else
      {
	result = (MEMORY_REF (state_space, STATE_SPACE_NEAREST_POINT));
	MEMORY_SET (state_space, STATE_SPACE_NEAREST_POINT, state_point);
      }
    PRIMITIVE_RETURN (result);
  }
}

/* Interrupts */

DEFINE_PRIMITIVE ("GET-INTERRUPT-ENABLES", Prim_get_interrupt_enables, 0, 0,
  "Returns the current interrupt mask.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (FETCH_INTERRUPT_MASK ()));
}

DEFINE_PRIMITIVE ("SET-INTERRUPT-ENABLES!", Prim_set_interrupt_enables, 1, 1,
  "Sets the interrupt mask to NEW-INT-ENABLES; returns previous mask value.\n\
See `mask_interrupt_enables' for more information on interrupts.")
{
  PRIMITIVE_HEADER (1);
  {
    long previous = (FETCH_INTERRUPT_MASK ());
    SET_INTERRUPT_MASK ((arg_integer (1)) & INT_Mask);
    PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (previous));
  }
}

DEFINE_PRIMITIVE ("CLEAR-INTERRUPTS!", Prim_clear_interrupts, 1, 1,
  "Clears the interrupt bits in the MASK argument.\n\
The bits in MASK are interpreted as for `get-interrupt-enables'.")
{
  PRIMITIVE_HEADER (1);
  CLEAR_INTERRUPT ((arg_integer (1)) & INT_Mask);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DISABLE-INTERRUPTS!", Prim_disable_interrupts, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    fast long previous = (FETCH_INTERRUPT_MASK ());
    SET_INTERRUPT_MASK (previous &~ ((arg_integer (1)) & INT_Mask));
    PRIMITIVE_RETURN (LONG_TO_FIXNUM (previous));
  }
}

DEFINE_PRIMITIVE ("ENABLE-INTERRUPTS!", Prim_enable_interrupts, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    fast long previous = (FETCH_INTERRUPT_MASK ());
    SET_INTERRUPT_MASK (previous | ((arg_integer (1)) & INT_Mask));
    PRIMITIVE_RETURN (LONG_TO_FIXNUM (previous));
  }
}

DEFINE_PRIMITIVE ("WITH-INTERRUPT-MASK", Prim_with_interrupt_mask, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  {
    long new_mask = (INT_Mask & (arg_integer (1)));
    SCHEME_OBJECT thunk = (ARG_REF (2));
    SCHEME_OBJECT old_mask = (LONG_TO_FIXNUM (FETCH_INTERRUPT_MASK ()));
    POP_PRIMITIVE_FRAME (2);
  Will_Push (CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS + 2);
    Store_Return (RC_RESTORE_INT_MASK);
    Store_Expression (old_mask);
    Save_Cont ();
    STACK_PUSH (old_mask);
    STACK_PUSH (thunk);
    STACK_PUSH (STACK_FRAME_HEADER + 1);
  Pushed ();
    SET_INTERRUPT_MASK (new_mask);
    PRIMITIVE_ABORT (PRIM_APPLY);
    /*NOTREACHED*/
  }
}

DEFINE_PRIMITIVE ("WITH-INTERRUPTS-REDUCED", Prim_with_interrupts_reduced, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_CANONICALIZE_CONTEXT();
  {
    long new_mask = (INT_Mask & (arg_integer (1)));
    SCHEME_OBJECT thunk = (ARG_REF (2));
    long old_mask = (FETCH_INTERRUPT_MASK ());
    POP_PRIMITIVE_FRAME (2);
  Will_Push (CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS + 2);
    Store_Return (RC_RESTORE_INT_MASK);
    Store_Expression (old_mask);
    Save_Cont ();
    STACK_PUSH (LONG_TO_FIXNUM (old_mask));
    STACK_PUSH (thunk);
    STACK_PUSH (STACK_FRAME_HEADER + 1);
  Pushed ();
    SET_INTERRUPT_MASK
      ((new_mask > old_mask) ? new_mask : (new_mask & old_mask));
    PRIMITIVE_ABORT (PRIM_APPLY);
    /*NOTREACHED*/
  }
}

/* History */

SCHEME_OBJECT
initialize_history ()
{
  /* Dummy History Structure */
  History = (Make_Dummy_History ());
  return
    (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, (Make_Dummy_History ())));
}

DEFINE_PRIMITIVE ("SET-CURRENT-HISTORY!", Prim_set_current_history, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  CHECK_ARG (1, HUNK3_P);
  Val = (*History);
#ifdef COMPILE_HISTORY
  History = (OBJECT_ADDRESS (ARG_REF (1)));
#else
  History = (OBJECT_ADDRESS (Get_Fixed_Obj_Slot (Dummy_History)));
#endif
  POP_PRIMITIVE_FRAME (1);
  PRIMITIVE_ABORT (PRIM_POP_RETURN);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("WITH-HISTORY-DISABLED", Prim_with_history_disabled, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  {
    SCHEME_OBJECT thunk = (ARG_REF (1));
    /* Remove one reduction from the history before saving it */
    SCHEME_OBJECT * first_rib = (OBJECT_ADDRESS (History [HIST_RIB]));
    SCHEME_OBJECT * second_rib =
      (OBJECT_ADDRESS (first_rib [RIB_NEXT_REDUCTION]));
    if ((first_rib != second_rib) &&
	(! (HISTORY_MARKED_P (first_rib [RIB_MARK]))))
      {
	HISTORY_MARK (second_rib [RIB_MARK]);
	{
	  SCHEME_OBJECT * rib = first_rib;
	  while (1)
	    {
	      fast SCHEME_OBJECT * next_rib =
		(OBJECT_ADDRESS (rib [RIB_NEXT_REDUCTION]));
	      if (next_rib == first_rib)
		break;
	      rib = next_rib;
	    }
	  /* This maintains the mark in (History [HIST_RIB]). */
	  (History [HIST_RIB]) =
	    (MAKE_POINTER_OBJECT ((OBJECT_TYPE (History [HIST_RIB])), rib));
	}
      }
    POP_PRIMITIVE_FRAME (1);
    Stop_History ();
  Will_Push (STACK_ENV_EXTRA_SLOTS + 1);
    STACK_PUSH (thunk);
    STACK_PUSH (STACK_FRAME_HEADER);
  Pushed ();
    PRIMITIVE_ABORT (PRIM_APPLY);
    /*NOTREACHED*/
  }
}

/* Miscellaneous State */

DEFINE_PRIMITIVE ("GET-FLUID-BINDINGS", Prim_get_fluid_bindings, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (Fluid_Bindings);
}

DEFINE_PRIMITIVE ("SET-FLUID-BINDINGS!", Prim_set_fluid_bindings, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, APPARENT_LIST_P);
  {
    SCHEME_OBJECT old_bindings = Fluid_Bindings;
    Fluid_Bindings = (ARG_REF (1));
    PRIMITIVE_RETURN (old_bindings);
  }
}

DEFINE_PRIMITIVE ("GET-FIXED-OBJECTS-VECTOR", Prim_get_fixed_objects_vector, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  if (Valid_Fixed_Obj_Vector ())
    PRIMITIVE_RETURN (Get_Fixed_Obj_Slot (Me_Myself));
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("SET-FIXED-OBJECTS-VECTOR!", Prim_set_fixed_objects_vector, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, VECTOR_P);
  {
    fast SCHEME_OBJECT vector = (ARG_REF (1));
    if ((VECTOR_LENGTH (vector)) < NFixed_Objects)
      error_bad_range_arg (1);
    {
      SCHEME_OBJECT result =
	((Valid_Fixed_Obj_Vector ())
	 ? (Get_Fixed_Obj_Slot (Me_Myself))
	 : SHARP_F);
      Set_Fixed_Obj_Hook (vector);
      Set_Fixed_Obj_Slot (Me_Myself, vector);
      PRIMITIVE_RETURN (result);
    }
  }
}
