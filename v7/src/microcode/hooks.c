/* -*-C-*-

$Id: hooks.c,v 9.67 2007/01/05 15:33:06 cph Exp $

Copyright (c) 1988-2002 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* This file contains various hooks and handles that connect the
   primitives with the main interpreter. */

#include "scheme.h"
#include "prims.h"
#include "winder.h"
#include "history.h"

DEFINE_PRIMITIVE ("APPLY", Prim_apply, 2, 2,
		  "(PROCEDURE LIST-OF-ARGS)\n\
Invoke PROCEDURE on the arguments contained in list-of-ARGS.")
{
  SCHEME_OBJECT procedure;
  SCHEME_OBJECT argument_list;
  fast long number_of_args;
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
     is sufficiently high that it probably makes up for the time saved.
   */
  {
    fast SCHEME_OBJECT scan_list, scan_list_trail;
    TOUCH_IN_PRIMITIVE (argument_list, scan_list);
    if (! (PAIR_P (scan_list)))
      number_of_args = 0;
    else
    {
      number_of_args = 1;
      scan_list_trail = scan_list;
      TOUCH_IN_PRIMITIVE ((PAIR_CDR (scan_list)), scan_list);
      while (true)
      {
	if (scan_list == scan_list_trail)
	  error_bad_range_arg (2);
	if (! (PAIR_P (scan_list)))
	  break;
	TOUCH_IN_PRIMITIVE ((PAIR_CDR (scan_list)), scan_list);
	if (scan_list == scan_list_trail)
	  error_bad_range_arg (2);
	if (! (PAIR_P (scan_list)))
	{
	  number_of_args += 1;
	  break;
	}
	TOUCH_IN_PRIMITIVE ((PAIR_CDR (scan_list)), scan_list);
	scan_list_trail = (PAIR_CDR (scan_list_trail));
	number_of_args += 2;
      }
    }
    if (!EMPTY_LIST_P (scan_list))
      error_wrong_type_arg (2);
  }

#ifdef USE_STACKLETS
  /* This is conservative: if the number of arguments is large enough
     the Will_Push below may try to allocate space on the heap for the
     stack frame. */
  Primitive_GC_If_Needed
    (New_Stacklet_Size (number_of_args + STACK_ENV_EXTRA_SLOTS + 1));
#endif /* USE_STACKLETS */

#ifdef USE_STACKLETS
  POP_PRIMITIVE_FRAME (2);
 Will_Push (number_of_args + STACK_ENV_EXTRA_SLOTS + 1);
#else
  /* Don't use Will_Push for this -- if the length of the list is too
     large to fit on the stack, it could cause Scheme to terminate.  */
  if ((sp_register - (number_of_args + STACK_ENV_EXTRA_SLOTS + 1))
      <= Stack_Guard)
    error_bad_range_arg (2);
  POP_PRIMITIVE_FRAME (2);
#endif
  {
    fast long i;
    fast SCHEME_OBJECT * scan_stack = (STACK_LOC (- number_of_args));
    fast SCHEME_OBJECT scan_list;
    TOUCH_IN_PRIMITIVE (argument_list, scan_list);
    for (i = number_of_args; (i > 0); i -= 1)
    {
#ifdef LOSING_PARALLEL_PROCESSOR
      /* This half-measure should be replaced by some kind of lock
	 or something else that guarantees that the code will win.  */
      /* Check for abominable case of someone bashing the arg list. */
      if (! (PAIR_P (scan_list)))
      {
	/* Re-push the primitive's frame. */
	STACK_PUSH (argument_list);
	STACK_PUSH (procedure);
	error_bad_range_arg (2);
      }
#endif /* LOSING_PARALLEL_PROCESSOR */
      (*scan_stack++) = (PAIR_CAR (scan_list));
      TOUCH_IN_PRIMITIVE ((PAIR_CDR (scan_list)), scan_list);
    }
  }
  sp_register = (STACK_LOC (- number_of_args));
  STACK_PUSH (procedure);
  STACK_PUSH (STACK_FRAME_HEADER + number_of_args);
#ifdef USE_STACKLETS
 Pushed ();
#endif

  if (COMPILED_CODE_ADDRESS_P (STACK_REF (number_of_args + 2)))
  {
    extern SCHEME_OBJECT EXFUN (apply_compiled_from_primitive, (int));
    PRIMITIVE_RETURN (apply_compiled_from_primitive (2));
  }

  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
  return (0);
}

/* CALL-WITH-CURRENT-CONTINUATION

   Implementation detail: in addition to setting aside the old
   stacklet on a catch, the new stacklet is cleared and a return
   code is placed at the base of the (now clear) stack indicating
   that a return back through here requires restoring the stacklet.
   The current enabled interrupts are also saved in the old stacklet.

   >>> Temporarily (maybe) the act of doing a CATCH will disable any
   >>> return hook that may be in the stack.
 */

#ifdef USE_STACKLETS

#define CWCC_STACK_SIZE()		(2 * Default_Stacklet_Size)
#define NON_REENTRANT_RC_RESTORE	RC_RESTORE_DONT_COPY_HISTORY
#define NON_REENTRANT_FLAG		SHARP_T

#else /* not USE_STACKLETS */

#define CWCC_STACK_SIZE()						\
  ((Stack_Top - sp_register) + STACKLET_HEADER_SIZE			\
   + CONTINUATION_SIZE + HISTORY_SIZE)

/* When there are no stacklets, the two versions of CWCC are identical. */

#define NON_REENTRANT_RC_RESTORE	RC_RESTORE_HISTORY
#define NON_REENTRANT_FLAG		SHARP_F

#endif /* USE_STACKLETS */

void
DEFUN (CWCC, (return_code, reuse_flag, receiver),
       long return_code
       AND SCHEME_OBJECT reuse_flag
       AND SCHEME_OBJECT receiver)
{
  SCHEME_OBJECT control_point;

  Primitive_GC_If_Needed (CWCC_STACK_SIZE ());
  POP_PRIMITIVE_FRAME (1);
  if (Return_Hook_Address != NULL)
  {
    (* Return_Hook_Address) = Old_Return_Code;
    Return_Hook_Address = NULL;
  }

  /* Tail recursion hacking in CWCC.
     If the current stack contains only a frame to restore
     another control point that looks like the result of CWCC,
     then there is no need to push anything else on the stack
     or cons anything on the heap.

     This hackery would be considerably simpler if the interrupt
     mask and history information were kept explicitly instead
     of implicitly (pushed with appropriate restore return codes).
   */

  if (((STACK_LOC (CONTINUATION_SIZE)) == (Get_End_Of_Stacklet ()))
      && ((OBJECT_DATUM (STACK_REF (CONTINUATION_RETURN_CODE)))
	  == RC_JOIN_STACKLETS))
  {
    control_point = (STACK_REF (CONTINUATION_EXPRESSION));

    if (((OBJECT_TYPE (control_point)) == TC_CONTROL_POINT)
	&& ((reuse_flag == SHARP_F)
	    || ((MEMORY_REF (control_point, STACKLET_REUSE_FLAG))
		== SHARP_F)))
    {
      SCHEME_OBJECT * prev_stack
	= (MEMORY_LOC (control_point,
		       (STACKLET_HEADER_SIZE
			+ (OBJECT_DATUM (MEMORY_REF
					 (control_point,
					  STACKLET_UNUSED_LENGTH))))));
      SCHEME_OBJECT * ret_ptr
	= (STACK_LOCATIVE_OFFSET (prev_stack,
				  (CONTINUATION_SIZE
				   + CONTINUATION_RETURN_CODE)));

      if ((ret_ptr
	   <= (VECTOR_LOC (control_point, (VECTOR_LENGTH (control_point)))))
	  && ((OBJECT_DATUM (STACK_LOCATIVE_REFERENCE
			     (prev_stack,
			      CONTINUATION_RETURN_CODE)))
	      == RC_RESTORE_INT_MASK))
      {
	long ret_code = (OBJECT_DATUM (*ret_ptr));

	if ((ret_code == RC_RESTORE_HISTORY) || (ret_code == return_code))
	{
	  history_register
	    = (OBJECT_ADDRESS (Get_Fixed_Obj_Slot (Dummy_History)));
	  STACK_RESET ();
	  /* Will_Push(3); */
	  STACK_PUSH (control_point);
	  STACK_PUSH (receiver);
	  STACK_PUSH (STACK_FRAME_HEADER + 1);
	  /*  Pushed(); */

	  PRIMITIVE_ABORT (PRIM_APPLY);
	  /*NOTREACHED*/
	}
      }
    }
  }

  /*
    Put down frames to restore history and interrupts so that these
    operations will be performed on a throw.
   */
 Will_Push (HISTORY_SIZE);
  Save_History (return_code);
 Pushed ();
  preserve_interrupt_mask ();
  /* There is no history to use since the
     last control point was formed.
   */
  Prev_Restore_History_Stacklet = NULL;
  Prev_Restore_History_Offset = 0;

#ifdef USE_STACKLETS
  {
    control_point = (Get_Current_Stacklet ());
    Allocate_New_Stacklet (3);
  }
#else /* not USE_STACKLETS */
  {
    fast long n_words = (Stack_Top - sp_register);
    control_point = (allocate_marked_vector
		     (TC_CONTROL_POINT,
		      (n_words + (STACKLET_HEADER_SIZE - 1)),
		      false));
    FAST_MEMORY_SET (control_point, STACKLET_REUSE_FLAG, reuse_flag);
    FAST_MEMORY_SET (control_point,
		     STACKLET_UNUSED_LENGTH,
		     (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0)));
    {
      fast SCHEME_OBJECT * scan =
	(MEMORY_LOC (control_point, STACKLET_HEADER_SIZE));
      while ((n_words--) > 0)
	(*scan++) = (STACK_POP ());
    }
    if (Consistency_Check && (sp_register != Stack_Top))
      Microcode_Termination (TERM_BAD_STACK);
    CLEAR_INTERRUPT (INT_Stack_Overflow);
    STACK_RESET ();
    Will_Push (CONTINUATION_SIZE);
    Store_Return (RC_JOIN_STACKLETS);
    exp_register = control_point;
    Save_Cont ();
    Pushed ();
  }
#endif /* USE_STACKLETS */

  /* we just cleared the stack so there MUST be room */
  /* Will_Push(3); */
  STACK_PUSH (control_point);
  STACK_PUSH (receiver);
  STACK_PUSH (STACK_FRAME_HEADER + 1);
  /*  Pushed(); */

  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
}

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

DEFINE_PRIMITIVE ("CALL-WITH-CURRENT-CONTINUATION", Prim_catch, 1, 1,
		  "(RECEIVER)\n\
Invoke RECEIVER with a reentrant copy of the current control stack.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  CWCC (RC_RESTORE_HISTORY, SHARP_F, (ARG_REF (1)));
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("NON-REENTRANT-CALL-WITH-CURRENT-CONTINUATION",
		  Prim_non_reentrant_catch, 1, 1,
		  "(RECEIVER)\n\
Invoke RECEIVER with a non-reentrant copy of the current control stack.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT();
  CWCC (NON_REENTRANT_RC_RESTORE, NON_REENTRANT_FLAG, (ARG_REF (1)));
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* (WITHIN-CONTROL-POINT control-point thunk)

   Invoke THUNK (a procedure of no arguments) with CONTROL-POINT as
   the pending stack.  control-point is created by CWCC.
   The restoration of the stack is delayed until THUNK returns.
   If THUNK never returns (it diverges or throws elsewhere),
   the stack is never restored.
   WITHIN-CONTROL-POINT clears the current stack, pushes a frame
   that restores control-point when THUNK returns, and sets up
   an apply frame for THUNK.
 */   

DEFINE_PRIMITIVE ("WITHIN-CONTROL-POINT", Prim_within_control_point, 2, 2,
		  "(CONTROL-POINT THUNK)\n\
Invoke THUNK with CONTROL-POINT as its control stack.")
{
  SCHEME_OBJECT control_point, thunk;
  PRIMITIVE_HEADER (2);

  PRIMITIVE_CANONICALIZE_CONTEXT();
  CHECK_ARG (1, CONTROL_POINT_P);
  control_point = (ARG_REF (1));
  thunk = (ARG_REF (2));

  /* This KNOWS the direction of stack growth. */
  sp_register = (Get_End_Of_Stacklet ());
  /* We've discarded the history with the stack contents.  */
  Prev_Restore_History_Stacklet = NULL;
  Prev_Restore_History_Offset = 0;
  CLEAR_INTERRUPT (INT_Stack_Overflow);

 Will_Push (CONTINUATION_SIZE);
  exp_register = control_point;
  Store_Return (RC_JOIN_STACKLETS);
  Save_Cont ();
 Pushed ();

 Will_Push (STACK_ENV_EXTRA_SLOTS + 1);
  STACK_PUSH (thunk);
  STACK_PUSH (STACK_FRAME_HEADER);
 Pushed ();

  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("ERROR-PROCEDURE", Prim_error_procedure, 3, 3,
		  "(MESSAGE IRRITANTS ENVIRONMENT)\nSignal an error.")
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
    back_out_of_primitive ();
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
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("SCODE-EVAL", Prim_scode_eval, 2, 2,
		  "(SCODE-EXPRESSION ENVIRONMENT)\n\
Evaluate SCODE-EXPRESSION in ENVIRONMENT.")
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  CHECK_ARG (2, ENVIRONMENT_P);
  {
    fast SCHEME_OBJECT expression = (ARG_REF (1));
    fast SCHEME_OBJECT environment = (ARG_REF (2));
    POP_PRIMITIVE_FRAME (2);
    env_register = environment;
    exp_register = expression;
  }
  PRIMITIVE_ABORT (PRIM_DO_EXPRESSION);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FORCE", Prim_force, 1, 1,
		  "(PROMISE)\n\
Return the value memoized in PROMISE, computing it if it has not been\n\
memoized yet.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, PROMISE_P);
  {
    fast SCHEME_OBJECT thunk = (ARG_REF (1));
    fast SCHEME_OBJECT State = (MEMORY_REF (thunk, THUNK_SNAPPED));
    if (State == SHARP_T)
      PRIMITIVE_RETURN (MEMORY_REF (thunk, THUNK_VALUE));
    else if (State ==  FIXNUM_ZERO)
    {
      /* New-style thunk used by compiled code. */
      PRIMITIVE_CANONICALIZE_CONTEXT();
      POP_PRIMITIVE_FRAME (1);
     Will_Push (CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS + 1);
      Store_Return (RC_SNAP_NEED_THUNK);
      exp_register = thunk;
      Save_Cont ();
      STACK_PUSH (MEMORY_REF (thunk, THUNK_VALUE));
      STACK_PUSH (STACK_FRAME_HEADER);
     Pushed ();
      PRIMITIVE_ABORT (PRIM_APPLY);
      /*NOTREACHED*/
      PRIMITIVE_RETURN (UNSPECIFIC);
    }
    else
    {
      /* Old-style thunk used by interpreted code. */
      PRIMITIVE_CANONICALIZE_CONTEXT();
      POP_PRIMITIVE_FRAME (1);
     Will_Push (CONTINUATION_SIZE);
      Store_Return (RC_SNAP_NEED_THUNK);
      exp_register = thunk;
      Save_Cont ();
     Pushed ();
      env_register = (FAST_MEMORY_REF (thunk, THUNK_ENVIRONMENT));
      exp_register = (FAST_MEMORY_REF (thunk, THUNK_PROCEDURE));
      PRIMITIVE_ABORT (PRIM_DO_EXPRESSION);
      /*NOTREACHED*/
      PRIMITIVE_RETURN (UNSPECIFIC);
    }
  }
}

/* State Space Implementation */

DEFINE_PRIMITIVE ("EXECUTE-AT-NEW-STATE-POINT",
		  Prim_execute_at_new_point, 4, 4,
		  "(OLD-STATE-POINT BEFORE-THUNK DURING-THUNK AFTER-THUNK)\n\
Invoke DURING-THUNK in a new state point defined by the transition\n\
<BEFORE-THUNK, AFTER-THUNK> from OLD-STATE-POINT.\n\
If OLD-STATE-POINT is #F, the current state point in the global state\n\
space is used as the starting point.")
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
      exp_register = old_point;
      Store_Return (RC_RESTORE_TO_STATE_POINT);
      Save_Cont ();
      /* Push a stack frame which will call the body after we have moved
	 into the new state point */
      STACK_PUSH (during_thunk);
      STACK_PUSH (STACK_FRAME_HEADER);
      /* Push the continuation to go with the stack frame */
      exp_register = SHARP_F;
      Store_Return (RC_INTERNAL_APPLY);
      Save_Cont ();
    Pushed ();
      Translate_To_Point (new_point);
      /*NOTREACHED*/
      PRIMITIVE_RETURN (UNSPECIFIC);
    }
  }
}

DEFINE_PRIMITIVE ("TRANSLATE-TO-STATE-POINT", Prim_translate_to_point, 1, 1,
		  "(STATE-POINT)\nRestore the dynamic state to STATE-POINT.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  CHECK_ARG (1, STATE_POINT_P);
  {
    SCHEME_OBJECT state_point = (ARG_REF (1));
    POP_PRIMITIVE_FRAME (1);
    Translate_To_Point (state_point);
    /*NOTREACHED*/
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("MAKE-STATE-SPACE", Prim_make_state_space, 1, 1,
		  "(MUTABLE?)\n\
Return a newly-allocated state-space.\n\
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

DEFINE_PRIMITIVE ("CURRENT-DYNAMIC-STATE", Prim_current_dynamic_state, 1, 1,
		  "(STATE-SPACE)\n\
Return the current state point in STATE-SPACE. If STATE-SPACE is #F,\n\
return the current state point in the global state space.")
{
  PRIMITIVE_HEADER (1);

  guarantee_state_point ();
  if ((ARG_REF (1)) == SHARP_F)
    PRIMITIVE_RETURN (Current_State_Point);
  CHECK_ARG (1, STATE_SPACE_P);
  PRIMITIVE_RETURN (MEMORY_REF ((ARG_REF (1)), STATE_SPACE_NEAREST_POINT));
}

DEFINE_PRIMITIVE ("SET-CURRENT-DYNAMIC-STATE!", Prim_set_dynamic_state, 1, 1,
		  "(STATE-POINT)\n\
Set the current dynamic state point to STATE-POINT.")
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
		  "()\n\
Returns the current interrupt mask.\n\
There are two interrupt bit masks:\n\
- The interrupt mask has a one bit for every enabled interrupt.\n\
- The interrupt code has a one bit for every interrupt pending service.\n\
Interrupts are prioritized according to their bit position (LSB is highest).\n\
At any interrupt polling point, the highest enabled pending interrupt is\n\
serviced.  The interrupt handler is a two-argument Scheme procedure\n\
invoked with all interrupts disabled and with the interrupt code and mask\n\
as arguments.  The interrupt mask is restored on return from the interrupt\n\
handler.  To prevent re-servicing the interrupt, the interrupt handler\n\
should clear the corresponding interrupt bit.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (FETCH_INTERRUPT_MASK ()));
}

DEFINE_PRIMITIVE ("SET-INTERRUPT-ENABLES!", Prim_set_interrupt_enables, 1, 1,
		  "(INTERRUPT-MASK)\n\
Sets the interrupt mask to INTERRUPT-MASK; returns previous mask value.\n\
See `get-interrupt-enables' for more information on interrupts.")
{
  PRIMITIVE_HEADER (1);
  {
    long previous = (FETCH_INTERRUPT_MASK ());
    SET_INTERRUPT_MASK ((arg_integer (1)) & INT_Mask);
    PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (previous));
  }
}

DEFINE_PRIMITIVE ("CLEAR-INTERRUPTS!", Prim_clear_interrupts, 1, 1,
		  "(INTERRUPT-MASK)\n\
Clears the interrupt bits in INTERRUPT-MASK by clearing the\n\
corresponding bits in the interrupt code.\n\
See `get-interrupt-enables' for more information on interrupts.")
{
  PRIMITIVE_HEADER (1);
  CLEAR_INTERRUPT ((arg_integer (1)) & INT_Mask);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DISABLE-INTERRUPTS!", Prim_disable_interrupts, 1, 1, 
		  "(INTERRUPT-MASK)\n\
Disables the interrupts specified in INTERRUPT-MASK by clearing the\n\
corresponding bits in the interrupt mask. Returns previous mask value.\n\
See `get-interrupt-enables' for more information on interrupts.")
{
  PRIMITIVE_HEADER (1);
  {
    fast long previous = (FETCH_INTERRUPT_MASK ());
    SET_INTERRUPT_MASK (previous &~ ((arg_integer (1)) & INT_Mask));
    PRIMITIVE_RETURN (LONG_TO_FIXNUM (previous));
  }
}

DEFINE_PRIMITIVE ("ENABLE-INTERRUPTS!", Prim_enable_interrupts, 1, 1,
		  "(INTERRUPT-MASK)\n\
Enables the interrupts specified in INTERRUPT-MASK by setting the\n\
corresponding bits in the interrupt mask. Returns previous mask value.\n\
See `get-interrupt-enables' for more information on interrupts.")
{
  PRIMITIVE_HEADER (1);
  {
    fast long previous = (FETCH_INTERRUPT_MASK ());
    SET_INTERRUPT_MASK (previous | ((arg_integer (1)) & INT_Mask));
    PRIMITIVE_RETURN (LONG_TO_FIXNUM (previous));
  }
}

DEFINE_PRIMITIVE ("REQUEST-INTERRUPTS!", Prim_request_interrupts, 1, 1,
		  "(INTERRUPT-MASK)\n\
Requests the interrupt bits in INTERRUPT-MASK by setting the\n\
corresponding bits in the interrupt code.\n\
See `get-interrupt-enables' for more information on interrupts.")
{
  PRIMITIVE_HEADER (1);
  REQUEST_INTERRUPT ((arg_integer (1)) & INT_Mask);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("RETURN-TO-APPLICATION",
		  Prim_return_to_application, 2, LEXPR,
  "(THUNK PROCEDURE . ARGS)\n\
Invokes THUNK with no arguments and a special return address.\n\
The return address calls PROCEDURE on ARGS.\n\
This is used by the runtime system to create stack frames that can be\n\
identified by the continuation parser.")
{
  PRIMITIVE_HEADER (LEXPR);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  {
    long nargs = (LEXPR_N_ARGUMENTS ());
    if (nargs < 2)
      signal_error_from_primitive (ERR_WRONG_NUMBER_OF_ARGUMENTS);
    {
      SCHEME_OBJECT thunk = (STACK_POP ());
      STACK_PUSH (STACK_FRAME_HEADER + (nargs - 2));
      env_register = THE_NULL_ENV;
      exp_register = SHARP_F;
      Store_Return (RC_INTERNAL_APPLY);
      Save_Cont ();
    Will_Push (STACK_ENV_EXTRA_SLOTS + 1);
      STACK_PUSH (thunk);
      STACK_PUSH (STACK_FRAME_HEADER);
    Pushed ();
    }
  }
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("WITH-STACK-MARKER", Prim_with_stack_marker, 3, 3,
		  "(THUNK MARKER1 MARKER2)\n\
Call THUNK with a continuation that has a special marker.\n\
When THUNK returns, the marker is discarded.\n\
The value of THUNK is returned to the continuation of this primitive.\n\
The marker consists of MARKER1 and MARKER2.\n\
By convention, MARKER1 is a tag identifying the kind of marker,\n\
and MARKER2 is data identifying the marker instance.")
{
  SCHEME_OBJECT thunk;
  PRIMITIVE_HEADER (3);

  thunk = (ARG_REF (1));

  if ((COMPILED_CODE_ADDRESS_P (STACK_REF (3)))
      && (COMPILED_CODE_ADDRESS_P (thunk)))
  {
    extern SCHEME_OBJECT EXFUN (compiled_with_stack_marker, (SCHEME_OBJECT));

    (void) STACK_POP ();
    return (compiled_with_stack_marker (thunk));
  }
  else
  {
    PRIMITIVE_CANONICALIZE_CONTEXT ();

    (void) STACK_POP ();
    STACK_PUSH (MAKE_OBJECT (TC_RETURN_CODE, RC_STACK_MARKER));
   Will_Push (STACK_ENV_EXTRA_SLOTS + 1);
    STACK_PUSH (thunk);
    STACK_PUSH (STACK_FRAME_HEADER);
   Pushed ();
    PRIMITIVE_ABORT (PRIM_APPLY);
    /*NOTREACHED*/
    return (0);
  }
}

static SCHEME_OBJECT 
DEFUN (with_new_interrupt_mask, (new_mask), unsigned long new_mask)
{
  SCHEME_OBJECT receiver = (ARG_REF (2));

  if ((COMPILED_CODE_ADDRESS_P (STACK_REF (2)))
      && (COMPILED_CODE_ADDRESS_P (receiver)))
  {
    extern SCHEME_OBJECT
      EXFUN (compiled_with_interrupt_mask, (unsigned long,
					    SCHEME_OBJECT,
					    unsigned long));
    unsigned long current_mask = (FETCH_INTERRUPT_MASK ());

    POP_PRIMITIVE_FRAME (2);
    SET_INTERRUPT_MASK (new_mask);

    PRIMITIVE_RETURN
      (compiled_with_interrupt_mask (current_mask, receiver, new_mask));
  }
  else
  {
    PRIMITIVE_CANONICALIZE_CONTEXT ();
    POP_PRIMITIVE_FRAME (2);
    preserve_interrupt_mask ();
  Will_Push (STACK_ENV_EXTRA_SLOTS + 2);
    STACK_PUSH (LONG_TO_FIXNUM (FETCH_INTERRUPT_MASK ()));
    STACK_PUSH (receiver);
    STACK_PUSH (STACK_FRAME_HEADER + 1);
  Pushed ();
    SET_INTERRUPT_MASK (new_mask);
    PRIMITIVE_ABORT (PRIM_APPLY);
    /*NOTREACHED*/
    return (0);
  }
}

DEFINE_PRIMITIVE ("WITH-INTERRUPT-MASK", Prim_with_interrupt_mask, 2, 2,
		  "(MASK RECEIVER)\n\
Set the interrupt mask to MASK for the duration of the call to RECEIVER.\n\
RECEIVER is passed the old interrupt mask as its argument.")
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN (with_new_interrupt_mask (INT_Mask & (arg_integer (1))));
}

DEFINE_PRIMITIVE ("WITH-INTERRUPTS-REDUCED",
		  Prim_with_interrupts_reduced, 2, 2,
		  "(MASK RECEIVER)\n\
Like `with-interrupt-mask', but only disables interrupts.")
{
  unsigned long old_mask, new_mask;
  PRIMITIVE_HEADER (2);

  old_mask = (FETCH_INTERRUPT_MASK ());
  new_mask = (INT_Mask & (arg_integer (1)));
  PRIMITIVE_RETURN (with_new_interrupt_mask ((new_mask > old_mask) ?
					     new_mask :
					     (new_mask & old_mask)));
}

/* History */

SCHEME_OBJECT
initialize_history ()
{
  /* Dummy History Structure */
  history_register = (Make_Dummy_History ());
  return
    (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, (Make_Dummy_History ())));
}

DEFINE_PRIMITIVE ("SET-CURRENT-HISTORY!", Prim_set_current_history, 1, 1,
		  "(HISTORY)\n\
Set the interpreter's history object to HISTORY.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  CHECK_ARG (1, HUNK3_P);
  val_register = (*history_register);
#ifndef DISABLE_HISTORY
  history_register = (OBJECT_ADDRESS (ARG_REF (1)));
#else
  history_register = (OBJECT_ADDRESS (Get_Fixed_Obj_Slot (Dummy_History)));
#endif
  POP_PRIMITIVE_FRAME (1);
  PRIMITIVE_ABORT (PRIM_POP_RETURN);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("WITH-HISTORY-DISABLED", Prim_with_history_disabled, 1, 1,
		  "(THUNK)\nExecute THUNK with the interpreter's history OFF.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  {
    SCHEME_OBJECT thunk = (ARG_REF (1));
    /* Remove one reduction from the history before saving it */
    SCHEME_OBJECT * first_rib = (OBJECT_ADDRESS (history_register [HIST_RIB]));
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
	  /* This maintains the mark in (history_register [HIST_RIB]). */
	  (history_register [HIST_RIB]) =
	    (MAKE_POINTER_OBJECT ((OBJECT_TYPE (history_register [HIST_RIB])),
				  rib));
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
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

/* Miscellaneous State */

DEFINE_PRIMITIVE ("GET-FLUID-BINDINGS", Prim_get_fluid_bindings, 0, 0,
		  "()\nReturn the current deep fluid bindings.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (Fluid_Bindings);
}

DEFINE_PRIMITIVE ("SET-FLUID-BINDINGS!", Prim_set_fluid_bindings, 1, 1,
		  "(FLUID-BINDINGS-ALIST)\n\
Set the current deep fluid bindings alist to FLUID-BINDINGS-ALIST.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, APPARENT_LIST_P);
  {
    SCHEME_OBJECT old_bindings = Fluid_Bindings;
    Fluid_Bindings = (ARG_REF (1));
    PRIMITIVE_RETURN (old_bindings);
  }
}

DEFINE_PRIMITIVE ("GET-FIXED-OBJECTS-VECTOR",
		  Prim_get_fixed_objects_vector, 0, 0,
		  "()\nReturn the fixed objects vector (TM).") 
{
  PRIMITIVE_HEADER (0);
  if (Valid_Fixed_Obj_Vector ())
    PRIMITIVE_RETURN (Get_Fixed_Obj_Slot (Me_Myself));
  PRIMITIVE_RETURN (SHARP_F);
}

#ifndef SET_FIXED_OBJ_HOOK
# define SET_FIXED_OBJ_HOOK(vector) Fixed_Objects = (vector)
#endif

DEFINE_PRIMITIVE ("SET-FIXED-OBJECTS-VECTOR!",
		  Prim_set_fixed_objects_vector, 1, 1,
		  "(NEW-FOV)\nSet the fixed objects vector (TM) to NEW-FOV.")
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
      SET_FIXED_OBJ_HOOK (vector);
      Set_Fixed_Obj_Slot (Me_Myself, vector);
      PRIMITIVE_RETURN (result);
    }
  }
}
