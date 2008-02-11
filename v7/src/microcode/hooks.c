/* -*-C-*-

$Id: hooks.c,v 9.71 2008/02/11 21:07:21 riastradh Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

static SCHEME_OBJECT allocate_control_point (unsigned long, bool);
static void with_new_interrupt_mask (unsigned long);

/* This is a kludge to compensate for the interpreter popping
   a primitive's frame off the stack after it returns.  */
#define UN_POP_PRIMITIVE_FRAME(n) (stack_pointer = (STACK_LOC (-(n))))

DEFINE_PRIMITIVE ("APPLY", Prim_apply, 2, 2, "(PROCEDURE ARG-LIST)\n\
Invokes PROCEDURE on the arguments in ARG-LIST.")
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT procedure = (ARG_REF (1));
    SCHEME_OBJECT args = (ARG_REF (2));
    unsigned long n_args = 0;

    /* Since this primitive must pop its own frame off and push a new
       frame on the stack, it has to be careful.  Its own stack frame
       is needed if an error or GC is required.  So these checks are
       done first (at the cost of traversing the argument list twice),
       then the primitive's frame is popped, and finally the new frame
       is constructed.  */

    {
      SCHEME_OBJECT p1 = args;
      SCHEME_OBJECT p2 = p1;

      while (PAIR_P (p1))
	{
	  p1 = (PAIR_CDR (p1));
	  n_args += 1;
	  if (p1 == p2)
	    error_bad_range_arg (2);
	  if (!PAIR_P (p1))
	    break;

	  p1 = (PAIR_CDR (p1));
	  n_args += 1;
	  if (p1 == p2)
	    error_bad_range_arg (2);
	  if (!PAIR_P (p1))
	    break;

	  p2 = (PAIR_CDR (p2));
	}
      if (!EMPTY_LIST_P (p1))
	error_wrong_type_arg (2);
    }

    if (!CAN_PUSH_P (n_args + 2))
      error_bad_range_arg (2);
    POP_PRIMITIVE_FRAME (2);

    {
      SCHEME_OBJECT p1 = args;
      SCHEME_OBJECT * sp = (STACK_LOC (-n_args));
      SCHEME_OBJECT * s1 = sp;
      while (s1 != stack_pointer)
	{
	  (STACK_LOCATIVE_POP (s1)) = (PAIR_CAR (p1));
	  p1 = (PAIR_CDR (p1));
	}
      stack_pointer = sp;
    }

#ifdef CC_SUPPORT_P
    if (CC_ENTRY_P (STACK_REF (n_args)))
      {
	apply_compiled_from_primitive (n_args, procedure);
	UN_POP_PRIMITIVE_FRAME (2);
	PRIMITIVE_RETURN (UNSPECIFIC);
      }
#endif

    STACK_PUSH (procedure);
    PUSH_APPLY_FRAME_HEADER (n_args);
    PRIMITIVE_ABORT (PRIM_APPLY);
    /*NOTREACHED*/
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

/* CALL-WITH-CURRENT-CONTINUATION creates a control point (a pointer
   to the current stack) and passes it to PROCEDURE as its only
   argument.  The inverse operation, typically called THROW, is
   performed by using the control point as you would a procedure.  The
   control point accepts one argument that is returned as the value of
   the call to this primitive.  The control point may be reused as
   often as desired since the stack will be copied on every throw.  */

DEFINE_PRIMITIVE ("CALL-WITH-CURRENT-CONTINUATION", Prim_catch, 1, 1,
		  "(PROCEDURE)\n\
Invoke PROCEDURE with a copy of the current control stack.")
{
  PRIMITIVE_HEADER (1);
  canonicalize_primitive_context ();
  {
    SCHEME_OBJECT procedure = (ARG_REF (1));
    SCHEME_OBJECT cp;

    /* Optimization: if the current stack consists only of an
       RC_JOIN_STACKLETS frame, there's no need to create a new
       control point.  */

    if (((STACK_LOC (1 + CONTINUATION_SIZE)) == STACK_BOTTOM)
	&& (CHECK_RETURN_CODE (RC_JOIN_STACKLETS, 1))
	&& (CONTROL_POINT_P (CONT_EXP (1))))
      {
	cp = (CONT_EXP (1));
	history_register = (OBJECT_ADDRESS (READ_DUMMY_HISTORY ()));
	POP_PRIMITIVE_FRAME (1);
	STACK_RESET ();
      }
    else
      {
	cp = (allocate_control_point ((CONTINUATION_SIZE
				       + HISTORY_SIZE
				       + (STACK_N_PUSHED - 1)),
				      true));
	POP_PRIMITIVE_FRAME (1);

	SAVE_HISTORY (RC_RESTORE_HISTORY);
	preserve_interrupt_mask ();
	prev_restore_history_offset = 0;
	{
	  SCHEME_OBJECT * scan = (control_point_start (cp));
	  while (STACK_N_PUSHED > 0)
	    (*scan++) = (STACK_POP ());
	}
#ifdef ENABLE_DEBUGGING_TOOLS
	if (STACK_N_PUSHED != 0)
	  Microcode_Termination (TERM_BAD_STACK);
#endif

	CLEAR_INTERRUPT (INT_Stack_Overflow);
	STACK_RESET ();
	SET_RC (RC_JOIN_STACKLETS);
	SET_EXP (cp);
	SAVE_CONT ();
      }

    STACK_PUSH (cp);
    STACK_PUSH (procedure);
    PUSH_APPLY_FRAME_HEADER (1);
  }
  PRIMITIVE_ABORT (PRIM_APPLY);
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
   an apply frame for THUNK.  */

DEFINE_PRIMITIVE ("WITHIN-CONTROL-POINT", Prim_within_control_point, 2, 2,
		  "(CONTROL-POINT THUNK)\n\
Invoke THUNK with CONTROL-POINT as its control stack.")
{
  SCHEME_OBJECT control_point, thunk;
  PRIMITIVE_HEADER (2);

  canonicalize_primitive_context();
  CHECK_ARG (1, CONTROL_POINT_P);
  control_point = (ARG_REF (1));
  thunk = (ARG_REF (2));

  stack_pointer = STACK_BOTTOM;
  /* We've discarded the history with the stack contents.  */
  prev_restore_history_offset = 0;
  CLEAR_INTERRUPT (INT_Stack_Overflow);

 Will_Push (CONTINUATION_SIZE);
  SET_EXP (control_point);
  SET_RC (RC_JOIN_STACKLETS);
  SAVE_CONT ();
 Pushed ();

 Will_Push (STACK_ENV_EXTRA_SLOTS + 1);
  STACK_PUSH (thunk);
  PUSH_APPLY_FRAME_HEADER (0);
 Pushed ();

  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static SCHEME_OBJECT
allocate_control_point (unsigned long n, bool gc_p)
{
  SCHEME_OBJECT cp
    = (allocate_marked_vector (TC_CONTROL_POINT, (n + 2), gc_p));
  VECTOR_SET (cp, 0, SHARP_F);
  VECTOR_SET (cp, 1, (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0)));
  return (cp);
}

SCHEME_OBJECT *
control_point_start (SCHEME_OBJECT cp)
{
  return (VECTOR_LOC (cp, 2));
}

SCHEME_OBJECT *
control_point_end (SCHEME_OBJECT cp)
{
  return (VECTOR_LOC (cp, (VECTOR_LENGTH (cp))));
}

void
unpack_control_point (SCHEME_OBJECT cp)
{
  WHEN_DEBUGGING
    ({
      if (!CONTROL_POINT_P (cp))
	Microcode_Termination (TERM_BAD_STACK);
    });
  {
    SCHEME_OBJECT * scan_from = (control_point_end (cp));
    SCHEME_OBJECT * end_from = (control_point_start (cp));

    stack_pointer = STACK_BOTTOM;
    CLEAR_INTERRUPT (INT_Stack_Overflow);
    STACK_CHECK (end_from - scan_from);
    
    while (scan_from > end_from)
      STACK_PUSH (*--scan_from);
  }
  STACK_RESET ();
}

DEFINE_PRIMITIVE ("ERROR-PROCEDURE", Prim_error_procedure, 3, 3,
		  "(MESSAGE IRRITANTS ENVIRONMENT)\nSignal an error.")
{
  PRIMITIVE_HEADER (3);
  canonicalize_primitive_context ();
  {
    SCHEME_OBJECT message = (ARG_REF (1));
    SCHEME_OBJECT irritants = (ARG_REF (2));
    SCHEME_OBJECT environment = (ARG_REF (3));
    /* This is done outside the Will_Push because the space for it
       is guaranteed by the interpreter before it gets here.
       If done inside, this could break when using stacklets. */
    back_out_of_primitive ();
  Will_Push (HISTORY_SIZE + STACK_ENV_EXTRA_SLOTS + 4);
    stop_history ();
    /* Stepping should be cleared here! */
    STACK_PUSH (environment);
    STACK_PUSH (irritants);
    STACK_PUSH (message);
    STACK_PUSH (VECTOR_REF (fixed_objects, Error_Procedure));
    PUSH_APPLY_FRAME_HEADER (3);
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
  canonicalize_primitive_context ();
  CHECK_ARG (2, ENVIRONMENT_P);
  {
    SCHEME_OBJECT expression = (ARG_REF (1));
    SCHEME_OBJECT environment = (ARG_REF (2));
    POP_PRIMITIVE_FRAME (2);
    SET_ENV (environment);
    SET_EXP (expression);
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
    SCHEME_OBJECT thunk = (ARG_REF (1));
    SCHEME_OBJECT State = (MEMORY_REF (thunk, THUNK_SNAPPED));
    if (State == SHARP_T)
      PRIMITIVE_RETURN (MEMORY_REF (thunk, THUNK_VALUE));
    else if (State ==  FIXNUM_ZERO)
    {
      /* New-style thunk used by compiled code. */
      canonicalize_primitive_context ();
      POP_PRIMITIVE_FRAME (1);
     Will_Push (CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS + 1);
      SET_RC (RC_SNAP_NEED_THUNK);
      SET_EXP (thunk);
      SAVE_CONT ();
      STACK_PUSH (MEMORY_REF (thunk, THUNK_VALUE));
      PUSH_APPLY_FRAME_HEADER (0);
     Pushed ();
      PRIMITIVE_ABORT (PRIM_APPLY);
      /*NOTREACHED*/
      PRIMITIVE_RETURN (UNSPECIFIC);
    }
    else
    {
      /* Old-style thunk used by interpreted code. */
      canonicalize_primitive_context ();
      POP_PRIMITIVE_FRAME (1);
     Will_Push (CONTINUATION_SIZE);
      SET_RC (RC_SNAP_NEED_THUNK);
      SET_EXP (thunk);
      SAVE_CONT ();
     Pushed ();
      SET_ENV (MEMORY_REF (thunk, THUNK_ENVIRONMENT));
      SET_EXP (MEMORY_REF (thunk, THUNK_PROCEDURE));
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

  canonicalize_primitive_context ();
  {
    SCHEME_OBJECT old_point;
    if ((ARG_REF (1)) == SHARP_F)
      old_point = current_state_point;
    else
      {
	CHECK_ARG (1, STATE_SPACE_P);
	old_point = (MEMORY_REF ((ARG_REF (1)), STATE_SPACE_NEAREST_POINT));
      }
    {
      SCHEME_OBJECT new_point =
	(allocate_marked_vector (TC_VECTOR, STATE_POINT_LENGTH, true));
      SCHEME_OBJECT during_thunk = (ARG_REF (3));
      MEMORY_SET (new_point, STATE_POINT_TAG,
		  (VECTOR_REF (fixed_objects, State_Point_Tag)));
      MEMORY_SET (new_point, STATE_POINT_BEFORE_THUNK, (ARG_REF (2)));
      MEMORY_SET (new_point, STATE_POINT_AFTER_THUNK, (ARG_REF (4)));
      MEMORY_SET (new_point, STATE_POINT_NEARER_POINT, old_point);
      MEMORY_SET
	(new_point,
	 STATE_POINT_DISTANCE_TO_ROOT,
	 (1 + (MEMORY_REF (old_point, STATE_POINT_DISTANCE_TO_ROOT))));

      POP_PRIMITIVE_FRAME (4);
    Will_Push((2 * CONTINUATION_SIZE) + (STACK_ENV_EXTRA_SLOTS + 1));
      /* Push a continuation to go back to the current state after the
	 body is evaluated */
      SET_EXP (old_point);
      SET_RC (RC_RESTORE_TO_STATE_POINT);
      SAVE_CONT ();
      /* Push a stack frame which will call the body after we have moved
	 into the new state point */
      STACK_PUSH (during_thunk);
      PUSH_APPLY_FRAME_HEADER (0);
      /* Push the continuation to go with the stack frame */
      SET_EXP (SHARP_F);
      SET_RC (RC_INTERNAL_APPLY);
      SAVE_CONT ();
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
  canonicalize_primitive_context ();
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
    SCHEME_OBJECT new_point =
      (allocate_marked_vector (TC_VECTOR, STATE_POINT_LENGTH, true));
    MEMORY_SET (new_point, STATE_POINT_TAG,
		     (VECTOR_REF (fixed_objects, State_Point_Tag)));
    MEMORY_SET (new_point, STATE_POINT_BEFORE_THUNK, SHARP_F);
    MEMORY_SET (new_point, STATE_POINT_AFTER_THUNK, SHARP_F);
    MEMORY_SET
      (new_point, STATE_POINT_DISTANCE_TO_ROOT, (LONG_TO_UNSIGNED_FIXNUM (0)));
    if ((ARG_REF (1)) == SHARP_F)
      {
	MEMORY_SET (new_point, STATE_POINT_NEARER_POINT, SHARP_F);
	current_state_point = new_point;
	PRIMITIVE_RETURN (SHARP_F);
      }
    else
      {
	SCHEME_OBJECT new_space =
	  (allocate_marked_vector (TC_VECTOR, STATE_SPACE_LENGTH, true));
	MEMORY_SET (new_space, STATE_SPACE_TAG,
			 (VECTOR_REF (fixed_objects, State_Space_Tag)));
	MEMORY_SET (new_space, STATE_SPACE_NEAREST_POINT, new_point);
	MEMORY_SET (new_point, STATE_POINT_NEARER_POINT, new_space);
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

  if ((ARG_REF (1)) == SHARP_F)
    PRIMITIVE_RETURN (current_state_point);
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
    SCHEME_OBJECT state_point = (ARG_REF (1));
    SCHEME_OBJECT state_space = (Find_State_Space (state_point));
    SCHEME_OBJECT result;
    if (state_space == SHARP_F)
      {
	result = current_state_point;
	current_state_point = state_point;
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
  PRIMITIVE_RETURN (ULONG_TO_FIXNUM (GET_INT_MASK));
}

DEFINE_PRIMITIVE ("SET-INTERRUPT-ENABLES!", Prim_set_interrupt_enables, 1, 1,
		  "(INTERRUPT-MASK)\n\
Sets the interrupt mask to INTERRUPT-MASK; returns previous mask value.\n\
See `get-interrupt-enables' for more information on interrupts.")
{
  PRIMITIVE_HEADER (1);
  {
    unsigned long previous = GET_INT_MASK;
    SET_INTERRUPT_MASK ((arg_ulong_integer (1)) & INT_Mask);
    PRIMITIVE_RETURN (ULONG_TO_FIXNUM (previous));
  }
}

DEFINE_PRIMITIVE ("CLEAR-INTERRUPTS!", Prim_clear_interrupts, 1, 1,
		  "(INTERRUPT-MASK)\n\
Clears the interrupt bits in INTERRUPT-MASK by clearing the\n\
corresponding bits in the interrupt code.\n\
See `get-interrupt-enables' for more information on interrupts.")
{
  PRIMITIVE_HEADER (1);
  CLEAR_INTERRUPT ((arg_ulong_integer (1)) & INT_Mask);
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
    unsigned long previous = GET_INT_MASK;
    SET_INTERRUPT_MASK (previous &~ ((arg_ulong_integer (1)) & INT_Mask));
    PRIMITIVE_RETURN (ULONG_TO_FIXNUM (previous));
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
    unsigned long previous = GET_INT_MASK;
    SET_INTERRUPT_MASK (previous | ((arg_ulong_integer (1)) & INT_Mask));
    PRIMITIVE_RETURN (ULONG_TO_FIXNUM (previous));
  }
}

DEFINE_PRIMITIVE ("REQUEST-INTERRUPTS!", Prim_request_interrupts, 1, 1,
		  "(INTERRUPT-MASK)\n\
Requests the interrupt bits in INTERRUPT-MASK by setting the\n\
corresponding bits in the interrupt code.\n\
See `get-interrupt-enables' for more information on interrupts.")
{
  PRIMITIVE_HEADER (1);
  REQUEST_INTERRUPT ((arg_ulong_integer (1)) & INT_Mask);
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
  canonicalize_primitive_context ();
  {
    unsigned long nargs = GET_LEXPR_ACTUALS;
    if (nargs < 2)
      signal_error_from_primitive (ERR_WRONG_NUMBER_OF_ARGUMENTS);
    {
      SCHEME_OBJECT thunk = (STACK_POP ());
      PUSH_APPLY_FRAME_HEADER (nargs - 2);
      SET_ENV (THE_NULL_ENV);
      SET_EXP (SHARP_F);
      SET_RC (RC_INTERNAL_APPLY);
      SAVE_CONT ();
    Will_Push (STACK_ENV_EXTRA_SLOTS + 1);
      STACK_PUSH (thunk);
      PUSH_APPLY_FRAME_HEADER (0);
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
  PRIMITIVE_HEADER (3);
  {
    SCHEME_OBJECT thunk = (ARG_REF (1));
#ifdef CC_SUPPORT_P
    if ((CC_ENTRY_P (STACK_REF (3))) && (CC_ENTRY_P (thunk)))
      {
	(void) STACK_POP ();
	compiled_with_stack_marker (thunk);
	UN_POP_PRIMITIVE_FRAME (3);
      }
    else
#endif
      {
	canonicalize_primitive_context ();
	(void) STACK_POP ();
	STACK_PUSH (MAKE_RETURN_CODE (RC_STACK_MARKER));
	Will_Push (STACK_ENV_EXTRA_SLOTS + 1);
	STACK_PUSH (thunk);
	PUSH_APPLY_FRAME_HEADER (0);
	Pushed ();
	PRIMITIVE_ABORT (PRIM_APPLY);
	/*NOTREACHED*/
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("WITH-INTERRUPT-MASK", Prim_with_interrupt_mask, 2, 2,
		  "(MASK RECEIVER)\n\
Set the interrupt mask to MASK for the duration of the call to RECEIVER.\n\
RECEIVER is passed the old interrupt mask as its argument.")
{
  PRIMITIVE_HEADER (2);
  with_new_interrupt_mask (INT_Mask & (arg_integer (1)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("WITH-INTERRUPTS-REDUCED", Prim_with_interrupts_reduced,
		  2, 2, "(MASK RECEIVER)\n\
Like WITH-INTERRUPT-MASK, but only disables interrupts.")
{
  unsigned long old_mask, new_mask;
  PRIMITIVE_HEADER (2);

  old_mask = GET_INT_MASK;
  new_mask = (INT_Mask & (arg_ulong_integer (1)));
  with_new_interrupt_mask
    ((new_mask > old_mask) ? new_mask : (new_mask & old_mask));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
with_new_interrupt_mask (unsigned long new_mask)
{
  SCHEME_OBJECT receiver = (ARG_REF (2));

#ifdef CC_SUPPORT_P
  if ((CC_ENTRY_P (STACK_REF (2))) && (CC_ENTRY_P (receiver)))
    {
      unsigned long current_mask = GET_INT_MASK;
      POP_PRIMITIVE_FRAME (2);
      compiled_with_interrupt_mask (current_mask, receiver, new_mask);
      UN_POP_PRIMITIVE_FRAME (2);
      SET_INTERRUPT_MASK (new_mask);
    }
  else
#endif
    {
      canonicalize_primitive_context ();
      POP_PRIMITIVE_FRAME (2);
      preserve_interrupt_mask ();
      Will_Push (STACK_ENV_EXTRA_SLOTS + 2);
      STACK_PUSH (ULONG_TO_FIXNUM (GET_INT_MASK));
      STACK_PUSH (receiver);
      PUSH_APPLY_FRAME_HEADER (1);
      Pushed ();
      SET_INTERRUPT_MASK (new_mask);
      PRIMITIVE_ABORT (PRIM_APPLY);
    }
}

/* History */

SCHEME_OBJECT
initialize_history (void)
{
  /* Dummy History Structure */
  history_register = (make_dummy_history ());
  return
    (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, (make_dummy_history ())));
}

DEFINE_PRIMITIVE ("SET-CURRENT-HISTORY!", Prim_set_current_history, 1, 1,
		  "(HISTORY)\n\
Set the interpreter's history object to HISTORY.")
{
  PRIMITIVE_HEADER (1);
  canonicalize_primitive_context ();
  CHECK_ARG (1, HUNK3_P);
  SET_VAL (*history_register);
#ifndef DISABLE_HISTORY
  history_register = (OBJECT_ADDRESS (ARG_REF (1)));
#else
  history_register = (OBJECT_ADDRESS (READ_DUMMY_HISTORY ()));
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
  canonicalize_primitive_context ();
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
	      SCHEME_OBJECT * next_rib =
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
    stop_history ();
  Will_Push (STACK_ENV_EXTRA_SLOTS + 1);
    STACK_PUSH (thunk);
    PUSH_APPLY_FRAME_HEADER (0);
  Pushed ();
    PRIMITIVE_ABORT (PRIM_APPLY);
    /*NOTREACHED*/
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("GET-FIXED-OBJECTS-VECTOR",
		  Prim_get_fixed_objects_vector, 0, 0,
		  "()\n\
Return the fixed objects vector (TM).")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (fixed_objects);
}

DEFINE_PRIMITIVE ("SET-FIXED-OBJECTS-VECTOR!",
		  Prim_set_fixed_objects_vector, 1, 1,
		  "(NEW-FOV)\n\
Set the fixed objects vector (TM) to NEW-FOV.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, VECTOR_P);
  {
    SCHEME_OBJECT old = fixed_objects;
    SCHEME_OBJECT new = (ARG_REF (1));
    if ((VECTOR_LENGTH (new)) < N_FIXED_OBJECTS)
      error_bad_range_arg (1);
    fixed_objects = new;
    PRIMITIVE_RETURN (old);
  }
}
