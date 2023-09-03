/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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
#include "history.h"

static SCHEME_OBJECT allocate_control_point (unsigned long, bool);
static void with_new_interrupt_mask (unsigned long);

/* This is a kludge to compensate for the interpreter popping
   a primitive's frame off the stack after it returns.  */
#define un_pop_primitive_frame decrement_sp

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

    if (!stack_can_push_p (n_args + 2))
      error_bad_range_arg (2);
    pop_primitive_frame (2);

    {
      SCHEME_OBJECT p1 = args;
      SCHEME_OBJECT* s1 = stack_pointer - n_args;
      while (s1 < stack_pointer)
	{
          *s1++ = pair_car (p1);
	  p1 = pair_cdr (p1);
	}
      decrement_sp (n_args);
    }

#ifdef CC_SUPPORT_P
    if (CC_RETURN_P (stack_ref (n_args)))
      {
	apply_compiled_from_primitive (n_args, procedure);
	un_pop_primitive_frame (2);
	PRIMITIVE_RETURN (UNSPECIFIC);
      }
    else
      {
	assert (RETURN_CODE_P (stack_ref (n_args)));
      }
#endif

    stack_push (procedure);
    stack_push (make_apply_frame_header (n_args + 1));
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

    if (stack_loc (1 + CONT_SIZE) == STACK_BOTTOM
	&& cont_frame_ret (stack_loc (1))
            == MAKE_RETURN_CODE (RC_JOIN_STACKLETS)
	&& CONTROL_POINT_P (cont_frame_exp (stack_loc (1))))
      {
	cp = cont_frame_exp (stack_loc (1));
	history_register = object_address (READ_DUMMY_HISTORY ());
	pop_primitive_frame (1);
	STACK_RESET ();
      }
    else
      {
	cp = (allocate_control_point ((CONT_SIZE
				       + HISTORY_CONT_SIZE
				       + (stack_n_pushed () - 1)),
				      true));
	pop_primitive_frame (1);

	save_history (RC_RESTORE_HISTORY);
	preserve_interrupt_mask ();
	prev_restore_history_offset = 0;
	{
	  SCHEME_OBJECT * scan = (control_point_start (cp));
	  while (stack_n_pushed () > 0)
	    (*scan++) = (stack_pop ());
	}
#ifdef ENABLE_DEBUGGING_TOOLS
	if (stack_n_pushed () != 0)
	  Microcode_Termination (TERM_BAD_STACK);
#endif

	CLEAR_INTERRUPT (INT_Stack_Overflow);
	STACK_RESET ();
        push_cont (RC_JOIN_STACKLETS, cp);
      }

    stack_push (cp);
    stack_push (procedure);
    stack_push (make_apply_frame_header (2));
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
  PRIMITIVE_HEADER (2);

  canonicalize_primitive_context();
  CHECK_ARG (1, CONTROL_POINT_P);
  SCHEME_OBJECT control_point = (ARG_REF (1));
  SCHEME_OBJECT thunk = (ARG_REF (2));

  stack_pointer = STACK_BOTTOM;
  /* We've discarded the history with the stack contents.  */
  prev_restore_history_offset = 0;
  CLEAR_INTERRUPT (INT_Stack_Overflow);

  stack_check (CONT_SIZE + 2);
  push_cont (RC_JOIN_STACKLETS, control_point);
  stack_push (thunk);
  stack_push (make_apply_frame_header (1));

  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static SCHEME_OBJECT
allocate_control_point (unsigned long n, bool gc_p)
{
  SCHEME_OBJECT cp
    = (allocate_marked_vector (TC_CONTROL_POINT, (n + 2), gc_p));
  vector_set (cp, 0, SHARP_F);
  vector_set (cp, 1, ULONG_TO_FIXNUM (0));
  return (cp);
}

SCHEME_OBJECT *
control_point_start (SCHEME_OBJECT cp)
{
  return (vector_loc (cp, 2));
}

SCHEME_OBJECT *
control_point_end (SCHEME_OBJECT cp)
{
  return (vector_loc (cp, (vector_length (cp))));
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
    stack_check (scan_from - end_from);

    while (scan_from > end_from)
      stack_push (*--scan_from);
  }
  STACK_RESET ();
}

DEFINE_PRIMITIVE ("ERROR-PROCEDURE", Prim_error_procedure, 3, 3,
		  "(MESSAGE IRRITANTS ENVIRONMENT)\nSignal an error.")
{
  PRIMITIVE_HEADER (3);
  canonicalize_primitive_context ();

  SCHEME_OBJECT message = ARG_REF (1);
  SCHEME_OBJECT irritants = ARG_REF (2);
  SCHEME_OBJECT environment = ARG_REF (3);
  /* This is done outside the stack_check because the space for it
     is guaranteed by the interpreter before it gets here.
     If done inside, this could break when using stacklets. */
  back_out_of_primitive ();
  stack_check (HISTORY_CONT_SIZE + 5);
  stop_history ();
  /* Stepping should be cleared here! */
  stack_push (environment);
  stack_push (irritants);
  stack_push (message);
  stack_push (vector_ref (fixed_objects, ERROR_PROCEDURE));
  stack_push (make_apply_frame_header (4));

  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
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
    pop_primitive_frame (2);
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
  SCHEME_OBJECT delayed = ARG_REF (1);
  SCHEME_OBJECT state = delayed_snapped (delayed);
  if (state == SHARP_T)
    PRIMITIVE_RETURN (delayed_value (delayed));
  else if (state ==  FIXNUM_ZERO)
    {
      /* New-style delayed used by compiled code. */
      canonicalize_primitive_context ();
      pop_primitive_frame (1);

      stack_check (CONT_SIZE + 2);
      push_cont (RC_SNAP_NEED_THUNK, delayed);
      stack_push (delayed_value (delayed));
      stack_push (make_apply_frame_header (1));

      PRIMITIVE_ABORT (PRIM_APPLY);
      /*NOTREACHED*/
      PRIMITIVE_RETURN (UNSPECIFIC);
    }
  else
    {
      /* Old-style delayed used by interpreted code. */
      canonicalize_primitive_context ();
      pop_primitive_frame (1);

      stack_check (CONT_SIZE);
      push_cont (RC_SNAP_NEED_THUNK, delayed);
      SET_ENV (delayed_env (delayed));
      SET_EXP (delayed_proc (delayed));

      PRIMITIVE_ABORT (PRIM_DO_EXPRESSION);
      /*NOTREACHED*/
      PRIMITIVE_RETURN (UNSPECIFIC);
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

  unsigned long nargs = GET_LEXPR_ACTUALS;
  if (nargs < 2)
    signal_error_from_primitive (ERR_WRONG_NUMBER_OF_ARGUMENTS);

  SCHEME_OBJECT thunk = stack_pop ();
  stack_push (make_apply_frame_header (nargs - 1));
  SET_ENV (THE_NULL_ENV);
  push_cont (RC_INTERNAL_APPLY, SHARP_F);

  stack_check (2);
  stack_push (thunk);
  stack_push (make_apply_frame_header (1));

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

  SCHEME_OBJECT thunk = (ARG_REF (1));
#ifdef CC_SUPPORT_P
  if ((CC_RETURN_P (stack_ref (3))) && (CC_ENTRY_P (thunk)))
    {
      increment_sp (1);
      compiled_with_stack_marker (thunk);
      un_pop_primitive_frame (3);
    }
  else
#endif
  {
    canonicalize_primitive_context ();
    increment_sp (1);
    stack_push (MAKE_RETURN_CODE (RC_STACK_MARKER));
    stack_check (2);
    stack_push (thunk);
    stack_push (make_apply_frame_header (1));

    PRIMITIVE_ABORT (PRIM_APPLY);
    /*NOTREACHED*/
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
  if ((CC_RETURN_P (stack_ref (2))) && (CC_ENTRY_P (receiver)))
    {
      unsigned long current_mask = GET_INT_MASK;
      pop_primitive_frame (2);
      compiled_with_interrupt_mask (current_mask, receiver, new_mask);
      un_pop_primitive_frame (2);
      SET_INTERRUPT_MASK (new_mask);
    }
  else
#endif
    {
      canonicalize_primitive_context ();
      pop_primitive_frame (2);
      preserve_interrupt_mask ();

      stack_check (3);
      stack_push (ULONG_TO_FIXNUM (GET_INT_MASK));
      stack_push (receiver);
      stack_push (make_apply_frame_header (2));

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
    (make_pointer_object (TC_HISTORY_UNMARKED, (make_dummy_history ())));
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
  history_register = (object_address (ARG_REF (1)));
#else
  history_register = (object_address (READ_DUMMY_HISTORY ()));
#endif
  pop_primitive_frame (1);
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
    SCHEME_OBJECT * first_rib = (object_address (history_register [HIST_RIB]));
    SCHEME_OBJECT * second_rib =
      (object_address (first_rib [RIB_NEXT_REDUCTION]));
    if ((first_rib != second_rib) &&
	(! (HISTORY_MARKED_P (first_rib [RIB_MARK]))))
      {
	HISTORY_MARK (second_rib [RIB_MARK]);
	{
	  SCHEME_OBJECT * rib = first_rib;
	  while (1)
	    {
	      SCHEME_OBJECT * next_rib =
		(object_address (rib [RIB_NEXT_REDUCTION]));
	      if (next_rib == first_rib)
		break;
	      rib = next_rib;
	    }
	  /* This maintains the mark in (history_register [HIST_RIB]). */
	  (history_register [HIST_RIB]) =
	    (make_pointer_object ((object_type (history_register [HIST_RIB])),
				  rib));
	}
      }
    pop_primitive_frame (1);
    stop_history ();
    stack_check (2);
    stack_push (thunk);
    stack_push (make_apply_frame_header (1));

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
    if ((vector_length (new)) < N_FIXED_OBJECTS)
      error_bad_range_arg (1);
    fixed_objects = new;
    PRIMITIVE_RETURN (old);
  }
}
