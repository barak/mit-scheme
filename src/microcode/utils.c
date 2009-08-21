/* -*-C-*-

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

/* This file contains utilities for interrupts, errors, etc. */

#include "scheme.h"
#include "prims.h"
#include "winder.h"
#include "history.h"
#include "syscall.h"

#ifdef __OS2__
  extern void OS2_handle_attention_interrupt (void);
#endif

SCHEME_OBJECT * history_register;
unsigned long prev_restore_history_offset;

static SCHEME_OBJECT copy_history (SCHEME_OBJECT);

/* Helper procedures for setup_interrupt, which follows. */

static unsigned long
compute_interrupt_number (unsigned long masked_interrupts)
{
  unsigned long interrupt_number = 0;
  unsigned long bit_mask = 1;
  while ((interrupt_number <= MAX_INTERRUPT_NUMBER)
	 && ((masked_interrupts & bit_mask) == 0))
    {
      interrupt_number += 1;
      bit_mask <<= 1;
    }
  return (interrupt_number);
}

static unsigned long
compute_interrupt_handler_mask (SCHEME_OBJECT interrupt_masks,
				unsigned long interrupt_number)
{
  if ((VECTOR_P (interrupt_masks))
      && (interrupt_number <= (VECTOR_LENGTH (interrupt_masks))))
    {
      SCHEME_OBJECT mask
	= (VECTOR_REF (interrupt_masks, interrupt_number));
      if ((INTEGER_P (mask)) && (integer_to_ulong_p (mask)))
	/* Guarantee that the given interrupt is disabled.  */
	return ((integer_to_ulong (mask)) &~ (1UL << interrupt_number));
    }
  return
    ((interrupt_number <= MAX_INTERRUPT_NUMBER)
     ? ((1UL << interrupt_number) - 1)
     : GET_INT_MASK);
}

static void
terminate_no_interrupt_handler (unsigned long masked_interrupts)
{
  outf_fatal ("\nInterrupts = %#08lx, Mask = %#08lx, Masked = %#08lx\n",
	      GET_INT_CODE,
	      GET_INT_MASK,
	      masked_interrupts);
  Microcode_Termination (TERM_NO_INTERRUPT_HANDLER);
}

SCHEME_OBJECT
initialize_interrupt_handler_vector (void)
{
  return (make_vector ((MAX_INTERRUPT_NUMBER + 2), SHARP_F, false));
}

SCHEME_OBJECT
initialize_interrupt_mask_vector (void)
{
  SCHEME_OBJECT v = (make_vector ((MAX_INTERRUPT_NUMBER + 2), SHARP_F, false));
  unsigned long interrupt_number = 0;
  while (interrupt_number <= MAX_INTERRUPT_NUMBER)
    {
      VECTOR_SET (v,
		  interrupt_number,
		  (ulong_to_integer ((1UL << interrupt_number) - 1)));
      interrupt_number += 1;
    }
  return (v);
}

/* setup_interrupt is called from the Interrupt macro to do all of the
   setup for calling the user's interrupt routines. */

void
setup_interrupt (unsigned long masked_interrupts)
{
  SCHEME_OBJECT interrupt_handlers = SHARP_F;
  SCHEME_OBJECT interrupt_masks = SHARP_F;
  unsigned long interrupt_number
    = (compute_interrupt_number (masked_interrupts));
  unsigned long interrupt_mask;
  SCHEME_OBJECT interrupt_handler;

#ifdef __OS2__
  if ((1UL << interrupt_number) == INT_Global_1)
    {
      OS2_handle_attention_interrupt ();
      abort_to_interpreter (PRIM_POP_RETURN);
    }
#endif
  if (!VECTOR_P (fixed_objects))
    {
      outf_fatal ("\nInvalid fixed-objects vector");
      terminate_no_interrupt_handler (masked_interrupts);
    }
  interrupt_handlers = (VECTOR_REF (fixed_objects, SYSTEM_INTERRUPT_VECTOR));
  interrupt_masks = (VECTOR_REF (fixed_objects, FIXOBJ_INTERRUPT_MASK_VECTOR));
  if (! ((VECTOR_P (interrupt_handlers))
	 && (interrupt_number < (VECTOR_LENGTH (interrupt_handlers)))))
    {
      outf_fatal ("\nUnable to get interrupt handler.");
      terminate_no_interrupt_handler (masked_interrupts);
    }
  interrupt_mask
    = (compute_interrupt_handler_mask (interrupt_masks, interrupt_number));
  interrupt_handler = (VECTOR_REF (interrupt_handlers, interrupt_number));

  stop_history ();
  preserve_interrupt_mask ();
 Will_Push (STACK_ENV_EXTRA_SLOTS + 3);

  /* There used to be some code here for gc checks, but that is done
     uniformly now by RC_NORMAL_GC_DONE. */

  /* Now make an environment frame for use in calling the
     user supplied interrupt routine.  It will be given two arguments:
     the UNmasked interrupt requests, and the currently enabled
     interrupts.  */
  STACK_PUSH (ULONG_TO_FIXNUM (GET_INT_MASK));
  STACK_PUSH (ULONG_TO_FIXNUM (GET_INT_CODE));
  STACK_PUSH (interrupt_handler);
  PUSH_APPLY_FRAME_HEADER (2);
 Pushed ();
  /* Turn off interrupts: */
  SET_INTERRUPT_MASK (interrupt_mask);
}

/* Error processing utilities */

void
err_print (long error_code, outf_channel where)
{
  const char * message
    = ((error_code <= MAX_ERROR)
       ? (Error_Names[error_code])
       : 0);
  if (message == 0)
    outf (where, "Unknown error code %#lx.\n", error_code);
  else
    outf (where, "Error code %#lx (%s).\n", error_code, message);
}

long death_blow;

static void
error_death (long code, char * message)
{
  death_blow = code;
  outf_fatal ("\nMicrocode Error: %s.\n", message);
  err_print (code, FATAL_OUTPUT);
  outf_error ("\n**** Stack Trace ****\n\n");
  Back_Trace (ERROR_OUTPUT);
  termination_no_error_handler ();
  /*NOTREACHED*/
}

void
Stack_Death (void)
{
  outf_fatal("\nWill_Push vs. Pushed inconsistency.\n");
  Microcode_Termination (TERM_BAD_STACK);
  /*NOTREACHED*/
}

void
preserve_interrupt_mask (void)
{
 Will_Push (CONTINUATION_SIZE);
  SET_RC (RC_RESTORE_INT_MASK);
  SET_EXP (ULONG_TO_FIXNUM (GET_INT_MASK));
  SAVE_CONT ();
 Pushed ();
}

/* canonicalize_primitive_context should be used by "unsafe"
   primitives to guarantee that their execution context is the
   expected one, ie.  they are called from the interpreter.  If they
   are called from compiled code, they should abort to the interpreter
   and reenter.  */

void
canonicalize_primitive_context (void)
{
  SCHEME_OBJECT primitive = GET_PRIMITIVE;
  unsigned long n_args;

  assert (PRIMITIVE_P (primitive));
  n_args = (PRIMITIVE_N_ARGUMENTS (primitive));

#ifdef CC_SUPPORT_P
  if (CC_ENTRY_P (STACK_REF (n_args)))
    {
      /* The primitive has been invoked from compiled code. */
      STACK_PUSH (primitive);
      PUSH_APPLY_FRAME_HEADER (n_args);
      guarantee_interp_return ();
      SET_PRIMITIVE (SHARP_F);
      PRIMITIVE_ABORT (PRIM_APPLY);
      /*NOTREACHED*/
    }
#endif
}

/* back_out_of_primitive sets the registers up so that the backout
   mechanism in "interp.c" will cause the primitive to be
   restarted if the error/interrupt is proceeded.  */

void
back_out_of_primitive (void)
{
  SCHEME_OBJECT primitive = GET_PRIMITIVE;
  assert (PRIMITIVE_P (primitive));
  STACK_PUSH (primitive);
  PUSH_APPLY_FRAME_HEADER (PRIMITIVE_N_ARGUMENTS (primitive));
  guarantee_interp_return ();
  SET_PRIMITIVE (SHARP_F);
  SET_EXP (SHARP_F);
  SET_RC (RC_INTERNAL_APPLY);
  SAVE_CONT ();
  SET_ENV (THE_NULL_ENV);
  SET_VAL (SHARP_F);
}

/* Useful error procedures */

/* Note that backing out of the primitives happens after aborting,
   not before.
   This guarantees that the interpreter state is consistent, since the
   longjmp restores the relevant registers even if the primitive was
   invoked from compiled code. */

void
signal_error_from_primitive (long error_code)
{
  PRIMITIVE_ABORT (error_code);
  /*NOTREACHED*/
}

void
signal_interrupt_from_primitive (void)
{
  PRIMITIVE_ABORT (PRIM_INTERRUPT);
  /*NOTREACHED*/
}

void
error_wrong_type_arg (int n)
{
  long error_code;

  switch (n)
    {
    case 1: error_code = ERR_ARG_1_WRONG_TYPE; break;
    case 2: error_code = ERR_ARG_2_WRONG_TYPE; break;
    case 3: error_code = ERR_ARG_3_WRONG_TYPE; break;
    case 4: error_code = ERR_ARG_4_WRONG_TYPE; break;
    case 5: error_code = ERR_ARG_5_WRONG_TYPE; break;
    case 6: error_code = ERR_ARG_6_WRONG_TYPE; break;
    case 7: error_code = ERR_ARG_7_WRONG_TYPE; break;
    case 8: error_code = ERR_ARG_8_WRONG_TYPE; break;
    case 9: error_code = ERR_ARG_9_WRONG_TYPE; break;
    case 10: error_code = ERR_ARG_10_WRONG_TYPE; break;
    default: error_code = ERR_EXTERNAL_RETURN; break;
    }
  signal_error_from_primitive (error_code);
}

void
error_bad_range_arg (int n)
{
  long error_code;

  switch (n)
    {
    case 1: error_code = ERR_ARG_1_BAD_RANGE; break;
    case 2: error_code = ERR_ARG_2_BAD_RANGE; break;
    case 3: error_code = ERR_ARG_3_BAD_RANGE; break;
    case 4: error_code = ERR_ARG_4_BAD_RANGE; break;
    case 5: error_code = ERR_ARG_5_BAD_RANGE; break;
    case 6: error_code = ERR_ARG_6_BAD_RANGE; break;
    case 7: error_code = ERR_ARG_7_BAD_RANGE; break;
    case 8: error_code = ERR_ARG_8_BAD_RANGE; break;
    case 9: error_code = ERR_ARG_9_BAD_RANGE; break;
    case 10: error_code = ERR_ARG_10_BAD_RANGE; break;
    default: error_code = ERR_EXTERNAL_RETURN; break;
    }
  signal_error_from_primitive (error_code);
}

void
error_external_return (void)
{
  signal_error_from_primitive (ERR_EXTERNAL_RETURN);
}

static SCHEME_OBJECT error_argument;

void
error_with_argument (SCHEME_OBJECT argument)
{
  error_argument = argument;
  signal_error_from_primitive
    (((VECTOR_P (argument))
      && ((VECTOR_LENGTH (argument)) > 0)
      && ((VECTOR_REF (argument, 0))
	  == (LONG_TO_UNSIGNED_FIXNUM (ERR_IN_SYSTEM_CALL))))
     ? ERR_IN_SYSTEM_CALL
     : ERR_WITH_ARGUMENT);
  /*NOTREACHED*/
}

void
error_in_system_call (enum syserr_names err, enum syscall_names name)
{
  /* System call errors have some additional information.
     Encode this as a vector in place of the error code.  */
  SCHEME_OBJECT v = (allocate_marked_vector (TC_VECTOR, 3, 0));
  VECTOR_SET (v, 0, (LONG_TO_UNSIGNED_FIXNUM (ERR_IN_SYSTEM_CALL)));
  VECTOR_SET (v, 1, (LONG_TO_UNSIGNED_FIXNUM ((unsigned int) err)));
  VECTOR_SET (v, 2, (LONG_TO_UNSIGNED_FIXNUM ((unsigned int) name)));
  error_argument = v;
  signal_error_from_primitive (ERR_IN_SYSTEM_CALL);
  /*NOTREACHED*/
}

void
error_system_call (int code, enum syscall_names name)
{
  error_in_system_call ((OS_error_code_to_syserr (code)), name);
  /*NOTREACHED*/
}

long
arg_integer (int arg_number)
{
  SCHEME_OBJECT object = (ARG_REF (arg_number));
  if (! (INTEGER_P (object)))
    error_wrong_type_arg (arg_number);
  if (! (integer_to_long_p (object)))
    error_bad_range_arg (arg_number);
  return (integer_to_long (object));
}

long
arg_nonnegative_integer (int arg_number)
{
  long result = (arg_integer (arg_number));
  if (result < 0)
    error_bad_range_arg (arg_number);
  return (result);
}

long
arg_index_integer (int arg_number, long upper_limit)
{
  long result = (arg_integer (arg_number));
  if ((result < 0) || (result >= upper_limit))
    error_bad_range_arg (arg_number);
  return (result);
}

long
arg_integer_in_range (int arg_number, long lower_limit, long upper_limit)
{
  long result = (arg_integer (arg_number));
  if ((result < lower_limit) || (result >= upper_limit))
    error_bad_range_arg (arg_number);
  return (result);
}

unsigned long
arg_ulong_integer (int arg_number)
{
  SCHEME_OBJECT object = (ARG_REF (arg_number));
  if (! (INTEGER_P (object)))
    error_wrong_type_arg (arg_number);
  if (! (integer_to_ulong_p (object)))
    error_bad_range_arg (arg_number);
  return (integer_to_ulong (object));
}

unsigned long
arg_ulong_index_integer (int arg_number, unsigned long upper_limit)
{
  unsigned long result = (arg_ulong_integer (arg_number));
  if (result >= upper_limit)
    error_bad_range_arg (arg_number);
  return (result);
}

unsigned long
arg_ulong_integer_in_range (int arg_number,
			    unsigned long lower_limit,
			    unsigned long upper_limit)
{
  unsigned long result = (arg_ulong_integer (arg_number));
  if (! ((result >= lower_limit) && (result < upper_limit)))
    error_bad_range_arg (arg_number);
  return (result);
}

bool
real_number_to_double_p (SCHEME_OBJECT x)
{
  return ((! (BIGNUM_P (x))) || (BIGNUM_TO_DOUBLE_P (x)));
}

double
real_number_to_double (SCHEME_OBJECT x)
{
  return
    ((FIXNUM_P (x))
     ? (FIXNUM_TO_DOUBLE (x))
     : (BIGNUM_P (x))
     ? (bignum_to_double (x))
     : (FLONUM_TO_DOUBLE (x)));
}

double
arg_real_number (int arg_number)
{
  SCHEME_OBJECT number = (ARG_REF (arg_number));
  if (! (REAL_P (number)))
    error_wrong_type_arg (arg_number);
  if (! (real_number_to_double_p (number)))
    error_bad_range_arg (arg_number);
  return (real_number_to_double (number));
}

double
arg_real_in_range (int arg_number, double lower_limit, double upper_limit)
{
  double result = (arg_real_number (arg_number));
  if ((result < lower_limit) || (result > upper_limit))
    error_bad_range_arg (arg_number);
  return (result);
}

bool
interpreter_applicable_p (SCHEME_OBJECT object)
{
 tail_recurse:
  switch (OBJECT_TYPE (object))
    {
    case TC_PRIMITIVE:
    case TC_PROCEDURE:
    case TC_EXTENDED_PROCEDURE:
    case TC_CONTROL_POINT:
      return (true);

    case TC_ENTITY:
      {
	object = (MEMORY_REF (object, ENTITY_OPERATOR));
	goto tail_recurse;
      }
#ifdef CC_SUPPORT_P
    case TC_COMPILED_ENTRY:
      {
	cc_entry_type_t cet;
	return
	  ((read_cc_entry_type ((&cet), (CC_ENTRY_ADDRESS (object))))
	   ? false
	   : ((cet.marker) == CET_PROCEDURE));
      }
#endif
    default:
      return (false);
    }
}

/* Error handling

   It is assumed that any caller of the error code has already
   restored its state to a situation which will make it restartable if
   the error handler returns normally.  As a result, the only work to
   be done on an error is to verify that there is an error handler,
   save the current continuation and create a new one if entered from
   Pop_Return rather than Eval, turn off interrupts, and call it with
   two arguments: the error code and interrupt enables.  */

void
Do_Micro_Error (long error_code, bool from_pop_return_p)
{
  SCHEME_OBJECT handler = SHARP_F;

#ifdef ENABLE_DEBUGGING_TOOLS
  err_print (error_code, ERROR_OUTPUT);
  if ((GET_RC == RC_INTERNAL_APPLY)
      || (GET_RC == RC_INTERNAL_APPLY_VAL))
    {
      SCHEME_OBJECT * sp = (STACK_LOC (CONTINUATION_SIZE));
      Print_Expression ((sp[STACK_ENV_FUNCTION]), "Procedure was");
      outf_error ("\n");
      outf_error ("# of arguments: %lu\n",
		  (APPLY_FRAME_HEADER_N_ARGS (sp[STACK_ENV_HEADER])));
    }
  else
    {
      Print_Expression (GET_EXP, "Expression was");
      outf_error ("\n");
      Print_Expression (GET_ENV, "Environment was");
      outf_error ("\n");
    }
  Print_Return ("Return code");
  outf_error ("\n");
#endif

  if (Trace_On_Error)
    {
      outf_error ("\n\n**** Stack Trace ****\n\n");
      Back_Trace (ERROR_OUTPUT);
    }

#ifdef ENABLE_DEBUGGING_TOOLS
  {
    unsigned int * from = local_circle;
    unsigned int * end = (from + local_nslots);
    unsigned int * to = debug_circle;
    while (from < end)
      (*to++) = (*from++);
  }
  debug_nslots = local_nslots;
  debug_slotno = local_slotno;
#endif

  Will_Push (CONTINUATION_SIZE + (from_pop_return_p ? 0 : 1));
  if (from_pop_return_p)
    SET_EXP (GET_VAL);
  else
    PUSH_ENV ();
  SET_RC (from_pop_return_p ? RC_POP_RETURN_ERROR : RC_EVAL_ERROR);
  SAVE_CONT ();
  Pushed ();

  {
    SCHEME_OBJECT error_vector = SHARP_F;
    if (VECTOR_P (fixed_objects))
      error_vector = (VECTOR_REF (fixed_objects, SYSTEM_ERROR_VECTOR));
    if (!VECTOR_P (error_vector))
      error_death (error_code, "No error handlers");
    if ((error_code >= 0) && (error_code < (VECTOR_LENGTH (error_vector))))
      handler = (VECTOR_REF (error_vector, error_code));
    else if (ERR_BAD_ERROR_CODE < (VECTOR_LENGTH (error_vector)))
      handler = (VECTOR_REF (error_vector, ERR_BAD_ERROR_CODE));
    else
      error_death (error_code, "No error handlers");
  }

  /* Return from error handler will re-enable interrupts & restore history */
  stop_history ();
  preserve_interrupt_mask ();

  Will_Push (STACK_ENV_EXTRA_SLOTS + 3);
  /* Arg 2:     interrupt mask */
  STACK_PUSH (ULONG_TO_FIXNUM (GET_INT_MASK));
  /* Arg 1:     error code  */
  if ((error_code == ERR_WITH_ARGUMENT) || (error_code == ERR_IN_SYSTEM_CALL))
    STACK_PUSH (error_argument);
  else
    STACK_PUSH (long_to_integer (error_code));
  STACK_PUSH (handler);
  PUSH_APPLY_FRAME_HEADER (2);
  Pushed ();

  /* Disable all interrupts */
  SET_INTERRUPT_MASK (0);
}

/* History */

void
reset_history (void)
{
  prev_restore_history_offset = 0;
  history_register
    = (((VECTOR_P (fixed_objects))
	&& ((READ_DUMMY_HISTORY ()) != SHARP_F))
       ? (OBJECT_ADDRESS (READ_DUMMY_HISTORY ()))
       : (make_dummy_history ()));
}

SCHEME_OBJECT *
make_dummy_history (void)
{
  SCHEME_OBJECT * rib = Free;
  (Free[RIB_EXP]) = SHARP_F;
  (Free[RIB_ENV]) = SHARP_F;
  (Free[RIB_NEXT_REDUCTION])
    = (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, rib));
  Free += 3;
  {
    SCHEME_OBJECT * history = Free;
    (Free[HIST_RIB])
      = (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, rib));
    (Free[HIST_NEXT_SUBPROBLEM])
      = (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, history));
    (Free[HIST_PREV_SUBPROBLEM])
      = (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, history));
    Free += 3;
    return (history);
  }
}

/* save_history places a restore history frame on the stack. Such a
   frame consists of a normal continuation frame plus a pointer to the
   stacklet on which the last restore history is located and the
   offset within that stacklet.  If the last restore history is in
   this stacklet then the history pointer is #F to signify this.  If
   there is no previous restore history then the history pointer is #F
   and the offset is 0. */

void
save_history (unsigned long rc)
{
  Will_Push (HISTORY_SIZE);
  STACK_PUSH (SHARP_F);		/* Prev_Restore_History_Stacklet */
  STACK_PUSH (ULONG_TO_FIXNUM (prev_restore_history_offset));
  SET_EXP (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, history_register));
  SET_RC (rc);
  SAVE_CONT ();
  Pushed ();
  history_register = (OBJECT_ADDRESS (READ_DUMMY_HISTORY ()));
}

/* restore_history pops a history object off the stack and makes a
   copy of it the current history collection object.  This is called
   only from the RC_RESTORE_HISTORY case in "interp.c".  */

bool
restore_history (SCHEME_OBJECT hist_obj)
{
  SCHEME_OBJECT new_hist = (copy_history (hist_obj));
  if (new_hist == SHARP_F)
    return (false);
  history_register = (OBJECT_ADDRESS (new_hist));
  return (true);
}

/* The entire trick to history is right here: it is either copied or
   reused when restored.  Initially, stop_history marks the stack so
   that the history will merely be popped and reused.  On a catch,
   however, the return code is changed to force the history to be
   copied instead.  Thus, histories saved as part of a control point
   are not side-effected in the history collection process.  */

void
stop_history (void)
{
  SCHEME_OBJECT exp = GET_EXP;
  SCHEME_OBJECT ret = GET_RET;
  SAVE_HISTORY (RC_RESTORE_DONT_COPY_HISTORY);
  prev_restore_history_offset = (STACK_N_PUSHED + CONTINUATION_RETURN_CODE);
  SET_RET (ret);
  SET_EXP (exp);
}

void
new_subproblem (SCHEME_OBJECT expression, SCHEME_OBJECT environment)
{
  history_register = (OBJECT_ADDRESS (history_register[HIST_NEXT_SUBPROBLEM]));
  HISTORY_MARK (history_register[HIST_MARK]);
  {
    SCHEME_OBJECT * rib = (OBJECT_ADDRESS (history_register[HIST_RIB]));
    HISTORY_MARK (rib[RIB_MARK]);
    (rib[RIB_ENV]) = environment;
    (rib[RIB_EXP]) = expression;
  }
}

void
reuse_subproblem (SCHEME_OBJECT expression, SCHEME_OBJECT environment)
{
  SCHEME_OBJECT * rib = (OBJECT_ADDRESS (history_register[HIST_RIB]));
  HISTORY_MARK (rib[RIB_MARK]);
  (rib[RIB_ENV]) = environment;
  (rib[RIB_EXP]) = expression;
}

void
new_reduction (SCHEME_OBJECT expression, SCHEME_OBJECT environment)
{
  SCHEME_OBJECT * rib
    = (OBJECT_ADDRESS
       (MEMORY_REF ((history_register[HIST_RIB]), RIB_NEXT_REDUCTION)));
  (history_register[HIST_RIB])
    = (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, rib));
  (rib[RIB_ENV]) = (environment);
  (rib[RIB_EXP]) = (expression);
  HISTORY_UNMARK (rib[RIB_MARK]);
}

void
end_subproblem (void)
{
  HISTORY_UNMARK (history_register[HIST_MARK]);
  history_register = (OBJECT_ADDRESS (history_register[HIST_PREV_SUBPROBLEM]));
}

void
compiler_new_subproblem (void)
{
  new_subproblem (SHARP_F, (MAKE_RETURN_CODE (RC_POP_FROM_COMPILED_CODE)));
}

void
compiler_new_reduction (void)
{
  new_reduction (SHARP_F, (MAKE_RETURN_CODE (RC_POP_FROM_COMPILED_CODE)));
}

/* Returns SHARP_F if insufficient space available.  */

static SCHEME_OBJECT
copy_history (SCHEME_OBJECT hist_obj)
{
  unsigned long space_left, vert_type, rib_type;
  SCHEME_OBJECT new_hunk, * last_hunk, * hist_ptr, * orig_hist, temp;
  SCHEME_OBJECT * orig_rib, * source_rib, * rib_slot;

  assert (HUNK3_P (hist_obj));

  space_left = (SPACE_BEFORE_GC ());
  if (space_left < 3)
    return (SHARP_F);
  space_left -= 3;

  vert_type = (OBJECT_TYPE (hist_obj));
  orig_hist = (OBJECT_ADDRESS (hist_obj));
  hist_ptr = orig_hist;
  last_hunk = (heap_end - 3);

  do
    {
      /* Allocate and link the vertebra. */
      if (space_left < 3)
	return (SHARP_F);
      space_left -= 3;

      new_hunk = (MAKE_POINTER_OBJECT (vert_type, Free));
      (last_hunk[HIST_NEXT_SUBPROBLEM]) = new_hunk;

      (Free[HIST_PREV_SUBPROBLEM])
	= (MAKE_POINTER_OBJECT ((OBJECT_TYPE (hist_ptr[HIST_PREV_SUBPROBLEM])),
				last_hunk));
      last_hunk = Free;
      Free += 3;

      /* Copy the rib. */
      temp = (hist_ptr[HIST_RIB]);
      rib_type = (OBJECT_TYPE (temp));
      orig_rib = (OBJECT_ADDRESS (temp));
      rib_slot = (last_hunk + HIST_RIB);

      source_rib = orig_rib;

      do
	{
	  if (space_left < 3)
	    return (SHARP_F);
	  space_left -= 3;

	  (*rib_slot) = (MAKE_POINTER_OBJECT (rib_type, Free));
	  (Free[RIB_EXP]) = (source_rib[RIB_EXP]);
	  (Free[RIB_ENV]) = (source_rib[RIB_ENV]);
	  rib_slot = (Free + RIB_NEXT_REDUCTION);
	  Free += 3;
	  temp = (source_rib[RIB_NEXT_REDUCTION]);
	  rib_type = (OBJECT_TYPE (temp));
	  source_rib = (OBJECT_ADDRESS (temp));
	}
      while (source_rib != orig_rib);

      (*rib_slot) = (OBJECT_NEW_TYPE (rib_type, (last_hunk[HIST_RIB])));

      temp = (hist_ptr[HIST_NEXT_SUBPROBLEM]);
      vert_type = (OBJECT_TYPE (temp));
      hist_ptr = (OBJECT_ADDRESS (temp));
    }
  while (hist_ptr != orig_hist);

  new_hunk = (heap_end [HIST_NEXT_SUBPROBLEM - 3]);
  (last_hunk[HIST_NEXT_SUBPROBLEM]) = (OBJECT_NEW_TYPE (vert_type, new_hunk));
  MEMORY_SET (new_hunk, HIST_PREV_SUBPROBLEM,
	      (MAKE_POINTER_OBJECT
	       ((OBJECT_TYPE (hist_ptr[HIST_PREV_SUBPROBLEM])),
		last_hunk)));
  return (new_hunk);
}

/* If a "debugging" version of the interpreter is made, then this
   procedure is called to actually invoke a primitive.  When a
   "production" version is made, all of the consistency checks are
   omitted and a macro from "interp.h" is used to directly code the
   call to the primitive function. */

#ifdef ENABLE_DEBUGGING_TOOLS

void
primitive_apply_internal (SCHEME_OBJECT primitive)
{
  if (Primitive_Debug)
    Print_Primitive (primitive);
#if 0
  {
    SCHEME_OBJECT * saved_stack = stack_pointer;
    PRIMITIVE_APPLY_INTERNAL (primitive);
    /* Some primitives violate this condition, for example,
       WITH-INTERRUPT-MASK.  */
    if (saved_stack != stack_pointer)
      {
	unsigned long arity = (PRIMITIVE_N_ARGUMENTS (primitive));
	Print_Expression (primitive, "Stack bad after ");
	outf_fatal ("\nStack was %#lx, now %#lx, #args=%lu.\n",
		    ((unsigned long) saved_stack),
		    ((unsigned long) stack_pointer),
		    arity);
	Microcode_Termination (TERM_EXIT);
      }
  }
#else
  PRIMITIVE_APPLY_INTERNAL (primitive);
#endif
  if (Primitive_Debug)
    {
      Print_Expression (GET_VAL, "Primitive Result");
      outf_error("\n");
      outf_flush_error();
    }
}

#endif /* ENABLE_DEBUGGING_TOOLS */

#ifdef ENABLE_PRIMITIVE_PROFILING

/* The profiling mechanism is enabled by storing a vector in the fixed
   objects vector.  The vector should be initialized to contain all
   zeros.  */

void
record_primitive_entry (SCHEME_OBJECT primitive)
{

  if (VECTOR_P (fixed_objects))
    {
      SCHEME_OBJECT table
	= (VECTOR_REF (fixed_objects, Primitive_Profiling_Table));
      if (VECTOR_P (table))
	{
	  unsigned long index = (OBJECT_DATUM (primitive));
	  VECTOR_SET (table,
		      index,
		      (ulong_to_integer
		       (1 + (integer_to_ulong (VECTOR_REF (table, index))))));
	}
    }
}

#endif /* ENABLE_PRIMITIVE_PROFILING */

/* Dynamic Winder support code */

SCHEME_OBJECT
Find_State_Space (SCHEME_OBJECT State_Point)
{
  long How_Far =
    (UNSIGNED_FIXNUM_TO_LONG
     (MEMORY_REF (State_Point, STATE_POINT_DISTANCE_TO_ROOT)));
  long i;
  SCHEME_OBJECT Point = State_Point;

  for (i=0; i <= How_Far; i++)
  {
#ifdef ENABLE_DEBUGGING_TOOLS
    if (Point == SHARP_F)
    {
      outf_fatal("\nState_Point %#lx wrong: count was %ld, #F at %ld\n",
	      ((long) State_Point), ((long) How_Far), ((long) i));
      Microcode_Termination(TERM_EXIT);
      /*NOTREACHED*/
    }
#endif /* ENABLE_DEBUGGING_TOOLS */
    Point = MEMORY_REF (Point, STATE_POINT_NEARER_POINT);
  }
  return (Point);
}

/* Assumptions:

     (1) On a single processor, things should work with multiple state
	 spaces.  The microcode variable current_state_point tracks
	 the location in the "boot" space (i.e. the one whose space is
	 #F) and the state spaces themselves (roots of the space
	 trees) track the other spaces.
     (2) On multi-processors, multiple spaces DO NOT work.  Only the
	 initial space (#F) is tracked by the microcode (it is
	 swapped on every task switch), but no association with trees
	 is kept.  This will work since the initial tree has no space
	 at the root, indicating that the microcode variable rather
	 than the state space contains the current state space
	 location.

   NOTE: This procedure is invoked both by primitives and the interpreter
   itself.  As such, it is using the pun that PRIMITIVE_ABORT is just a
   (non-local) return to the interpreter.  This should be cleaned up.
   NOTE: Any primitive that invokes this procedure must do a
   canonicalize_primitive_context() first!  */

void
Translate_To_Point (SCHEME_OBJECT Target)
{
  SCHEME_OBJECT State_Space, Current_Location, *Path;
  SCHEME_OBJECT Path_Point, *Path_Ptr;
  long Distance, Merge_Depth, From_Depth, i;

  State_Space = Find_State_Space(Target);
  Path = Free;
  Distance =
    (UNSIGNED_FIXNUM_TO_LONG
     (MEMORY_REF (Target, STATE_POINT_DISTANCE_TO_ROOT)));
  if (State_Space == SHARP_F)
    Current_Location = current_state_point;
  else
    Current_Location = MEMORY_REF (State_Space, STATE_SPACE_NEAREST_POINT);

  if (Target == Current_Location)
  {
    PRIMITIVE_ABORT (PRIM_POP_RETURN);
    /*NOTREACHED*/
  }

  for (Path_Ptr = (&(Path[Distance])), Path_Point = Target, i = 0;
       i <= Distance;
       i++)
  {
    *Path_Ptr-- = Path_Point;
    Path_Point = MEMORY_REF (Path_Point, STATE_POINT_NEARER_POINT);
  }

  From_Depth =
    (UNSIGNED_FIXNUM_TO_LONG
     (MEMORY_REF (Current_Location, STATE_POINT_DISTANCE_TO_ROOT)));

  for (Path_Point = Current_Location, Merge_Depth = From_Depth;
       Merge_Depth > Distance;
       Merge_Depth--)
    Path_Point = MEMORY_REF (Path_Point, STATE_POINT_NEARER_POINT);

  for (Path_Ptr = (&(Path[Merge_Depth]));
       Merge_Depth >= 0;
       Merge_Depth--, Path_Ptr--)
  {
    if (*Path_Ptr == Path_Point)
      break;
    Path_Point = MEMORY_REF (Path_Point, STATE_POINT_NEARER_POINT);
  }

#ifdef ENABLE_DEBUGGING_TOOLS
  if (Merge_Depth < 0)
  {
    outf_fatal ("\nMerge_Depth went negative: %ld\n", Merge_Depth);
    Microcode_Termination (TERM_EXIT);
  }
#endif /* ENABLE_DEBUGGING_TOOLS */

  preserve_interrupt_mask ();
 Will_Push(CONTINUATION_SIZE + 4);
  STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM((Distance - Merge_Depth)));
  STACK_PUSH (Target);
  STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM((From_Depth - Merge_Depth)));
  STACK_PUSH (Current_Location);
  SET_EXP (State_Space);
  SET_RC(RC_MOVE_TO_ADJACENT_POINT);
  SAVE_CONT();
 Pushed();

  /* Disable lower than GC level */
  SET_INTERRUPT_MASK (GET_INT_MASK & ((INT_GC << 1) - 1));
  PRIMITIVE_ABORT (PRIM_POP_RETURN);
  /*NOTREACHED*/
}

#ifdef __WIN32__

#include <windows.h>

SCHEME_OBJECT
Compiler_Get_Fixed_Objects (void)
{
  return ((VECTOR_P (fixed_objects)) ? fixed_objects : SHARP_F);
}

extern SCHEME_OBJECT Re_Enter_Interpreter (void);
extern SCHEME_OBJECT C_call_scheme
  (SCHEME_OBJECT, long, SCHEME_OBJECT *);

SCHEME_OBJECT
C_call_scheme (SCHEME_OBJECT proc,
       long n_args,
       SCHEME_OBJECT * argvec)
{
  SCHEME_OBJECT primitive, prim_lexpr, * sp, result;
  SCHEME_OBJECT * callers_last_return_code;

#ifdef CC_IS_NATIVE
  extern void * C_Frame_Pointer;
  extern void * C_Stack_Pointer;
  void * cfp = C_Frame_Pointer;
  void * csp = C_Stack_Pointer;
#ifdef CL386
  __try
#endif
#endif
  {
    primitive = GET_PRIMITIVE;
    prim_lexpr = GET_LEXPR_ACTUALS;
    callers_last_return_code = last_return_code;

    if (! (PRIMITIVE_P (primitive)))
      abort_to_interpreter (ERR_CANNOT_RECURSE);
      /*NOTREACHED*/
    sp = stack_pointer;

   Will_Push ((2 * CONTINUATION_SIZE) + (n_args + STACK_ENV_EXTRA_SLOTS + 1));
    {
      long i;

      SET_RC (RC_END_OF_COMPUTATION);
      SET_EXP (primitive);
      SAVE_CONT ();

      for (i = n_args; --i >= 0; )
	STACK_PUSH (argvec[i]);
      STACK_PUSH (proc);
      PUSH_APPLY_FRAME_HEADER (n_args);

      SET_RC (RC_INTERNAL_APPLY);
      SET_EXP (SHARP_F);
      SAVE_CONT ();
    }
   Pushed ();
    result = (Re_Enter_Interpreter ());

    if (stack_pointer != sp)
      signal_error_from_primitive (ERR_STACK_HAS_SLIPPED);
      /*NOTREACHED*/

    last_return_code = callers_last_return_code;
    SET_LEXPR_ACTUALS (prim_lexpr);
    SET_PRIMITIVE (primitive);
  }
#ifdef CC_IS_NATIVE
#ifdef CL386
  __finally
#endif
  {
    C_Frame_Pointer = cfp;
    C_Stack_Pointer = csp;
  }
#endif

  return  result;
}

#endif /* __WIN32__ */

void
set_ptr_register (unsigned int index, SCHEME_OBJECT * p)
{
  (Registers[index]) = ((SCHEME_OBJECT) p);
}

void
set_ulong_register (unsigned int index, unsigned long value)
{
  (Registers[index]) = ((SCHEME_OBJECT) value);
}
