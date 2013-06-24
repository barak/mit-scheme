/* -*-C-*-

$Id: utils.c,v 9.79 2001/07/31 03:12:15 cph Exp $

Copyright (c) 1987-2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
*/

/* This file contains utilities for interrupts, errors, etc. */

#include "scheme.h"
#include "prims.h"
#include "winder.h"
#include "history.h"
#include "cmpint.h"
#include "syscall.h"

#ifdef __OS2__
extern void OS2_handle_attention_interrupt (void);
#endif

/* Helper procedures for Setup_Interrupt, which follows. */

static long
DEFUN (compute_interrupt_number, (masked_interrupts),
       long masked_interrupts)
{
  long interrupt_number = 0;
  long bit_mask = 1;
  while ((interrupt_number <= MAX_INTERRUPT_NUMBER)
	 && ((masked_interrupts & bit_mask) == 0))
    {
      interrupt_number += 1;
      bit_mask <<= 1;
    }
  return (interrupt_number);
}

/* This default is solely for compatibility with the previous behavior
   of the microcode.  It is not a good default and should be
   overridden by the runtime system.  */
#define DEFAULT_INTERRUPT_HANDLER_MASK(interrupt_number)		\
  ((1 << (interrupt_number)) - 1)

static long
DEFUN (compute_interrupt_handler_mask, (interrupt_masks, interrupt_number),
       SCHEME_OBJECT interrupt_masks AND
       long interrupt_number)
{
  if ((VECTOR_P (interrupt_masks))
      && (interrupt_number <= ((long) (VECTOR_LENGTH (interrupt_masks)))))
    {
      SCHEME_OBJECT mask =
	(VECTOR_REF (interrupt_masks, interrupt_number));
      if ((INTEGER_P (mask)) && (integer_to_long_p (mask)))
	/* Guarantee that the given interrupt is disabled.  */
	return ((integer_to_long (mask)) &~ (1 << interrupt_number));
    }
  return
    ((interrupt_number <= MAX_INTERRUPT_NUMBER)
     ? (DEFAULT_INTERRUPT_HANDLER_MASK (interrupt_number))
     : (FETCH_INTERRUPT_MASK ()));
}

static void
DEFUN (terminate_no_interrupt_handler, (masked_interrupts),
       long masked_interrupts)
{
  outf_fatal("\nInterrupts = 0x%08lx, Mask = 0x%08lx, Masked = 0x%08lx\n",
	     (FETCH_INTERRUPT_CODE ()),
	     (FETCH_INTERRUPT_MASK ()),
	     masked_interrupts);
  Microcode_Termination (TERM_NO_INTERRUPT_HANDLER);
}

SCHEME_OBJECT
DEFUN_VOID (initialize_interrupt_handler_vector)
{
  return (make_vector ((MAX_INTERRUPT_NUMBER + 2), SHARP_F, false));
}

SCHEME_OBJECT
DEFUN_VOID (initialize_interrupt_mask_vector)
{
  SCHEME_OBJECT result =
    (make_vector ((MAX_INTERRUPT_NUMBER + 2), SHARP_F, false));
  long interrupt_number;

  for (interrupt_number = 0;
       (interrupt_number <= MAX_INTERRUPT_NUMBER);
       interrupt_number += 1)
    VECTOR_SET
      (result, interrupt_number,
       (long_to_integer (DEFAULT_INTERRUPT_HANDLER_MASK (interrupt_number))));
  return (result);
}

/* Setup_Interrupt is called from the Interrupt macro to do all of the
   setup for calling the user's interrupt routines. */

void
DEFUN (Setup_Interrupt, (masked_interrupts), long masked_interrupts)
{
  SCHEME_OBJECT interrupt_handlers = SHARP_F;
  SCHEME_OBJECT interrupt_masks = SHARP_F;
  long interrupt_number = (compute_interrupt_number (masked_interrupts));
  long interrupt_mask;
  SCHEME_OBJECT interrupt_handler;

#ifdef __OS2__
  if ((1 << interrupt_number) == INT_Global_1)
    {
      OS2_handle_attention_interrupt ();
      abort_to_interpreter (PRIM_POP_RETURN);
    }
#endif /* __OS2__ */
  if (! (Valid_Fixed_Obj_Vector ()))
    {
      outf_fatal ("\nInvalid fixed-objects vector.");
      terminate_no_interrupt_handler (masked_interrupts);
    }
  interrupt_handlers = (Get_Fixed_Obj_Slot (System_Interrupt_Vector));
  interrupt_masks = (Get_Fixed_Obj_Slot (FIXOBJ_INTERRUPT_MASK_VECTOR));
  if (! (VECTOR_P (interrupt_handlers)))
    {
      outf_fatal ("\nInvalid handlers vector (0x%lx).", interrupt_handlers);
      terminate_no_interrupt_handler (masked_interrupts);
    }
  if (interrupt_number >= ((long) (VECTOR_LENGTH (interrupt_handlers))))
    {
      outf_fatal("\nInterrupt out of range: %ld (vector length = %ld).",
		 interrupt_number,
		 (VECTOR_LENGTH (interrupt_handlers)));
      terminate_no_interrupt_handler (masked_interrupts);
    }
  interrupt_mask =
    (compute_interrupt_handler_mask (interrupt_masks, interrupt_number));
  Global_Interrupt_Hook ();
  interrupt_handler = (VECTOR_REF (interrupt_handlers, interrupt_number));

#if 0
  /* This label may be used in Global_Interrupt_Hook: */
 passed_checks:
#endif
  Stop_History ();
  preserve_interrupt_mask ();
 Will_Push (STACK_ENV_EXTRA_SLOTS + 3);

  /* There used to be some code here for gc checks, but that is done
     uniformly now by RC_NORMAL_GC_DONE. */

  /* Now make an environment frame for use in calling the
     user supplied interrupt routine.  It will be given two arguments:
     the UNmasked interrupt requests, and the currently enabled
     interrupts.  */
  STACK_PUSH (LONG_TO_FIXNUM (FETCH_INTERRUPT_MASK ()));
  STACK_PUSH (LONG_TO_FIXNUM (FETCH_INTERRUPT_CODE ()));
  STACK_PUSH (interrupt_handler);
  STACK_PUSH (STACK_FRAME_HEADER + 2);
 Pushed ();
  /* Turn off interrupts: */
  SET_INTERRUPT_MASK (interrupt_mask);
}

/* Error processing utilities */

void
DEFUN (err_print, (error_code, where), long error_code AND outf_channel where)
{
  extern char * Error_Names [];

  if (error_code > MAX_ERROR)
    outf (where, "Unknown error code 0x%lx.\n", error_code);
  else
    outf (where, "Error code 0x%lx (%s).\n",
	     error_code,
	     (Error_Names [error_code]));
  return;
}

extern long death_blow;
long death_blow;

void
DEFUN (error_death, (code, message), long code AND char * message)
{
  death_blow = code;
  outf_fatal ("\nMicrocode Error: %s.\n", message);
  err_print (code, fatal_output);
  outf_error ("\n**** Stack Trace ****\n\n");
  Back_Trace (error_output);
  termination_no_error_handler ();
  /*NOTREACHED*/
}

void
DEFUN_VOID (Stack_Death)
{
  outf_fatal("\nWill_Push vs. Pushed inconsistency.\n");
  Microcode_Termination (TERM_BAD_STACK);
  /*NOTREACHED*/
}

void
DEFUN_VOID (preserve_interrupt_mask)
{
 Will_Push (CONTINUATION_SIZE);
  Store_Return (RC_RESTORE_INT_MASK);
  Store_Expression (LONG_TO_FIXNUM (FETCH_INTERRUPT_MASK ()));
  Save_Cont ();
 Pushed ();
  return;
}

/* back_out_of_primitive sets the registers up so that the backout
   mechanism in interpret.c will cause the primitive to be
   restarted if the error/interrupt is proceeded. */

void
DEFUN_VOID (back_out_of_primitive_internal)
{
  long nargs;
  SCHEME_OBJECT primitive;

  /* Setup a continuation to return to compiled code if the primitive is
     restarted and completes successfully. */

  primitive = (Regs [REGBLOCK_PRIMITIVE]);
  if (! (PRIMITIVE_P (primitive)))
    {
      outf_fatal(
	      "\nback_out_of_primitive backing out when not in primitive!\n");
      Microcode_Termination (TERM_BAD_BACK_OUT);
    }
  nargs = (PRIMITIVE_N_ARGUMENTS (primitive));
  if (COMPILED_CODE_ADDRESS_P (STACK_REF (nargs)))
    compiler_apply_procedure (nargs);
  STACK_PUSH (primitive);
  STACK_PUSH (STACK_FRAME_HEADER + nargs);
  Store_Env (THE_NULL_ENV);
  Val = SHARP_F;
  Store_Return (RC_INTERNAL_APPLY);
  Store_Expression (SHARP_F);
  (Regs [REGBLOCK_PRIMITIVE]) = SHARP_F;
  return;
}

void
DEFUN_VOID (back_out_of_primitive)
{
  back_out_of_primitive_internal ();
  Save_Cont ();
  return;
}

/* canonicalize_primitive_context should be used by "unsafe" primitives
   to guarantee that their execution context is the expected one, ie.
   they are called from the interpreter.
   If they are called from compiled code, they should abort to the
   interpreter and reenter.
   Note: This is called only from the macro PRIMITIVE_CANONICALIZE_CONTEXT,
   so that the work can be divided between them if it is an issue. */

void
DEFUN_VOID (canonicalize_primitive_context)
{
  long nargs;
  SCHEME_OBJECT primitive;

  primitive = (Regs [REGBLOCK_PRIMITIVE]);
  if (! (PRIMITIVE_P (primitive)))
    {
      outf_fatal
	("\ncanonicalize_primitive_context invoked when not in primitive!\n");
      Microcode_Termination (TERM_BAD_BACK_OUT);
    }
  nargs = (PRIMITIVE_N_ARGUMENTS (primitive));
  if (! (COMPILED_CODE_ADDRESS_P (STACK_REF (nargs))))
    return;
  /* The primitive has been invoked from compiled code. */
  PRIMITIVE_ABORT (PRIM_REENTER);
  /*NOTREACHED*/
}

/* Useful error procedures */

/* Note that backing out of the primitives happens after aborting,
   not before.
   This guarantees that the interpreter state is consistent, since the
   longjmp restores the relevant registers even if the primitive was
   invoked from compiled code. */

void
DEFUN (signal_error_from_primitive, (error_code), long error_code)
{
  PRIMITIVE_ABORT (error_code);
  /*NOTREACHED*/
}

void
DEFUN_VOID (signal_interrupt_from_primitive)
{
  PRIMITIVE_ABORT (PRIM_INTERRUPT);
  /*NOTREACHED*/
}

void
DEFUN (error_wrong_type_arg, (n), int n)
{
  fast long error_code;

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
DEFUN (error_bad_range_arg, (n), int n)
{
  fast long error_code;

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
DEFUN_VOID (error_external_return)
{
  signal_error_from_primitive (ERR_EXTERNAL_RETURN);
}

static SCHEME_OBJECT error_argument;

void
DEFUN (error_with_argument, (argument), SCHEME_OBJECT argument)
{
  error_argument = argument;
  signal_error_from_primitive (ERR_WITH_ARGUMENT);
  /*NOTREACHED*/
}

void
DEFUN (error_in_system_call, (err, name),
       enum syserr_names err AND enum syscall_names name)
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
DEFUN (error_system_call, (code, name),
       int code AND enum syscall_names name)
{
  error_in_system_call ((OS_error_code_to_syserr (code)), name);
  /*NOTREACHED*/
}

long
DEFUN (arg_integer, (arg_number), int arg_number)
{
  fast SCHEME_OBJECT object = (ARG_REF (arg_number));
  if (! (INTEGER_P (object)))
    error_wrong_type_arg (arg_number);
  if (! (integer_to_long_p (object)))
    error_bad_range_arg (arg_number);
  return (integer_to_long (object));
}

long
DEFUN (arg_nonnegative_integer, (arg_number), int arg_number)
{
  fast long result = (arg_integer (arg_number));
  if (result < 0)
    error_bad_range_arg (arg_number);
  return (result);
}

long
DEFUN (arg_index_integer, (arg_number, upper_limit),
       int arg_number AND long upper_limit)
{
  fast long result = (arg_integer (arg_number));
  if ((result < 0) || (result >= upper_limit))
    error_bad_range_arg (arg_number);
  return (result);
}

long
DEFUN (arg_integer_in_range,
       (arg_number, lower_limit, upper_limit),
       int arg_number AND long lower_limit AND long upper_limit)
{
  fast long result = (arg_integer (arg_number));
  if ((result < lower_limit) || (result >= upper_limit))
    error_bad_range_arg (arg_number);
  return (result);
}

unsigned long
DEFUN (arg_ulong_integer, (arg_number), int arg_number)
{
  fast SCHEME_OBJECT object = (ARG_REF (arg_number));
  if (! (INTEGER_P (object)))
    error_wrong_type_arg (arg_number);
  if (! (integer_to_ulong_p (object)))
    error_bad_range_arg (arg_number);
  return (integer_to_ulong (object));
}

unsigned long
DEFUN (arg_ulong_index_integer, (arg_number, upper_limit),
       int arg_number AND unsigned long upper_limit)
{
  fast unsigned long result = (arg_ulong_integer (arg_number));
  if (result >= upper_limit)
    error_bad_range_arg (arg_number);
  return (result);
}

Boolean
DEFUN (real_number_to_double_p, (x), fast SCHEME_OBJECT x)
{
  return ((! (BIGNUM_P (x))) || (BIGNUM_TO_DOUBLE_P (x)));
}

double
DEFUN (real_number_to_double, (x), fast SCHEME_OBJECT x)
{
  return
    ((FIXNUM_P (x))
     ? (FIXNUM_TO_DOUBLE (x))
     : (BIGNUM_P (x))
     ? (bignum_to_double (x))
     : (FLONUM_TO_DOUBLE (x)));
}

double
DEFUN (arg_real_number, (arg_number), int arg_number)
{
  fast SCHEME_OBJECT number = (ARG_REF (arg_number));
  if (! (REAL_P (number)))
    error_wrong_type_arg (arg_number);
  if (! (real_number_to_double_p (number)))
    error_bad_range_arg (arg_number);
  return (real_number_to_double (number));
}

double
DEFUN (arg_real_in_range, (arg_number, lower_limit, upper_limit),
       int arg_number AND double lower_limit AND double upper_limit)
{
  fast double result = (arg_real_number (arg_number));
  if ((result < lower_limit) || (result > upper_limit))
    error_bad_range_arg (arg_number);
  return (result);
}

Boolean
DEFUN (interpreter_applicable_p, (object), fast SCHEME_OBJECT object)
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
    case TC_COMPILED_ENTRY:
      {
	long results [3];
	compiled_entry_type (object, results);
	return ((results [0]) == 0);
      }
    default:
      return (false);
    }
}

                      /******************/
                      /* ERROR HANDLING */
                      /******************/

/* It is assumed that any caller of the error code has already
 * restored its state to a situation which will make it
 * restartable if the error handler returns normally.  As a
 * result, the only work to be done on an error is to verify
 * that there is an error handler, save the current continuation and
 * create a new one if entered from Pop_Return rather than Eval,
 * turn off interrupts, and call it with two arguments: Error-Code
 * and Interrupt-Enables.
 */

void
DEFUN (Do_Micro_Error, (Err, From_Pop_Return),
       long Err AND Boolean From_Pop_Return)
{
  SCHEME_OBJECT Error_Vector = SHARP_F;
  SCHEME_OBJECT Handler;

  if (Consistency_Check)
  {
    err_print(Err, error_output);
    Print_Expression(Fetch_Expression(), "Expression was");
    outf_error ("\nEnvironment 0x%lx (#%lo).\n",
	    ((long) (Fetch_Env ())), ((long) (Fetch_Env ())));
    Print_Return("Return code");
    outf_error ("\n");
  }

  Error_Exit_Hook();

  if (Trace_On_Error)
  {
    outf_error ("\n\n**** Stack Trace ****\n\n");
    Back_Trace (error_output);
  }

#ifdef ENABLE_DEBUGGING_TOOLS
  {
    int *From = &(local_circle[0]), *To = &(debug_circle[0]), i;

    for (i = 0; i < local_nslots; i++)
      *To++ = *From++;
    debug_nslots = local_nslots;
    debug_slotno = local_slotno;
  }
#endif

/* Do_Micro_Error continues on the next page. */

/* Do_Micro_Error, continued */

  /* This can NOT be folded into the Will_Push below since we cannot
     afford to have the Will_Push put down its own continuation.
     There is guaranteed to be enough space for this one
     continuation; in fact, the Will_Push here is really unneeded!
   */

  if (From_Pop_Return)
  {
   Will_Push (CONTINUATION_SIZE);
    Save_Cont ();
   Pushed ();
  }

 Will_Push (CONTINUATION_SIZE + (From_Pop_Return ? 0 : 1));
  if (From_Pop_Return)
    Store_Expression (Val);
  else
    STACK_PUSH (Fetch_Env ());
  Store_Return ((From_Pop_Return) ?
		RC_POP_RETURN_ERROR :
		RC_EVAL_ERROR);
  Save_Cont ();
 Pushed ();

/* Do_Micro_Error continues on the next page. */

/* Do_Micro_Error, continued */

  if ((!Valid_Fixed_Obj_Vector()) ||
      (OBJECT_TYPE ((Error_Vector =
		    Get_Fixed_Obj_Slot(System_Error_Vector))) !=
       TC_VECTOR))
  {
    error_death (Err,
		 (((Valid_Fixed_Obj_Vector())
		   && (Error_Vector == SHARP_F))
		  ? "No error handlers"
		  : "No error handlers: Bad handlers vector"));
    /*NOTREACHED*/
  }

  if ((Err < 0) || (Err >= ((long) (VECTOR_LENGTH (Error_Vector)))))
  {
    if (VECTOR_LENGTH (Error_Vector) == 0)
      error_death (Err, "No error handlers: Empty handlers vector");
      /*NOTREACHED*/
    Handler = (VECTOR_REF (Error_Vector, ERR_BAD_ERROR_CODE));
  }
  else
    Handler = (VECTOR_REF (Error_Vector, Err));

  /* Return from error handler will re-enable interrupts & restore history */
  Stop_History();
  preserve_interrupt_mask ();

 Will_Push (STACK_ENV_EXTRA_SLOTS + 3);
  /* Arg 2:     Int. mask */
  STACK_PUSH (LONG_TO_FIXNUM(FETCH_INTERRUPT_MASK()));
  /* Arg 1:     Err. No   */
  if ((Err == ERR_WITH_ARGUMENT) || (Err == ERR_IN_SYSTEM_CALL))
    STACK_PUSH (error_argument);
  else if ((Err >= SMALLEST_FIXNUM) && (Err <= BIGGEST_FIXNUM))
    STACK_PUSH (LONG_TO_FIXNUM (Err));
  else
    STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (ERR_BAD_ERROR_CODE));
  /* Procedure: Handler   */
  STACK_PUSH (Handler);
  STACK_PUSH (STACK_FRAME_HEADER + 2);
 Pushed();

  /* Disable all interrupts */
  SET_INTERRUPT_MASK(0);
  return;
}

/* HISTORY manipulation */

SCHEME_OBJECT *
DEFUN_VOID (Make_Dummy_History)
{
  SCHEME_OBJECT *History_Rib = Free;
  SCHEME_OBJECT *Result;

  Free[RIB_EXP] = SHARP_F;
  Free[RIB_ENV] = SHARP_F;
  Free[RIB_NEXT_REDUCTION] =
    MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, History_Rib);
  Free += 3;
  Result = Free;
  Free[HIST_RIB] = MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, History_Rib);
  Free[HIST_NEXT_SUBPROBLEM] =
    MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, Result);
  Free[HIST_PREV_SUBPROBLEM] =
    MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, Result);
  Free += 3;
  return (Result);
}

/* The entire trick to history is right here: it is either copied or
   reused when restored.  Initially, Stop_History marks the stack so
   that the history will merely be popped and reused.  On a catch,
   however, the return code is changed to force the history to be
   copied instead.  Thus, histories saved as part of a control point
   are not side-effected in the history collection process.
*/

void
DEFUN_VOID (Stop_History)
{
  SCHEME_OBJECT Saved_Expression;
  long Saved_Return_Code;

  Saved_Expression = Fetch_Expression();
  Saved_Return_Code = Fetch_Return();
 Will_Push(HISTORY_SIZE);
  Save_History(RC_RESTORE_DONT_COPY_HISTORY);
 Pushed();
  Prev_Restore_History_Stacklet = NULL;
  Prev_Restore_History_Offset = ((Get_End_Of_Stacklet() - Stack_Pointer) +
				 CONTINUATION_RETURN_CODE);
  Store_Expression(Saved_Expression);
  Store_Return(Saved_Return_Code);
  return;
}

/* This returns a history object,
   or SHARP_F if it needs to GC,
   or SHARP_T if it is not a valid history object.
 */

SCHEME_OBJECT
DEFUN (copy_history, (hist_obj), SCHEME_OBJECT hist_obj)
{
  long space_left, vert_type, rib_type;
  SCHEME_OBJECT *fast_free;
  SCHEME_OBJECT new_hunk, *last_hunk, *hist_ptr, *orig_hist, temp;
  SCHEME_OBJECT *orig_rib, *source_rib, *rib_slot;

  if (!(HUNK3_P (hist_obj)))
    return (SHARP_T);

  space_left = ((Space_Before_GC ()) - 3);
  fast_free = Free;

  vert_type = (OBJECT_TYPE (hist_obj));
  orig_hist = (OBJECT_ADDRESS (hist_obj));
  hist_ptr = orig_hist;
  last_hunk = (Heap_Top - 3);

  do
  {
    /* Allocate and link the vertebra. */

    space_left -= 3;
    if (space_left < 0)
      return (SHARP_F);

    new_hunk = (MAKE_POINTER_OBJECT (vert_type, fast_free));
    last_hunk[HIST_NEXT_SUBPROBLEM] = new_hunk;

    fast_free[HIST_PREV_SUBPROBLEM] =
      (MAKE_POINTER_OBJECT ((OBJECT_TYPE (hist_ptr[HIST_PREV_SUBPROBLEM])),
			    last_hunk));
    last_hunk = fast_free;
    fast_free += 3;

    /* Copy the rib. */

    temp = hist_ptr[HIST_RIB];
    rib_type = (OBJECT_TYPE (temp));
    orig_rib = (OBJECT_ADDRESS (temp));
    rib_slot = (last_hunk + HIST_RIB);
    
    source_rib = orig_rib;

    do
    {
      space_left -= 3;
      if (space_left < 0)
	return (SHARP_F);

      *rib_slot = (MAKE_POINTER_OBJECT (rib_type, fast_free));
      fast_free[RIB_EXP] = source_rib[RIB_EXP];
      fast_free[RIB_ENV] = source_rib[RIB_ENV];
      rib_slot = (fast_free + RIB_NEXT_REDUCTION);
      fast_free += 3;
      
      temp = source_rib[RIB_NEXT_REDUCTION];
      rib_type = (OBJECT_TYPE (temp));
      source_rib = (OBJECT_ADDRESS (temp));
    } while (source_rib != orig_rib);
      
    *rib_slot = (OBJECT_NEW_TYPE (rib_type, last_hunk[HIST_RIB]));

    temp = hist_ptr[HIST_NEXT_SUBPROBLEM];
    vert_type = (OBJECT_TYPE (temp));
    hist_ptr = (OBJECT_ADDRESS (temp));
  } while (hist_ptr != orig_hist);

  Free = fast_free;
  new_hunk = Heap_Top[HIST_NEXT_SUBPROBLEM - 3];
  last_hunk[HIST_NEXT_SUBPROBLEM] = (OBJECT_NEW_TYPE (vert_type, new_hunk));
  FAST_MEMORY_SET (new_hunk, HIST_PREV_SUBPROBLEM,
		   (MAKE_POINTER_OBJECT
		    ((OBJECT_TYPE (hist_ptr[HIST_PREV_SUBPROBLEM])),
		     last_hunk)));
  return (new_hunk);
}

/* Restore_History pops a history object off the stack and
   makes a COPY of it the current history collection object.
   This is called only from the RC_RESTORE_HISTORY case in
   interpret.c .
 */

Boolean
DEFUN (Restore_History, (hist_obj), SCHEME_OBJECT hist_obj)
{
  SCHEME_OBJECT new_hist;

  new_hist = (copy_history (hist_obj));
  if (new_hist == SHARP_F)
    return (false);
  else if (new_hist == SHARP_T)
  {
    outf_fatal ("\nBad history to restore.\n");
    Microcode_Termination (TERM_EXIT);
    /*NOTREACHED*/
    return (0);
  }
  else
  {
    History = (OBJECT_ADDRESS (new_hist));
    return (true);
  }
}

/* If a "debugging" version of the interpreter is made, then this
   procedure is called to actually invoke a primitive.  When a
   "production" version is made, all of the consistency checks are
   omitted and a macro from "default.h" is used to directly code the
   call to the primitive function. */

#ifdef ENABLE_DEBUGGING_TOOLS

SCHEME_OBJECT
DEFUN (primitive_apply_internal, (primitive), SCHEME_OBJECT primitive)
{
  SCHEME_OBJECT result;
  if (Primitive_Debug)
    Print_Primitive (primitive);
  {
    SCHEME_OBJECT * saved_stack = Stack_Pointer;
    PRIMITIVE_APPLY_INTERNAL (result, primitive);
    if (saved_stack != Stack_Pointer)
      {
	int arity = (PRIMITIVE_N_ARGUMENTS (primitive));
	Print_Expression (primitive, "Stack bad after ");
	outf_fatal ("\nStack was 0x%lx, now 0x%lx, #args=%ld.\n",
		    ((long) saved_stack), ((long) Stack_Pointer), ((long) arity));
	Microcode_Termination (TERM_EXIT);
      }
  }
  if (Primitive_Debug)
    {
      Print_Expression (result, "Primitive Result");
      outf_error("\n");
      outf_flush_error();
    }
  return (result);
}

#endif /* ENABLE_DEBUGGING_TOOLS */

#ifdef ENABLE_PRIMITIVE_PROFILING

/* The profiling mechanism is enabled by storing a vector in the fixed
   objects vector.  The vector should be initialized to contain all zeros
 */

void
DEFUN (record_primitive_entry, (primitive), SCHEME_OBJECT primitive)
{
  SCHEME_OBJECT table;

  if ((Fixed_Objects != SHARP_F) &&
      ((table = Get_Fixed_Obj_Slot (Primitive_Profiling_Table)) != SHARP_F))
  {
    long index = (1 + (OBJECT_DATUM (primitive)));
    MEMORY_SET
      (table,
       index,
       (long_to_integer (1 + (integer_to_long (MEMORY_REF (table, index))))));
  }
  return;
}

#endif /* ENABLE_PRIMITIVE_PROFILING */

#ifdef USE_STACKLETS
                      /******************/
                      /*   STACKLETS    */
                      /******************/

void
DEFUN (Allocate_New_Stacklet, (N), long N)
{
  SCHEME_OBJECT Old_Expression, *Old_Stacklet, Old_Return;

  Old_Stacklet = Current_Stacklet;
  Terminate_Old_Stacklet();
  if ((Free_Stacklets == NULL) ||
      ((N + STACKLET_SLACK) >
       (OBJECT_DATUM (Free_Stacklets[STACKLET_LENGTH]))))
  {
    long size;

    /*
      Room is set aside for the header bytes of a stacklet plus
      the two words required for the RC_JOIN_STACKLETS frame.
     */

    size = New_Stacklet_Size(N);
    if (GC_Check(size))
    {
      Request_GC(size);
      if ((Free + size) >= Heap_Top)
	Microcode_Termination(TERM_STACK_OVERFLOW);
    }
    Free[STACKLET_LENGTH] = MAKE_OBJECT (TC_MANIFEST_VECTOR, (size - 1));
    SET_STACK_GUARD (& (Free[STACKLET_HEADER_SIZE]));
    Free += size;
    Stack_Pointer = Free;
  }
  else
  {
    /* Grab first one on the free list */

    SCHEME_OBJECT *New_Stacklet;

    New_Stacklet = Free_Stacklets;
    Free_Stacklets =
      ((SCHEME_OBJECT *) Free_Stacklets[STACKLET_FREE_LIST_LINK]);
    Stack_Pointer =
      &New_Stacklet[1 + (OBJECT_DATUM (New_Stacklet[STACKLET_LENGTH]))];
    SET_STACK_GUARD (& (New_Stacklet[STACKLET_HEADER_SIZE]));
  }
  Old_Expression = Fetch_Expression();
  Old_Return = Fetch_Return();
  Store_Expression(MAKE_POINTER_OBJECT (TC_CONTROL_POINT, Old_Stacklet));
  Store_Return(RC_JOIN_STACKLETS);
  /*
    Will_Push omitted because size calculation includes enough room.
   */
  Save_Cont();
  Store_Expression(Old_Expression);
  Store_Return(Old_Return);
  return;
}

#endif /* USE_STACKLETS */

/* Dynamic Winder support code */

SCHEME_OBJECT
DEFUN (Find_State_Space, (State_Point), SCHEME_OBJECT State_Point)
{
  long How_Far =
    (UNSIGNED_FIXNUM_TO_LONG
     (FAST_MEMORY_REF (State_Point, STATE_POINT_DISTANCE_TO_ROOT)));
  long i;
  fast SCHEME_OBJECT Point = State_Point;

  for (i=0; i <= How_Far; i++)
  {
#ifdef ENABLE_DEBUGGING_TOOLS
    if (Point == SHARP_F)
    {
      outf_fatal(
	      "\nState_Point 0x%lx wrong: count was %ld, #F at %ld\n",
	      ((long) State_Point), ((long) How_Far), ((long) i));
      Microcode_Termination(TERM_EXIT);
      /*NOTREACHED*/
    }
#endif /* ENABLE_DEBUGGING_TOOLS */
    Point = FAST_MEMORY_REF (Point, STATE_POINT_NEARER_POINT);
  }
  return (Point);
}

/* ASSUMPTION: State points, which are created only by the interpreter,
   never contain FUTUREs except possibly as the thunks (which are handled
   by the apply code).

   Furthermore:
     (1) On a single processor, things should work with multiple state
	 spaces.  The microcode variable Current_State_Point tracks
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
   PRIMITIVE_CANONICALIZE_CONTEXT() first!
*/

void
DEFUN (Translate_To_Point, (Target), SCHEME_OBJECT Target)
{
  SCHEME_OBJECT State_Space, Current_Location, *Path;
  fast SCHEME_OBJECT Path_Point, *Path_Ptr;
  long Distance, Merge_Depth, From_Depth, i;

  State_Space = Find_State_Space(Target);
  Path = Free;
  guarantee_state_point();
  Distance =
    (UNSIGNED_FIXNUM_TO_LONG
     (FAST_MEMORY_REF (Target, STATE_POINT_DISTANCE_TO_ROOT)));
  if (State_Space == SHARP_F)
    Current_Location = Current_State_Point;
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
    Path_Point = FAST_MEMORY_REF (Path_Point, STATE_POINT_NEARER_POINT);
  }

  From_Depth =
    (UNSIGNED_FIXNUM_TO_LONG
     (FAST_MEMORY_REF (Current_Location, STATE_POINT_DISTANCE_TO_ROOT)));

  for (Path_Point = Current_Location, Merge_Depth = From_Depth;
       Merge_Depth > Distance;
       Merge_Depth--)
    Path_Point = FAST_MEMORY_REF (Path_Point, STATE_POINT_NEARER_POINT);

  for (Path_Ptr = (&(Path[Merge_Depth]));
       Merge_Depth >= 0;
       Merge_Depth--, Path_Ptr--)
  {
    if (*Path_Ptr == Path_Point)
      break;
    Path_Point = FAST_MEMORY_REF (Path_Point, STATE_POINT_NEARER_POINT);
  }

#ifdef ENABLE_DEBUGGING_TOOLS
  if (Merge_Depth < 0)
  {
    outf_fatal("\nMerge_Depth went negative: %d\n", Merge_Depth);
    Microcode_Termination (TERM_EXIT);
  }
#endif /* ENABLE_DEBUGGING_TOOLS */

  preserve_interrupt_mask ();
 Will_Push(CONTINUATION_SIZE + 4);
  STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM((Distance - Merge_Depth)));
  STACK_PUSH (Target);
  STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM((From_Depth - Merge_Depth)));
  STACK_PUSH (Current_Location);
  Store_Expression(State_Space);
  Store_Return(RC_MOVE_TO_ADJACENT_POINT);
  Save_Cont();
 Pushed();

  {
    long mask;

    /* Disable lower than GC level */
    mask = (FETCH_INTERRUPT_MASK() & ((INT_GC << 1) - 1));
    SET_INTERRUPT_MASK(mask);
  }
  PRIMITIVE_ABORT (PRIM_POP_RETURN);
  /*NOTREACHED*/
}

#ifndef __OS2__

SCHEME_OBJECT
DEFUN_VOID (Compiler_Get_Fixed_Objects)
{
  if (Valid_Fixed_Obj_Vector())
    return (Get_Fixed_Obj_Slot(Me_Myself));
  else
    return (SHARP_F);
}

extern SCHEME_OBJECT EXFUN (Re_Enter_Interpreter, (void));
extern SCHEME_OBJECT EXFUN
  (C_call_scheme, (SCHEME_OBJECT, long, SCHEME_OBJECT *));

#ifdef __WIN32__
#  include <windows.h>
#endif

SCHEME_OBJECT
DEFUN (C_call_scheme, (proc, nargs, argvec),
       SCHEME_OBJECT proc
       AND long nargs
       AND SCHEME_OBJECT * argvec)
{
  SCHEME_OBJECT primitive, prim_lexpr, * sp, result;
  SCHEME_OBJECT * callers_last_return_code;

#ifdef __IA32__
  extern void * C_Frame_Pointer;
  extern void * C_Stack_Pointer;
  void * cfp = C_Frame_Pointer;
  void * csp = C_Stack_Pointer;
#ifdef CL386
  __try
#endif
#endif
  {  
    primitive = (Regs [REGBLOCK_PRIMITIVE]);
    prim_lexpr = (Regs [REGBLOCK_LEXPR_ACTUALS]);
    callers_last_return_code = last_return_code;

    if (! (PRIMITIVE_P (primitive)))
      abort_to_interpreter (ERR_CANNOT_RECURSE);
      /*NOTREACHED*/
    sp = Stack_Pointer;

   Will_Push ((2 * CONTINUATION_SIZE) + (nargs + STACK_ENV_EXTRA_SLOTS + 1));
    {
      long i;

      Store_Return (RC_END_OF_COMPUTATION);
      Store_Expression (primitive);
      Save_Cont ();

      for (i = nargs; --i >= 0; )
	STACK_PUSH (argvec[i]);
      STACK_PUSH (proc);
      STACK_PUSH (STACK_FRAME_HEADER + nargs);

      Store_Return (RC_INTERNAL_APPLY);
      Store_Expression (SHARP_F);
      Save_Cont ();
    }
   Pushed ();
    result = (Re_Enter_Interpreter ());

    if (Stack_Pointer != sp)
      signal_error_from_primitive (ERR_STACK_HAS_SLIPPED);
      /*NOTREACHED*/

    last_return_code = callers_last_return_code;
    Regs [REGBLOCK_LEXPR_ACTUALS] = prim_lexpr;
    Regs [REGBLOCK_PRIMITIVE] = primitive;
  }
#ifdef __IA32__
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

#endif /* not __OS2__ */
