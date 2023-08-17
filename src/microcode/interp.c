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

/* The interpreter */

#include "scheme.h"
#include "trap.h"
#include "lookup.h"
#include "history.h"

extern void * obstack_chunk_alloc (size_t);
#define obstack_chunk_free free
extern void preserve_signal_mask (void);
extern void fixup_float_environment (void);

/* In order to make the interpreter tail recursive (i.e.
 * to avoid calling procedures and thus saving unnecessary
 * state information), the main body of the interpreter
 * is coded in a continuation passing style.
 *
 * Basically, this is done by dispatching on the type code
 * for an Scode item.  At each dispatch, some processing
 * is done which may include setting the return address
 * register, saving the current continuation (return address
 * and current expression) and jumping to the start of
 * the interpreter.
 *
 * It may be helpful to think of this program as being what
 * you would get if you wrote the straightforward Scheme
 * interpreter and then converted it into continuation
 * passing style as follows.  At every point where you would
 * call EVAL to handle a sub-form, you put a jump back to
 * do_expression.  Now, if there was code after the call to
 * EVAL you first push a "return code" (using PUSH_CONT) on
 * the stack and move the code that used to be after the
 * call down into the part of this file after the tag
 * pop_return.
 *
 * Notice that because of the caller saves convention used
 * here, all of the registers which are of interest have
 * been SAVEd on the racks by the time interpretation arrives
 * at do_expression (the top of EVAL).
 *
 * For notes on error handling and interrupts, see the file
 * utils.c.
 *
 * This file is divided into two parts. The first
 * corresponds is called the EVAL dispatch, and is ordered
 * alphabetically by the SCode item handled.  The second,
 * called the return dispatch, begins at pop_return and is
 * ordered alphabetically by return code name.
 */

static inline void
prepare_pop_return_interrupt (unsigned long rc, SCHEME_OBJECT val)
{
  PUSH_CONT_RC (rc, GET_EXP);
  PUSH_CONT_RC (RC_RESTORE_VALUE, val);
}

static inline void
prepare_apply_interrupt(void)
{
  SET_EXP (SHARP_F);
  prepare_pop_return_interrupt
    (RC_INTERNAL_APPLY_VAL, (APPLY_FRAME_PROCEDURE ()));
}

static inline void
application_error(long code)
{
  PUSH_CONT_RC (RC_INTERNAL_APPLY_VAL, SHARP_F);
  SET_VAL (APPLY_FRAME_PROCEDURE ());
  Do_Micro_Error (code, true);
}

static inline void
immediate_gc (unsigned long n)
{
  REQUEST_GC (n);
  setup_interrupt (PENDING_INTERRUPTS ());
}

#define EVAL_GC_CHECK(Amount)                                           \
{                                                                       \
  if (GC_NEEDED_P (Amount))                                             \
    {                                                                   \
      prepare_eval_repeat ();                                           \
      immediate_gc (Amount);                                            \
      goto perform_application;                                         \
    }                                                                   \
}

static inline void
prepare_eval_repeat (void)
{
  STACK_CHECK (CONTINUATION_SIZE + 1);
  STACK_PUSH (GET_ENV);
  PUSH_CONT_RC (RC_EVAL_ERROR, GET_EXP);
}

static inline void
pop_return_error (long code)
{
  SAVE_CONT ();
  Do_Micro_Error (code, true);
}

static inline void
reduces_to (SCHEME_OBJECT exp)
{
  SET_EXP (exp);
  NEW_REDUCTION (GET_EXP, GET_ENV);
}

static inline void
reduces_to_nth (unsigned long n)
{
  reduces_to (MEMORY_REF (GET_EXP, n));
}

static inline void
do_nth_then (unsigned long rc, unsigned long n)
{
  PUSH_CONT_RC (rc, GET_EXP);
  SET_EXP (MEMORY_REF (GET_EXP, n));
  NEW_SUBPROBLEM (GET_EXP, GET_ENV);
}

static inline void
push_nth_then (unsigned long rc, unsigned long n)
{
  PUSH_CONT_RC (rc, GET_EXP);
  SET_EXP (MEMORY_REF (GET_EXP, n));
  NEW_SUBPROBLEM (GET_EXP, GET_ENV);
}

static inline void
do_another_then (unsigned long rc, unsigned long n)
{
  PUSH_CONT_RC (rc, GET_EXP);
  SET_EXP (MEMORY_REF (GET_EXP, n));
  REUSE_SUBPROBLEM (GET_EXP, GET_ENV);
}

#ifdef COMPILE_STEPPER

#define FETCH_EVAL_TRAPPER()						\
  (MEMORY_REF ((VECTOR_REF (fixed_objects, STEPPER_STATE)), HUNK_CXR0))

#define FETCH_APPLY_TRAPPER()						\
  (MEMORY_REF ((VECTOR_REF (fixed_objects, STEPPER_STATE)), HUNK_CXR1))

#define FETCH_RETURN_TRAPPER()						\
  (MEMORY_REF ((VECTOR_REF (fixed_objects, STEPPER_STATE)), HUNK_CXR2))

#endif /* COMPILE_STEPPER */

/* The EVAL/APPLY yin/yang */

interpreter_state_t interpreter_state = NULL_INTERPRETER_STATE;

void
bind_interpreter_state (interpreter_state_t s)
{
  (s -> previous_state) = interpreter_state;
  (s -> nesting_level) =
    ((interpreter_state == NULL_INTERPRETER_STATE)
     ? 0
     : (1 + (interpreter_state -> nesting_level)));
  (s -> dstack_position) = dstack_position;
  interpreter_state = s;
}

void
unbind_interpreter_state (interpreter_state_t s)
{
  interpreter_state = s;
  {
    unsigned long old_mask = GET_INT_MASK;
    SET_INTERRUPT_MASK (0);
    dstack_set_position (s -> dstack_position);
    SET_INTERRUPT_MASK (old_mask);
  }
  interpreter_state = (s -> previous_state);
}

void
abort_to_interpreter (int argument)
{
  if (interpreter_state == NULL_INTERPRETER_STATE)
  {
    outf_fatal ("abort_to_interpreter: Interpreter not set up.\n");
    termination_init_error ();
  }

  interpreter_throw_argument = argument;
  {
    unsigned long old_mask = GET_INT_MASK;
    SET_INTERRUPT_MASK (0);
    dstack_set_position (interpreter_catch_dstack_position);
    SET_INTERRUPT_MASK (old_mask);
  }
  obstack_free ((&scratch_obstack), 0);
  obstack_init (&scratch_obstack);
  longjmp (interpreter_catch_env, argument);
}

int
abort_to_interpreter_argument (void)
{
  return (interpreter_throw_argument);
}

long prim_apply_error_code;

void
Interpret (void)
{
  long dispatch_code;
  struct interpreter_state_s new_state;

  /* Primitives jump back here for errors, requests to evaluate an
     expression, apply a function, or handle an interrupt request.  On
     errors or interrupts they leave their arguments on the stack, the
     primitive itself in GET_EXP.  The code should do a primitive
     backout in these cases, but not in others (apply, eval, etc.),
     since the primitive itself will have left the state of the
     interpreter ready for operation.  */

  bind_interpreter_state (&new_state);
  dispatch_code = (setjmp (interpreter_catch_env));
  preserve_signal_mask ();
  fixup_float_environment ();

  switch (dispatch_code)
    {
    case 0:			/* first time */
      break;			/* fall into eval */

    case PRIM_APPLY:
      SET_PRIMITIVE (SHARP_F);
      goto internal_apply;

    case PRIM_NO_TRAP_APPLY:
      SET_PRIMITIVE (SHARP_F);
      goto Apply_Non_Trapping;

    case PRIM_APPLY_INTERRUPT:
      SET_PRIMITIVE (SHARP_F);
      prepare_apply_interrupt ();
      setup_interrupt (PENDING_INTERRUPTS ());
      goto perform_application;

    case PRIM_APPLY_ERROR:
      SET_PRIMITIVE (SHARP_F);
      application_error (prim_apply_error_code);
      goto internal_apply;

    case PRIM_DO_EXPRESSION:
      SET_VAL (GET_EXP);
      SET_PRIMITIVE (SHARP_F);
      reduces_to (GET_VAL);
      goto do_expression;

    case PRIM_NO_TRAP_EVAL:
      SET_VAL (GET_EXP);
      SET_PRIMITIVE (SHARP_F);
      NEW_REDUCTION (GET_VAL, GET_ENV);
      goto eval_non_trapping;

    case PRIM_POP_RETURN:
      SET_PRIMITIVE (SHARP_F);
      goto pop_return;

    case PRIM_RETURN_TO_C:
      SET_PRIMITIVE (SHARP_F);
      unbind_interpreter_state (interpreter_state);
      return;

    case PRIM_NO_TRAP_POP_RETURN:
      SET_PRIMITIVE (SHARP_F);
      goto pop_return_non_trapping;

    case PRIM_INTERRUPT:
      back_out_of_primitive ();
      setup_interrupt (PENDING_INTERRUPTS ());
      goto perform_application;

    case PRIM_ABORT_TO_C:
      back_out_of_primitive ();
      unbind_interpreter_state (interpreter_state);
      return;

    case ERR_ARG_1_WRONG_TYPE:
      back_out_of_primitive ();
      Do_Micro_Error (ERR_ARG_1_WRONG_TYPE, true);
      goto internal_apply;

    case ERR_ARG_2_WRONG_TYPE:
      back_out_of_primitive ();
      Do_Micro_Error (ERR_ARG_2_WRONG_TYPE, true);
      goto internal_apply;

    case ERR_ARG_3_WRONG_TYPE:
      back_out_of_primitive ();
      Do_Micro_Error (ERR_ARG_3_WRONG_TYPE, true);
      goto internal_apply;

    default:
      back_out_of_primitive ();
      Do_Micro_Error (dispatch_code, true);
      goto internal_apply;
    }

 do_expression:

  /* GET_EXP has an Scode item in it that should be evaluated and the
     result left in GET_VAL.

     A "break" after the code for any operation indicates that all
     processing for this operation has been completed, and the next
     step will be to pop a return code off the stack and proceed at
     pop_return.  This is sometimes called "executing the
     continuation" since the return code can be considered the
     continuation to be performed after the operation.

     An operation can terminate with a REDUCES_TO or REDUCES_TO_NTH
     macro.  This indicates that the value of the current Scode item
     is the value returned when the new expression is evaluated.
     Therefore no new continuation is created and processing continues
     at do_expression with the new expression in GET_EXP.

     Finally, an operation can terminate with a DO_NTH_THEN macro.
     This indicates that another expression must be evaluated and them
     some additional processing will be performed before the value of
     this S-Code item available.  Thus a new continuation is created
     and placed on the stack (using PUSH_CONT), the new expression is
     placed in the GET_EXP, and processing continues at do_expression.
     */

  /* Handling of Eval Trapping.

     If we are handling traps and there is an Eval Trap set, turn off
     all trapping and then go to internal_apply to call the user
     supplied eval hook with the expression to be evaluated and the
     environment.  */

#ifdef COMPILE_STEPPER
  if (trapping
      && (!WITHIN_CRITICAL_SECTION_P ())
      && ((FETCH_EVAL_TRAPPER ()) != SHARP_F))
    {
      trapping = false;
      STACK_CHECK (4);
      STACK_PUSH (GET_ENV);
      STACK_PUSH (GET_EXP);
      STACK_PUSH (FETCH_EVAL_TRAPPER ());
      PUSH_APPLY_FRAME_HEADER (2);
      goto Apply_Non_Trapping;
    }
#endif /* COMPILE_STEPPER */

 eval_non_trapping:
#ifdef EVAL_UCODE_HOOK
  EVAL_UCODE_HOOK ();
#endif
  switch (OBJECT_TYPE (GET_EXP))
    {
    case TC_BIG_FIXNUM:		/* The self evaluating items */
    case TC_BIG_FLONUM:
    case TC_BYTEVECTOR:
    case TC_CHARACTER:
    case TC_CHARACTER_STRING:
    case TC_COMPILED_CODE_BLOCK:
    case TC_COMPLEX:
    case TC_CONSTANT:
    case TC_CONTROL_POINT:
    case TC_DELAYED:
    case TC_ENTITY:
    case TC_ENVIRONMENT:
    case TC_EXTENDED_PROCEDURE:
    case TC_FALSE:
    case TC_FIXNUM:
    case TC_HUNK3_A:
    case TC_HUNK3_B:
    case TC_INTERNED_SYMBOL:
    case TC_LIST:
    case TC_NON_MARKED_VECTOR:
    case TC_PRIMITIVE:
    case TC_PROCEDURE:
    case TC_QUAD:
    case TC_RATNUM:
    case TC_RECORD:
    case TC_REFERENCE_TRAP:
    case TC_RETURN_CODE:
    case TC_UNICODE_STRING:
    case TC_UNINTERNED_SYMBOL:
    case TC_VECTOR:
    case TC_VECTOR_16B:
    case TC_VECTOR_1B:
    default:
      SET_VAL (GET_EXP);
      break;

    case TC_ACCESS:
      STACK_CHECK (CONTINUATION_SIZE);
      push_nth_then (RC_EXECUTE_ACCESS_FINISH, ACCESS_ENVIRONMENT);
      goto do_expression;

    case TC_ASSIGNMENT:
      STACK_CHECK (CONTINUATION_SIZE + 1);
      STACK_PUSH (GET_ENV);
      push_nth_then (RC_EXECUTE_ASSIGNMENT_FINISH, ASSIGN_VALUE);
      goto do_expression;

    case TC_BROKEN_HEART:
      Microcode_Termination (TERM_BROKEN_HEART);

    case TC_COMBINATION:
      {
	unsigned long nargs = VECTOR_LENGTH (GET_EXP) - 1;
	STACK_CHECK (CONTINUATION_SIZE + 2 + nargs);
	stack_pointer = STACK_LOC (-nargs);
	STACK_PUSH (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, nargs));
	/* The finger: last argument number */
	if (nargs == 0)
	  {
	    PUSH_APPLY_FRAME_HEADER (0);
	    do_nth_then (RC_COMB_APPLY_FUNCTION, COMB_FN_SLOT);
            goto do_expression;
	  }
	STACK_PUSH (GET_ENV);
	do_nth_then (RC_COMB_SAVE_VALUE, nargs + 1);
        goto do_expression;
      }

    case TC_COMMENT:
      reduces_to_nth (COMMENT_EXPRESSION);
      goto do_expression;

    case TC_CONDITIONAL:
      STACK_CHECK (CONTINUATION_SIZE + 1);
      STACK_PUSH (GET_ENV);
      push_nth_then (RC_CONDITIONAL_DECIDE, COND_PREDICATE);
      goto do_expression;

#ifdef CC_SUPPORT_P
    case TC_COMPILED_ENTRY:
      dispatch_code = (enter_compiled_expression ());
      goto return_from_compiled_code;
#endif

    case TC_DEFINITION:
      STACK_CHECK (CONTINUATION_SIZE + 1);
      STACK_PUSH (GET_ENV);
      push_nth_then (RC_EXECUTE_DEFINITION_FINISH, DEFINE_VALUE);
      goto do_expression;

    case TC_DELAY:
      /* Deliberately omitted: EVAL_GC_CHECK (2); */
      SET_VAL (MAKE_POINTER_OBJECT (TC_DELAYED, Free));
      (Free[THUNK_ENVIRONMENT]) = GET_ENV;
      (Free[THUNK_PROCEDURE]) = (MEMORY_REF (GET_EXP, DELAY_OBJECT));
      Free += 2;
      break;

    case TC_DISJUNCTION:
      STACK_CHECK (CONTINUATION_SIZE + 1);
      STACK_PUSH (GET_ENV);
      push_nth_then (RC_DISJUNCTION_DECIDE, OR_PREDICATE);
      goto do_expression;

    case TC_EXTENDED_LAMBDA:
      /* Deliberately omitted: EVAL_GC_CHECK (2); */
      SET_VAL (MAKE_POINTER_OBJECT (TC_EXTENDED_PROCEDURE, Free));
      (Free[PROCEDURE_LAMBDA_EXPR]) = GET_EXP;
      (Free[PROCEDURE_ENVIRONMENT]) = GET_ENV;
      Free += 2;
      break;

    case TC_LAMBDA:
    case TC_LEXPR:
      /* Deliberately omitted: EVAL_GC_CHECK (2); */
      SET_VAL (MAKE_POINTER_OBJECT (TC_PROCEDURE, Free));
      (Free[PROCEDURE_LAMBDA_EXPR]) = GET_EXP;
      (Free[PROCEDURE_ENVIRONMENT]) = GET_ENV;
      Free += 2;
      break;

    case TC_MANIFEST_NM_VECTOR:
      Do_Micro_Error (ERR_EXECUTE_MANIFEST_VECTOR, false);
      goto internal_apply;

    case TC_SCODE_QUOTE:
      SET_VAL (MEMORY_REF (GET_EXP, SCODE_QUOTE_OBJECT));
      break;

    case TC_SEQUENCE:
      STACK_CHECK (CONTINUATION_SIZE + 1);
      STACK_PUSH (GET_ENV);
      push_nth_then (RC_EXECUTE_SEQUENCE_FINISH, SEQUENCE_1);
      goto do_expression;

    case TC_SYNTAX_ERROR:
      Do_Micro_Error (ERR_SYNTAX_ERROR, false);
      goto internal_apply;

    case TC_THE_ENVIRONMENT:
      SET_VAL (GET_ENV);
      break;

    case TC_VARIABLE:
      {
	SCHEME_OBJECT val;
	long code = lookup_variable (GET_ENV, VARIABLE_SYMBOL (GET_EXP), &val);
	if (code == PRIM_DONE)
          {
	    SET_VAL (val);
            break;
          }
	if (VARIABLE_SAFE_P (GET_EXP) && code == ERR_UNASSIGNED_VARIABLE)
          {
	    SET_VAL (UNASSIGNED_OBJECT);
            break;
          }
	/* Back out of the evaluation. */
	if (code == PRIM_INTERRUPT)
	  {
            prepare_eval_repeat ();
            setup_interrupt (PENDING_INTERRUPTS ());
            goto perform_application;
          }
	Do_Micro_Error (code, false);
        goto internal_apply;
      }
    }

  /* Now restore the continuation saved during an earlier part of the
     EVAL cycle and continue as directed.  */

 pop_return:

#ifdef COMPILE_STEPPER
  if (trapping
      && (!WITHIN_CRITICAL_SECTION_P ())
      && ((FETCH_RETURN_TRAPPER ()) != SHARP_F))
    {
      STACK_CHECK (3);
      STACK_PUSH (GET_VAL);
      STACK_PUSH (FETCH_RETURN_TRAPPER ());
      PUSH_APPLY_FRAME_HEADER (1);
      trapping = false;
      goto Apply_Non_Trapping;
    }
#endif /* COMPILE_STEPPER */

 pop_return_non_trapping:
#ifdef POP_RETURN_UCODE_HOOK
  POP_RETURN_UCODE_HOOK ();
#endif
  SET_RET (STACK_POP ());
  SET_EXP (STACK_POP ());
#ifdef ENABLE_DEBUGGING_TOOLS
  if (!RETURN_CODE_P (GET_RET))
    {
      STACK_PUSH (GET_VAL);	/* For possible stack trace */
      SAVE_CONT ();
      Microcode_Termination (TERM_BAD_STACK);
    }
#endif

  /* Dispatch on the return code.  A BREAK here will cause
     a "goto pop_return" to occur, since this is the most
     common occurrence.
   */

  switch (OBJECT_DATUM (GET_RET))
    {

    case RC_COMB_APPLY_FUNCTION:
      END_SUBPROBLEM ();
      goto internal_apply_val;

    case RC_COMB_SAVE_VALUE:
      {
	SET_ENV (STACK_POP ());
	unsigned long arg = OBJECT_DATUM (STACK_REF (STACK_COMB_FINGER)) - 1;
	STACK_REF (STACK_COMB_FIRST_ARG + arg) = GET_VAL;
	STACK_REF (STACK_COMB_FINGER)
	  = MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, arg);
	/* DO NOT count on the type code being NMVector here, since
	   the stack parser may create them with #F here! */
	if (arg > 0)
	  {
	    STACK_PUSH (GET_ENV);
	    do_another_then (RC_COMB_SAVE_VALUE, COMB_ARG_1_SLOT - 1 + arg);
            goto do_expression;
	  }
	else
	  {
	    // apply_frame_header
	    STACK_PUSH (MEMORY_REF (GET_EXP, COMB_VECTOR_HEADER));
	    do_another_then (RC_COMB_APPLY_FUNCTION, COMB_FN_SLOT);
            goto do_expression;
	  }
      }

#ifdef CC_SUPPORT_P

#define DEFINE_COMPILER_RESTART(return_code, entry)			\
    case return_code:							\
      {									\
	dispatch_code = (entry ());					\
	goto return_from_compiled_code;					\
      }

      DEFINE_COMPILER_RESTART
	(RC_COMP_INTERRUPT_RESTART, comp_interrupt_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_LOOKUP_TRAP_RESTART, comp_lookup_trap_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_ASSIGNMENT_TRAP_RESTART, comp_assignment_trap_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_OP_REF_TRAP_RESTART, comp_op_lookup_trap_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_CACHE_REF_APPLY_RESTART, comp_cache_lookup_apply_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_SAFE_REF_TRAP_RESTART, comp_safe_lookup_trap_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_UNASSIGNED_TRAP_RESTART, comp_unassigned_p_trap_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_LINK_CACHES_RESTART, comp_link_caches_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_ERROR_RESTART, comp_error_restart);

    case RC_REENTER_COMPILED_CODE:
      dispatch_code = (return_to_compiled_code ());
      goto return_from_compiled_code;

#endif

    case RC_CONDITIONAL_DECIDE:
      END_SUBPROBLEM ();
      SET_ENV (STACK_POP ());
      reduces_to_nth
	((GET_VAL == SHARP_F) ? COND_ALTERNATIVE : COND_CONSEQUENT);
      goto do_expression;

    case RC_DISJUNCTION_DECIDE:
      /* Return predicate if it isn't #F; else do ALTERNATIVE */
      END_SUBPROBLEM ();
      SET_ENV (STACK_POP ());
      if (GET_VAL != SHARP_F)
	goto pop_return;
      reduces_to_nth (OR_ALTERNATIVE);
      goto do_expression;

    case RC_END_OF_COMPUTATION:
      {
	/* Signals bottom of stack */
	interpreter_state_t previous_state
	  = (interpreter_state -> previous_state);
	if (previous_state == NULL_INTERPRETER_STATE)
	  {
	    termination_end_of_computation ();
	    /*NOTREACHED*/
	  }
	else
	  {
	    dstack_position = interpreter_catch_dstack_position;
	    interpreter_state = previous_state;
	    return;
	  }
      }

    case RC_EVAL_ERROR:
      /* Should be called RC_REDO_EVALUATION. */
      SET_ENV (STACK_POP ());
      reduces_to (GET_EXP);
      goto do_expression;

    case RC_EXECUTE_ACCESS_FINISH:
      {
	SCHEME_OBJECT val;
	long code
	  = (lookup_variable (GET_VAL,
			      (MEMORY_REF (GET_EXP, ACCESS_NAME)),
			      (&val)));
	if (code == PRIM_DONE)
	  SET_VAL (val);
	else if (code == PRIM_INTERRUPT)
	  {
	    prepare_pop_return_interrupt (RC_EXECUTE_ACCESS_FINISH, GET_VAL);
	    setup_interrupt (PENDING_INTERRUPTS ());
            goto perform_application;
	  }
	else
          {
	    pop_return_error (code);
            goto internal_apply;
          }
      }
      END_SUBPROBLEM ();
      break;

    case RC_EXECUTE_ASSIGNMENT_FINISH:
      {
	SCHEME_OBJECT variable = (MEMORY_REF (GET_EXP, ASSIGN_NAME));
	SCHEME_OBJECT old_val;
	long code;

	SET_ENV (STACK_POP ());
	if (TC_VARIABLE == (OBJECT_TYPE (variable)))
	  code = (assign_variable (GET_ENV,
				   (VARIABLE_SYMBOL (variable)),
				   GET_VAL,
				   (&old_val)));
	else
	  code = ERR_BAD_FRAME;
	if (code == PRIM_DONE)
	  SET_VAL (old_val);
	else
	  {
	    STACK_PUSH (GET_ENV);
	    if (code == PRIM_INTERRUPT)
	      {
		prepare_pop_return_interrupt
		  (RC_EXECUTE_ASSIGNMENT_FINISH, GET_VAL);
		setup_interrupt (PENDING_INTERRUPTS ());
                goto perform_application;
	      }
	    else
              {
                pop_return_error (code);
                goto internal_apply;
              }
	  }
      }
      END_SUBPROBLEM ();
      break;

    case RC_EXECUTE_DEFINITION_FINISH:
      {
	SCHEME_OBJECT name = (MEMORY_REF (GET_EXP, DEFINE_NAME));
	SCHEME_OBJECT value = GET_VAL;
	SET_ENV (STACK_POP ());
	long result = (define_variable (GET_ENV, name, value));
	if (result == PRIM_DONE)
	  {
	    END_SUBPROBLEM ();
	    SET_VAL (name);
	    break;
	  }
	STACK_PUSH (GET_ENV);
	if (result == PRIM_INTERRUPT)
	  {
	    prepare_pop_return_interrupt
	      (RC_EXECUTE_DEFINITION_FINISH, value);
	    setup_interrupt (PENDING_INTERRUPTS ());
            goto perform_application;
	  }
	SET_VAL (value);
	pop_return_error (result);
        goto internal_apply;
      }

    case RC_HALT:
      Microcode_Termination (TERM_TERM_HANDLER);

    case RC_HARDWARE_TRAP:
      {
	/* This just reinvokes the handler */
	SCHEME_OBJECT info = (STACK_REF (0));
	SAVE_CONT ();
	SCHEME_OBJECT handler
	  = ((VECTOR_P (fixed_objects))
	     ? (VECTOR_REF (fixed_objects, TRAP_HANDLER))
	     : SHARP_F);
	if (handler == SHARP_F)
	  {
	    outf_fatal ("There is no trap handler for recovery!\n");
	    termination_trap ();
	    /*NOTREACHED*/
	  }
	STACK_CHECK (STACK_ENV_EXTRA_SLOTS + 2);
	STACK_PUSH (info);
	STACK_PUSH (handler);
	PUSH_APPLY_FRAME_HEADER (1);
      }
      goto internal_apply;

      /* internal_apply, the core of the application mechanism.

	 Branch here to perform a function application.

	 At this point the top of the stack contains an application
	 frame which consists of the following elements (see sdata.h):

	 - A header specifying the frame length.
	 - A procedure.
	 - The actual (evaluated) arguments.

	 No registers (except the stack pointer) are meaningful at
	 this point.  Before interrupts or errors are processed, some
	 registers are cleared to avoid holding onto garbage if a
	 garbage collection occurs.  */

    case RC_INTERNAL_APPLY_VAL:
    internal_apply_val:
      (APPLY_FRAME_PROCEDURE ()) = GET_VAL;
      FALLTHROUGH ();

    case RC_INTERNAL_APPLY:
    internal_apply:

#ifdef COMPILE_STEPPER
      if (trapping
	  && (!WITHIN_CRITICAL_SECTION_P ())
	  && ((FETCH_APPLY_TRAPPER ()) != SHARP_F))
	{
	  unsigned long frame_size = (APPLY_FRAME_SIZE ());
	  (* (STACK_LOC (0))) = (FETCH_APPLY_TRAPPER ());
	  PUSH_APPLY_FRAME_HEADER (frame_size);
	  trapping = false;
	}
#endif /* COMPILE_STEPPER */

    Apply_Non_Trapping:
      if (PENDING_INTERRUPTS_P)
	{
	  unsigned long interrupts = (PENDING_INTERRUPTS ());
	  prepare_apply_interrupt ();
	  setup_interrupt (interrupts);
          goto perform_application;
	}

    perform_application:
#ifdef APPLY_UCODE_HOOK
      APPLY_UCODE_HOOK ();
#endif
    apply_dispatch:
      {
	SCHEME_OBJECT proc = (APPLY_FRAME_PROCEDURE ());
	switch (OBJECT_TYPE (proc))
	  {
	  case TC_ENTITY:
	    {
	      unsigned long frame_size = (APPLY_FRAME_SIZE ());
	      SCHEME_OBJECT data = (MEMORY_REF (proc, ENTITY_DATA));
	      if ((VECTOR_P (data))
		  && (frame_size < (VECTOR_LENGTH (data)))
		  && ((VECTOR_REF (data, frame_size)) != SHARP_F)
		  && ((VECTOR_REF (data, 0))
		      == (VECTOR_REF (fixed_objects, ARITY_DISPATCHER_TAG))))
		{
		  (APPLY_FRAME_PROCEDURE ()) = (VECTOR_REF (data, frame_size));
		  goto apply_dispatch;
		}
	      (STACK_REF (0)) = (MEMORY_REF (proc, ENTITY_OPERATOR));
	      PUSH_APPLY_FRAME_HEADER (frame_size);

	    entity_apply:
	      /* This must be done to prevent an infinite push loop by
		 an entity whose handler is the entity itself or some
		 other such loop.  Of course, it will die if stack overflow
		 interrupts are disabled.  */
	      STACK_CHECK (0);
	      goto internal_apply;
	    }

	  case TC_RECORD:
	    {
	      SCHEME_OBJECT applicator = record_applicator (proc);
	      if (applicator == SHARP_F)
                {
		  application_error (ERR_INAPPLICABLE_OBJECT);
                  goto internal_apply;
                }
	      unsigned long frame_size = (APPLY_FRAME_SIZE ());
	      (STACK_REF (0)) = applicator;
	      PUSH_APPLY_FRAME_HEADER (frame_size);
	      goto entity_apply;
	    }

	  case TC_PROCEDURE:
	    {
	      unsigned long frame_size = APPLY_FRAME_SIZE ();
	      SCHEME_OBJECT lambda = MEMORY_REF (proc, PROCEDURE_LAMBDA_EXPR);
	      {
		unsigned long nparams
                  = VECTOR_LENGTH (MEMORY_REF (lambda, LAMBDA_FORMALS));
		if (! (frame_size == nparams
                       || (OBJECT_TYPE (lambda) == TC_LEXPR
                           && frame_size < nparams)))
                  {
		    application_error (ERR_WRONG_NUMBER_OF_ARGUMENTS);
                    goto internal_apply;
                  }
	      }
              unsigned long nwords = frame_size + 1;
	      if (GC_NEEDED_P (nwords))
		{
		  prepare_apply_interrupt ();
		  immediate_gc (nwords);
                  goto perform_application;
		}
	      {
		SCHEME_OBJECT * end = Free + nwords;
		SCHEME_OBJECT env = MAKE_POINTER_OBJECT (TC_ENVIRONMENT, Free);
		*Free++ = MAKE_OBJECT (TC_MANIFEST_VECTOR, frame_size);
		(void) STACK_POP (); // discard apply_frame_header
		while (Free < end)
		  *Free++ = STACK_POP ();
		SET_ENV (env);
		reduces_to (MEMORY_REF (lambda, LAMBDA_SCODE));
                goto do_expression;
	      }
	    }

	  case TC_CONTROL_POINT:
	    if ((APPLY_FRAME_SIZE ()) != 2)
              {
                application_error (ERR_WRONG_NUMBER_OF_ARGUMENTS);
                goto internal_apply;
              }
	    SET_VAL (* (APPLY_FRAME_ARGS ()));
	    unpack_control_point (proc);
	    RESET_HISTORY ();
	    goto pop_return;

	    /* After checking the number of arguments, remove the
	       frame header since primitives do not expect it. */

	  case TC_PRIMITIVE:
	    if (!IMPLEMENTED_PRIMITIVE_P (proc))
              {
                application_error (ERR_UNIMPLEMENTED_PRIMITIVE);
                goto internal_apply;
              }
	    {
	      unsigned long n_args = (APPLY_FRAME_N_ARGS ());

	      /* Note that the first test below will fail for lexpr
		 primitives.  */

	      if (n_args != (PRIMITIVE_ARITY (proc)))
		{
		  if ((PRIMITIVE_ARITY (proc)) != LEXPR_PRIMITIVE_ARITY)
                    {
		      application_error (ERR_WRONG_NUMBER_OF_ARGUMENTS);
                      goto internal_apply;
                    }
		  SET_LEXPR_ACTUALS (n_args);
		}
	      stack_pointer = (APPLY_FRAME_ARGS ());
	      SET_EXP (proc);
	      APPLY_PRIMITIVE_FROM_INTERPRETER (proc);
	      POP_PRIMITIVE_FRAME (n_args);
	      goto pop_return;
	    }

	  case TC_EXTENDED_PROCEDURE:
	    {
	      SCHEME_OBJECT lambda = GET_PROCEDURE_LAMBDA (proc);
	      unsigned long reqs = ELAMBDA_REQS (lambda);
	      unsigned long opts = ELAMBDA_OPTS (lambda);
	      unsigned long rest = ELAMBDA_REST (lambda);
	      unsigned long nfixed = reqs + opts;
	      unsigned long nargs = POP_APPLY_FRAME_HEADER ();

	      if (nargs < reqs || (rest == 0 && nargs > nfixed))
		{
		  PUSH_APPLY_FRAME_HEADER (nargs);
		  application_error (ERR_WRONG_NUMBER_OF_ARGUMENTS);
                  goto internal_apply;
		}

	      unsigned long size = /* proc: */ 1 + nfixed + rest;
              unsigned long nwords
                = 1             // vector header
                  + size
                  // rest list:
                  + (nargs > nfixed) ? 2 * (nargs - nfixed) : 0;
	      if (GC_NEEDED_P (nwords))
		{
		  PUSH_APPLY_FRAME_HEADER (nargs);
		  prepare_apply_interrupt ();
		  immediate_gc (nwords);
                  goto perform_application;
		}
	      SCHEME_OBJECT * scan = Free;
	      SCHEME_OBJECT temp = MAKE_POINTER_OBJECT (TC_ENVIRONMENT, scan);
	      *scan++ = MAKE_OBJECT (TC_MANIFEST_VECTOR, size);
	      if (nargs <= nfixed)
		{
                  *scan++ = STACK_POP (); // proc
		  for (unsigned int i = 0; i < nargs; i += 1)
		    *scan++ = STACK_POP ();
		  for (unsigned int i = nargs; i < nfixed; i += 1)
		    *scan++ = DEFAULT_OBJECT;
		  if (rest == 1)
		    *scan++ = EMPTY_LIST;
		}
	      else
		{
		  /* assert (rest == 1) */
		  SCHEME_OBJECT list
		    = MAKE_POINTER_OBJECT (TC_LIST, scan + size);
                  *scan++ = STACK_POP (); // proc
		  for (unsigned int i = 0; i < nfixed; i += 1)
		    *scan++ = STACK_POP ();
		  *scan++ = list;
		  /* Now scan == OBJECT_ADDRESS (list) */
		  for (unsigned int i = nfixed; i < nargs; i += 1)
		    {
		      *scan++ = STACK_POP ();
		      *scan = MAKE_POINTER_OBJECT (TC_LIST, scan + 1);
		      scan += 1;
		    }
		  scan[-1] = EMPTY_LIST;
		}
	      Free = scan;
	      SET_ENV (temp);
	      reduces_to (ELAMBDA_BODY (lambda));
              goto do_expression;
	    }

#ifdef CC_SUPPORT_P
	  case TC_COMPILED_ENTRY:
	    {
	      guarantee_cc_return (1 + (APPLY_FRAME_SIZE ()));
	      dispatch_code = (apply_compiled_procedure ());

	    return_from_compiled_code:
	      switch (dispatch_code)
		{
		case PRIM_DONE:
		  goto pop_return;

		case PRIM_APPLY:
		  goto internal_apply;

		case PRIM_INTERRUPT:
		  setup_interrupt (PENDING_INTERRUPTS ());
                  goto perform_application;

		case PRIM_APPLY_INTERRUPT:
		  prepare_apply_interrupt ();
		  setup_interrupt (PENDING_INTERRUPTS ());
                  goto perform_application;

		case ERR_INAPPLICABLE_OBJECT:
		case ERR_WRONG_NUMBER_OF_ARGUMENTS:
		  application_error (dispatch_code);
                  goto internal_apply;

		default:
		  Do_Micro_Error (dispatch_code, true);
		  goto internal_apply;
		}
	    }
#endif

	  default:
	    application_error (ERR_INAPPLICABLE_OBJECT);
            goto internal_apply;
	  }
      }

    case RC_JOIN_STACKLETS:
      unpack_control_point (GET_EXP);
      break;

    case RC_NORMAL_GC_DONE:
      SET_VAL (GET_EXP);
      /* Paranoia */
      if (GC_NEEDED_P (gc_space_needed))
	termination_gc_out_of_space ();
      gc_space_needed = 0;
      EXIT_CRITICAL_SECTION ({ SAVE_CONT (); });
      break;

    case RC_POP_RETURN_ERROR:
    case RC_RESTORE_VALUE:
      SET_VAL (GET_EXP);
      break;

      /* The following two return codes are both used to restore a
	 saved history object.	The difference is that the first does
	 not copy the history object while the second does.  In both
	 cases, the GET_EXP contains the history object and the
	 next item to be popped off the stack contains the offset back
	 to the previous restore history return code.  */

    case RC_RESTORE_DONT_COPY_HISTORY:
      {
	prev_restore_history_offset = (OBJECT_DATUM (STACK_POP ()));
	(void) STACK_POP ();
	history_register = (OBJECT_ADDRESS (GET_EXP));
	break;
      }

    case RC_RESTORE_HISTORY:
      {
	if (!restore_history (GET_EXP))
	  {
	    SAVE_CONT ();
	    STACK_CHECK (CONTINUATION_SIZE);
	    PUSH_CONT_RC (RC_RESTORE_VALUE, GET_VAL);
	    immediate_gc (HEAP_AVAILABLE);
            goto perform_application;
	  }
	prev_restore_history_offset = (OBJECT_DATUM (STACK_POP ()));
	(void) STACK_POP ();
	if (prev_restore_history_offset > 0)
	  (STACK_LOCATIVE_REFERENCE (STACK_BOTTOM,
				     (-prev_restore_history_offset)))
	    = (MAKE_RETURN_CODE (RC_RESTORE_HISTORY));
	break;
      }

    case RC_RESTORE_INT_MASK:
      SET_INTERRUPT_MASK (UNSIGNED_FIXNUM_TO_LONG (GET_EXP));
      if (GC_NEEDED_P (0))
	REQUEST_GC (0);
      if (PENDING_INTERRUPTS_P)
	{
	  PUSH_CONT_RC (RC_RESTORE_VALUE, GET_VAL);
	  setup_interrupt (PENDING_INTERRUPTS ());
          goto perform_application;
	}
      break;

    case RC_STACK_MARKER:
      /* Frame consists of the return code followed by two objects.
	 The first object has already been popped into GET_EXP,
	 so just pop the second argument.  */
      stack_pointer = (STACK_LOCATIVE_OFFSET (stack_pointer, 1));
      break;

    case RC_EXECUTE_SEQUENCE_FINISH:
      END_SUBPROBLEM ();
      SET_ENV (STACK_POP ());
      reduces_to_nth (SEQUENCE_2);
      goto do_expression;

    case RC_SNAP_NEED_THUNK:
      /* Don't snap thunk twice; evaluation of the thunk's body might
	 have snapped it already.  */
      if ((MEMORY_REF (GET_EXP, THUNK_SNAPPED)) == SHARP_T)
	SET_VAL (MEMORY_REF (GET_EXP, THUNK_VALUE));
      else
	{
	  MEMORY_SET (GET_EXP, THUNK_SNAPPED, SHARP_T);
	  MEMORY_SET (GET_EXP, THUNK_VALUE, GET_VAL);
	}
      break;

    default:
      pop_return_error (ERR_INAPPLICABLE_CONTINUATION);
      goto internal_apply;
    }
  goto pop_return;
}
