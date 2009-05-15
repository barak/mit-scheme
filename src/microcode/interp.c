/* -*-C-*-

$Id: interp.c,v 9.111 2008/09/27 03:59:09 cph Exp $

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

/* The interpreter */

#include "scheme.h"
#include "trap.h"
#include "lookup.h"
#include "winder.h"
#include "history.h"

extern void * obstack_chunk_alloc (size_t);
#define obstack_chunk_free free
extern void preserve_signal_mask (void);
extern void fixup_float_rounding_mode (void);

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
 * EVAL you first push a "return code" (using SAVE_CONT) on
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

#define SIGNAL_INTERRUPT(Masked_Code)					\
{									\
  setup_interrupt (Masked_Code);					\
  goto perform_application;						\
}

#define PREPARE_POP_RETURN_INTERRUPT(Return_Code, Contents_of_Val)	\
{									\
  SCHEME_OBJECT temp = (Contents_of_Val);				\
  SET_RC (Return_Code);							\
  SAVE_CONT ();								\
  SET_RC (RC_RESTORE_VALUE);						\
  SET_EXP (temp);							\
  SAVE_CONT ();								\
}

#define PREPARE_APPLY_INTERRUPT()					\
{									\
  SET_EXP (SHARP_F);							\
  PREPARE_POP_RETURN_INTERRUPT						\
    (RC_INTERNAL_APPLY_VAL, (APPLY_FRAME_PROCEDURE ()));		\
}

#define APPLICATION_ERROR(code) do					\
{									\
  SET_EXP (SHARP_F);							\
  SET_RC (RC_INTERNAL_APPLY_VAL);					\
  SAVE_CONT ();								\
  SET_VAL (APPLY_FRAME_PROCEDURE ());					\
  Do_Micro_Error (code, true);						\
  goto internal_apply;							\
} while (0)

#define IMMEDIATE_GC(N)							\
{									\
  REQUEST_GC (N);							\
  SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());				\
}

#define EVAL_GC_CHECK(Amount)						\
{									\
  if (GC_NEEDED_P (Amount))						\
    {									\
      PREPARE_EVAL_REPEAT ();						\
      IMMEDIATE_GC (Amount);						\
    }									\
}

#define PREPARE_EVAL_REPEAT() do					\
{									\
  Will_Push (CONTINUATION_SIZE + 1);					\
  PUSH_ENV ();								\
  SET_RC (RC_EVAL_ERROR);						\
  SAVE_CONT ();								\
  Pushed ();								\
} while (0)

#define EVAL_ERROR(code) do						\
{									\
  Do_Micro_Error (code, false);						\
  goto internal_apply;							\
} while (0)

#define POP_RETURN_ERROR(code) do					\
{									\
  SAVE_CONT ();								\
  Do_Micro_Error (code, true);						\
  goto internal_apply;							\
} while (0)

#define PROCEED_AFTER_PRIMITIVE() SET_PRIMITIVE (SHARP_F)

#define REDUCES_TO(expression) do					\
{									\
  SET_EXP (expression);							\
  NEW_REDUCTION (GET_EXP, GET_ENV);					\
  goto do_expression;							\
} while (0)

#define REDUCES_TO_NTH(n) REDUCES_TO (MEMORY_REF (GET_EXP, (n)))

#define DO_NTH_THEN(Return_Code, n) do					\
{									\
  SET_RC (Return_Code);							\
  SAVE_CONT ();								\
  SET_EXP (MEMORY_REF (GET_EXP, (n)));					\
  NEW_SUBPROBLEM (GET_EXP, GET_ENV);					\
  goto do_expression;							\
} while (0)

#define PUSH_NTH_THEN(Return_Code, n)					\
  SET_RC (Return_Code);							\
  SAVE_CONT ();								\
  SET_EXP (MEMORY_REF (GET_EXP, (n)));					\
  NEW_SUBPROBLEM (GET_EXP, GET_ENV);					\
  Pushed ();								\
  goto do_expression

#define DO_ANOTHER_THEN(Return_Code, N) do				\
{									\
  SET_RC (Return_Code);							\
  SAVE_CONT ();								\
  SET_EXP (MEMORY_REF (GET_EXP, (N)));					\
  REUSE_SUBPROBLEM (GET_EXP, GET_ENV);					\
  goto do_expression;							\
} while (0)

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
Interpret (int pop_return_p)
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
  fixup_float_rounding_mode ();

  switch (dispatch_code)
    {
    case 0:			/* first time */
      if (pop_return_p)
	goto pop_return;	/* continue */
      else
	break;			/* fall into eval */

    case PRIM_APPLY:
      PROCEED_AFTER_PRIMITIVE ();
      goto internal_apply;

    case PRIM_NO_TRAP_APPLY:
      PROCEED_AFTER_PRIMITIVE ();
      goto Apply_Non_Trapping;

    case PRIM_APPLY_INTERRUPT:
      PROCEED_AFTER_PRIMITIVE ();
      PREPARE_APPLY_INTERRUPT ();
      SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());

    case PRIM_APPLY_ERROR:
      PROCEED_AFTER_PRIMITIVE ();
      APPLICATION_ERROR (prim_apply_error_code);

    case PRIM_DO_EXPRESSION:
      SET_VAL (GET_EXP);
      PROCEED_AFTER_PRIMITIVE ();
      REDUCES_TO (GET_VAL);

    case PRIM_NO_TRAP_EVAL:
      SET_VAL (GET_EXP);
      PROCEED_AFTER_PRIMITIVE ();
      NEW_REDUCTION (GET_VAL, GET_ENV);
      goto eval_non_trapping;

    case PRIM_POP_RETURN:
      PROCEED_AFTER_PRIMITIVE ();
      goto pop_return;

    case PRIM_RETURN_TO_C:
      PROCEED_AFTER_PRIMITIVE ();
      unbind_interpreter_state (interpreter_state);
      return;

    case PRIM_NO_TRAP_POP_RETURN:
      PROCEED_AFTER_PRIMITIVE ();
      goto pop_return_non_trapping;

    case PRIM_INTERRUPT:
      back_out_of_primitive ();
      SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());

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
     and placed on the stack (using SAVE_CONT), the new expression is
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
      Will_Push (4);
      PUSH_ENV ();
      PUSH_EXP ();
      STACK_PUSH (FETCH_EVAL_TRAPPER ());
      PUSH_APPLY_FRAME_HEADER (2);
      Pushed ();
      goto Apply_Non_Trapping;
    }
#endif /* COMPILE_STEPPER */

 eval_non_trapping:
#ifdef EVAL_UCODE_HOOK
  EVAL_UCODE_HOOK ();
#endif
  switch (OBJECT_TYPE (GET_EXP))
    {
    case TC_BIG_FIXNUM:         /* The self evaluating items */
    case TC_BIG_FLONUM:
    case TC_CHARACTER_STRING:
    case TC_CHARACTER:
    case TC_COMPILED_CODE_BLOCK:
    case TC_COMPLEX:
    case TC_CONTROL_POINT:
    case TC_DELAYED:
    case TC_ENTITY:
    case TC_ENVIRONMENT:
    case TC_EXTENDED_PROCEDURE:
    case TC_FIXNUM:
    case TC_HUNK3_A:
    case TC_HUNK3_B:
    case TC_INTERNED_SYMBOL:
    case TC_LIST:
    case TC_NON_MARKED_VECTOR:
    case TC_NULL:
    case TC_PRIMITIVE:
    case TC_PROCEDURE:
    case TC_QUAD:
    case TC_RATNUM:
    case TC_REFERENCE_TRAP:
    case TC_RETURN_CODE:
    case TC_UNINTERNED_SYMBOL:
    case TC_CONSTANT:
    case TC_VECTOR:
    case TC_VECTOR_16B:
    case TC_VECTOR_1B:
    default:
      SET_VAL (GET_EXP);
      break;

    case TC_ACCESS:
      Will_Push (CONTINUATION_SIZE);
      PUSH_NTH_THEN (RC_EXECUTE_ACCESS_FINISH, ACCESS_ENVIRONMENT);

    case TC_ASSIGNMENT:
      Will_Push (CONTINUATION_SIZE + 1);
      PUSH_ENV ();
      PUSH_NTH_THEN (RC_EXECUTE_ASSIGNMENT_FINISH, ASSIGN_VALUE);

    case TC_BROKEN_HEART:
      Microcode_Termination (TERM_BROKEN_HEART);

    case TC_COMBINATION:
      {
	long length = ((VECTOR_LENGTH (GET_EXP)) - 1);
	Will_Push (length + 2 + CONTINUATION_SIZE);
	stack_pointer = (STACK_LOC (-length));
        STACK_PUSH (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, length));
	/* The finger: last argument number */
	Pushed ();
        if (length == 0)
	  {
	    PUSH_APPLY_FRAME_HEADER (0); /* Frame size */
	    DO_NTH_THEN (RC_COMB_APPLY_FUNCTION, COMB_FN_SLOT);
	  }
	PUSH_ENV ();
	DO_NTH_THEN (RC_COMB_SAVE_VALUE, (length + 1));
      }

    case TC_COMBINATION_1:
      Will_Eventually_Push (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 1);
      PUSH_ENV ();
      DO_NTH_THEN (RC_COMB_1_PROCEDURE, COMB_1_ARG_1);

    case TC_COMBINATION_2:
      Will_Eventually_Push (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 2);
      PUSH_ENV ();
      DO_NTH_THEN (RC_COMB_2_FIRST_OPERAND, COMB_2_ARG_2);

    case TC_COMMENT:
      REDUCES_TO_NTH (COMMENT_EXPRESSION);

    case TC_CONDITIONAL:
      Will_Push (CONTINUATION_SIZE + 1);
      PUSH_ENV ();
      PUSH_NTH_THEN (RC_CONDITIONAL_DECIDE, COND_PREDICATE);

#ifdef CC_SUPPORT_P
    case TC_COMPILED_ENTRY:
      dispatch_code = (enter_compiled_expression ());
      goto return_from_compiled_code;
#endif

    case TC_DEFINITION:
      Will_Push (CONTINUATION_SIZE + 1);
      PUSH_ENV ();
      PUSH_NTH_THEN (RC_EXECUTE_DEFINITION_FINISH, DEFINE_VALUE);

    case TC_DELAY:
      /* Deliberately omitted: EVAL_GC_CHECK (2); */
      SET_VAL (MAKE_POINTER_OBJECT (TC_DELAYED, Free));
      (Free[THUNK_ENVIRONMENT]) = GET_ENV;
      (Free[THUNK_PROCEDURE]) = (MEMORY_REF (GET_EXP, DELAY_OBJECT));
      Free += 2;
      break;

    case TC_DISJUNCTION:
      Will_Push (CONTINUATION_SIZE + 1);
      PUSH_ENV ();
      PUSH_NTH_THEN (RC_DISJUNCTION_DECIDE, OR_PREDICATE);

    case TC_EXTENDED_LAMBDA:
      /* Deliberately omitted: EVAL_GC_CHECK (2); */
      SET_VAL (MAKE_POINTER_OBJECT (TC_EXTENDED_PROCEDURE, Free));
      (Free[PROCEDURE_LAMBDA_EXPR]) = GET_EXP;
      (Free[PROCEDURE_ENVIRONMENT]) = GET_ENV;
      Free += 2;
      break;

    case TC_IN_PACKAGE:
      Will_Push (CONTINUATION_SIZE);
      PUSH_NTH_THEN (RC_EXECUTE_IN_PACKAGE_CONTINUE, IN_PACKAGE_ENVIRONMENT);

    case TC_LAMBDA:
    case TC_LEXPR:
      /* Deliberately omitted: EVAL_GC_CHECK (2); */
      SET_VAL (MAKE_POINTER_OBJECT (TC_PROCEDURE, Free));
      (Free[PROCEDURE_LAMBDA_EXPR]) = GET_EXP;
      (Free[PROCEDURE_ENVIRONMENT]) = GET_ENV;
      Free += 2;
      break;

    case TC_MANIFEST_NM_VECTOR:
      EVAL_ERROR (ERR_EXECUTE_MANIFEST_VECTOR);

    case TC_PCOMB0:
      /* The argument to Will_Eventually_Push is determined by how
	 much will be on the stack if we back out of the primitive.  */
      Will_Eventually_Push (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      Finished_Eventual_Pushing (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      SET_EXP (OBJECT_NEW_TYPE (TC_PRIMITIVE, GET_EXP));
      goto primitive_internal_apply;

    case TC_PCOMB1:
      Will_Eventually_Push (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 1);
      DO_NTH_THEN (RC_PCOMB1_APPLY, PCOMB1_ARG_SLOT);

    case TC_PCOMB2:
      Will_Eventually_Push (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 2);
      PUSH_ENV ();
      DO_NTH_THEN (RC_PCOMB2_DO_1, PCOMB2_ARG_2_SLOT);

    case TC_PCOMB3:
      Will_Eventually_Push (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 3);
      PUSH_ENV ();
      DO_NTH_THEN (RC_PCOMB3_DO_2, PCOMB3_ARG_3_SLOT);

    case TC_SCODE_QUOTE:
      SET_VAL (MEMORY_REF (GET_EXP, SCODE_QUOTE_OBJECT));
      break;

    case TC_SEQUENCE_2:
      Will_Push (CONTINUATION_SIZE + 1);
      PUSH_ENV ();
      PUSH_NTH_THEN (RC_SEQ_2_DO_2, SEQUENCE_1);

    case TC_SEQUENCE_3:
      Will_Push (CONTINUATION_SIZE + 1);
      PUSH_ENV ();
      PUSH_NTH_THEN (RC_SEQ_3_DO_2, SEQUENCE_1);

    case TC_THE_ENVIRONMENT:
      SET_VAL (GET_ENV);
      break;

    case TC_VARIABLE:
      {
	SCHEME_OBJECT val = GET_VAL;
	long temp = (lookup_variable (GET_ENV, GET_EXP, (&val)));
	if (temp != PRIM_DONE)
	  {
	    /* Back out of the evaluation. */
	    if (temp == PRIM_INTERRUPT)
	      {
		PREPARE_EVAL_REPEAT ();
		SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());
	      }
	    EVAL_ERROR (temp);
	  }
	SET_VAL (val);
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
      Will_Push (3);
      trapping = false;
      PUSH_VAL ();
      STACK_PUSH (FETCH_RETURN_TRAPPER ());
      PUSH_APPLY_FRAME_HEADER (1);
      Pushed ();
      goto Apply_Non_Trapping;
    }
#endif /* COMPILE_STEPPER */

 pop_return_non_trapping:
#ifdef POP_RETURN_UCODE_HOOK
  POP_RETURN_UCODE_HOOK ();
#endif
  RESTORE_CONT ();
#ifdef ENABLE_DEBUGGING_TOOLS
  if (!RETURN_CODE_P (GET_RET))
    {
      PUSH_VAL ();		/* For possible stack trace */
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
    case RC_COMB_1_PROCEDURE:
      POP_ENV ();
      PUSH_VAL ();		/* Arg. 1 */
      STACK_PUSH (SHARP_F);	/* Operator */
      PUSH_APPLY_FRAME_HEADER (1);
      Finished_Eventual_Pushing (CONTINUATION_SIZE);
      DO_ANOTHER_THEN (RC_COMB_APPLY_FUNCTION, COMB_1_FN);

    case RC_COMB_2_FIRST_OPERAND:
      POP_ENV ();
      PUSH_VAL ();
      PUSH_ENV ();
      DO_ANOTHER_THEN (RC_COMB_2_PROCEDURE, COMB_2_ARG_1);

    case RC_COMB_2_PROCEDURE:
      POP_ENV ();
      PUSH_VAL ();		/* Arg 1, just calculated */
      STACK_PUSH (SHARP_F);	/* Function */
      PUSH_APPLY_FRAME_HEADER (2);
      Finished_Eventual_Pushing (CONTINUATION_SIZE);
      DO_ANOTHER_THEN (RC_COMB_APPLY_FUNCTION, COMB_2_FN);

    case RC_COMB_APPLY_FUNCTION:
      END_SUBPROBLEM ();
      goto internal_apply_val;

    case RC_COMB_SAVE_VALUE:
      {
	long Arg_Number;

	POP_ENV ();
	Arg_Number = ((OBJECT_DATUM (STACK_REF (STACK_COMB_FINGER))) - 1);
	(STACK_REF (STACK_COMB_FIRST_ARG + Arg_Number)) = GET_VAL;
	(STACK_REF (STACK_COMB_FINGER))
	  = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, Arg_Number));
	/* DO NOT count on the type code being NMVector here, since
	   the stack parser may create them with #F here! */
	if (Arg_Number > 0)
	  {
	    PUSH_ENV ();
	    DO_ANOTHER_THEN
	      (RC_COMB_SAVE_VALUE, ((COMB_ARG_1_SLOT - 1) + Arg_Number));
	  }
	/* Frame Size */
	STACK_PUSH (MEMORY_REF (GET_EXP, 0));
	DO_ANOTHER_THEN (RC_COMB_APPLY_FUNCTION, COMB_FN_SLOT);
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
      POP_ENV ();
      REDUCES_TO_NTH
	((GET_VAL == SHARP_F) ? COND_ALTERNATIVE : COND_CONSEQUENT);

    case RC_DISJUNCTION_DECIDE:
      /* Return predicate if it isn't #F; else do ALTERNATIVE */
      END_SUBPROBLEM ();
      POP_ENV ();
      if (GET_VAL != SHARP_F)
	goto pop_return;
      REDUCES_TO_NTH (OR_ALTERNATIVE);

    case RC_END_OF_COMPUTATION:
      {
	/* Signals bottom of stack */

	interpreter_state_t previous_state;
	previous_state = (interpreter_state -> previous_state);
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
      POP_ENV ();
      REDUCES_TO (GET_EXP);

    case RC_EXECUTE_ACCESS_FINISH:
      {
	SCHEME_OBJECT val;
	long code;

	if (!ENVIRONMENT_P (GET_VAL))
	  POP_RETURN_ERROR (ERR_BAD_FRAME);
	code = (lookup_variable (GET_VAL,
				 (MEMORY_REF (GET_EXP, ACCESS_NAME)),
				 (&val)));
	if (code == PRIM_DONE)
	  SET_VAL (val);
	else if (code == PRIM_INTERRUPT)
	  {
	    PREPARE_POP_RETURN_INTERRUPT (RC_EXECUTE_ACCESS_FINISH, GET_VAL);
	    SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());
	  }
	else
	  POP_RETURN_ERROR (code);
      }
      END_SUBPROBLEM ();
      break;

    case RC_EXECUTE_ASSIGNMENT_FINISH:
      {
	SCHEME_OBJECT old_val;
	long code;

	POP_ENV ();
	code = (assign_variable (GET_ENV,
				 (MEMORY_REF (GET_EXP, ASSIGN_NAME)),
				 GET_VAL,
				 (&old_val)));
	if (code == PRIM_DONE)
	  SET_VAL (old_val);
	else
	  {
	    PUSH_ENV ();
	    if (code == PRIM_INTERRUPT)
	      {
		PREPARE_POP_RETURN_INTERRUPT
		  (RC_EXECUTE_ASSIGNMENT_FINISH, GET_VAL);
		SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());
	      }
	    else
	      POP_RETURN_ERROR (code);
	  }
      }
      END_SUBPROBLEM ();
      break;

    case RC_EXECUTE_DEFINITION_FINISH:
      {
	SCHEME_OBJECT name = (MEMORY_REF (GET_EXP, DEFINE_NAME));
	SCHEME_OBJECT value = GET_VAL;
        long result;

        POP_ENV ();
        result = (define_variable (GET_ENV, name, value));
        if (result == PRIM_DONE)
	  {
	    END_SUBPROBLEM ();
	    SET_VAL (name);
	    break;
	  }
	PUSH_ENV ();
	if (result == PRIM_INTERRUPT)
	  {
	    PREPARE_POP_RETURN_INTERRUPT (RC_EXECUTE_DEFINITION_FINISH,
					  value);
	    SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());
	  }
	SET_VAL (value);
        POP_RETURN_ERROR (result);
      }

    case RC_EXECUTE_IN_PACKAGE_CONTINUE:
      if (ENVIRONMENT_P (GET_VAL))
	{
	  END_SUBPROBLEM ();
	  SET_ENV (GET_VAL);
	  REDUCES_TO_NTH (IN_PACKAGE_EXPRESSION);
	}
      POP_RETURN_ERROR (ERR_BAD_FRAME);

    case RC_HALT:
      Microcode_Termination (TERM_TERM_HANDLER);

    case RC_HARDWARE_TRAP:
      {
	/* This just reinvokes the handler */
	SCHEME_OBJECT info = (STACK_REF (0));
	SCHEME_OBJECT handler = SHARP_F;
	SAVE_CONT ();
	if (VECTOR_P (fixed_objects))
	  handler = (VECTOR_REF (fixed_objects, TRAP_HANDLER));
	if (handler == SHARP_F)
	  {
	    outf_fatal ("There is no trap handler for recovery!\n");
	    termination_trap ();
	    /*NOTREACHED*/
	  }
	Will_Push (STACK_ENV_EXTRA_SLOTS + 2);
	STACK_PUSH (info);
	STACK_PUSH (handler);
	PUSH_APPLY_FRAME_HEADER (1);
	Pushed ();
      }
      goto internal_apply;

      /* internal_apply, the core of the application mechanism.

	 Branch here to perform a function application.

	 At this point the top of the stack contains an application
	 frame which consists of the following elements (see sdata.h):

	 - A header specifying the frame length.
	 - A procedure.
	 - The actual (evaluated) arguments.

	 No registers (except the stack pointer) are meaning full at
	 this point.  Before interrupts or errors are processed, some
	 registers are cleared to avoid holding onto garbage if a
	 garbage collection occurs.  */

    case RC_INTERNAL_APPLY_VAL:
    internal_apply_val:

      (APPLY_FRAME_PROCEDURE ()) = GET_VAL;

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
	  PREPARE_APPLY_INTERRUPT ();
	  SIGNAL_INTERRUPT (interrupts);
	}

    perform_application:
#ifdef APPLY_UCODE_HOOK
      APPLY_UCODE_HOOK ();
#endif
      {
	SCHEME_OBJECT Function = (APPLY_FRAME_PROCEDURE ());

      apply_dispatch:
	switch (OBJECT_TYPE (Function))
	  {
	  case TC_ENTITY:
	    {
	      unsigned long frame_size = (APPLY_FRAME_SIZE ());
	      SCHEME_OBJECT data = (MEMORY_REF (Function, ENTITY_DATA));
	      if ((VECTOR_P (data))
		  && (frame_size < (VECTOR_LENGTH (data)))
		  && ((VECTOR_REF (data, frame_size)) != SHARP_F)
		  && ((VECTOR_REF (data, 0))
		      == (VECTOR_REF (fixed_objects, ARITY_DISPATCHER_TAG))))
		{
		  Function = (VECTOR_REF (data, frame_size));
		  (APPLY_FRAME_PROCEDURE ()) = Function;
		  goto apply_dispatch;
		}

	      (STACK_REF (0)) = (MEMORY_REF (Function, ENTITY_OPERATOR));
	      PUSH_APPLY_FRAME_HEADER (frame_size);
	      /* This must be done to prevent an infinite push loop by
		 an entity whose handler is the entity itself or some
		 other such loop.  Of course, it will die if stack overflow
		 interrupts are disabled.  */
	      STACK_CHECK (0);
	      goto internal_apply;
	    }

	  case TC_PROCEDURE:
	    {
	      unsigned long frame_size = (APPLY_FRAME_SIZE ());
	      Function = (MEMORY_REF (Function, PROCEDURE_LAMBDA_EXPR));
	      {
		SCHEME_OBJECT formals
		  = (MEMORY_REF (Function, LAMBDA_FORMALS));

		if ((frame_size != (VECTOR_LENGTH (formals)))
		    && (((OBJECT_TYPE (Function)) != TC_LEXPR)
			|| (frame_size < (VECTOR_LENGTH (formals)))))
		  APPLICATION_ERROR (ERR_WRONG_NUMBER_OF_ARGUMENTS);
	      }
	      if (GC_NEEDED_P (frame_size + 1))
		{
		  PREPARE_APPLY_INTERRUPT ();
		  IMMEDIATE_GC (frame_size + 1);
		}
	      {
		SCHEME_OBJECT * end = (Free + 1 + frame_size);
		SCHEME_OBJECT env
		  = (MAKE_POINTER_OBJECT (TC_ENVIRONMENT, Free));
		(*Free++) = (MAKE_OBJECT (TC_MANIFEST_VECTOR, frame_size));
		(void) STACK_POP ();
		while (Free < end)
		  (*Free++) = (STACK_POP ());
		SET_ENV (env);
		REDUCES_TO (MEMORY_REF (Function, LAMBDA_SCODE));
	      }
	    }

	  case TC_CONTROL_POINT:
	    if ((APPLY_FRAME_SIZE ()) != 2)
	      APPLICATION_ERROR (ERR_WRONG_NUMBER_OF_ARGUMENTS);
	    SET_VAL (* (APPLY_FRAME_ARGS ()));
	    unpack_control_point (Function);
	    RESET_HISTORY ();
	    goto pop_return;

	    /* After checking the number of arguments, remove the
	       frame header since primitives do not expect it.

	       NOTE: This code must match the application code which
	       follows primitive_internal_apply.  */

	  case TC_PRIMITIVE:
	    if (!IMPLEMENTED_PRIMITIVE_P (Function))
	      APPLICATION_ERROR (ERR_UNIMPLEMENTED_PRIMITIVE);
	    {
	      unsigned long n_args = (APPLY_FRAME_N_ARGS ());


	      /* Note that the first test below will fail for lexpr
		 primitives.  */

	      if (n_args != (PRIMITIVE_ARITY (Function)))
		{
		  if ((PRIMITIVE_ARITY (Function)) != LEXPR_PRIMITIVE_ARITY)
		    APPLICATION_ERROR (ERR_WRONG_NUMBER_OF_ARGUMENTS);
		  SET_LEXPR_ACTUALS (n_args);
		}
	      stack_pointer = (APPLY_FRAME_ARGS ());
	      SET_EXP (Function);
	      APPLY_PRIMITIVE_FROM_INTERPRETER (Function);
	      POP_PRIMITIVE_FRAME (n_args);
	      goto pop_return;
	    }

	  case TC_EXTENDED_PROCEDURE:
	    {
	      SCHEME_OBJECT lambda;
	      SCHEME_OBJECT temp;
	      unsigned long nargs;
	      unsigned long nparams;
	      unsigned long formals;
	      unsigned long params;
	      unsigned long auxes;
	      long rest_flag;
	      long size;
	      long i;
	      SCHEME_OBJECT * scan;

	      nargs = (POP_APPLY_FRAME_HEADER ());
	      lambda = (MEMORY_REF (Function, PROCEDURE_LAMBDA_EXPR));
	      Function = (MEMORY_REF (lambda, ELAMBDA_NAMES));
	      nparams = ((VECTOR_LENGTH (Function)) - 1);
	      Function = (Get_Count_Elambda (lambda));
	      formals = (Elambda_Formals_Count (Function));
	      params = ((Elambda_Opts_Count (Function)) + formals);
	      rest_flag = (Elambda_Rest_Flag (Function));
	      auxes = (nparams - (params + rest_flag));

	      if ((nargs < formals) || (!rest_flag && (nargs > params)))
		{
		  PUSH_APPLY_FRAME_HEADER (nargs);
		  APPLICATION_ERROR (ERR_WRONG_NUMBER_OF_ARGUMENTS);
		}
	      /* size includes the procedure slot, but not the header.  */
	      size = (params + rest_flag + auxes + 1);
	      if (GC_NEEDED_P
		  (size + 1
		   + ((nargs > params)
		      ? (2 * (nargs - params))
		      : 0)))
		{
		  PUSH_APPLY_FRAME_HEADER (nargs);
		  PREPARE_APPLY_INTERRUPT ();
		  IMMEDIATE_GC
		    (size + 1
		     + ((nargs > params)
			? (2 * (nargs - params))
			: 0));
		}
	      scan = Free;
	      temp = (MAKE_POINTER_OBJECT (TC_ENVIRONMENT, scan));
	      (*scan++) = (MAKE_OBJECT (TC_MANIFEST_VECTOR, size));
	      if (nargs <= params)
		{
		  for (i = (nargs + 1); (--i) >= 0; )
		    (*scan++) = (STACK_POP ());
		  for (i = (params - nargs); (--i) >= 0; )
		    (*scan++) = DEFAULT_OBJECT;
		  if (rest_flag)
		    (*scan++) = EMPTY_LIST;
		  for (i = auxes; (--i) >= 0; )
		    (*scan++) = UNASSIGNED_OBJECT;
		}
	      else
		{
		  /* rest_flag must be true. */
		  SCHEME_OBJECT list
		    = (MAKE_POINTER_OBJECT (TC_LIST, (scan + size)));
		  for (i = (params + 1); (--i) >= 0; )
		    (*scan++) = (STACK_POP ());
		  (*scan++) = list;
		  for (i = auxes; (--i) >= 0; )
		    (*scan++) = UNASSIGNED_OBJECT;
		  /* Now scan == OBJECT_ADDRESS (list) */
		  for (i = (nargs - params); (--i) >= 0; )
		    {
		      (*scan++) = (STACK_POP ());
		      (*scan) = MAKE_POINTER_OBJECT (TC_LIST, (scan + 1));
		      scan += 1;
		    }
		  (scan[-1]) = EMPTY_LIST;
		}

	      Free = scan;
	      SET_ENV (temp);
	      REDUCES_TO (Get_Body_Elambda (lambda));
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
		  SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());

		case PRIM_APPLY_INTERRUPT:
		  PREPARE_APPLY_INTERRUPT ();
		  SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());

		case ERR_INAPPLICABLE_OBJECT:
		case ERR_WRONG_NUMBER_OF_ARGUMENTS:
		  APPLICATION_ERROR (dispatch_code);

		default:
		  Do_Micro_Error (dispatch_code, true);
		  goto internal_apply;
		}
	    }
#endif

	  default:
	    APPLICATION_ERROR (ERR_INAPPLICABLE_OBJECT);
	  }
      }

    case RC_MOVE_TO_ADJACENT_POINT:
      /* GET_EXP contains the space in which we are moving */
      {
	long From_Count;
	SCHEME_OBJECT Thunk;
	SCHEME_OBJECT New_Location;

	From_Count
	  = (UNSIGNED_FIXNUM_TO_LONG (STACK_REF (TRANSLATE_FROM_DISTANCE)));
	if (From_Count != 0)
	  {
	    SCHEME_OBJECT Current = STACK_REF (TRANSLATE_FROM_POINT);
	    STACK_REF (TRANSLATE_FROM_DISTANCE)
	      = (LONG_TO_UNSIGNED_FIXNUM (From_Count - 1));
	    Thunk = (MEMORY_REF (Current, STATE_POINT_AFTER_THUNK));
	    New_Location
	      = (MEMORY_REF (Current, STATE_POINT_NEARER_POINT));
	    (STACK_REF (TRANSLATE_FROM_POINT)) = New_Location;
	    if ((From_Count == 1)
		&& ((STACK_REF (TRANSLATE_TO_DISTANCE))
		    == (LONG_TO_UNSIGNED_FIXNUM (0))))
	      stack_pointer = (STACK_LOC (4));
	    else
	      SAVE_CONT ();
	  }
	else
	  {
	    long To_Count;
	    SCHEME_OBJECT To_Location;
	    long i;

	    To_Count
	      = ((UNSIGNED_FIXNUM_TO_LONG (STACK_REF (TRANSLATE_TO_DISTANCE)))
		 - 1);
	    To_Location = (STACK_REF (TRANSLATE_TO_POINT));
	    for (i = 0; (i < To_Count); i += 1)
	      To_Location
		= (MEMORY_REF (To_Location, STATE_POINT_NEARER_POINT));
	    Thunk = (MEMORY_REF (To_Location, STATE_POINT_BEFORE_THUNK));
	    New_Location = To_Location;
	    (STACK_REF (TRANSLATE_TO_DISTANCE))
	      = (LONG_TO_UNSIGNED_FIXNUM (To_Count));
	    if (To_Count == 0)
	      stack_pointer = (STACK_LOC (4));
	    else
	      SAVE_CONT ();
	  }
	if (GET_EXP != SHARP_F)
	  {
	    MEMORY_SET (GET_EXP, STATE_SPACE_NEAREST_POINT, New_Location);
	  }
	else
	  current_state_point = New_Location;
	Will_Push (2);
	STACK_PUSH (Thunk);
	PUSH_APPLY_FRAME_HEADER (0);
	Pushed ();
	goto internal_apply;
      }

    case RC_INVOKE_STACK_THREAD:
      /* Used for WITH_THREADED_STACK primitive.  */
      Will_Push (3);
      PUSH_VAL ();		/* Value calculated by thunk.  */
      PUSH_EXP ();
      PUSH_APPLY_FRAME_HEADER (1);
      Pushed ();
      goto internal_apply;

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

    case RC_PCOMB1_APPLY:
      END_SUBPROBLEM ();
      PUSH_VAL ();		/* Argument value */
      Finished_Eventual_Pushing (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      SET_EXP (MEMORY_REF (GET_EXP, PCOMB1_FN_SLOT));

    primitive_internal_apply:

#ifdef COMPILE_STEPPER
      if (trapping
	  && (!WITHIN_CRITICAL_SECTION_P ())
	  && ((FETCH_APPLY_TRAPPER ()) != SHARP_F))
	{
	  Will_Push (3);
	  PUSH_EXP ();
	  STACK_PUSH (FETCH_APPLY_TRAPPER ());
	  PUSH_APPLY_FRAME_HEADER (1 + (PRIMITIVE_N_PARAMETERS (GET_EXP)));
	  Pushed ();
	  trapping = false;
	  goto Apply_Non_Trapping;
	}
#endif /* COMPILE_STEPPER */

      /* NOTE: This code must match the code in the TC_PRIMITIVE
	 case of internal_apply.
	 This code is simpler because:
	 1) The arity was checked at syntax time.
	 2) We don't have to deal with "lexpr" primitives.
	 3) We don't need to worry about unimplemented primitives because
	 unimplemented primitives will cause an error at invocation.  */
      {
	SCHEME_OBJECT primitive = GET_EXP;
	APPLY_PRIMITIVE_FROM_INTERPRETER (primitive);
	POP_PRIMITIVE_FRAME (PRIMITIVE_ARITY (primitive));
	break;
      }

    case RC_PCOMB2_APPLY:
      END_SUBPROBLEM ();
      PUSH_VAL ();		/* Value of arg. 1 */
      Finished_Eventual_Pushing (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      SET_EXP (MEMORY_REF (GET_EXP, PCOMB2_FN_SLOT));
      goto primitive_internal_apply;

    case RC_PCOMB2_DO_1:
      POP_ENV ();
      PUSH_VAL ();		/* Save value of arg. 2 */
      DO_ANOTHER_THEN (RC_PCOMB2_APPLY, PCOMB2_ARG_1_SLOT);

    case RC_PCOMB3_APPLY:
      END_SUBPROBLEM ();
      PUSH_VAL ();		/* Save value of arg. 1 */
      Finished_Eventual_Pushing (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      SET_EXP (MEMORY_REF (GET_EXP, PCOMB3_FN_SLOT));
      goto primitive_internal_apply;

    case RC_PCOMB3_DO_1:
      {
	SCHEME_OBJECT Temp = (STACK_POP ()); /* Value of arg. 3 */
	POP_ENV ();
	STACK_PUSH (Temp);	/* Save arg. 3 again */
	PUSH_VAL ();		/* Save arg. 2 */
	DO_ANOTHER_THEN (RC_PCOMB3_APPLY, PCOMB3_ARG_1_SLOT);
      }

    case RC_PCOMB3_DO_2:
      SET_ENV (STACK_REF (0));
      PUSH_VAL ();		/* Save value of arg. 3 */
      DO_ANOTHER_THEN (RC_PCOMB3_DO_1, PCOMB3_ARG_2_SLOT);

    case RC_POP_RETURN_ERROR:
    case RC_RESTORE_VALUE:
      SET_VAL (GET_EXP);
      break;

      /* The following two return codes are both used to restore a
	 saved history object.  The difference is that the first does
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
	    Will_Push (CONTINUATION_SIZE);
	    SET_EXP (GET_VAL);
	    SET_RC (RC_RESTORE_VALUE);
	    SAVE_CONT ();
	    Pushed ();
	    IMMEDIATE_GC (HEAP_AVAILABLE);
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
	  SET_RC (RC_RESTORE_VALUE);
	  SET_EXP (GET_VAL);
	  SAVE_CONT ();
	  SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());
	}
      break;

    case RC_STACK_MARKER:
      /* Frame consists of the return code followed by two objects.
	 The first object has already been popped into GET_EXP,
         so just pop the second argument.  */
      stack_pointer = (STACK_LOCATIVE_OFFSET (stack_pointer, 1));
      break;

    case RC_RESTORE_TO_STATE_POINT:
      {
	SCHEME_OBJECT Where_To_Go = GET_EXP;
	Will_Push (CONTINUATION_SIZE);
	/* Restore the contents of GET_VAL after moving to point */
	SET_EXP (GET_VAL);
	SET_RC (RC_RESTORE_VALUE);
	SAVE_CONT ();
	Pushed ();
	Translate_To_Point (Where_To_Go);
	break;			/* We never get here.... */
      }

    case RC_SEQ_2_DO_2:
      END_SUBPROBLEM ();
      POP_ENV ();
      REDUCES_TO_NTH (SEQUENCE_2);

    case RC_SEQ_3_DO_2:
      SET_ENV (STACK_REF (0));
      DO_ANOTHER_THEN (RC_SEQ_3_DO_3, SEQUENCE_2);

    case RC_SEQ_3_DO_3:
      END_SUBPROBLEM ();
      POP_ENV ();
      REDUCES_TO_NTH (SEQUENCE_3);

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
      POP_RETURN_ERROR (ERR_INAPPLICABLE_CONTINUATION);
    }
  goto pop_return;
}
