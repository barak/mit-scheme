/* -*-C-*-

$Id: interp.c,v 9.101 2004/11/18 18:14:06 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,2000,2001,2002,2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

/* The interpreter */

#include "scheme.h"
#include "locks.h"
#include "trap.h"
#include "lookup.h"
#include "winder.h"
#include "history.h"
#include "cmpint.h"
#include "zones.h"
#include "prmcon.h"

extern PTR EXFUN (obstack_chunk_alloc, (unsigned int size));
extern void EXFUN (free, (PTR ptr));
#define obstack_chunk_free free
extern void EXFUN (back_out_of_primitive_internal, (void));
extern void EXFUN (preserve_signal_mask, (void));
extern long EXFUN (enter_compiled_expression, (void));
extern long EXFUN (apply_compiled_procedure, (void));
extern long EXFUN (return_to_compiled_code, (void));

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
 * EVAL you first push a "return code" (using Save_Cont) on
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
  Setup_Interrupt (Masked_Code);					\
  goto perform_application;						\
}

#define PREPARE_POP_RETURN_INTERRUPT(Return_Code, Contents_of_Val)	\
{									\
  SCHEME_OBJECT temp = (Contents_of_Val);				\
  Store_Return (Return_Code);						\
  Save_Cont ();								\
  Store_Return (RC_RESTORE_VALUE);					\
  exp_register = temp;							\
  Save_Cont ();								\
}

#define PREPARE_APPLY_INTERRUPT()					\
{									\
  exp_register = SHARP_F;						\
  PREPARE_POP_RETURN_INTERRUPT						\
    (RC_INTERNAL_APPLY_VAL, (STACK_REF (STACK_ENV_FUNCTION)));		\
}

#define APPLICATION_ERROR(N)						\
{									\
  exp_register = SHARP_F;						\
  Store_Return (RC_INTERNAL_APPLY_VAL);					\
  val_register = (STACK_REF (STACK_ENV_FUNCTION));			\
  POP_RETURN_ERROR (N);							\
}

#define IMMEDIATE_GC(N)							\
{									\
  Request_GC (N);							\
  SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());				\
}

#define EVAL_GC_CHECK(Amount)						\
{									\
  if (GC_Check (Amount))						\
    {									\
      PREPARE_EVAL_REPEAT ();						\
      IMMEDIATE_GC (Amount);						\
    }									\
}

#define PREPARE_EVAL_REPEAT()						\
{									\
  Will_Push (CONTINUATION_SIZE + 1);					\
  STACK_PUSH (env_register);						\
  Store_Return (RC_EVAL_ERROR);						\
  Save_Cont ();								\
  Pushed ();								\
}

#define EVAL_ERROR(Err)							\
{									\
  Do_Micro_Error (Err, 0);						\
  goto internal_apply;							\
}

#define POP_RETURN_ERROR(Err)						\
{									\
  Do_Micro_Error (Err, 1);						\
  goto internal_apply;							\
}

#define BACK_OUT_AFTER_PRIMITIVE back_out_of_primitive_internal

#define REDUCES_TO(expression)						\
{									\
  exp_register = (expression);						\
  New_Reduction (exp_register, env_register);				\
  goto do_expression;							\
}

#define REDUCES_TO_NTH(n) REDUCES_TO (FAST_MEMORY_REF (exp_register, (n)))

#define DO_NTH_THEN(Return_Code, n)					\
{									\
  Store_Return (Return_Code);						\
  Save_Cont ();								\
  exp_register = (FAST_MEMORY_REF (exp_register, (n)));			\
  New_Subproblem (exp_register, env_register);				\
  goto do_expression;							\
}

#define PUSH_NTH_THEN(Return_Code, n)					\
{									\
  Store_Return (Return_Code);						\
  Save_Cont ();								\
  exp_register = (FAST_MEMORY_REF (exp_register, (n)));			\
  New_Subproblem (exp_register, env_register);				\
  Pushed ();								\
  goto do_expression;							\
}

#define DO_ANOTHER_THEN(Return_Code, N)					\
{									\
  Store_Return (Return_Code);						\
  Save_Cont ();								\
  exp_register = (FAST_MEMORY_REF (exp_register, (N)));			\
  Reuse_Subproblem (exp_register, env_register);			\
  goto do_expression;							\
}

#ifdef COMPILE_STEPPER

#define FETCH_EVAL_TRAPPER()						\
  (MEMORY_REF ((Get_Fixed_Obj_Slot (Stepper_State)), HUNK_CXR0))

#define FETCH_APPLY_TRAPPER()						\
  (MEMORY_REF ((Get_Fixed_Obj_Slot (Stepper_State)), HUNK_CXR1))

#define FETCH_RETURN_TRAPPER()						\
  (MEMORY_REF ((Get_Fixed_Obj_Slot (Stepper_State)), HUNK_CXR2))

#endif /* COMPILE_STEPPER */

/* Macros for handling FUTUREs */

#ifdef COMPILE_FUTURES

/* ARG_TYPE_ERROR handles the error returns from primitives which type
   check their arguments and restarts them or suspends if the argument
   is a future.  */

#define ARG_TYPE_ERROR(Arg_No, Err_No)					\
{									\
  SCHEME_OBJECT * Arg							\
    = (& (STACK_REF ((Arg_No - 1) + STACK_ENV_FIRST_ARG)));		\
  SCHEME_OBJECT Orig_Arg = (*Arg);					\
  if (OBJECT_TYPE (*Arg) != TC_FUTURE)					\
    POP_RETURN_ERROR (Err_No);						\
  while (((OBJECT_TYPE (*Arg)) == TC_FUTURE)				\
	 && (Future_Has_Value (*Arg)))					\
    {									\
      if (Future_Is_Keep_Slot (*Arg))					\
	Log_Touch_Of_Future (*Arg);					\
      (*Arg) = Future_Value (*Arg);					\
    }									\
  if ((OBJECT_TYPE (*Arg)) != TC_FUTURE)				\
    goto Apply_Non_Trapping;						\
  TOUCH_SETUP (*Arg);							\
  (*Arg) = Orig_Arg;							\
  goto Apply_Non_Trapping;						\
}

/* APPLY_FUTURE_CHECK is called at apply time to guarantee that
   certain objects (the procedure itself, and its LAMBDA components
   for user defined procedures) are not futures.  */

#define APPLY_FUTURE_CHECK(Name, Object)				\
{									\
  SCHEME_OBJECT * Arg = (& (Object));					\
  SCHEME_OBJECT Orig_Answer = (*Arg);					\
  while ((OBJECT_TYPE (*Arg)) == TC_FUTURE)				\
    {									\
      if (Future_Has_Value (*Arg))					\
	{								\
	  if (Future_Is_Keep_Slot (*Arg))				\
	    Log_Touch_Of_Future (*Arg);					\
	  (*Arg) = (Future_Value (*Arg));				\
	}								\
      else								\
	{								\
	  PREPARE_APPLY_INTERRUPT ();					\
	  TOUCH_SETUP (*Arg);						\
	  (*Arg) = Orig_Answer;						\
	  goto internal_apply;						\
	}								\
    }									\
  Name = (*Arg);							\
}

/* POP_RETURN_VAL_CHECK suspends the process if the value calculated
   by a recursive call to EVAL is an undetermined future.  */

#define POP_RETURN_VAL_CHECK()						\
{									\
  SCHEME_OBJECT Orig_Val = val_register;				\
  while ((OBJECT_TYPE (val_register)) == TC_FUTURE)			\
    {									\
      if (Future_Has_Value (val_register))				\
	{								\
	  if (Future_Is_Keep_Slot (val_register))			\
	    Log_Touch_Of_Future (val_register);				\
	  val_register = (Future_Value (val_register));			\
	}								\
      else								\
	{								\
	  Save_Cont ();							\
	  Will_Push (CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS + 2));	\
	  Store_Return (RC_RESTORE_VALUE);				\
	  exp_register = Orig_Val;					\
	  Save_Cont ();							\
	  STACK_PUSH (val_register);					\
	  STACK_PUSH (Get_Fixed_Obj_Slot (System_Scheduler));		\
	  STACK_PUSH (STACK_FRAME_HEADER + 1);				\
	  Pushed ();							\
	  goto internal_apply;						\
	}								\
    }									\
}

/* This saves stuff unnecessarily in most cases.
   For example, when dispatch_code is PRIM_APPLY, val_register,
   env_register, exp_register, and ret_register are undefined.  */

#define LOG_FUTURES()							\
{									\
  if (Must_Report_References ())					\
    {									\
      Save_Cont ();							\
      Will_Push (CONTINUATION_SIZE + 2);				\
      STACK_PUSH (val_register);					\
      STACK_PUSH (env_register);					\
      Store_Return (RC_REPEAT_DISPATCH);				\
      exp_register = (LONG_TO_FIXNUM (CODE_MAP (dispatch_code)));	\
      Save_Cont ();							\
      Pushed ();							\
      Call_Future_Logging ();						\
    }									\
}

#else /* not COMPILE_FUTURES */

#define POP_RETURN_VAL_CHECK()
#define APPLY_FUTURE_CHECK(Name, Object) Name = (Object)
#define ARG_TYPE_ERROR(Arg_No, Err_No) POP_RETURN_ERROR (Err_No)
#define LOG_FUTURES()

#endif /* not COMPILE_FUTURES */

/* Notes on repeat_dispatch:

   The codes used (values of dispatch_code) are divided into two
   groups: those for which the primitive has already backed out, and
   those for which the back out code has not yet been executed, and is
   therefore executed below.

   Under most circumstances the distinction is moot, but if there are
   futures in the system, and future touches must be logged, the code
   must be set up to "interrupt" the dispatch, and proceed it later.
   The primitive back out code must be done before the furure is
   logged, so all of these codes are split into two versions: one set
   before doing the back out, and another afterwards.  */

/* This is assumed to be larger (in absolute value) than any
   PRIM_<mumble> and ERR_<mumble>.  */
#define PRIM_BIAS_AMOUNT 1000

#if (MAX_ERROR >= PRIM_BIAS_AMOUNT)
#  include "Inconsistency: errors.h and interp.c"
#endif

#define CODE_MAP(code)							\
  (((code) < 0)								\
   ? ((code) - PRIM_BIAS_AMOUNT)					\
   : ((code) + PRIM_BIAS_AMOUNT))

#define CODE_UNMAP(code)						\
  (((code) < 0)								\
   ? ((code) + PRIM_BIAS_AMOUNT)					\
   : ((code) - PRIM_BIAS_AMOUNT))

#define CODE_MAPPED_P(code)						\
  (((code) < (-PRIM_BIAS_AMOUNT))					\
   || ((code) >= PRIM_BIAS_AMOUNT))

#define PROCEED_AFTER_PRIMITIVE()					\
{									\
  (Registers[REGBLOCK_PRIMITIVE]) = SHARP_F;				\
  LOG_FUTURES ();							\
}

/* The EVAL/APPLY yin/yang */

interpreter_state_t interpreter_state = NULL_INTERPRETER_STATE;

void
DEFUN (bind_interpreter_state, (s), interpreter_state_t s)
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
DEFUN (unbind_interpreter_state, (s), interpreter_state_t s)
{
  interpreter_state = s;
  {
    long old_mask = (FETCH_INTERRUPT_MASK ());
    SET_INTERRUPT_MASK (0);
    dstack_set_position (s -> dstack_position);
    SET_INTERRUPT_MASK (old_mask);
  }
  interpreter_state = (s -> previous_state);
}

void
DEFUN (abort_to_interpreter, (argument), int argument)
{
  if (interpreter_state == NULL_INTERPRETER_STATE)
  {
    outf_fatal ("abort_to_interpreter: Interpreter not set up.\n");
    termination_init_error ();
  }
  
  interpreter_throw_argument = argument;
  {
    long old_mask = (FETCH_INTERRUPT_MASK ());
    SET_INTERRUPT_MASK (0);
    dstack_set_position (interpreter_catch_dstack_position);
    SET_INTERRUPT_MASK (old_mask);
  }
  obstack_free ((&scratch_obstack), 0);
  obstack_init (&scratch_obstack);
  longjmp (interpreter_catch_env, argument);
}

int
DEFUN_VOID (abort_to_interpreter_argument)
{
  return (interpreter_throw_argument);
}

void
DEFUN (Interpret, (pop_return_p), int pop_return_p)
{
  long dispatch_code;
  struct interpreter_state_s new_state;

  /* Primitives jump back here for errors, requests to evaluate an
     expression, apply a function, or handle an interrupt request.  On
     errors or interrupts they leave their arguments on the stack, the
     primitive itself in exp_register.  The code should do a primitive
     backout in these cases, but not in others (apply, eval, etc.),
     since the primitive itself will have left the state of the
     interpreter ready for operation.  */

  bind_interpreter_state (&new_state);
  dispatch_code = (setjmp (interpreter_catch_env));
  preserve_signal_mask ();
  Set_Time_Zone (Zone_Working);

 repeat_dispatch:
  switch (dispatch_code)
    {
    case PRIM_APPLY:
      PROCEED_AFTER_PRIMITIVE ();
    case CODE_MAP (PRIM_APPLY):
      goto internal_apply;

    case PRIM_NO_TRAP_APPLY:
      PROCEED_AFTER_PRIMITIVE ();
    case CODE_MAP (PRIM_NO_TRAP_APPLY):
      goto Apply_Non_Trapping;

    case PRIM_DO_EXPRESSION:
      val_register = exp_register;
      PROCEED_AFTER_PRIMITIVE ();
    case CODE_MAP (PRIM_DO_EXPRESSION):
      REDUCES_TO (val_register);

    case PRIM_NO_TRAP_EVAL:
      val_register = exp_register;
      PROCEED_AFTER_PRIMITIVE ();
    case CODE_MAP (PRIM_NO_TRAP_EVAL):
      New_Reduction (val_register, env_register);
      goto eval_non_trapping;

    case 0:			/* first time */
      if (pop_return_p)
	goto pop_return;
      else
	break;			/* fall into eval */

    case PRIM_POP_RETURN:
      PROCEED_AFTER_PRIMITIVE ();
    case CODE_MAP (PRIM_POP_RETURN):
      goto pop_return;

    case PRIM_NO_TRAP_POP_RETURN:
      PROCEED_AFTER_PRIMITIVE ();
    case CODE_MAP (PRIM_NO_TRAP_POP_RETURN):
      goto pop_return_non_trapping;

    case PRIM_REENTER:
      BACK_OUT_AFTER_PRIMITIVE ();
      LOG_FUTURES ();
    case CODE_MAP (PRIM_REENTER):
      goto perform_application;

    case PRIM_TOUCH:
      {
	SCHEME_OBJECT temp = val_register;
	BACK_OUT_AFTER_PRIMITIVE ();
	val_register = temp;
	LOG_FUTURES ();
      }
      /* fall through */
    case CODE_MAP (PRIM_TOUCH):
      TOUCH_SETUP (val_register);
      goto internal_apply;

    case PRIM_INTERRUPT:
      BACK_OUT_AFTER_PRIMITIVE ();
      LOG_FUTURES ();
      /* fall through */
    case CODE_MAP (PRIM_INTERRUPT):
      Save_Cont ();
      SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());

    case ERR_ARG_1_WRONG_TYPE:
      BACK_OUT_AFTER_PRIMITIVE ();
      LOG_FUTURES ();
      /* fall through */
    case CODE_MAP (ERR_ARG_1_WRONG_TYPE):
      ARG_TYPE_ERROR (1, ERR_ARG_1_WRONG_TYPE);

    case ERR_ARG_2_WRONG_TYPE:
      BACK_OUT_AFTER_PRIMITIVE ();
      LOG_FUTURES ();
      /* fall through */
    case CODE_MAP (ERR_ARG_2_WRONG_TYPE):
      ARG_TYPE_ERROR (2, ERR_ARG_2_WRONG_TYPE);

    case ERR_ARG_3_WRONG_TYPE:
      BACK_OUT_AFTER_PRIMITIVE ();
      LOG_FUTURES ();
      /* fall through */
    case CODE_MAP (ERR_ARG_3_WRONG_TYPE):
      ARG_TYPE_ERROR (3, ERR_ARG_3_WRONG_TYPE);

    default:
      {
	if (!CODE_MAPPED_P (dispatch_code))
	  {
	    BACK_OUT_AFTER_PRIMITIVE ();
	    LOG_FUTURES ();
	  }
	else
	  dispatch_code = (CODE_UNMAP (dispatch_code));
	POP_RETURN_ERROR (dispatch_code);
      }
    }

 do_expression:

#if 0
  if (Eval_Debug)
    {
      Print_Expression (exp_register, "Eval, expression");
      outf_console ("\n");
    }
#endif

  /* exp_register has an Scode item in it that should be evaluated and
     the result left in val_register.

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
     at do_expression with the new expression in exp_register.

     Finally, an operation can terminate with a DO_NTH_THEN macro.
     This indicates that another expression must be evaluated and them
     some additional processing will be performed before the value of
     this S-Code item available.  Thus a new continuation is created
     and placed on the stack (using Save_Cont), the new expression is
     placed in the exp_register, and processing continues at
     do_expression.  */

  /* Handling of Eval Trapping.
     
     If we are handling traps and there is an Eval Trap set, turn off
     all trapping and then go to internal_apply to call the user
     supplied eval hook with the expression to be evaluated and the
     environment.  */

#ifdef COMPILE_STEPPER
  if (Trapping
      && (!WITHIN_CRITICAL_SECTION_P ())
      && ((FETCH_EVAL_TRAPPER ()) != SHARP_F))
    {
      Stop_Trapping ();
      Will_Push (4);
      STACK_PUSH (env_register);
      STACK_PUSH (exp_register);
      STACK_PUSH (FETCH_EVAL_TRAPPER ());
      STACK_PUSH (STACK_FRAME_HEADER + 2);
      Pushed ();
      goto Apply_Non_Trapping;
    }
#endif /* COMPILE_STEPPER */

 eval_non_trapping:
  Eval_Ucode_Hook ();
  switch (OBJECT_TYPE (exp_register))
    {
    default:
#if 0
      EVAL_ERROR (ERR_UNDEFINED_USER_TYPE);
#else
      /* fall through to self evaluating. */
#endif

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
      val_register = exp_register;
      break;

    case TC_ACCESS:
      Will_Push (CONTINUATION_SIZE);
      PUSH_NTH_THEN (RC_EXECUTE_ACCESS_FINISH, ACCESS_ENVIRONMENT);

    case TC_ASSIGNMENT:
      Will_Push (CONTINUATION_SIZE + 1);
      STACK_PUSH (env_register);
      PUSH_NTH_THEN (RC_EXECUTE_ASSIGNMENT_FINISH, ASSIGN_VALUE);

    case TC_BROKEN_HEART:
      Microcode_Termination (TERM_BROKEN_HEART);

    case TC_COMBINATION:
      {
	long length = ((VECTOR_LENGTH (exp_register)) - 1);
#ifdef USE_STACKLETS
	/* Finger */
        EVAL_GC_CHECK (New_Stacklet_Size (length + 2 + CONTINUATION_SIZE));
#endif /* USE_STACKLETS */
	Will_Push (length + 2 + CONTINUATION_SIZE);
	sp_register = (STACK_LOC (-length));
        STACK_PUSH (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, length));
	/* The finger: last argument number */
	Pushed ();
        if (length == 0)
	  {
	    STACK_PUSH (STACK_FRAME_HEADER); /* Frame size */
	    DO_NTH_THEN (RC_COMB_APPLY_FUNCTION, COMB_FN_SLOT);
	  }
	STACK_PUSH (env_register);
	DO_NTH_THEN (RC_COMB_SAVE_VALUE, (length + 1));
      }

    case TC_COMBINATION_1:
      Will_Eventually_Push (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 1);
      STACK_PUSH (env_register);
      DO_NTH_THEN (RC_COMB_1_PROCEDURE, COMB_1_ARG_1);

    case TC_COMBINATION_2:
      Will_Eventually_Push (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 2);
      STACK_PUSH (env_register);
      DO_NTH_THEN (RC_COMB_2_FIRST_OPERAND, COMB_2_ARG_2);

    case TC_COMMENT:
      REDUCES_TO_NTH (COMMENT_EXPRESSION);

    case TC_CONDITIONAL:
      Will_Push (CONTINUATION_SIZE + 1);
      STACK_PUSH (env_register);
      PUSH_NTH_THEN (RC_CONDITIONAL_DECIDE, COND_PREDICATE);

    case TC_COMPILED_ENTRY:
      {
	SCHEME_OBJECT compiled_expression = exp_register;
	execute_compiled_setup ();
	exp_register = compiled_expression;
	dispatch_code = (enter_compiled_expression ());
	goto return_from_compiled_code;
      }

    case TC_DEFINITION:
      Will_Push (CONTINUATION_SIZE + 1);
      STACK_PUSH (env_register);
      PUSH_NTH_THEN (RC_EXECUTE_DEFINITION_FINISH, DEFINE_VALUE);

    case TC_DELAY:
      /* Deliberately omitted: EVAL_GC_CHECK (2); */
      val_register = (MAKE_POINTER_OBJECT (TC_DELAYED, Free));
      (Free[THUNK_ENVIRONMENT]) = env_register;
      (Free[THUNK_PROCEDURE]) = (FAST_MEMORY_REF (exp_register, DELAY_OBJECT));
      Free += 2;
      break;

    case TC_DISJUNCTION:
      Will_Push (CONTINUATION_SIZE + 1);
      STACK_PUSH (env_register);
      PUSH_NTH_THEN (RC_DISJUNCTION_DECIDE, OR_PREDICATE);

    case TC_EXTENDED_LAMBDA:
      /* Deliberately omitted: EVAL_GC_CHECK (2); */
      val_register = (MAKE_POINTER_OBJECT (TC_EXTENDED_PROCEDURE, Free));
      (Free[PROCEDURE_LAMBDA_EXPR]) = exp_register;
      (Free[PROCEDURE_ENVIRONMENT]) = env_register;
      Free += 2;
      break;

#ifdef COMPILE_FUTURES
    case TC_FUTURE:
      if (Future_Has_Value (exp_register))
	{
	  SCHEME_OBJECT Future = exp_register;
	  if (Future_Is_Keep_Slot (Future))
	    Log_Touch_Of_Future (Future);
	  REDUCES_TO_NTH (FUTURE_VALUE);
	}
      PREPARE_EVAL_REPEAT ();
      Will_Push (STACK_ENV_EXTRA_SLOTS+2);
      STACK_PUSH (exp_register); /* Arg: FUTURE object */
      STACK_PUSH (Get_Fixed_Obj_Slot (System_Scheduler));
      STACK_PUSH (STACK_FRAME_HEADER+1);
      Pushed ();
      goto internal_apply;
#endif

    case TC_IN_PACKAGE:
      Will_Push (CONTINUATION_SIZE);
      PUSH_NTH_THEN (RC_EXECUTE_IN_PACKAGE_CONTINUE, IN_PACKAGE_ENVIRONMENT);

    case TC_LAMBDA:
    case TC_LEXPR:
      /* Deliberately omitted: EVAL_GC_CHECK (2); */
      val_register = (MAKE_POINTER_OBJECT (TC_PROCEDURE, Free));
      (Free[PROCEDURE_LAMBDA_EXPR]) = exp_register;
      (Free[PROCEDURE_ENVIRONMENT]) = env_register;
      Free += 2;
      break;

    case TC_MANIFEST_NM_VECTOR:
    case TC_MANIFEST_SPECIAL_NM_VECTOR:
      EVAL_ERROR (ERR_EXECUTE_MANIFEST_VECTOR);

    case TC_PCOMB0:
      /* The argument to Will_Eventually_Push is determined by how
	 much will be on the stack if we back out of the primitive.  */
      Will_Eventually_Push (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      Finished_Eventual_Pushing (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      exp_register = (OBJECT_NEW_TYPE (TC_PRIMITIVE, exp_register));
      goto primitive_internal_apply;

    case TC_PCOMB1:
      Will_Eventually_Push (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 1);
      DO_NTH_THEN (RC_PCOMB1_APPLY, PCOMB1_ARG_SLOT);

    case TC_PCOMB2:
      Will_Eventually_Push (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 2);
      STACK_PUSH (env_register);
      DO_NTH_THEN (RC_PCOMB2_DO_1, PCOMB2_ARG_2_SLOT);

    case TC_PCOMB3:
      Will_Eventually_Push (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 3);
      STACK_PUSH (env_register);
      DO_NTH_THEN (RC_PCOMB3_DO_2, PCOMB3_ARG_3_SLOT);

    case TC_SCODE_QUOTE:
      val_register = (FAST_MEMORY_REF (exp_register, SCODE_QUOTE_OBJECT));
      break;

    case TC_SEQUENCE_2:
      Will_Push (CONTINUATION_SIZE + 1);
      STACK_PUSH (env_register);
      PUSH_NTH_THEN (RC_SEQ_2_DO_2, SEQUENCE_1);

    case TC_SEQUENCE_3:
      Will_Push (CONTINUATION_SIZE + 1);
      STACK_PUSH (env_register);
      PUSH_NTH_THEN (RC_SEQ_3_DO_2, SEQUENCE_1);

    case TC_THE_ENVIRONMENT:
      val_register = env_register;
      break;

    case TC_VARIABLE:
      {
	long temp;

	Set_Time_Zone (Zone_Lookup);
	temp = (lookup_variable (env_register, exp_register, (&val_register)));
	if (temp == PRIM_DONE)
	  goto pop_return;

	/* Back out of the evaluation. */

	Set_Time_Zone (Zone_Working);
	if (temp == PRIM_INTERRUPT)
	  {
	    PREPARE_EVAL_REPEAT ();
	    SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());
	  }
	EVAL_ERROR (temp);
      }

      SITE_EXPRESSION_DISPATCH_HOOK ();
    }

  /* Now restore the continuation saved during an earlier part of the
     EVAL cycle and continue as directed.  */

 pop_return:

#ifdef COMPILE_STEPPER
  if (Trapping
      && (!WITHIN_CRITICAL_SECTION_P ())
      && ((FETCH_RETURN_TRAPPER ()) != SHARP_F))
    {
      Will_Push (3);
      Stop_Trapping ();
      STACK_PUSH (val_register);
      STACK_PUSH (FETCH_RETURN_TRAPPER ());
      STACK_PUSH (STACK_FRAME_HEADER + 1);
      Pushed ();
      goto Apply_Non_Trapping;
    }
#endif /* COMPILE_STEPPER */

 pop_return_non_trapping:
  Pop_Return_Ucode_Hook ();
  Restore_Cont ();
  if (Consistency_Check && ((OBJECT_TYPE (ret_register)) != TC_RETURN_CODE))
    {
      STACK_PUSH (val_register); /* For possible stack trace */
      Save_Cont ();
      Microcode_Termination (TERM_BAD_STACK);
    }
#if 0
  if (Eval_Debug)
    {
      Print_Return ("pop_return, return code");
      Print_Expression (val_register, "pop_return, value");
      outf_console ("\n");
    }
#endif

  /* Dispatch on the return code.  A BREAK here will cause
     a "goto pop_return" to occur, since this is the most
     common occurrence.
   */

  switch (OBJECT_DATUM (ret_register))
    {
    case RC_COMB_1_PROCEDURE:
      env_register = (STACK_POP ());
      STACK_PUSH (val_register); /* Arg. 1 */
      STACK_PUSH (SHARP_F);	/* Operator */
      STACK_PUSH (STACK_FRAME_HEADER + 1);
      Finished_Eventual_Pushing (CONTINUATION_SIZE);
      DO_ANOTHER_THEN (RC_COMB_APPLY_FUNCTION, COMB_1_FN);

    case RC_COMB_2_FIRST_OPERAND:
      env_register = (STACK_POP ());
      STACK_PUSH (val_register);
      STACK_PUSH (env_register);
      DO_ANOTHER_THEN (RC_COMB_2_PROCEDURE, COMB_2_ARG_1);

    case RC_COMB_2_PROCEDURE:
      env_register = (STACK_POP ());
      STACK_PUSH (val_register); /* Arg 1, just calculated */
      STACK_PUSH (SHARP_F);	/* Function */
      STACK_PUSH (STACK_FRAME_HEADER + 2);
      Finished_Eventual_Pushing (CONTINUATION_SIZE);
      DO_ANOTHER_THEN (RC_COMB_APPLY_FUNCTION, COMB_2_FN);

    case RC_COMB_APPLY_FUNCTION:
      End_Subproblem ();
      goto internal_apply_val;

    case RC_COMB_SAVE_VALUE:
      {
	long Arg_Number;

	env_register = (STACK_POP ());
	Arg_Number = ((OBJECT_DATUM (STACK_REF (STACK_COMB_FINGER))) - 1);
	(STACK_REF (STACK_COMB_FIRST_ARG + Arg_Number)) = val_register;
	(STACK_REF (STACK_COMB_FINGER))
	  = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, Arg_Number));
	/* DO NOT count on the type code being NMVector here, since
	   the stack parser may create them with #F here! */
	if (Arg_Number > 0)
	  {
	    STACK_PUSH (env_register);
	    DO_ANOTHER_THEN
	      (RC_COMB_SAVE_VALUE, ((COMB_ARG_1_SLOT - 1) + Arg_Number));
	  }
	/* Frame Size */
	STACK_PUSH (FAST_MEMORY_REF (exp_register, 0));
	DO_ANOTHER_THEN (RC_COMB_APPLY_FUNCTION, COMB_FN_SLOT);
      }

#define DEFINE_COMPILER_RESTART(return_code, entry)			\
    case return_code:							\
      {									\
	extern long EXFUN (entry, (void));				\
	compiled_code_restart ();					\
	dispatch_code = (entry ());					\
	goto return_from_compiled_code;					\
      }

      DEFINE_COMPILER_RESTART
	(RC_COMP_INTERRUPT_RESTART, comp_interrupt_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_LOOKUP_APPLY_RESTART, comp_lookup_apply_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_REFERENCE_RESTART, comp_reference_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_ACCESS_RESTART, comp_access_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_UNASSIGNED_P_RESTART, comp_unassigned_p_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_UNBOUND_P_RESTART, comp_unbound_p_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_ASSIGNMENT_RESTART, comp_assignment_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_DEFINITION_RESTART, comp_definition_restart);

      DEFINE_COMPILER_RESTART
	(RC_COMP_SAFE_REFERENCE_RESTART, comp_safe_reference_restart);

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
      compiled_code_restart ();
      dispatch_code = (return_to_compiled_code ());
      goto return_from_compiled_code;

    case RC_CONDITIONAL_DECIDE:
      POP_RETURN_VAL_CHECK ();
      End_Subproblem ();
      env_register = (STACK_POP ());
      REDUCES_TO_NTH
	((val_register == SHARP_F) ? COND_ALTERNATIVE : COND_CONSEQUENT);

    case RC_DISJUNCTION_DECIDE:
      /* Return predicate if it isn't #F; else do ALTERNATIVE */
      POP_RETURN_VAL_CHECK ();
      End_Subproblem ();
      env_register = (STACK_POP ());
      if (val_register != SHARP_F)
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
      env_register = (STACK_POP ());
      REDUCES_TO (exp_register);

    case RC_EXECUTE_ACCESS_FINISH:
      {
	long Result;
	SCHEME_OBJECT value;

	POP_RETURN_VAL_CHECK ();
	value = val_register;
	if (ENVIRONMENT_P (val_register))
	  {
	    Result
	      = (lookup_variable
		 (value,
		  (FAST_MEMORY_REF (exp_register, ACCESS_NAME)),
		  (&val_register)));
	    if (Result == PRIM_DONE)
	      {
		End_Subproblem ();
		break;
	      }
	    if (Result != PRIM_INTERRUPT)
	      {
		val_register = value;
		POP_RETURN_ERROR (Result);
	      }
	    PREPARE_POP_RETURN_INTERRUPT (RC_EXECUTE_ACCESS_FINISH, value);
	    SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());
	  }
	val_register = value;
	POP_RETURN_ERROR (ERR_BAD_FRAME);
      }

    case RC_EXECUTE_ASSIGNMENT_FINISH:
      {
	long temp;
	SCHEME_OBJECT value;
#ifdef DECLARE_LOCK
	DECLARE_LOCK (set_serializer);
#endif

	value = val_register;
	Set_Time_Zone (Zone_Lookup);
	env_register = (STACK_POP ());
	temp
	  = (assign_variable
	     (env_register,
	      (MEMORY_REF (exp_register, ASSIGN_NAME)),
	      value,
	      (&val_register)));
	if (temp == PRIM_DONE)
	  {
	    End_Subproblem ();
	    Set_Time_Zone (Zone_Working);
	    break;
	  }
	Set_Time_Zone (Zone_Working);
	STACK_PUSH (env_register);
	if (temp != PRIM_INTERRUPT)
	  {
	    val_register = value;
	    POP_RETURN_ERROR (temp);
	  }
	PREPARE_POP_RETURN_INTERRUPT (RC_EXECUTE_ASSIGNMENT_FINISH, value);
	SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());
      }

    case RC_EXECUTE_DEFINITION_FINISH:
      {
	SCHEME_OBJECT name = (FAST_MEMORY_REF (exp_register, DEFINE_NAME));
	SCHEME_OBJECT value = val_register;
        long result;

        env_register = (STACK_POP ());
        result = (define_variable (env_register, name, value));
        if (result == PRIM_DONE)
	  {
	    End_Subproblem ();
	    val_register = name;
	    break;
	  }
	STACK_PUSH (env_register);
	if (result == PRIM_INTERRUPT)
	  {
	    PREPARE_POP_RETURN_INTERRUPT (RC_EXECUTE_DEFINITION_FINISH,
					  value);
	    SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());
	  }
	val_register = value;
        POP_RETURN_ERROR (result);
      }

    case RC_EXECUTE_IN_PACKAGE_CONTINUE:
      POP_RETURN_VAL_CHECK ();
      if (ENVIRONMENT_P (val_register))
	{
	  End_Subproblem ();
	  env_register = val_register;
	  REDUCES_TO_NTH (IN_PACKAGE_EXPRESSION);
	}
      POP_RETURN_ERROR (ERR_BAD_FRAME);

#ifdef COMPILE_FUTURES
    case RC_FINISH_GLOBAL_INT:
      val_register = (Global_Int_Part_2 (exp_register, val_register));
      break;
#endif

    case RC_HALT:
      Microcode_Termination (TERM_TERM_HANDLER);

    case RC_HARDWARE_TRAP:
      {
	/* This just reinvokes the handler */
	SCHEME_OBJECT info = (STACK_REF (0));
	SCHEME_OBJECT handler = SHARP_F;
	Save_Cont ();
	if (Valid_Fixed_Obj_Vector ())
	  handler = (Get_Fixed_Obj_Slot (Trap_Handler));
	if (handler == SHARP_F)
	  {
	    outf_fatal ("There is no trap handler for recovery!\n");
	    termination_trap ();
	    /*NOTREACHED*/
	  }
	Will_Push (STACK_ENV_EXTRA_SLOTS + 2);
	STACK_PUSH (info);
	STACK_PUSH (handler);
	STACK_PUSH (STACK_FRAME_HEADER + 1);
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

      STACK_REF (STACK_ENV_FUNCTION) = val_register;

    case RC_INTERNAL_APPLY:
    internal_apply:

#ifdef COMPILE_STEPPER
      if (Trapping
	  && (!WITHIN_CRITICAL_SECTION_P ())
	  && ((FETCH_APPLY_TRAPPER ()) != SHARP_F))
	{
	  long Count = (OBJECT_DATUM (STACK_REF (STACK_ENV_HEADER)));
	  (* (STACK_LOC (0))) = (FETCH_APPLY_TRAPPER ());
	  STACK_PUSH (STACK_FRAME_HEADER + Count);
	  Stop_Trapping ();
	}
#endif /* COMPILE_STEPPER */

    Apply_Non_Trapping:
      if ((PENDING_INTERRUPTS ()) != 0)
	{
	  long interrupts = (PENDING_INTERRUPTS ());
	  PREPARE_APPLY_INTERRUPT ();
	  SIGNAL_INTERRUPT (interrupts);
	}

    perform_application:
      Apply_Ucode_Hook ();
      {
	SCHEME_OBJECT Function;
	SCHEME_OBJECT orig_proc;

	APPLY_FUTURE_CHECK (Function, (STACK_REF (STACK_ENV_FUNCTION)));
	orig_proc = Function;

      apply_dispatch:
	switch (OBJECT_TYPE (Function))
	  {
	  case TC_ENTITY:
	    {
	      long nargs = (STACK_POP ());
	      long nactuals = (OBJECT_DATUM (nargs));
	      SCHEME_OBJECT data = (MEMORY_REF (Function, ENTITY_DATA));

	      /* Will_Pushed omitted since frame must be contiguous.
		 combination code must ensure one more slot.  */

	      /* This code assumes that adding 1 to nactuals takes care
		 of everything, including type code, etc.  */

	      if ((VECTOR_P (data))
		  && (nactuals < ((long) (VECTOR_LENGTH (data))))
		  && ((VECTOR_REF (data, nactuals)) != SHARP_F)
		  && ((VECTOR_REF (data, 0))
		      == (Get_Fixed_Obj_Slot (ARITY_DISPATCHER_TAG))))
		{
		  SCHEME_OBJECT nproc = (VECTOR_REF (data, nactuals));
		  if ((Function == orig_proc) && (nproc != Function))
		    {
		      Function = nproc;
		      STACK_PUSH (nargs);
		      STACK_REF (STACK_ENV_FUNCTION) = nproc;
		      goto apply_dispatch;
		    }
		  else
		    {
		      Function = orig_proc;
		      STACK_REF (STACK_ENV_FUNCTION - 1) = orig_proc;
		    }
		}
	    
	      STACK_PUSH (FAST_MEMORY_REF (Function, ENTITY_OPERATOR));
	      STACK_PUSH (nargs + 1);
	      /* This must be done to prevent an infinite push loop by
		 an entity whose handler is the entity itself or some
		 other such loop.  Of course, it will die if stack overflow
		 interrupts are disabled.  */
	      Stack_Check (sp_register);
	      goto internal_apply;
	    }

	  case TC_PROCEDURE:
	    {
	      long nargs = (OBJECT_DATUM (STACK_POP ()));
	      Function = (FAST_MEMORY_REF (Function, PROCEDURE_LAMBDA_EXPR));
	      {
		SCHEME_OBJECT formals;

		APPLY_FUTURE_CHECK
		  (formals, (FAST_MEMORY_REF (Function, LAMBDA_FORMALS)));
		if ((nargs != ((long) (VECTOR_LENGTH (formals))))
		    && (((OBJECT_TYPE (Function)) != TC_LEXPR)
			|| (nargs < ((long) (VECTOR_LENGTH (formals))))))
		  {
		    STACK_PUSH (STACK_FRAME_HEADER + nargs - 1);
		    APPLICATION_ERROR (ERR_WRONG_NUMBER_OF_ARGUMENTS);
		  }
	      }
#if 0
	      if (Eval_Debug)
		{
		  Print_Expression
		    ((LONG_TO_UNSIGNED_FIXNUM (nargs)),
		     "APPLY: Number of arguments");
		  outf_console ("\n");
		}
#endif
	      if (GC_Check (nargs + 1))
		{
		  STACK_PUSH (STACK_FRAME_HEADER + nargs - 1);
		  PREPARE_APPLY_INTERRUPT ();
		  IMMEDIATE_GC (nargs + 1);
		}
	      {
		SCHEME_OBJECT * scan = Free;
		SCHEME_OBJECT temp
		  = (MAKE_POINTER_OBJECT (TC_ENVIRONMENT, scan));
		(*scan++) = (MAKE_OBJECT (TC_MANIFEST_VECTOR, nargs));
		while ((--nargs) >= 0)
		  (*scan++) = (STACK_POP ());
		Free = scan;
		env_register = temp;
		REDUCES_TO (FAST_MEMORY_REF (Function, LAMBDA_SCODE));
	      }
	    }

	  case TC_CONTROL_POINT:
	    if ((OBJECT_DATUM (STACK_REF (STACK_ENV_HEADER)))
		!= STACK_ENV_FIRST_ARG)
	      APPLICATION_ERROR (ERR_WRONG_NUMBER_OF_ARGUMENTS);
	    val_register = (STACK_REF (STACK_ENV_FIRST_ARG));
	    Our_Throw (0, Function);
	    Apply_Stacklet_Backout ();
	    Our_Throw_Part_2();
	    goto pop_return;

	    /* After checking the number of arguments, remove the
	       frame header since primitives do not expect it.
	      
	       NOTE: This code must match the application code which
	       follows primitive_internal_apply.  */

	  case TC_PRIMITIVE:
	    {
	      long nargs;

	      if (!IMPLEMENTED_PRIMITIVE_P (Function))
		APPLICATION_ERROR (ERR_UNIMPLEMENTED_PRIMITIVE);

	      /* Note that the first test below will fail for lexpr
		 primitives.  */

	      nargs
		= ((OBJECT_DATUM (STACK_REF (STACK_ENV_HEADER)))
		   - (STACK_ENV_FIRST_ARG - 1));
	      if (nargs != (PRIMITIVE_ARITY (Function)))
		{
		  if ((PRIMITIVE_ARITY (Function)) != LEXPR_PRIMITIVE_ARITY)
		    APPLICATION_ERROR (ERR_WRONG_NUMBER_OF_ARGUMENTS);
		  (Registers[REGBLOCK_LEXPR_ACTUALS])
		    = ((SCHEME_OBJECT) nargs);
		}
	      sp_register = (STACK_LOC (STACK_ENV_FIRST_ARG));
	      exp_register = Function;
	      APPLY_PRIMITIVE_FROM_INTERPRETER (val_register, Function);
	      POP_PRIMITIVE_FRAME (nargs);
	      if (Must_Report_References ())
		{
		  exp_register = val_register;
		  Store_Return (RC_RESTORE_VALUE);
		  Save_Cont ();
		  Call_Future_Logging ();
		}
	      goto pop_return;
	    }

	  case TC_EXTENDED_PROCEDURE:
	    {
	      SCHEME_OBJECT lambda;
	      SCHEME_OBJECT temp;
	      long nargs;
	      long nparams;
	      long formals;
	      long params;
	      long auxes;
	      long rest_flag;
	      long size;
	      long i;
	      SCHEME_OBJECT * scan;

	      nargs = ((OBJECT_DATUM (STACK_POP ())) - STACK_FRAME_HEADER);
#if 0
	      if (Eval_Debug)
		{
		  Print_Expression
		    ((LONG_TO_UNSIGNED_FIXNUM (nargs + STACK_FRAME_HEADER)),
		     "APPLY: Number of arguments");
		  outf_console ("\n");
		}
#endif

	      lambda = (FAST_MEMORY_REF (Function, PROCEDURE_LAMBDA_EXPR));
	      APPLY_FUTURE_CHECK
		(Function, (FAST_MEMORY_REF (lambda, ELAMBDA_NAMES)));
	      nparams = ((VECTOR_LENGTH (Function)) - 1);
	      APPLY_FUTURE_CHECK (Function, (Get_Count_Elambda (lambda)));
	      formals = (Elambda_Formals_Count (Function));
	      params = ((Elambda_Opts_Count (Function)) + formals);
	      rest_flag = (Elambda_Rest_Flag (Function));
	      auxes = (nparams - (params + rest_flag));

	      if ((nargs < formals) || (!rest_flag && (nargs > params)))
		{
		  STACK_PUSH (STACK_FRAME_HEADER + nargs);
		  APPLICATION_ERROR (ERR_WRONG_NUMBER_OF_ARGUMENTS);
		}
	      /* size includes the procedure slot, but not the header.  */
	      size = (params + rest_flag + auxes + 1);
	      if (GC_Check
		  (size + 1
		   + ((nargs > params)
		      ? (2 * (nargs - params))
		      : 0)))
		{
		  STACK_PUSH (STACK_FRAME_HEADER + nargs);
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
		    (*scan++) = DEFAULT_OBJECT;
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
		    (*scan++) = DEFAULT_OBJECT;
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
	      env_register = temp;
	      REDUCES_TO (Get_Body_Elambda (lambda));
	    }

	  case TC_COMPILED_ENTRY:
	    {
	      apply_compiled_setup
		(STACK_ENV_EXTRA_SLOTS
		 + (OBJECT_DATUM (STACK_REF (STACK_ENV_HEADER))));
	      dispatch_code = (apply_compiled_procedure ());

	    return_from_compiled_code:
	      switch (dispatch_code)
		{
		case PRIM_DONE:
		  {
		    compiled_code_done ();
		    goto pop_return;
		  }

		case PRIM_APPLY:
		  {
		    compiler_apply_procedure
		      (STACK_ENV_EXTRA_SLOTS
		       + (OBJECT_DATUM (STACK_REF (STACK_ENV_HEADER))));
		    goto internal_apply;
		  }

		case PRIM_INTERRUPT:
		  {
		    compiled_error_backout ();
		    Save_Cont ();
		    SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());
		  }

		case PRIM_APPLY_INTERRUPT:
		  {
		    apply_compiled_backout ();
		    PREPARE_APPLY_INTERRUPT ();
		    SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());
		  }

		case ERR_INAPPLICABLE_OBJECT:

		  /* This error code means that
		     apply_compiled_procedure was called on an object
		     which is not a compiled procedure, or it was
		     called in a system without compiler support.
		     
		     Fall through...  */

		case ERR_WRONG_NUMBER_OF_ARGUMENTS:
		  {
		    apply_compiled_backout ();
		    APPLICATION_ERROR (dispatch_code);
		  }

		case ERR_EXECUTE_MANIFEST_VECTOR:
		  {
		    /* This error code means that
		       enter_compiled_expression was called in a
		       system without compiler support.  This is a
		       kludge!  */
		    execute_compiled_backout ();
		    val_register
		      = (OBJECT_NEW_TYPE (TC_COMPILED_ENTRY, exp_register));
		    POP_RETURN_ERROR (dispatch_code);
		  }

		case ERR_INAPPLICABLE_CONTINUATION:
		  {
		    /* This error code means that
		       return_to_compiled_code saw a non-continuation
		       on the stack, or was called in a system without
		       compiler support.  */
		    exp_register = SHARP_F;
		    Store_Return (RC_REENTER_COMPILED_CODE);
		    POP_RETURN_ERROR (dispatch_code);
		  }

		default:
		  compiled_error_backout ();
		  POP_RETURN_ERROR (dispatch_code);
		}
	    }

	  default:
	    APPLICATION_ERROR (ERR_INAPPLICABLE_OBJECT);
	  }
      }

    case RC_MOVE_TO_ADJACENT_POINT:
      /* exp_register contains the space in which we are moving */
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
	    Thunk = (FAST_MEMORY_REF (Current, STATE_POINT_AFTER_THUNK));
	    New_Location
	      = (FAST_MEMORY_REF (Current, STATE_POINT_NEARER_POINT));
	    (STACK_REF (TRANSLATE_FROM_POINT)) = New_Location;
	    if ((From_Count == 1)
		&& ((STACK_REF (TRANSLATE_TO_DISTANCE))
		    == (LONG_TO_UNSIGNED_FIXNUM (0))))
	      sp_register = (STACK_LOC (4));
	    else
	      Save_Cont ();
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
		= (FAST_MEMORY_REF (To_Location, STATE_POINT_NEARER_POINT));
	    Thunk = (FAST_MEMORY_REF (To_Location, STATE_POINT_BEFORE_THUNK));
	    New_Location = To_Location;
	    (STACK_REF (TRANSLATE_TO_DISTANCE))
	      = (LONG_TO_UNSIGNED_FIXNUM (To_Count));
	    if (To_Count == 0)
	      sp_register = (STACK_LOC (4));
	    else
	      Save_Cont ();
	  }
	if (exp_register != SHARP_F)
	  {
	    MEMORY_SET (exp_register, STATE_SPACE_NEAREST_POINT, New_Location);
	  }
	else
	  Current_State_Point = New_Location;
	Will_Push (2);
	STACK_PUSH (Thunk);
	STACK_PUSH (STACK_FRAME_HEADER);
	Pushed ();
	goto internal_apply;
      }

    case RC_INVOKE_STACK_THREAD:
      /* Used for WITH_THREADED_STACK primitive.  */
      Will_Push (3);
      STACK_PUSH (val_register); /* Value calculated by thunk.  */
      STACK_PUSH (exp_register);
      STACK_PUSH (STACK_FRAME_HEADER+1);
      Pushed ();
      goto internal_apply;

    case RC_JOIN_STACKLETS:
      Our_Throw (1, exp_register);
      Join_Stacklet_Backout ();
      Our_Throw_Part_2 ();
      break;

    case RC_NORMAL_GC_DONE:
      val_register = exp_register;
      /* Paranoia */
      if (GC_Space_Needed < 0)
	GC_Space_Needed = 0;
      if (GC_Check (GC_Space_Needed))
	termination_gc_out_of_space ();
      GC_Space_Needed = 0;
      EXIT_CRITICAL_SECTION ({ Save_Cont (); });
      End_GC_Hook ();
      break;

    case RC_PCOMB1_APPLY:
      End_Subproblem ();
      STACK_PUSH (val_register); /* Argument value */
      Finished_Eventual_Pushing (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      exp_register = (FAST_MEMORY_REF (exp_register, PCOMB1_FN_SLOT));

    primitive_internal_apply:

#ifdef COMPILE_STEPPER
      if (Trapping
	  && (!WITHIN_CRITICAL_SECTION_P ())
	  && ((FETCH_APPLY_TRAPPER ()) != SHARP_F))
	{
	  /* Does this work in the stacklet case?
	     We may have a non-contiguous frame. -- Jinx  */
	  Will_Push (3);
	  STACK_PUSH (exp_register);
	  STACK_PUSH (FETCH_APPLY_TRAPPER ());
	  STACK_PUSH
	    (STACK_FRAME_HEADER + 1 + (PRIMITIVE_N_PARAMETERS (exp_register)));
	  Pushed ();
	  Stop_Trapping ();
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
	SCHEME_OBJECT primitive = exp_register;
	APPLY_PRIMITIVE_FROM_INTERPRETER (val_register, primitive);
	POP_PRIMITIVE_FRAME (PRIMITIVE_ARITY (primitive));
	if (Must_Report_References ())
	  {
	    exp_register = val_register;
	    Store_Return (RC_RESTORE_VALUE);
	    Save_Cont ();
	    Call_Future_Logging ();
	  }
	break;
      }

    case RC_PCOMB2_APPLY:
      End_Subproblem ();
      STACK_PUSH (val_register); /* Value of arg. 1 */
      Finished_Eventual_Pushing (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      exp_register = (FAST_MEMORY_REF (exp_register, PCOMB2_FN_SLOT));
      goto primitive_internal_apply;

    case RC_PCOMB2_DO_1:
      env_register = (STACK_POP ());
      STACK_PUSH (val_register); /* Save value of arg. 2 */
      DO_ANOTHER_THEN (RC_PCOMB2_APPLY, PCOMB2_ARG_1_SLOT);

    case RC_PCOMB3_APPLY:
      End_Subproblem ();
      STACK_PUSH (val_register); /* Save value of arg. 1 */
      Finished_Eventual_Pushing (CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      exp_register = (FAST_MEMORY_REF (exp_register, PCOMB3_FN_SLOT));
      goto primitive_internal_apply;

    case RC_PCOMB3_DO_1:
      {
	SCHEME_OBJECT Temp = (STACK_POP ()); /* Value of arg. 3 */
	env_register = (STACK_POP ());
	STACK_PUSH (Temp);	/* Save arg. 3 again */
	STACK_PUSH (val_register); /* Save arg. 2 */
	DO_ANOTHER_THEN (RC_PCOMB3_APPLY, PCOMB3_ARG_1_SLOT);
      }

    case RC_PCOMB3_DO_2:
      env_register = (STACK_REF (0));
      STACK_PUSH (val_register); /* Save value of arg. 3 */
      DO_ANOTHER_THEN (RC_PCOMB3_DO_1, PCOMB3_ARG_2_SLOT);

    case RC_POP_RETURN_ERROR:
    case RC_RESTORE_VALUE:
      val_register = exp_register;
      break;

    case RC_PRIMITIVE_CONTINUE:
      val_register = (continue_primitive ());
      break;

    case RC_REPEAT_DISPATCH:
      dispatch_code = (FIXNUM_TO_LONG (exp_register));
      env_register = (STACK_POP ());
      val_register = (STACK_POP ());
      Restore_Cont ();
      goto repeat_dispatch;

      /* The following two return codes are both used to restore a
	 saved history object.  The difference is that the first does
	 not copy the history object while the second does.  In both
	 cases, the exp_register contains the history object and the
	 next item to be popped off the stack contains the offset back
	 to the previous restore history return code.

	 ASSUMPTION: History objects are never created using futures.  */

    case RC_RESTORE_DONT_COPY_HISTORY:
      {
	SCHEME_OBJECT Stacklet;

	Prev_Restore_History_Offset = (OBJECT_DATUM (STACK_POP ()));
	Stacklet = (STACK_POP ());
	history_register = (OBJECT_ADDRESS (exp_register));
	if (Prev_Restore_History_Offset == 0)
	  Prev_Restore_History_Stacklet = 0;
	else if (Stacklet == SHARP_F)
	  Prev_Restore_History_Stacklet = 0;
	else
	  Prev_Restore_History_Stacklet = (OBJECT_ADDRESS (Stacklet));
	break;
      }

    case RC_RESTORE_HISTORY:
      {
	SCHEME_OBJECT Stacklet;

	if (!Restore_History (exp_register))
	  {
	    Save_Cont ();
	    Will_Push (CONTINUATION_SIZE);
	    exp_register = val_register;
	    Store_Return (RC_RESTORE_VALUE);
	    Save_Cont ();
	    Pushed ();
	    IMMEDIATE_GC ((Free > MemTop) ? 0 : ((MemTop - Free) + 1));
	  }
	Prev_Restore_History_Offset = (OBJECT_DATUM (STACK_POP ()));
	Stacklet = (STACK_POP ());
	if (Prev_Restore_History_Offset == 0)
	  Prev_Restore_History_Stacklet = 0;
	else
	  {
	    if (Stacklet == SHARP_F)
	      {
		Prev_Restore_History_Stacklet = 0;
		((Get_End_Of_Stacklet ()) [-Prev_Restore_History_Offset])
		  = (MAKE_OBJECT (TC_RETURN_CODE, RC_RESTORE_HISTORY));
	      }
	    else
	      {
		Prev_Restore_History_Stacklet = (OBJECT_ADDRESS (Stacklet));
		(Prev_Restore_History_Stacklet [-Prev_Restore_History_Offset])
		  = (MAKE_OBJECT (TC_RETURN_CODE, RC_RESTORE_HISTORY));
	      }
	  }
	break;
      }

    case RC_RESTORE_FLUIDS:
      Fluid_Bindings = exp_register;
      break;

    case RC_RESTORE_INT_MASK:
      SET_INTERRUPT_MASK (UNSIGNED_FIXNUM_TO_LONG (exp_register));
      if (GC_Check (0))
        Request_GC (0);
      if ((PENDING_INTERRUPTS ()) != 0)
	{
	  Store_Return (RC_RESTORE_VALUE);
	  exp_register = val_register;
	  Save_Cont ();
	  SIGNAL_INTERRUPT (PENDING_INTERRUPTS ());
	}
      break;

    case RC_STACK_MARKER:
      /* Frame consists of the return code followed by two objects.
	 The first object has already been popped into exp_register,
         so just pop the second argument.  */
      sp_register = (STACK_LOCATIVE_OFFSET (sp_register, 1));
      break;

    case RC_RESTORE_TO_STATE_POINT:
      {
	SCHEME_OBJECT Where_To_Go = exp_register;
	Will_Push (CONTINUATION_SIZE);
	/* Restore the contents of val_register after moving to point */
	exp_register = val_register;
	Store_Return (RC_RESTORE_VALUE);
	Save_Cont ();
	Pushed ();
	Translate_To_Point (Where_To_Go);
	break;			/* We never get here.... */
      }

    case RC_SEQ_2_DO_2:
      End_Subproblem ();
      env_register = (STACK_POP ());
      REDUCES_TO_NTH (SEQUENCE_2);

    case RC_SEQ_3_DO_2:
      env_register = (STACK_REF (0));
      DO_ANOTHER_THEN (RC_SEQ_3_DO_3, SEQUENCE_2);

    case RC_SEQ_3_DO_3:
      End_Subproblem ();
      env_register = (STACK_POP ());
      REDUCES_TO_NTH (SEQUENCE_3);

    case RC_SNAP_NEED_THUNK:
      /* Don't snap thunk twice; evaluation of the thunk's body might
	 have snapped it already.  */
      if ((MEMORY_REF (exp_register, THUNK_SNAPPED)) == SHARP_T)
	val_register = (MEMORY_REF (exp_register, THUNK_VALUE));
      else
	{
	  MEMORY_SET (exp_register, THUNK_SNAPPED, SHARP_T);
	  MEMORY_SET (exp_register, THUNK_VALUE, val_register);
	}
      break;

    case RC_AFTER_MEMORY_UPDATE:
    case RC_BAD_INTERRUPT_CONTINUE:
    case RC_COMPLETE_GC_DONE:
    case RC_RESTARTABLE_EXIT:
    case RC_RESTART_EXECUTION:
    case RC_RESTORE_CONTINUATION:
    case RC_RESTORE_STEPPER:
    case RC_POP_FROM_COMPILED_CODE:
      POP_RETURN_ERROR (ERR_INAPPLICABLE_CONTINUATION);

      SITE_RETURN_DISPATCH_HOOK ();

    default:
      POP_RETURN_ERROR (ERR_INAPPLICABLE_CONTINUATION);
    }
  goto pop_return;
}
