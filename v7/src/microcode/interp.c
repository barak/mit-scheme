/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/interp.c,v 9.37 1987/12/04 22:17:11 jinx Rel $
 *
 * This file contains the heart of the Scheme Scode
 * interpreter
 *
 */

#define In_Main_Interpreter	true
#include "scheme.h"
#include "locks.h"
#include "trap.h"
#include "lookup.h"
#include "history.h"
#include "cmpint.h"
#include "zones.h"

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
 * Do_Expression.  Now, if there was code after the call to
 * EVAL you first push a "return code" (using Save_Cont) on
 * the stack and move the code that used to be after the
 * call down into the part of this file after the tag
 * Pop_Return.
 *
 * Notice that because of the caller saves convention used
 * here, all of the registers which are of interest have
 * been SAVEd on the racks by the time interpretation arrives
 * at Do_Expression (the top of EVAL).
 *
 * For notes on error handling and interrupts, see the file
 * utils.c.
 *
 * This file is divided into two parts. The first
 * corresponds is called the EVAL dispatch, and is ordered
 * alphabetically by the SCode item handled.  The second,
 * called the return dispatch, begins at Pop_Return and is
 * ordered alphabetically by return code name.
 */

#define Prepare_Pop_Return_Interrupt(Return_Code, Contents_of_Val)	\
{									\
  Store_Return(Return_Code);						\
  Save_Cont();								\
  Store_Return(RC_RESTORE_VALUE);					\
  Store_Expression(Contents_of_Val);					\
  Save_Cont();								\
}

#define Interrupt(Masked_Code)						\
{									\
  Export_Registers();							\
  Setup_Interrupt(Masked_Code);						\
  Import_Registers();							\
  goto Perform_Application;						\
}

#define Immediate_GC(N)							\
{									\
  Request_GC(N);							\
  Interrupt(PENDING_INTERRUPTS());					\
}

#define Eval_GC_Check(Amount)						\
if (GC_Check(Amount))							\
{									\
  Prepare_Eval_Repeat();						\
  Immediate_GC(Amount);							\
}

#define Prepare_Eval_Repeat()						\
{									\
 Will_Push(CONTINUATION_SIZE+1);					\
  Push(Fetch_Env());							\
  Store_Return(RC_EVAL_ERROR);						\
  Save_Cont();								\
 Pushed();								\
}

#define Eval_Error(Err)							\
{									\
  Export_Registers();							\
  Do_Micro_Error(Err, false);						\
  Import_Registers();							\
  goto Internal_Apply;							\
}

#define Pop_Return_Error(Err)						\
{									\
  Export_Registers();							\
  Do_Micro_Error(Err, true);						\
  Import_Registers();							\
  goto Internal_Apply;							\
}

#define BACK_OUT_AFTER_PRIMITIVE()					\
{									\
  Export_Registers();							\
  Back_Out_Of_Primitive();						\
  Import_Registers();							\
}

#define Reduces_To(Expr)						\
	{ Store_Expression(Expr);					\
          New_Reduction(Fetch_Expression(), Fetch_Env());		\
          goto Do_Expression;						\
        }

#define Reduces_To_Nth(N)						\
        Reduces_To(Fast_Vector_Ref(Fetch_Expression(), (N)))

#define Do_Nth_Then(Return_Code, N, Extra)				\
	{ Store_Return(Return_Code);					\
	  Save_Cont();							\
	  Store_Expression(Fast_Vector_Ref(Fetch_Expression(), (N)));	\
	  New_Subproblem(Fetch_Expression(), Fetch_Env());		\
          Extra;							\
	  goto Do_Expression;						\
        }

#define Do_Another_Then(Return_Code, N)					\
	{ Store_Return(Return_Code);					\
          Save_Cont();							\
	  Store_Expression(Fast_Vector_Ref(Fetch_Expression(), (N)));	\
	  Reuse_Subproblem(Fetch_Expression(), Fetch_Env());		\
	  goto Do_Expression;						\
        }

#define Environment_P(Obj) (Obj == NIL || (Type_Code(Obj) == TC_ENVIRONMENT))

                      /***********************/
                      /* Macros for Stepping */
                      /***********************/

#define Fetch_Trapper(field)	\
        Vector_Ref(Get_Fixed_Obj_Slot(Stepper_State), (field))

#define Fetch_Eval_Trapper() Fetch_Trapper(HUNK_CXR0)
#define Fetch_Apply_Trapper() Fetch_Trapper(HUNK_CXR1)
#define Fetch_Return_Trapper() Fetch_Trapper(HUNK_CXR2)

/* Macros for handling FUTUREs */

#ifdef COMPILE_FUTURES

/* ARG_TYPE_ERROR handles the error returns from primitives which type check
   their arguments and restarts them or suspends if the argument is a future.
 */

#define ARG_TYPE_ERROR(Arg_No, Err_No)					\
{									\
  fast Pointer *Arg, Orig_Arg;						\
									\
  Arg = &(Stack_Ref((Arg_No - 1) + STACK_ENV_FIRST_ARG));		\
  Orig_Arg = *Arg;							\
									\
  if (OBJECT_TYPE(*Arg) != TC_FUTURE)					\
  {									\
    Pop_Return_Error(Err_No);						\
  }									\
									\
  while ((OBJECT_TYPE(*Arg) == TC_FUTURE) && (Future_Has_Value(*Arg)))	\
  {									\
    if (Future_Is_Keep_Slot(*Arg))					\
    {									\
      Log_Touch_Of_Future(*Arg);					\
    }									\
    *Arg = Future_Value(*Arg);						\
  }									\
  if (OBJECT_TYPE(*Arg) != TC_FUTURE)					\
  {									\
    goto Apply_Non_Trapping;						\
  }									\
									\
  TOUCH_SETUP(*Arg);							\
  *Arg = Orig_Arg;							\
  goto Apply_Non_Trapping;						\
}

/* Apply_Future_Check is called at apply time to guarantee that certain
   objects (the procedure itself, and its LAMBDA components for user defined
   procedures) are not futures
*/

#define Apply_Future_Check(Name, Object)				\
{									\
  fast Pointer *Arg, Orig_Answer;					\
									\
  Arg = &(Object);							\
  Orig_Answer = *Arg;							\
									\
  while (Type_Code(*Arg) == TC_FUTURE)					\
  {									\
    if (Future_Has_Value(*Arg))						\
    {									\
      if (Future_Is_Keep_Slot(*Arg))					\
      {									\
	Log_Touch_Of_Future(*Arg);					\
      }									\
      *Arg = Future_Value(*Arg);					\
    }									\
    else								\
    {									\
      Store_Return(RC_INTERNAL_APPLY);					\
      Val = NIL;							\
      TOUCH_SETUP(*Arg);						\
      *Arg = Orig_Answer;						\
      goto Internal_Apply;						\
    }									\
  }									\
  Name = *Arg;								\
}

/* Future handling macros continue on the next page */

/* Future handling macros, continued */

/* Pop_Return_Val_Check suspends the process if the value calculated by
   a recursive call to EVAL is an undetermined future */

#define Pop_Return_Val_Check()						\
{									\
  fast Pointer Orig_Val = Val;						\
									\
  while (OBJECT_TYPE(Val) == TC_FUTURE)					\
  {									\
    if (Future_Has_Value(Val))						\
    {									\
      if (Future_Is_Keep_Slot(Val))					\
      {									\
	Log_Touch_Of_Future(Val);					\
      }									\
      Val = Future_Value(Val);						\
    }									\
    else								\
    {									\
      Save_Cont();							\
     Will_Push(CONTINUATION_SIZE +  + (STACK_ENV_EXTRA_SLOTS + 2));	\
      Store_Return(RC_RESTORE_VALUE);					\
      Store_Expression(Orig_Val);					\
      Save_Cont();							\
      Push(Val);							\
      Push(Get_Fixed_Obj_Slot(System_Scheduler));			\
      Push(STACK_FRAME_HEADER + 1);					\
     Pushed();								\
      goto Internal_Apply;						\
    }									\
  }									\
}

/* This saves stuff unnecessarily in most cases.
   For example, when Which_Way is PRIM_APPLY, Val, Env, Expr,
   and Return_Code are undefined.
 */

#define LOG_FUTURES()							\
{									\
  if (Must_Report_References())						\
  {									\
    Save_Cont();							\
   Will_Push(CONTINUATION_SIZE + 2);					\
    Push(Val);								\
    Save_Env();								\
    Store_Return(RC_REPEAT_DISPATCH);					\
    Store_Expression(MAKE_SIGNED_FIXNUM(CODE_MAP(Which_Way)));		\
    Save_Cont();							\
   Pushed();								\
    Call_Future_Logging();						\
 }									\
}

#else /* not COMPILE_FUTURES */

#define Pop_Return_Val_Check()		

#define Apply_Future_Check(Name, Object)	Name = (Object)

#define ARG_TYPE_ERROR(Arg_No, Err_No)					\
{									\
  Pop_Return_Error(Err_No)						\
}

#define LOG_FUTURES()

#endif /* COMPILE_FUTURES */

/* Notes on Repeat_Dispatch:

   The codes used (values of Which_Way) are divided into two groups:
   Those for which the primitive has already backed out, and those for
   which the back out code has not yet been executed, and is therefore
   executed below.

   Under most circumstances the distinction is moot, but if there are
   futures in the system, and future touches must be logged, the code
   must be set up to "interrupt" the dispatch, and proceed it later.
   The primitive back out code must be done before the furure is
   logged, so all of these codes are split into two versions: one set
   before doing the back out, and another afterwards.
 */

/* This is assumed to be larger (in absolute value) than any PRIM_<mumble>
   and ERR_<mumble>.
 */
#define PRIM_BIAS_AMOUNT 1000

#if (MAX_ERROR >= PRIM_BIAS_AMOUNT)
#include "Inconsistency: errors.h and interp.c"
#endif

#define CODE_MAP(code)							\
((code < 0) ?								\
 (code - PRIM_BIAS_AMOUNT) :						\
 (code + PRIM_BIAS_AMOUNT))

#define CODE_UNMAP(code)						\
((code < 0) ?								\
 (code + PRIM_BIAS_AMOUNT) :						\
 (code - PRIM_BIAS_AMOUNT))

#define CODE_MAPPED_P(code)						\
((code < (- PRIM_BIAS_AMOUNT)) ||					\
 (code >= PRIM_BIAS_AMOUNT))

/*
  The EVAL/APPLY ying/yang
 */

void
Interpret(dumped_p)
     Boolean dumped_p;
{
  long Which_Way;
  fast Pointer *Reg_Block, *Reg_Stack_Pointer, *Reg_History;

  extern long enter_compiled_expression();
  extern long apply_compiled_procedure();
  extern long return_to_compiled_code();

  Reg_Block = &Registers[0];

  /* Primitives jump back here for errors, requests to evaluate an
   * expression, apply a function, or handle an interrupt request.  On
   * errors or interrupts they leave their arguments on the stack, the
   * primitive itself in Expression.  The code should do a primitive
   * backout in these cases, but not in others (apply, eval, etc.), since
   * the primitive itself will have left the state of the interpreter ready
   * for operation.
   */

  Which_Way = setjmp(*Back_To_Eval);
  Set_Time_Zone(Zone_Working);
  Import_Registers();

Repeat_Dispatch:
  switch (Which_Way)
  {
    case PRIM_APPLY:
      LOG_FUTURES();
      goto Internal_Apply;

    case PRIM_NO_TRAP_APPLY:
      LOG_FUTURES();
      goto Apply_Non_Trapping;

    case PRIM_DO_EXPRESSION:
      LOG_FUTURES();
      Reduces_To(Fetch_Expression());

    case PRIM_NO_TRAP_EVAL:
      LOG_FUTURES();
      New_Reduction(Fetch_Expression(), Fetch_Env());
      goto Eval_Non_Trapping;

    case 0:			/* first time */
      if (dumped_p)
      {
	goto Pop_Return;
      }
      else
      {
	break;			/* fall into eval */
      }

    case PRIM_POP_RETURN:
      LOG_FUTURES();
      goto Pop_Return;

    case PRIM_TOUCH:
      BACK_OUT_AFTER_PRIMITIVE();
      LOG_FUTURES();
      /* fall through */
    case CODE_MAP(PRIM_TOUCH):
      TOUCH_SETUP(Val);
      goto Internal_Apply;

    case PRIM_INTERRUPT:
      BACK_OUT_AFTER_PRIMITIVE();
      LOG_FUTURES();
      /* fall through */
    case CODE_MAP(PRIM_INTERRUPT):
      Save_Cont();
      Interrupt(PENDING_INTERRUPTS());

    case ERR_ARG_1_WRONG_TYPE:
      BACK_OUT_AFTER_PRIMITIVE();
      LOG_FUTURES();
      /* fall through */
    case CODE_MAP(ERR_ARG_1_WRONG_TYPE):
      ARG_TYPE_ERROR(1, ERR_ARG_1_WRONG_TYPE);

    case ERR_ARG_2_WRONG_TYPE:
      BACK_OUT_AFTER_PRIMITIVE();
      LOG_FUTURES();
      /* fall through */
    case CODE_MAP(ERR_ARG_2_WRONG_TYPE):
      ARG_TYPE_ERROR(2, ERR_ARG_2_WRONG_TYPE);

    case ERR_ARG_3_WRONG_TYPE:
      BACK_OUT_AFTER_PRIMITIVE();
      LOG_FUTURES();
      /* fall through */
    case CODE_MAP(ERR_ARG_3_WRONG_TYPE):
      ARG_TYPE_ERROR(3, ERR_ARG_3_WRONG_TYPE);
  
    default:
    {
      if (!CODE_MAPPED_P(Which_Way))
      {
	BACK_OUT_AFTER_PRIMITIVE();
	LOG_FUTURES();
      }
      else
      {
	Which_Way = CODE_UNMAP(Which_Way);
      }
      Pop_Return_Error(Which_Way);
    }
  }

Do_Expression:

  if (Eval_Debug)
  { Print_Expression(Fetch_Expression(), "Eval, expression");
    CRLF();
  }

/* The expression register has an Scode item in it which
 * should be evaluated and the result left in Val.
 *
 * A "break" after the code for any operation indicates that
 * all processing for this operation has been completed, and
 * the next step will be to pop a return code off the stack
 * and proceed at Pop_Return.  This is sometimes called
 * "executing the continuation" since the return code can be
 * considered the continuation to be performed after the
 * operation.
 *
 * An operation can terminate with a Reduces_To or
 * Reduces_To_Nth macro.  This indicates that the  value of
 * the current Scode item is the value returned when the
 * new expression is evaluated.  Therefore no new
 * continuation is created and processing continues at
 * Do_Expression with the new expression in the expression
 * register.
 *
 * Finally, an operation can terminate with a Do_Nth_Then
 * macro.  This indicates that another expression must be
 * evaluated and them some additional processing will be
 * performed before the value of this S-Code item available.
 * Thus a new continuation is created and placed on the
 * stack (using Save_Cont), the new expression is placed in
 * the Expression register, and processing continues at
 * Do_Expression.
 */

/* Handling of Eval Trapping.

   If we are handling traps and there is an Eval Trap set,
   turn off all trapping and then go to Internal_Apply to call the
   user supplied eval hook with the expression to be evaluated and the
   environment.

*/

  if (Microcode_Does_Stepping && Trapping && (Fetch_Eval_Trapper() != NIL))
  { Stop_Trapping();
   Will_Push(4);
    Push(Fetch_Env());
    Push(Fetch_Expression());
    Push(Fetch_Eval_Trapper());
    Push(STACK_FRAME_HEADER+2);
   Pushed();
    goto Apply_Non_Trapping;
  }

Eval_Non_Trapping:
  Eval_Ucode_Hook();
  switch (Type_Code(Fetch_Expression()))
  {
    case TC_BIG_FIXNUM:         /* The self evaluating items */
    case TC_BIG_FLONUM:
    case TC_CHARACTER_STRING:
    case TC_CHARACTER:
    case TC_COMPILED_PROCEDURE:
    case TC_COMPLEX:
    case TC_CONTROL_POINT:
    case TC_DELAYED:
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
    case TC_UNINTERNED_SYMBOL:
    case TC_TRUE: 
    case TC_VECTOR:
    case TC_VECTOR_16B:
    case TC_VECTOR_1B:
    case TC_REFERENCE_TRAP:
      Val = Fetch_Expression(); break;

    case TC_ACCESS:
     Will_Push(CONTINUATION_SIZE);
      Do_Nth_Then(RC_EXECUTE_ACCESS_FINISH, ACCESS_ENVIRONMENT, Pushed());

    case TC_ASSIGNMENT:
     Will_Push(CONTINUATION_SIZE + 1);
      Save_Env();
      Do_Nth_Then(RC_EXECUTE_ASSIGNMENT_FINISH, ASSIGN_VALUE, Pushed());

    case TC_BROKEN_HEART:
      Export_Registers();
      Microcode_Termination(TERM_BROKEN_HEART);

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case TC_COMBINATION:
      {
	long Array_Length;

	Array_Length = (Vector_Length(Fetch_Expression()) - 1);
#ifdef USE_STACKLETS
	/* Save_Env, Finger */
        Eval_GC_Check(New_Stacklet_Size(Array_Length + 1 + 1 + CONTINUATION_SIZE));
#endif /* USE_STACKLETS */
       Will_Push(Array_Length + 1 + 1 + CONTINUATION_SIZE);
	Stack_Pointer = Simulate_Pushing(Array_Length);
        Push(Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, Array_Length));
	/* The finger: last argument number */
       Pushed();
        if (Array_Length == 0)
	{
	  Push(STACK_FRAME_HEADER);   /* Frame size */
          Do_Nth_Then(RC_COMB_APPLY_FUNCTION, COMB_FN_SLOT, {});
	}
	Save_Env();
	Do_Nth_Then(RC_COMB_SAVE_VALUE, Array_Length+1, {});
      }

    case TC_COMBINATION_1:
     Will_Eventually_Push(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 1);
      Save_Env();
      Do_Nth_Then(RC_COMB_1_PROCEDURE, COMB_1_ARG_1, {});
  
    case TC_COMBINATION_2:
     Will_Eventually_Push(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 2);      
      Save_Env();
      Do_Nth_Then(RC_COMB_2_FIRST_OPERAND, COMB_2_ARG_2, {});

    case TC_COMMENT:
      Reduces_To_Nth(COMMENT_EXPRESSION);

    case TC_CONDITIONAL:
     Will_Push(CONTINUATION_SIZE + 1);
      Save_Env();
      Do_Nth_Then(RC_CONDITIONAL_DECIDE, COND_PREDICATE, Pushed());

    case TC_COMPILED_EXPRESSION:
      {
	Pointer compiled_expression;

	compiled_expression = (Fetch_Expression ());
	execute_compiled_setup();
	Store_Expression ((Pointer) (Get_Pointer (compiled_expression)));
	Export_Registers();
	Which_Way = enter_compiled_expression();
	goto return_from_compiled_code;
      }

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case TC_DEFINITION:
     Will_Push(CONTINUATION_SIZE + 1);
      Save_Env();
      Do_Nth_Then(RC_EXECUTE_DEFINITION_FINISH, DEFINE_VALUE, Pushed());

    case TC_DELAY:
      /* Deliberately omitted: Eval_GC_Check(2); */
      Val = Make_Pointer(TC_DELAYED, Free);
      Free[THUNK_ENVIRONMENT] = Fetch_Env();
      Free[THUNK_PROCEDURE] = 
        Fast_Vector_Ref(Fetch_Expression(), DELAY_OBJECT);
      Free += 2;
      break;       

    case TC_DISJUNCTION:
     Will_Push(CONTINUATION_SIZE + 1);
      Save_Env();
      Do_Nth_Then(RC_DISJUNCTION_DECIDE, OR_PREDICATE, Pushed());

    case TC_EXTENDED_LAMBDA:	/* Close the procedure */
    /* Deliberately omitted: Eval_GC_Check(2); */
      Val = Make_Pointer(TC_EXTENDED_PROCEDURE, Free);
      Free[PROCEDURE_LAMBDA_EXPR] = Fetch_Expression();
      Free[PROCEDURE_ENVIRONMENT] = Fetch_Env();
      Free += 2;
      break;

/* Interpret() continues on the next page */

/* Interpret(), continued */

#ifdef COMPILE_FUTURES
    case TC_FUTURE:
      if (Future_Has_Value(Fetch_Expression()))
      { Pointer Future = Fetch_Expression();
        if (Future_Is_Keep_Slot(Future)) Log_Touch_Of_Future(Future);
        Reduces_To_Nth(FUTURE_VALUE);
      }
      Prepare_Eval_Repeat();
     Will_Push(STACK_ENV_EXTRA_SLOTS+2);
      Push(Fetch_Expression());	/* Arg: FUTURE object */
      Push(Get_Fixed_Obj_Slot(System_Scheduler));
      Push(STACK_FRAME_HEADER+1);
     Pushed();
      goto Internal_Apply;
#endif

    case TC_IN_PACKAGE:
     Will_Push(CONTINUATION_SIZE);
      Do_Nth_Then(RC_EXECUTE_IN_PACKAGE_CONTINUE,
                  IN_PACKAGE_ENVIRONMENT, Pushed());

    case TC_LAMBDA:             /* Close the procedure */
    case TC_LEXPR:
    /* Deliberately omitted: Eval_GC_Check(2); */
      Val = Make_Pointer(TC_PROCEDURE, Free);
      Free[PROCEDURE_LAMBDA_EXPR] = Fetch_Expression();
      Free[PROCEDURE_ENVIRONMENT] = Fetch_Env();
      Free += 2;
      break;

    case TC_MANIFEST_NM_VECTOR:
    case TC_MANIFEST_SPECIAL_NM_VECTOR:
      Eval_Error(ERR_EXECUTE_MANIFEST_VECTOR);

/* Interpret() continues on the next page */

/* Interpret(), continued */

    /*
      The argument to Will_Eventually_Push is determined by how much
      will be on the stack if we back out of the primitive.
     */

    case TC_PCOMB0:
     Will_Eventually_Push(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
     Finished_Eventual_Pushing(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      Store_Expression(Make_New_Pointer(TC_PRIMITIVE, Fetch_Expression()));
      goto Primitive_Internal_Apply;

    case TC_PCOMB1:
     Will_Eventually_Push(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 1);
      Do_Nth_Then(RC_PCOMB1_APPLY, PCOMB1_ARG_SLOT, {});

    case TC_PCOMB2:
     Will_Eventually_Push(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 2);
      Save_Env();
      Do_Nth_Then(RC_PCOMB2_DO_1, PCOMB2_ARG_2_SLOT, {});

    case TC_PCOMB3:
     Will_Eventually_Push(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 3);
      Save_Env();
      Do_Nth_Then(RC_PCOMB3_DO_2, PCOMB3_ARG_3_SLOT, {});

    case TC_SCODE_QUOTE:
      Val = Fast_Vector_Ref(Fetch_Expression(), SCODE_QUOTE_OBJECT);
      break;

    case TC_SEQUENCE_2:
     Will_Push(CONTINUATION_SIZE + 1);
      Save_Env();
      Do_Nth_Then(RC_SEQ_2_DO_2, SEQUENCE_1, Pushed());

    case TC_SEQUENCE_3:
     Will_Push(CONTINUATION_SIZE + 1);
      Save_Env();
      Do_Nth_Then(RC_SEQ_3_DO_2, SEQUENCE_1, Pushed());

    case TC_THE_ENVIRONMENT:
      Val = Fetch_Env(); break;

/* Interpret() continues on the next page */

/* Interpret(), continued */
      
    case TC_VARIABLE:
    {
      long temp;

#ifndef No_In_Line_Lookup

      fast Pointer *cell;

      Set_Time_Zone(Zone_Lookup);
      cell = Get_Pointer(Fetch_Expression());
      lookup(cell, Fetch_Env(), cell, repeat_variable_lookup);

lookup_end_restart:

      Val = Fetch(cell[0]);
      if (Type_Code(Val) != TC_REFERENCE_TRAP)
      {
	Set_Time_Zone(Zone_Working);
	goto Pop_Return;
      }

      get_trap_kind(temp, Val);
      switch(temp)
      {
	case TRAP_DANGEROUS:
	case TRAP_UNBOUND_DANGEROUS:
	case TRAP_UNASSIGNED_DANGEROUS:
	case TRAP_FLUID_DANGEROUS:
	case TRAP_COMPILER_CACHED_DANGEROUS:
	  cell = Get_Pointer(Fetch_Expression());
	  temp =
	    deep_lookup_end(deep_lookup(Fetch_Env(),
					cell[VARIABLE_SYMBOL],
					cell),
			    cell);
	  Import_Val();
	  if (temp != PRIM_DONE)
	    break;
	  Set_Time_Zone(Zone_Working);
	  goto Pop_Return;

	case TRAP_COMPILER_CACHED:
	  cell = Nth_Vector_Loc(Fast_Vector_Ref(Val, TRAP_EXTRA),
				TRAP_EXTENSION_CELL);
	  goto lookup_end_restart;

	case TRAP_FLUID:
	  cell = lookup_fluid(Val);
	  goto lookup_end_restart;

/* Interpret() continues on the next page */

/* Interpret(), continued */

	case TRAP_UNBOUND:
	  temp = ERR_UNBOUND_VARIABLE;
	  break;

	case TRAP_UNASSIGNED:
	  temp = ERR_UNASSIGNED_VARIABLE;
	  break;

	default:
	  temp = ERR_ILLEGAL_REFERENCE_TRAP;
	  break;
      }

#else No_In_Line_Lookup

      Set_Time_Zone(Zone_Lookup);
      temp = Lex_Ref(Fetch_Env(), Fetch_Expression());
      Import_Val();
      if (temp == PRIM_DONE)
	goto Pop_Return;

#endif No_In_Line_Lookup

      /* Back out of the evaluation. */

      Set_Time_Zone(Zone_Working);

      if (temp == PRIM_INTERRUPT)
      {
	Prepare_Eval_Repeat();
	Interrupt(PENDING_INTERRUPTS());
      }

      Eval_Error(temp);
    }

    SITE_EXPRESSION_DISPATCH_HOOK()

    case TC_RETURN_CODE:
    default: Eval_Error(ERR_UNDEFINED_USER_TYPE);
  };

/* Interpret() continues on the next page */

/* Interpret(), continued */

/* Now restore the continuation saved during an earlier part
 * of the EVAL cycle and continue as directed.
 */

Pop_Return:
  Pop_Return_Ucode_Hook();	
  Restore_Cont();
  if (Consistency_Check &&
      (Type_Code(Fetch_Return()) != TC_RETURN_CODE))
  { Push(Val);			/* For possible stack trace */
    Save_Cont();
    Export_Registers();
    Microcode_Termination(TERM_BAD_STACK);
  }
  if (Eval_Debug)
  { Print_Return("Pop_Return, return code");
    Print_Expression(Val, "Pop_Return, value");
    CRLF();
  };

  /* Dispatch on the return code.  A BREAK here will cause
   * a "goto Pop_Return" to occur, since this is the most
   * common occurrence.
   */

  switch (Get_Integer(Fetch_Return()))
  {
    case RC_COMB_1_PROCEDURE:
      Restore_Env();
      Push(Val);                /* Arg. 1 */
      Push(NIL);                /* Operator */
      Push(STACK_FRAME_HEADER + 1);
     Finished_Eventual_Pushing(CONTINUATION_SIZE);
      Do_Another_Then(RC_COMB_APPLY_FUNCTION, COMB_1_FN);

    case RC_COMB_2_FIRST_OPERAND:
      Restore_Env();
      Push(Val);
      Save_Env();
      Do_Another_Then(RC_COMB_2_PROCEDURE, COMB_2_ARG_1);

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_COMB_2_PROCEDURE:
      Restore_Env();
      Push(Val);                /* Arg 1, just calculated */
      Push(NIL);                /* Function */
      Push(STACK_FRAME_HEADER + 2);
     Finished_Eventual_Pushing(CONTINUATION_SIZE);
      Do_Another_Then(RC_COMB_APPLY_FUNCTION, COMB_2_FN);

    case RC_COMB_APPLY_FUNCTION:
       End_Subproblem();
       Stack_Ref(STACK_ENV_FUNCTION) = Val;
       goto Internal_Apply;

    case RC_COMB_SAVE_VALUE:
      {	long Arg_Number;

        Restore_Env();
        Arg_Number = Get_Integer(Stack_Ref(STACK_COMB_FINGER))-1;
        Stack_Ref(STACK_COMB_FIRST_ARG+Arg_Number) = Val;
        Stack_Ref(STACK_COMB_FINGER) = 
          Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, Arg_Number);
	/* DO NOT count on the type code being NMVector here, since
	   the stack parser may create them with NIL here! */
        if (Arg_Number > 0)
        { Save_Env();
          Do_Another_Then(RC_COMB_SAVE_VALUE,
                          (COMB_ARG_1_SLOT - 1) + Arg_Number);
        }
	Push(Fast_Vector_Ref(Fetch_Expression(), 0)); /* Frame Size */
        Do_Another_Then(RC_COMB_APPLY_FUNCTION, COMB_FN_SLOT);
      }

/* Interpret() continues on the next page */

/* Interpret(), continued */

#define define_compiler_restart(return_code, entry)			\
    case return_code:							\
      {									\
	extern long entry();						\
	compiled_code_restart();					\
	Export_Registers();						\
	Which_Way = entry();						\
	goto return_from_compiled_code;					\
      }

      define_compiler_restart (RC_COMP_INTERRUPT_RESTART,
			       comp_interrupt_restart)

      define_compiler_restart (RC_COMP_LEXPR_INTERRUPT_RESTART,
			       comp_lexpr_interrupt_restart)

      define_compiler_restart (RC_COMP_LOOKUP_APPLY_RESTART,
			       comp_lookup_apply_restart)

      define_compiler_restart (RC_COMP_REFERENCE_RESTART,
			       comp_reference_restart)

      define_compiler_restart (RC_COMP_ACCESS_RESTART,
			       comp_access_restart)

      define_compiler_restart (RC_COMP_UNASSIGNED_P_RESTART,
			       comp_unassigned_p_restart)

      define_compiler_restart (RC_COMP_UNBOUND_P_RESTART,
			       comp_unbound_p_restart)

      define_compiler_restart (RC_COMP_ASSIGNMENT_RESTART,
			       comp_assignment_restart)

      define_compiler_restart (RC_COMP_DEFINITION_RESTART,
			       comp_definition_restart)

      define_compiler_restart (RC_COMP_SAFE_REFERENCE_RESTART,
			       comp_safe_reference_restart)

      define_compiler_restart (RC_COMP_CACHE_LOOKUP_RESTART,
			       comp_cache_lookup_restart)

      define_compiler_restart (RC_COMP_LOOKUP_TRAP_RESTART,
			       comp_lookup_trap_restart)

      define_compiler_restart (RC_COMP_CACHE_ASSIGN_RESTART,
			       comp_cache_assignment_restart)

      define_compiler_restart (RC_COMP_ASSIGNMENT_TRAP_RESTART,
			       comp_assignment_trap_restart)

      define_compiler_restart (RC_COMP_CACHE_OPERATOR_RESTART,
			       comp_cache_operator_restart)

      define_compiler_restart (RC_COMP_OP_REF_TRAP_RESTART,
			       comp_op_ref_trap_restart)

      define_compiler_restart (RC_COMP_CACHE_REF_APPLY_RESTART,
			       comp_cache_ref_apply_restart)

      define_compiler_restart (RC_COMP_SAFE_REF_TRAP_RESTART,
			       comp_safe_ref_trap_restart)

      define_compiler_restart (RC_COMP_UNASSIGNED_TRAP_RESTART,
			       comp_unassigned_p_trap_restart)

    case RC_REENTER_COMPILED_CODE:
      compiled_code_restart();
      Export_Registers();
      Which_Way = return_to_compiled_code();
      goto return_from_compiled_code;

    case RC_CONDITIONAL_DECIDE:
      Pop_Return_Val_Check();
      End_Subproblem();
      Restore_Env();
      Reduces_To_Nth((Val==NIL)? COND_ALTERNATIVE : COND_CONSEQUENT);

    case RC_DISJUNCTION_DECIDE:
      /* Return predicate if it isn't NIL; else do ALTERNATIVE */
      Pop_Return_Val_Check();
      End_Subproblem();
      Restore_Env();
      if (Val != NIL) goto Pop_Return;
      Reduces_To_Nth(OR_ALTERNATIVE);

    case RC_END_OF_COMPUTATION:
      /* Signals bottom of stack */
      Export_Registers();
      Microcode_Termination(TERM_END_OF_COMPUTATION);
 
    case RC_EVAL_ERROR:
      /* Should be called RC_REDO_EVALUATION. */
      Store_Env(Pop());
      Reduces_To(Fetch_Expression());

    case RC_EXECUTE_ACCESS_FINISH:
    {
      long Result;
      Pointer value;

      Pop_Return_Val_Check();
      value = Val;

      if (Environment_P(Val))
      { Result = Symbol_Lex_Ref(value,
				Fast_Vector_Ref(Fetch_Expression(),
						ACCESS_NAME));
	Import_Val();
	if (Result == PRIM_DONE)
	{
	  End_Subproblem();
	  break;
	}
	if (Result != PRIM_INTERRUPT)
	{
	  Val = value;
	  Pop_Return_Error(Result);
	}
	Prepare_Pop_Return_Interrupt(RC_EXECUTE_ACCESS_FINISH, value);
	Interrupt(PENDING_INTERRUPTS());
      }
      Val = value;
      Pop_Return_Error(ERR_BAD_FRAME);
    }

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_EXECUTE_ASSIGNMENT_FINISH:
    {
      long temp;
      Pointer value;
      Lock_Handle set_serializer;

#ifndef No_In_Line_Lookup

      Pointer bogus_unassigned;
      fast Pointer *cell;

      Set_Time_Zone(Zone_Lookup);
      Restore_Env();
      cell = Get_Pointer(Vector_Ref(Fetch_Expression(), ASSIGN_NAME));
      lookup(cell, Fetch_Env(), cell, repeat_assignment_lookup);

      value = Val;
      bogus_unassigned = Get_Fixed_Obj_Slot(Non_Object);
      if (value == bogus_unassigned)
	value = UNASSIGNED_OBJECT;

assignment_end_before_lock:

      setup_lock(set_serializer, cell);

assignment_end_after_lock:

      Val = *cell;

      if (Type_Code(*cell) != TC_REFERENCE_TRAP)
      {
normal_assignment_done:
	*cell = value;
	remove_lock(set_serializer);
	Set_Time_Zone(Zone_Working);
	End_Subproblem();
	goto Pop_Return;
      }

/* Interpret() continues on the next page */

/* Interpret(), continued */

      get_trap_kind(temp, *cell);
      switch(temp)
      {
	case TRAP_DANGEROUS:
	case TRAP_UNBOUND_DANGEROUS:
	case TRAP_UNASSIGNED_DANGEROUS:
	case TRAP_FLUID_DANGEROUS:
	case TRAP_COMPILER_CACHED_DANGEROUS:
	  remove_lock(set_serializer);
	  cell = Get_Pointer(Vector_Ref(Fetch_Expression(), ASSIGN_NAME));
	  temp =
	    deep_assignment_end(deep_lookup(Fetch_Env(),
					    cell[VARIABLE_SYMBOL],
					    cell),
				cell,
				value,
				false);
external_assignment_return:
	  Import_Val();
	  if (temp != PRIM_DONE)
	    break;
	  Set_Time_Zone(Zone_Working);
	  End_Subproblem();
	  goto Pop_Return;

	case TRAP_COMPILER_CACHED:
	{
	  Pointer extension, references;

	  extension = Fast_Vector_Ref(Val, TRAP_EXTRA);
	  references = Fast_Vector_Ref(extension, TRAP_EXTENSION_REFERENCES);

	  if (Fast_Vector_Ref(references, TRAP_REFERENCES_OPERATOR) != NIL)
	  {

	    /* There are uuo links.
	       wimp out and let deep_assignment_end handle it.
	     */

	    remove_lock(set_serializer);
	    temp = deep_assignment_end(cell,
				       fake_variable_object,
				       value,
				       false);
	    goto external_assignment_return;
	  }
	  cell = Nth_Vector_Loc(extension, TRAP_EXTENSION_CELL);
	  update_lock(set_serializer, cell);
	  goto assignment_end_after_lock;
	}	  

/* Interpret() continues on the next page */

/* Interpret(), continued */

	case TRAP_FLUID:
	  remove_lock(set_serializer);
	  cell = lookup_fluid(Val);
	  goto assignment_end_before_lock;

	case TRAP_UNBOUND:
	  remove_lock(set_serializer);
	  temp = ERR_UNBOUND_VARIABLE;
	  break;

	case TRAP_UNASSIGNED:
	  Val = bogus_unassigned;
	  goto normal_assignment_done;

	default:
	  remove_lock(set_serializer);
	  temp = ERR_ILLEGAL_REFERENCE_TRAP;
	  break;
      }

      if (value == UNASSIGNED_OBJECT)
	value = bogus_unassigned;
	
/* Interpret() continues on the next page */

/* Interpret(), continued */

#else No_In_Line_Lookup

      value = Val;
      Set_Time_Zone(Zone_Lookup);
      Restore_Env();
      temp = Lex_Set(Fetch_Env(),
		     Vector_Ref(Fetch_Expression(), ASSIGN_NAME),
		     value);
      Import_Val();
      if (temp == PRIM_DONE) 
      {
	End_Subproblem();
	Set_Time_Zone(Zone_Working);
	break;
      }

#endif No_In_Line_Lookup

      Set_Time_Zone(Zone_Working);
      Save_Env();
      if (temp != PRIM_INTERRUPT)
      {
	Val = value;
	Pop_Return_Error(temp);
      }

      Prepare_Pop_Return_Interrupt(RC_EXECUTE_ASSIGNMENT_FINISH,
				   value);
      Interrupt(PENDING_INTERRUPTS());
    }
      
/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_EXECUTE_DEFINITION_FINISH:
      {
	Pointer value;
        long result;

	value = Val;
        Restore_Env();
	Export_Registers();
        result = Local_Set(Fetch_Env(),
			   Fast_Vector_Ref(Fetch_Expression(), DEFINE_NAME),
			   Val);
        Import_Registers();
        if (result == PRIM_DONE)
        {
	  End_Subproblem();
          break;
	}
	Save_Env();
	if (result == PRIM_INTERRUPT)
	{
	  Prepare_Pop_Return_Interrupt(RC_EXECUTE_DEFINITION_FINISH,
				       value);
	  Interrupt(PENDING_INTERRUPTS());
	}
	Val = value;
        Pop_Return_Error(result);
      }

    case RC_EXECUTE_IN_PACKAGE_CONTINUE:
      Pop_Return_Val_Check();
      if (Environment_P(Val))
      {
	End_Subproblem();
        Store_Env(Val);
        Reduces_To_Nth(IN_PACKAGE_EXPRESSION);
      }
      Pop_Return_Error(ERR_BAD_FRAME);

#ifdef COMPILE_FUTURES
    case RC_FINISH_GLOBAL_INT:
      Export_Registers();
      Val = Global_Int_Part_2(Fetch_Expression(), Val);
      Import_Registers_Except_Val();
      break;
#endif

    case RC_HALT:
      Export_Registers();
      Microcode_Termination(TERM_TERM_HANDLER);


/* Internal_Apply, the core of the application mechanism.
   Branch here to perform a function application.  

   At this point the top of the stack contains an application frame
   which consists of the following elements (see sdata.h):
   - A header specifying the frame length.
   - A procedure.
   - The actual (evaluated) arguments.

   No registers (except the stack pointer) are meaning full at this point.
   Before interrupts or errors are processed, some registers are cleared
   to avoid holding onto garbage if a garbage collection occurs.
*/

#define Prepare_Apply_Interrupt()					\
{									\
  Store_Return(RC_INTERNAL_APPLY);					\
  Store_Expression(NIL);						\
  Save_Cont();								\
}
                          
#define Apply_Error(N)							\
{									\
  Store_Return(RC_INTERNAL_APPLY);					\
  Store_Expression(NIL);						\
  Val = NIL;								\
  Pop_Return_Error(N);							\
}

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_INTERNAL_APPLY:
Internal_Apply:

      if (Microcode_Does_Stepping && Trapping &&
	  (Fetch_Apply_Trapper() != NIL))
      {
	long Count;

	Count = Get_Integer(Stack_Ref(STACK_ENV_HEADER));
        Top_Of_Stack() = Fetch_Apply_Trapper();
        Push(STACK_FRAME_HEADER + Count);
        Stop_Trapping();
      }      

Apply_Non_Trapping:

      if ((PENDING_INTERRUPTS()) != 0)
      {
	long Interrupts;

	Interrupts = (PENDING_INTERRUPTS());
	Store_Expression(NIL);
	Val = NIL;
	Prepare_Apply_Interrupt();
	Interrupt(Interrupts);
      }

Perform_Application:

      Apply_Ucode_Hook();

      { 
        fast Pointer Function;

	Apply_Future_Check(Function, Stack_Ref(STACK_ENV_FUNCTION));

        switch(Type_Code(Function))
        { 

/* Interpret() continues on the next page */

/* Interpret(), continued */

	  case TC_PROCEDURE:
	  {
	    fast long nargs;

            nargs = Get_Integer(Pop());
	    Function = Fast_Vector_Ref(Function, PROCEDURE_LAMBDA_EXPR);

	    {
	      fast Pointer formals;

	      Apply_Future_Check(formals,
				 Fast_Vector_Ref(Function, LAMBDA_FORMALS));

	      if ((nargs != Vector_Length(formals)) &&
		  ((Type_Code(Function) != TC_LEXPR) ||
		  (nargs < Vector_Length(formals))))
	      {
		Push(STACK_FRAME_HEADER + nargs - 1);
		Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
	      }
	    }

	    if (Eval_Debug) 
	    {
	      Print_Expression(Make_Unsigned_Fixnum(nargs),
			       "APPLY: Number of arguments");
	    }

            if (GC_Check(nargs + 1))
            {
	      Push(STACK_FRAME_HEADER + nargs - 1);
              Prepare_Apply_Interrupt();
              Immediate_GC(nargs + 1);
            }

	    {
	      fast Pointer *scan;

	      scan = Free;
	      Store_Env(Make_Pointer(TC_ENVIRONMENT, scan));
	      *scan++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, nargs);
	      while(--nargs >= 0)
		*scan++ = Pop();
	      Free = scan;
	      Reduces_To(Fast_Vector_Ref(Function, LAMBDA_SCODE));
	    }
          }

/* Interpret() continues on the next page */

/* Interpret(), continued */

          case TC_CONTROL_POINT:
	  {
            if (Get_Integer(Stack_Ref(STACK_ENV_HEADER)) !=
                STACK_ENV_FIRST_ARG)
	    {
              Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
	    }
            Val = Stack_Ref(STACK_ENV_FIRST_ARG);
            Our_Throw(false, Function);
	    Apply_Stacklet_Backout();
	    Our_Throw_Part_2();
            goto Pop_Return;
	  }

/* Interpret() continues on the next page */

/* Interpret(), continued */

	  /*
	     After checking the number of arguments, remove the
	     frame header since primitives do not expect it.

	     NOTE: This code must match the application code which
	     follows Primitive_Internal_Apply.
	   */

          case TC_PRIMITIVE:
          { 
	    fast long nargs;

	    if (!IMPLEMENTED_PRIMITIVE_P(Function))
	    {
	      Apply_Error(ERR_UNIMPLEMENTED_PRIMITIVE);
	    }

	    /* Note that the first test below will fail for lexpr primitives. */
 
	    nargs = ((OBJECT_DATUM(Stack_Ref(STACK_ENV_HEADER))) -
		     (STACK_ENV_FIRST_ARG - 1));
            if (nargs != PRIMITIVE_ARITY(Function))
	    {
	      if (PRIMITIVE_ARITY(Function) != LEXPR_PRIMITIVE_ARITY)
	      {
		Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
	      }
	      Regs[REGBLOCK_LEXPR_ACTUALS] = ((Pointer) nargs);
	    }

            Stack_Pointer = Simulate_Popping(STACK_ENV_FIRST_ARG);
            Store_Expression(Function);

	    Export_Regs_Before_Primitive();
	    Metering_Apply_Primitive(Val, Function);
	    Import_Regs_After_Primitive();

	    Pop_Primitive_Frame(nargs);
	    if (Must_Report_References())
	    {
	      Store_Expression(Val);
	      Store_Return(RC_RESTORE_VALUE);
	      Save_Cont();
	      Call_Future_Logging();
	    }
	    goto Pop_Return;
	  }

/* Interpret() continues on the next page */

/* Interpret(), continued */

          case TC_EXTENDED_PROCEDURE:
          {
	    Pointer lambda;
            long nargs, nparams, formals, params, auxes,
                 rest_flag, size;

	    fast long i;
	    fast Pointer *scan;

            nargs = Get_Integer(Pop()) - STACK_FRAME_HEADER;

	    if (Eval_Debug) 
	    {
	      Print_Expression(Make_Unsigned_Fixnum(nargs+STACK_FRAME_HEADER),
			       "APPLY: Number of arguments");
	    }

            lambda = Fast_Vector_Ref(Function, PROCEDURE_LAMBDA_EXPR);
	    Apply_Future_Check(Function,
			       Fast_Vector_Ref(lambda, ELAMBDA_NAMES));
            nparams = Vector_Length(Function) - 1;

	    Apply_Future_Check(Function, Get_Count_Elambda(lambda));
            formals = Elambda_Formals_Count(Function);
            params = Elambda_Opts_Count(Function) + formals;
            rest_flag = Elambda_Rest_Flag(Function);
            auxes = nparams - (params + rest_flag);

            if ((nargs < formals) || (!rest_flag && (nargs > params)))
            {
	      Push(STACK_FRAME_HEADER + nargs);
              Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
            }

	    /* size includes the procedure slot, but not the header. */
            size = params + rest_flag + auxes + 1;
            if (GC_Check(size + 1 + ((nargs > params) ?
				     (2 * (nargs - params)) :
				     0)))
            {
	      Push(STACK_FRAME_HEADER + nargs);
              Prepare_Apply_Interrupt();
              Immediate_GC(size + 1 + ((nargs > params) ?
				       (2 * (nargs - params)) :
				       0));
            }

/* Interpret() continues on the next page */

/* Interpret(), continued */

	    scan = Free;
            Store_Env(Make_Pointer(TC_ENVIRONMENT, scan));
	    *scan++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, size);

	    if (nargs <= params)
	    {
	      for (i = (nargs + 1); --i >= 0; )
		*scan++ = Pop();
	      for (i = (params - nargs); --i >= 0; )
		*scan++ = UNASSIGNED_OBJECT;
	      if (rest_flag)
		*scan++ = NIL;
	      for (i = auxes; --i >= 0; )
		*scan++ = UNASSIGNED_OBJECT;
	    }
	    else
	    {
	      /* rest_flag must be true. */
	      Pointer list;
	      
	      list = Make_Pointer(TC_LIST, (scan + size));
	      for (i = (params + 1); --i >= 0; )
		*scan++ = Pop();
	      *scan++ = list;
	      for (i = auxes; --i >= 0; )
		*scan++ = UNASSIGNED_OBJECT;
	      /* Now scan == Get_Pointer(list) */
	      for (i = (nargs - params); --i >= 0; )
	      {
		*scan++ = Pop();
		*scan = Make_Pointer(TC_LIST, (scan + 1));
		scan += 1;
	      }
	      scan[-1] = NIL;
	    }

	    Free = scan;
            Reduces_To(Get_Body_Elambda(lambda));
          }

/* Interpret() continues on the next page */

/* Interpret(), continued */

          case TC_COMPILED_PROCEDURE:
	  {
	    apply_compiled_setup(STACK_ENV_EXTRA_SLOTS +
				 Get_Integer( Stack_Ref( STACK_ENV_HEADER)));
	    Export_Registers();
	    Which_Way = apply_compiled_procedure();

return_from_compiled_code:
	    Import_Registers();
            switch (Which_Way)
            {
	    case PRIM_DONE:
	    { compiled_code_done();
	      goto Pop_Return;
	    }

	    case PRIM_APPLY:
	    { compiler_apply_procedure(STACK_ENV_EXTRA_SLOTS +
				       Get_Integer( Stack_Ref( STACK_ENV_HEADER)));
	      goto Internal_Apply;
	    }

	    case ERR_COMPILED_CODE_ERROR:
	    { /* The compiled code is signalling a microcode error. */
	      compiled_error_backout();
	      /* The Save_Cont is done by Pop_Return_Error. */
	      Pop_Return_Error( compiled_code_error_code);
	    }

	    case PRIM_INTERRUPT:
	    {
	      compiled_error_backout();
	      Save_Cont();
	      Interrupt(PENDING_INTERRUPTS());
	    }

	    case ERR_WRONG_NUMBER_OF_ARGUMENTS:
	    {
	      apply_compiled_backout();
	      Apply_Error( Which_Way);
	    }

	    case ERR_UNIMPLEMENTED_PRIMITIVE:
	    {
	      /* This error code means that compiled code
		 attempted to call an unimplemented primitive.
	       */

	      BACK_OUT_AFTER_PRIMITIVE();
	      Pop_Return_Error( ERR_UNIMPLEMENTED_PRIMITIVE);
	    }

	    case ERR_EXECUTE_MANIFEST_VECTOR:
	    { /* This error code means that enter_compiled_expression
		 was called in a system without compiler support.
	       */
	      execute_compiled_backout();
	      Val = Make_Non_Pointer( TC_COMPILED_EXPRESSION,
				     Fetch_Expression());
	      Pop_Return_Error( Which_Way);
	    }

	    case ERR_INAPPLICABLE_OBJECT:
	    { /* This error code means that apply_compiled_procedure
		 was called in a system without compiler support.
	       */
	      apply_compiled_backout();
	      Apply_Error( Which_Way);
	    }

	    case ERR_INAPPLICABLE_CONTINUATION:
	    { /* This error code means that return_to_compiled_code
		 or some other compiler continuation was called in a
		 system without compiler support.
	       */
	      Store_Expression(NIL);
	      Store_Return(RC_REENTER_COMPILED_CODE);
	      Pop_Return_Error(Which_Way);
	    }

	    default: Microcode_Termination( TERM_COMPILER_DEATH);
            }
          }

          default:
            Apply_Error(ERR_INAPPLICABLE_OBJECT);
        }       /* End of switch in RC_INTERNAL_APPLY */
      }         /* End of RC_INTERNAL_APPLY case */

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_MOVE_TO_ADJACENT_POINT:
    /* Expression contains the space in which we are moving */
    { long From_Count = Get_Integer(Stack_Ref(TRANSLATE_FROM_DISTANCE));
      Pointer Thunk, New_Location;
      if (From_Count != 0)
      { Pointer Current = Stack_Ref(TRANSLATE_FROM_POINT);
	Stack_Ref(TRANSLATE_FROM_DISTANCE) = Make_Unsigned_Fixnum((From_Count - 1));
	Thunk = Fast_Vector_Ref(Current, STATE_POINT_AFTER_THUNK);
	New_Location = Fast_Vector_Ref(Current, STATE_POINT_NEARER_POINT);
	Stack_Ref(TRANSLATE_FROM_POINT) = New_Location;
	if ((From_Count == 1) &&
	    (Stack_Ref(TRANSLATE_TO_DISTANCE) == Make_Unsigned_Fixnum(0)))
	  Stack_Pointer = Simulate_Popping(4);
	else Save_Cont();
      }
      else
      { long To_Count = Get_Integer(Stack_Ref(TRANSLATE_TO_DISTANCE))-1;
	fast Pointer To_Location = Stack_Ref(TRANSLATE_TO_POINT);
	fast long i;
	for (i=0; i < To_Count; i++)
	  To_Location = Fast_Vector_Ref(To_Location, STATE_POINT_NEARER_POINT);
	Thunk = Fast_Vector_Ref(To_Location, STATE_POINT_BEFORE_THUNK);
	New_Location = To_Location;
	Stack_Ref(TRANSLATE_TO_DISTANCE) = Make_Unsigned_Fixnum(To_Count);
	if (To_Count==0) 
	  Stack_Pointer = Simulate_Popping(4);
	else Save_Cont();
      }
      if (Fetch_Expression() != NIL)
        Vector_Set(Fetch_Expression(), STATE_SPACE_NEAREST_POINT, New_Location);
      else Current_State_Point = New_Location;
     Will_Push(2);
      Push(Thunk);
      Push(STACK_FRAME_HEADER);
     Pushed();
      goto Internal_Apply;
    }

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_INVOKE_STACK_THREAD:
      /* Used for WITH_THREADED_STACK primitive */
     Will_Push(3);
      Push(Val);        /* Value calculated by thunk */
      Push(Fetch_Expression());
      Push(STACK_FRAME_HEADER+1);
     Pushed();
      goto Internal_Apply;

    case RC_JOIN_STACKLETS:
      Our_Throw(true, Fetch_Expression());
      Join_Stacklet_Backout();
      Our_Throw_Part_2();
      break;

    case RC_NORMAL_GC_DONE:
      End_GC_Hook();
      if (GC_Space_Needed < 0)
      {
	/* Paranoia */

	GC_Space_Needed = 0;
      }
      if (GC_Check(GC_Space_Needed))
      {
	Microcode_Termination(TERM_GC_OUT_OF_SPACE);
      }
      GC_Space_Needed = 0;
      Val = Fetch_Expression();
      break;

    case RC_PCOMB1_APPLY:
      End_Subproblem();
      Push(Val);		/* Argument value */
     Finished_Eventual_Pushing(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      Store_Expression(Fast_Vector_Ref(Fetch_Expression(), PCOMB1_FN_SLOT));

Primitive_Internal_Apply:
      if (Microcode_Does_Stepping && Trapping &&
	  (Fetch_Apply_Trapper() != NIL))
      {
	/* Does this work in the stacklet case?
	   We may have a non-contiguous frame. -- Jinx
	 */
       Will_Push(3); 
        Push(Fetch_Expression());
        Push(Fetch_Apply_Trapper());
        Push(STACK_FRAME_HEADER + 1 +
	     PRIMITIVE_N_PARAMETERS(Fetch_Expression()));
       Pushed();
        Stop_Trapping();
	goto Apply_Non_Trapping;
      }

      /* NOTE: This code must match the code in the TC_PRIMITIVE
	 case of Internal_Apply.
	 This code is simpler because:
	 1) The arity was checked at syntax time.
	 2) We don't have to deal with "lexpr" primitives.
	 3) We don't need to worry about unimplemented primitives because
	    unimplemented primitives will cause an error at invocation.
       */

      {
	fast Pointer primitive;

	primitive = Fetch_Expression();
	Export_Regs_Before_Primitive();
	Metering_Apply_Primitive(Val, primitive);
	Import_Regs_After_Primitive();

	Pop_Primitive_Frame(PRIMITIVE_ARITY(primitive));
	if (Must_Report_References())
	{
	  Store_Expression(Val);
	  Store_Return(RC_RESTORE_VALUE);
	  Save_Cont();
	  Call_Future_Logging();
	}
	break;
      }

    case RC_PCOMB2_APPLY:
      End_Subproblem();
      Push(Val);		/* Value of arg. 1 */
     Finished_Eventual_Pushing(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      Store_Expression(Fast_Vector_Ref(Fetch_Expression(), PCOMB2_FN_SLOT));
      goto Primitive_Internal_Apply;

    case RC_PCOMB2_DO_1:
      Restore_Env();
      Push(Val);		/* Save value of arg. 2 */
      Do_Another_Then(RC_PCOMB2_APPLY, PCOMB2_ARG_1_SLOT);

    case RC_PCOMB3_APPLY:
      End_Subproblem();
      Push(Val);		/* Save value of arg. 1 */
     Finished_Eventual_Pushing(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      Store_Expression(Fast_Vector_Ref(Fetch_Expression(), PCOMB3_FN_SLOT));
      goto Primitive_Internal_Apply;

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_PCOMB3_DO_1:
    {
      Pointer Temp;

      Temp = Pop();		/* Value of arg. 3 */
      Restore_Env();
      Push(Temp);		/* Save arg. 3 again */
      Push(Val);		/* Save arg. 2 */
      Do_Another_Then(RC_PCOMB3_APPLY, PCOMB3_ARG_1_SLOT);
    }

    case RC_PCOMB3_DO_2:
      Restore_Then_Save_Env();
      Push(Val);		/* Save value of arg. 3 */
      Do_Another_Then(RC_PCOMB3_DO_1, PCOMB3_ARG_2_SLOT);

    case RC_POP_RETURN_ERROR:
    case RC_RESTORE_VALUE:
      Val = Fetch_Expression();
      break;

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_PURIFY_GC_1:
    {
      Pointer GC_Daemon_Proc, Result;

      Export_Registers();
      Result = Purify_Pass_2(Fetch_Expression());
      Import_Registers();
      if (Result == NIL)
      {
	/* The object does not fit in Constant space.
	   There is no need to run the daemons, and we should let the runtime
	   system know what happened.
	 */
	Val = NIL;
        break;
      }
      GC_Daemon_Proc = Get_Fixed_Obj_Slot(GC_Daemon);
      if (GC_Daemon_Proc == NIL)
      {
	Val = TRUTH;
        break;
      }
      Store_Expression(NIL);
      Store_Return(RC_PURIFY_GC_2);
      Save_Cont();
     Will_Push(2);
      Push(GC_Daemon_Proc);
      Push(STACK_FRAME_HEADER);
     Pushed();
      goto Internal_Apply;
    }

    case RC_PURIFY_GC_2:
      Val = TRUTH;
      break;

    case RC_REPEAT_DISPATCH:
      Sign_Extend(Fetch_Expression(), Which_Way);
      Restore_Env();
      Val = Pop();
      Restore_Cont();
      goto Repeat_Dispatch;

/* Interpret() continues on the next page */

/* Interpret(), continued */

/* The following two return codes are both used to restore
   a saved history object.  The difference is that the first
   does not copy the history object while the second does.
   In both cases, the Expression register contains the history
   object and the next item to be popped off the stack contains
   the offset back to the previous restore history return code.

   ASSUMPTION: History objects are never created using futures.
*/

    case RC_RESTORE_DONT_COPY_HISTORY:
    {
      Pointer Stacklet;

      Prev_Restore_History_Offset = Get_Integer(Pop());
      Stacklet = Pop();
      History = Get_Pointer(Fetch_Expression());
      if (Prev_Restore_History_Offset == 0)
      {
	Prev_Restore_History_Stacklet = NULL;
      }
      else if (Stacklet == NIL)
      {
        Prev_Restore_History_Stacklet = NULL;
      }
      else
      {
	Prev_Restore_History_Stacklet = Get_Pointer(Stacklet);
      }
      break;
    }

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_RESTORE_HISTORY:
    {
      Pointer Stacklet;

      Export_Registers();
      if (! Restore_History(Fetch_Expression()))
      {
	Import_Registers();
        Save_Cont();
       Will_Push(CONTINUATION_SIZE);
        Store_Expression(Val);
        Store_Return(RC_RESTORE_VALUE);
        Save_Cont();
       Pushed();
        Immediate_GC((Free > MemTop) ? 0 : ((MemTop-Free)+1));
      }
      Import_Registers();
      Prev_Restore_History_Offset = Get_Integer(Pop());
      Stacklet = Pop();
      if (Prev_Restore_History_Offset == 0)
	Prev_Restore_History_Stacklet = NULL;
      else
      { if (Stacklet == NIL)
        { Prev_Restore_History_Stacklet = NULL;
	  Get_End_Of_Stacklet()[-Prev_Restore_History_Offset] =
            Make_Non_Pointer(TC_RETURN_CODE, RC_RESTORE_HISTORY);
        }
        else
	{ Prev_Restore_History_Stacklet = Get_Pointer(Stacklet);
	  Prev_Restore_History_Stacklet[-Prev_Restore_History_Offset] =
            Make_Non_Pointer(TC_RETURN_CODE, RC_RESTORE_HISTORY);
        }
      }
      break;
    }

    case RC_RESTORE_FLUIDS:
      Fluid_Bindings = Fetch_Expression();
      /* Why is this here? -- Jinx */ 
      COMPILER_SETUP_INTERRUPT();
      break;

    case RC_RESTORE_INT_MASK: 
      SET_INTERRUPT_MASK(Get_Integer(Fetch_Expression()));
      break;

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_RESTORE_TO_STATE_POINT:
    { Pointer Where_To_Go = Fetch_Expression();
     Will_Push(CONTINUATION_SIZE);
      /* Restore the contents of Val after moving to point */
      Store_Expression(Val);
      Store_Return(RC_RESTORE_VALUE);
      Save_Cont();
     Pushed();
      Export_Registers();
      Translate_To_Point(Where_To_Go);
      break;			/* We never get here.... */
    }

    case RC_RETURN_TRAP_POINT:
      Store_Return(Old_Return_Code);
     Will_Push(CONTINUATION_SIZE+3);
      Save_Cont();
      Return_Hook_Address = NULL;
      Stop_Trapping();
      Push(Val);
      Push(Fetch_Return_Trapper());
      Push(STACK_FRAME_HEADER+1);
     Pushed();
      goto Apply_Non_Trapping;

    case RC_SEQ_2_DO_2:
      End_Subproblem();
      Restore_Env();
      Reduces_To_Nth(SEQUENCE_2);

    case RC_SEQ_3_DO_2:
      Restore_Then_Save_Env();
      Do_Another_Then(RC_SEQ_3_DO_3, SEQUENCE_2);

    case RC_SEQ_3_DO_3:
      End_Subproblem();
      Restore_Env();
      Reduces_To_Nth(SEQUENCE_3);

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_SNAP_NEED_THUNK:
      Vector_Set(Fetch_Expression(), THUNK_SNAPPED, TRUTH);
      Vector_Set(Fetch_Expression(), THUNK_VALUE, Val);
      break;

    case RC_AFTER_MEMORY_UPDATE:
    case RC_BAD_INTERRUPT_CONTINUE:
    case RC_COMPLETE_GC_DONE:
    case RC_RESTARTABLE_EXIT:
    case RC_RESTART_EXECUTION:
    case RC_RESTORE_CONTINUATION:
    case RC_RESTORE_STEPPER:
    case RC_POP_FROM_COMPILED_CODE:
      Export_Registers();
      Microcode_Termination(TERM_UNIMPLEMENTED_CONTINUATION);

    SITE_RETURN_DISPATCH_HOOK()

    default:
      Export_Registers();
      Microcode_Termination(TERM_NON_EXISTENT_CONTINUATION);
  };
  goto Pop_Return;
}
