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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/interp.c,v 9.21 1987/01/22 14:27:50 jinx Exp $
 *
 * This file contains the heart of the Scheme Scode
 * interpreter
 *
 */

#define In_Main_Interpreter	true
#include "scheme.h"
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

#define Interrupt(Masked_Code) 						\
        { Export_Registers();						\
          Setup_Interrupt(Masked_Code);					\
          Import_Registers();						\
          goto Perform_Application;					\
        }

#define Immediate_GC(N)							\
	{ Request_GC(N);						\
	  Interrupt(IntCode & IntEnb);					\
        }
        
#define Prepare_Eval_Repeat()						\
	{Will_Push(CONTINUATION_SIZE+1);				\
          Push(Fetch_Env());						\
	  Store_Return(RC_EVAL_ERROR);					\
	  Save_Cont();							\
	 Pushed();							\
	}

#define Eval_GC_Check(Amount)						\
	if (GC_Check(Amount))						\
	{ Prepare_Eval_Repeat();					\
          Immediate_GC(Amount);						\
	}

#define Eval_Error(Err)							\
	{ Export_Registers();						\
	  Do_Micro_Error(Err, false);					\
	  Import_Registers();						\
	  goto Internal_Apply;						\
        }

#define Pop_Return_Error(Err)						\
	{ Export_Registers();						\
	  Do_Micro_Error(Err, true);					\
	  Import_Registers();						\
	  goto Internal_Apply;						\
        }
 
#define Prepare_Pop_Return_Interrupt(Return_Code, Contents_of_Val)	\
	Store_Return(Return_Code);					\
        Val = Contents_of_Val;						\
        Save_Cont()

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

/* This makes local variable references faster */

#if (LOCAL_REF == 0)
#define Local_Offset(Ind) Ind
#else
#define Local_Offset(Ind) Get_Integer(Ind)
#endif

#ifdef COMPILE_FUTURES
#define Splice_Future_Value(The_Loc)					\
{ while ((Type_Code(Val) == TC_FUTURE) && (Future_Spliceable(Val)))	\
  { Pointer *Location;							\
    Val = Future_Value(Val);						\
    Location = The_Loc;							\
    if Dangerous(*Location) Set_Danger_Bit(Val);			\
    *Location = Val;							\
    Clear_Danger_Bit(Val);						\
  }									\
  Set_Time_Zone(Zone_Working);						\
  break;								\
}
#else
#define Splice_Future_Value(The_Loc)					\
{ Set_Time_Zone(Zone_Working);						\
  break;								\
}
#endif

#ifdef TRAP_ON_REFERENCE
#define Trap(Value)	(Safe_Type_Code(Value) == TC_TRAP)
#else
#define Trap(Value)	false
#endif

#define MAGIC_RESERVE_SIZE	6	/* See SPMD.SCM */
#define Reserve_Stack_Space()	Will_Eventually_Push(MAGIC_RESERVE_SIZE)

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

/* Arg_Type_Error handles the error returns from primitives which type check
   their arguments and restarts them or suspends if the argument is a future. */

#define Arg_Type_Error(Arg_No, Err_No)					\
{ fast Pointer *Arg = &(Stack_Ref(Arg_No-1));				\
  fast Pointer Orig_Arg = *Arg;						\
  if (Type_Code(*Arg) != TC_FUTURE) Pop_Return_Error(Err_No);		\
  while ((Type_Code(*Arg) == TC_FUTURE) && (Future_Has_Value(*Arg)))	\
  { if (Future_Is_Keep_Slot(*Arg)) Log_Touch_Of_Future(*Arg);		\
    *Arg = Future_Value(*Arg);						\
  }									\
  if (Type_Code(*Arg) != TC_FUTURE) goto Prim_No_Trap_Apply;		\
  Save_Cont();								\
 Will_Push(STACK_ENV_EXTRA_SLOTS+2);					\
  Push(*Arg);			/* Arg 1: The future itself */		\
  Push(Get_Fixed_Obj_Slot(System_Scheduler));				\
  Push(STACK_FRAME_HEADER+1);						\
 Pushed();								\
  *Arg = Orig_Arg;							\
  goto Apply_Non_Trapping;						\
}

/* Apply_Future_Check is called at apply time to guarantee that certain
   objects (the procedure itself, and its LAMBDA components for user defined
   procedures) are not futures
*/

#define Apply_Future_Check(Name, Object)				\
{ fast Pointer *Arg = &(Object);					\
  fast Pointer Orig_Answer = *Arg;                              	\
  while (Type_Code(*Arg) == TC_FUTURE)					\
  { if (Future_Has_Value(*Arg))						\
    { if (Future_Is_Keep_Slot(*Arg)) Log_Touch_Of_Future(*Arg); 	\
      *Arg = Future_Value(*Arg);					\
    }                                                           	\
    else								\
    {									\
     Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS+2));		\
      Store_Return(RC_INTERNAL_APPLY);					\
      Val = NIL;							\
      Save_Cont();							\
      Push(*Arg);							\
      Push(Get_Fixed_Obj_Slot(System_Scheduler));			\
      Push(STACK_FRAME_HEADER+1);					\
     Pushed();								\
      *Arg = Orig_Answer;                                       	\
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
{ fast Pointer Orig_Val = Val;						\
  while (Type_Code(Val) == TC_FUTURE)					\
  { if (Future_Has_Value(Val))						\
    { if (Future_Is_Keep_Slot(Val)) Log_Touch_Of_Future(Val);   	\
      Val = Future_Value(Val);						\
    }                                                           	\
    else								\
    { Save_Cont();							\
     Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS+2));		\
      Store_Return(RC_RESTORE_VALUE);					\
      Store_Expression(Orig_Val);					\
      Save_Cont();							\
      Push(Val);							\
      Push(Get_Fixed_Obj_Slot(System_Scheduler));			\
      Push(STACK_FRAME_HEADER+1);					\
     Pushed();								\
      goto Internal_Apply;						\
    }									\
  }									\
}

#else			/* Not compiling FUTURES code */
#define Pop_Return_Val_Check()		
#define Apply_Future_Check(Name, Object)	Name = (Object)
#define Arg_Type_Error(Arg_No, Err_No)		Pop_Return_Error(Err_No)
#endif

/* The EVAL/APPLY ying/yang */

void
Interpret(dumped_p)
     Boolean dumped_p;
{ long Which_Way;
  fast Pointer Reg_Val, Reg_Expression, *Reg_Stack_Pointer;
  extern long enter_compiled_expression();
  extern long apply_compiled_procedure();
  extern long return_to_compiled_code();

  /* Primitives jump back here for errors, requests to
   * evaluate an expression, apply a function, or handle an
   * interrupt request. On errors or interrupts they leave
   * their arguments on the stack, the primitive itself in
   * Expression, and a RESTART_PRIMITIVE continuation in the
   * return register.  In the other cases, they have removed
   * their stack frames entirely.
   */

  Which_Way = setjmp(*Back_To_Eval);
  Set_Time_Zone(Zone_Working);
  Import_Registers();
  if (Must_Report_References())
  { Save_Cont();
   Will_Push(CONTINUATION_SIZE + 2);
    Push(Val);
    Save_Env();
    Store_Return(RC_REPEAT_DISPATCH);
    Store_Expression(Make_Non_Pointer(TC_FIXNUM, Which_Way));
    Save_Cont();
   Pushed();
    Call_Future_Logging();
  }
Repeat_Dispatch:
  switch (Which_Way)
  { case PRIM_APPLY:         goto Internal_Apply;
    case PRIM_NO_TRAP_APPLY: goto Apply_Non_Trapping;
    case PRIM_DO_EXPRESSION: Reduces_To(Fetch_Expression());
    case PRIM_NO_TRAP_EVAL:  New_Reduction(Fetch_Expression(),Fetch_Env());
	                     goto Eval_Non_Trapping;
    case 0: 		     if (!dumped_p) break; /* Else fall through */
    case PRIM_POP_RETURN:    goto Pop_Return;
    default:                 Pop_Return_Error(Which_Way);
    case PRIM_INTERRUPT:
    { Save_Cont();
      Interrupt(IntCode & IntEnb);
    }
    case ERR_ARG_1_WRONG_TYPE: Arg_Type_Error(1, ERR_ARG_1_WRONG_TYPE);
    case ERR_ARG_2_WRONG_TYPE: Arg_Type_Error(2, ERR_ARG_2_WRONG_TYPE);
    case ERR_ARG_3_WRONG_TYPE: Arg_Type_Error(3, ERR_ARG_3_WRONG_TYPE);
  }

                    /*****************/
                    /* Do_Expression */
                    /*****************/

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
 * the current S-Code item is the value returned when the
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

  if (Microcode_Does_Stepping && Trapping &&
      (Fetch_Eval_Trapper() != NIL))
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
  { case TC_BIG_FIXNUM:         /* The self evaluating items */
    case TC_BIG_FLONUM:
    case TC_CHARACTER_STRING:
    case TC_CHARACTER:
    case TC_COMPILED_PROCEDURE:
    case TC_COMPLEX:
    case TC_CONTROL_POINT:
    case TC_DELAYED:
    case TC_ENVIRONMENT:
    case TC_EXTENDED_FIXNUM:
    case TC_EXTENDED_PROCEDURE:
    case TC_FIXNUM:
    case TC_HUNK3:
    case TC_LIST:
    case TC_NON_MARKED_VECTOR:
    case TC_NULL:
    case TC_PRIMITIVE:
    case TC_PRIMITIVE_EXTERNAL:
    case TC_PROCEDURE:
    case TC_UNINTERNED_SYMBOL:
    case TC_INTERNED_SYMBOL:
    case TC_TRUE: 
    case TC_UNASSIGNED:
    case TC_VECTOR:
    case TC_VECTOR_16B:
    case TC_VECTOR_1B:
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
      { long Array_Length = Vector_Length(Fetch_Expression())-1;
        Eval_GC_Check(New_Stacklet_Size(Array_Length+1+1+CONTINUATION_SIZE));
       Will_Push(Array_Length + 1+1+CONTINUATION_SIZE); /* Save_Env, Finger */
	Stack_Pointer = Simulate_Pushing(Array_Length);
        Push(Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, Array_Length));
	                        /* The finger: last argument number */
       Pushed();
        if (Array_Length == 0)
	{ Push(STACK_FRAME_HEADER);   /* Frame size */
          Do_Nth_Then(RC_COMB_APPLY_FUNCTION, COMB_FN_SLOT, {});
	}
	Save_Env();
	Do_Nth_Then(RC_COMB_SAVE_VALUE, Array_Length+1, {});
      }

    case TC_COMBINATION_1:
      Reserve_Stack_Space();	/* STACK_ENV_EXTRA_SLOTS+2+CONTINUATION_SIZE */
      Save_Env();
      Do_Nth_Then(RC_COMB_1_PROCEDURE, COMB_1_ARG_1, {});
  
    case TC_COMBINATION_2:
      Reserve_Stack_Space();	/* STACK_ENV_EXTRA_SLOTS+3+CONTINUATION_SIZE */
      Save_Env();
      Do_Nth_Then(RC_COMB_2_FIRST_OPERAND, COMB_2_ARG_2, {});

    case TC_COMMENT:
      Reduces_To_Nth(COMMENT_EXPRESSION);

    case TC_CONDITIONAL:
     Will_Push(CONTINUATION_SIZE + 1);
      Save_Env();
      Do_Nth_Then(RC_CONDITIONAL_DECIDE, COND_PREDICATE, Pushed());

    case TC_COMPILED_EXPRESSION:
      execute_compiled_setup();
      Store_Expression( (Pointer) Get_Pointer( Fetch_Expression()));
      Export_Registers();
      Which_Way = enter_compiled_expression();
      goto return_from_compiled_code;

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

    case TC_PCOMB0:
      /* In case we back out */
      Reserve_Stack_Space();			/* CONTINUATION_SIZE */
      Finished_Eventual_Pushing();		/* of this primitive */
/* ASSUMPTION: The syntaxer is hereby assumed NEVER to generate primitive
   combinations unless the primitive itself is output in the code stream.
   Therefore, we don't have to explicitly check here that the expression
   register has a primitive in it.
*/
Primitive_Internal_Apply:
      if (Microcode_Does_Stepping && Trapping &&
           (Fetch_Apply_Trapper() != NIL))
      {Will_Push(3); 
        Push(Fetch_Expression());
        Push(Fetch_Apply_Trapper());
        Push(STACK_FRAME_HEADER + 1 + N_Args_Primitive(Fetch_Expression()));
       Pushed();
        Stop_Trapping();
	goto Apply_Non_Trapping;
      }
Prim_No_Trap_Apply:
      Export_Registers();
      Metering_Apply_Primitive(Val, Get_Integer(Fetch_Expression()));

/* Any primitive which does not do a long jump can have it's primitive
   frame popped off here.  At this point, it is guaranteed that the
   primitive is in the expression register in case the primitive needs
   to back out.
*/
      Import_Registers_Except_Val();
      Pop_Primitive_Frame(N_Args_Primitive(Fetch_Expression()));
      if (Must_Report_References())
      { Store_Expression(Val);
        Store_Return(RC_RESTORE_VALUE);
	Save_Cont();
        Call_Future_Logging();
      }
      break;

    case TC_PCOMB1:
       Reserve_Stack_Space();	/* 1+CONTINUATION_SIZE */
       Do_Nth_Then(RC_PCOMB1_APPLY, PCOMB1_ARG_SLOT, {});

    case TC_PCOMB2:
      Reserve_Stack_Space();	/* 2+CONTINUATION_SIZE */
      Save_Env();
      Do_Nth_Then(RC_PCOMB2_DO_1, PCOMB2_ARG_2_SLOT, {});

    case TC_PCOMB3:
      Reserve_Stack_Space();	/* 3+CONTINUATION_SIZE */
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
/* ASSUMPTION: The SYMBOL slot does NOT contain a future */
    { fast Pointer Compilation_Type, *Variable_Object;
      int The_Type;

      Set_Time_Zone(Zone_Lookup);
#ifndef No_In_Line_Lookup

      Variable_Object = Get_Pointer(Fetch_Expression());
      Compilation_Type = Variable_Object[VARIABLE_COMPILED_TYPE];
      The_Type = Type_Code(Compilation_Type);

      if (The_Type == LOCAL_REF)
      { fast Pointer *Frame;
	Frame = Get_Pointer(Fetch_Env());
	Val = Without_Danger_Bit(Frame[Local_Offset(Compilation_Type)]);
	if (!Trap(Val))
	  Splice_Future_Value(&(Frame[Local_Offset(Compilation_Type)]));
      }
      else if (The_Type == GLOBAL_REF)
      { Val = Vector_Ref(Compilation_Type, SYMBOL_GLOBAL_VALUE);
        if (Dangerous(Val))
	  Variable_Object[VARIABLE_COMPILED_TYPE] = UNCOMPILED_VARIABLE;
	else if (!Trap(Val))
	  Splice_Future_Value(Nth_Vector_Loc(Compilation_Type,
					     SYMBOL_GLOBAL_VALUE));
      }

/* Interpret() continues on the next page */

/* Interpret(), continued */

      else if (The_Type == FORMAL_REF)
      { fast long Frame_No;
	fast Pointer *Frame;

	Frame = Get_Pointer(Fetch_Env());
        Frame_No = Get_Integer(Variable_Object[VARIABLE_FRAME_NO]);
	while(--Frame_No >= 0)
          Frame = Get_Pointer(Fast_Vector_Ref(Frame[HEAP_ENV_FUNCTION],
					      PROCEDURE_ENVIRONMENT)); 
        Val = Frame[Local_Offset(Variable_Object[VARIABLE_OFFSET])];
        if (Dangerous(Val))
	  Variable_Object[VARIABLE_COMPILED_TYPE] = UNCOMPILED_VARIABLE;
	else if (!Trap(Val))
	  Splice_Future_Value(
	    &(Frame[Local_Offset(Variable_Object[VARIABLE_OFFSET])]));
      }
#endif
      /* Fall through in cases not handled above */
      { long Result;
	Result = Lex_Ref(Fetch_Env(), Fetch_Expression());
	Import_Val();
	Set_Time_Zone(Zone_Working);
	if (Result == PRIM_DONE) break;
	Eval_Error(Result);
      }
    }

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
  { case RC_COMB_1_PROCEDURE:
      Restore_Env();
      Push(Val);                /* Arg. 1 */
      Push(NIL);                /* Operator */
      Push(STACK_FRAME_HEADER+1);
      Finished_Eventual_Pushing();
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
      Push(STACK_FRAME_HEADER+2);
      Finished_Eventual_Pushing();
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

#define define_compiler_restart( return_code, entry)			\
    case return_code:							\
      { extern long entry();						\
	compiled_code_restart();					\
	Export_Registers();						\
	Which_Way = entry();						\
	goto return_from_compiled_code;					\
      }

      define_compiler_restart( RC_COMPILER_INTERRUPT_RESTART,
			      compiler_interrupt_restart)

      define_compiler_restart( RC_COMPILER_LEXPR_INTERRUPT_RESTART,
			      compiler_lexpr_interrupt_restart)

      define_compiler_restart( RC_COMPILER_LOOKUP_APPLY_RESTART,
			      compiler_lookup_apply_restart)

      define_compiler_restart( RC_COMPILER_REFERENCE_RESTART,
			      compiler_reference_restart)

      define_compiler_restart( RC_COMPILER_ACCESS_RESTART,
			      compiler_access_restart)

      define_compiler_restart( RC_COMPILER_UNASSIGNED_P_RESTART,
			      compiler_unassigned_p_restart)

      define_compiler_restart( RC_COMPILER_UNBOUND_P_RESTART,
			      compiler_unbound_p_restart)

      define_compiler_restart( RC_COMPILER_ASSIGNMENT_RESTART,
			      compiler_assignment_restart)

      define_compiler_restart( RC_COMPILER_DEFINITION_RESTART,
			      compiler_definition_restart)

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
      Store_Env(Pop());
      Reduces_To(Fetch_Expression());

    case RC_EXECUTE_ACCESS_FINISH:
    { long Result;
      Pop_Return_Val_Check();
      if (Environment_P(Val))
      { Result = Symbol_Lex_Ref(Val,
				Fast_Vector_Ref(Fetch_Expression(), ACCESS_NAME));
	Import_Val();
	if (Result != PRIM_DONE) Pop_Return_Error(Result);
	End_Subproblem();
	break;
      }
      Pop_Return_Error(ERR_BAD_FRAME);
    }

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_EXECUTE_ASSIGNMENT_FINISH:
    { fast Pointer Compilation_Type, *Variable_Object;
      Pointer The_Non_Object, Store_Value;
      int The_Type;

      Set_Time_Zone(Zone_Lookup);
      Restore_Env();
      The_Non_Object = Get_Fixed_Obj_Slot(Non_Object);
      Store_Value = (Val == The_Non_Object) ? UNASSIGNED_OBJECT : Val;

#ifndef No_In_Line_Lookup

      Variable_Object =
	Get_Pointer(Vector_Ref(Fetch_Expression(), ASSIGN_NAME));
      Compilation_Type = Variable_Object[VARIABLE_COMPILED_TYPE];
      The_Type = Type_Code(Compilation_Type);

      if (The_Type == LOCAL_REF)
      { fast Pointer *Frame;
	Frame = Get_Pointer(Fetch_Env());
	Val = Frame[Local_Offset(Compilation_Type)];
	if (Dangerous(Val))
	{ Set_Danger_Bit(Store_Value);
	  Clear_Danger_Bit(Val);
	}
	if (!Trap(Val))
	{ Frame[Local_Offset(Compilation_Type)] = Store_Value;
	  if (Val == UNASSIGNED_OBJECT) Val = The_Non_Object;
	  Set_Time_Zone(Zone_Working);
	  End_Subproblem();
	  break;
        }
      }
      else if (The_Type == GLOBAL_REF)
      { Val = Vector_Ref(Compilation_Type, SYMBOL_GLOBAL_VALUE);
        if (!Dangerous(Val) && !Trap(Val))
	{ Vector_Set(Compilation_Type, SYMBOL_GLOBAL_VALUE, Store_Value);
	  if (Val == UNASSIGNED_OBJECT) Val = The_Non_Object;
	  Set_Time_Zone(Zone_Working);
          End_Subproblem();
          break;
	}
	else if (Dangerous(Val))
	  Variable_Object[VARIABLE_COMPILED_TYPE] = UNCOMPILED_VARIABLE;
      }

/* Interpret() continues on the next page */

/* Interpret(), continued */

      else if (The_Type == FORMAL_REF)
      { fast long Frame_No;
	fast Pointer *Frame;

	Frame = Get_Pointer(Fetch_Env());
        Frame_No = Get_Integer(Variable_Object[VARIABLE_FRAME_NO]);
	while(--Frame_No >= 0)
          Frame = Get_Pointer(Fast_Vector_Ref(Frame[HEAP_ENV_FUNCTION],
					      PROCEDURE_ENVIRONMENT)); 
        Val = Frame[Local_Offset(Variable_Object[VARIABLE_OFFSET])];
        if (!Dangerous(Val) && !Trap(Val))
	{ Frame[Local_Offset(Variable_Object[VARIABLE_OFFSET])] =
	    Store_Value;
	  if (Val==UNASSIGNED_OBJECT) Val = The_Non_Object;
          Set_Time_Zone(Zone_Working);
          End_Subproblem();
          break;
	}
	else if (Dangerous(Val))
	  Variable_Object[VARIABLE_COMPILED_TYPE] = UNCOMPILED_VARIABLE;
      }
#endif
      /* Fall through in cases not handled above */
      { long Result;
	Result = Lex_Set(Fetch_Env(),
			 Vector_Ref(Fetch_Expression(), ASSIGN_NAME),
			 Store_Value);
	Import_Val();
	Set_Time_Zone(Zone_Working);
	if (Result == PRIM_DONE) 
	{ End_Subproblem();
	  break;
	}
	Save_Env();
	Pop_Return_Error(Result);
      }
    }
      
/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_EXECUTE_DEFINITION_FINISH:
      { Pointer Saved_Val;
        long Result;

	Saved_Val = Val;
        Restore_Env();
        Result = Local_Set(Fetch_Env(),
			   Fast_Vector_Ref(Fetch_Expression(), DEFINE_NAME),
			   Val);
        Import_Val();
        if (Result==PRIM_DONE)
        { End_Subproblem();
          break;
	}
	Save_Env();
	if (Result==PRIM_INTERRUPT)
	{ Prepare_Pop_Return_Interrupt(RC_EXECUTE_DEFINITION_FINISH,
				       Saved_Val);
	  Interrupt(IntCode & IntEnb);
	}
        Pop_Return_Error(Result);
      };

    case RC_EXECUTE_IN_PACKAGE_CONTINUE:
      Pop_Return_Val_Check();
      if (Environment_P(Val))
      { End_Subproblem();
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

    case RC_GC_CHECK:
      if (Get_Integer(Fetch_Expression()) > Space_Before_GC())
	{
	  Export_Registers();
	  Microcode_Termination(TERM_GC_OUT_OF_SPACE);
	}
      break;

    case RC_HALT:
      Export_Registers();
      Microcode_Termination(TERM_TERM_HANDLER);

/* Interpret() continues on the next page */

/* Interpret(), continued */

#define Prepare_Apply_Interrupt()       \
        Prepare_Pop_Return_Interrupt(RC_INTERNAL_APPLY, NIL)
                          
#define Apply_Error(N)							\
        { Store_Return(RC_INTERNAL_APPLY);      			\
          Val = NIL;                            			\
          Pop_Return_Error(N);                  			\
        }

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_INTERNAL_APPLY:
Internal_Apply:

/* Branch here to perform a function application.  At this point
   it is necessary that the top of the stack contain a frame
   for evaluation of the function to be applied. This frame
   DOES NOT contain "finger" and "combination" slots, although
   if the frame is to be copied into the heap, it will have NIL's
   in the "finger" and "combination" slots which will correspond 
   to "potentially-dangerous" and "auxilliary variables" slots.

   Note, also, that unlike most return codes Val is not used here.
   Thus, the error and interrupt macros above set it to NIL so that it
   will not 'hold on' to anything if a GC occurs.  Similarly, the
   contents of Expression are discarded.
*/
      if (Microcode_Does_Stepping && Trapping &&
            (Fetch_Apply_Trapper() != NIL))
      { long Count = Get_Integer(Stack_Ref(STACK_ENV_HEADER));
        Top_Of_Stack() = Fetch_Apply_Trapper();
        Push(STACK_FRAME_HEADER+Count);
        Stop_Trapping();
      }      
Apply_Non_Trapping:
      { long Interrupts;
        Pointer Function;

        Store_Expression(NIL);
        Interrupts = IntCode & IntEnb;
        if (Interrupts != 0)
        { Prepare_Apply_Interrupt();
          Interrupt(Interrupts);
        }

Perform_Application:
	Apply_Future_Check(Function, Stack_Ref(STACK_ENV_FUNCTION));
	Apply_Ucode_Hook();

/* Interpret() continues on the next page */

/* Interpret(), continued */

        switch(Type_Code(Function))
        { case TC_PROCEDURE:
          { Pointer Lambda_Expr, *Temp1, Temp2;
            long NParams, Size;
	    fast long NArgs;

	    Apply_Future_Check(Lambda_Expr,
			       Fast_Vector_Ref(Function,
					       PROCEDURE_LAMBDA_EXPR));
	    Temp1 = Get_Pointer(Lambda_Expr);
	    Apply_Future_Check(Temp2, Temp1[LAMBDA_FORMALS]);
            NArgs = Get_Integer(Pop());
            NParams = Vector_Length(Temp2);
	    if (Eval_Debug) 
	    { Print_Expression(FIXNUM_0+NArgs,
				 "APPLY: Number of arguments");
	      Print_Expression(FIXNUM_0+NParams,
			       "       Number of parameters");
	    }
            if (Type_Code(Lambda_Expr) == TC_LAMBDA)
            { if (NArgs != NParams)
              { Push(STACK_FRAME_HEADER+NArgs-1);
                Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
              }
	    }
            else if (NArgs < NParams)
                 { Push(STACK_FRAME_HEADER+NArgs-1);
                   Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
                 }
            Size = NArgs + (HEAP_ENV_EXTRA_SLOTS - 1);
            if (GC_Check(Size))
            { Push(STACK_FRAME_HEADER+NArgs-1);
              Prepare_Apply_Interrupt();
              Immediate_GC(Size);
            }
	    /* Store Environment Frame into heap, putting extra slots
               for Potentially Dangerous and Auxiliaries              */
            Store_Env(Make_Pointer(TC_ENVIRONMENT, Free));
	    *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, Size);
	    *Free++ = NIL; /* For PD list and Aux list */
	    *Free++ = NIL;
            for (; --NArgs >= 0; ) *Free++ = Pop();
            Reduces_To(Temp1[LAMBDA_SCODE]);
          }

/* Interpret() continues on the next page */

/* Interpret(), continued */

          case TC_CONTROL_POINT:
            if (Get_Integer(Stack_Ref(STACK_ENV_HEADER)) !=
                STACK_ENV_FIRST_ARG)
              Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS)
            Val = Stack_Ref(STACK_ENV_FIRST_ARG);
            Our_Throw(false, Function);
	    Apply_Stacklet_Backout();
	    Our_Throw_Part_2();
            goto Pop_Return;

          case TC_PRIMITIVE_EXTERNAL:
          { long NArgs, Proc = Datum(Function);
	    if (Proc > MAX_EXTERNAL_PRIMITIVE)
	      Apply_Error(ERR_UNDEFINED_PRIMITIVE);
            NArgs = Ext_Prim_Desc[Proc].arity;
            if (Get_Integer(Stack_Ref(STACK_ENV_HEADER)) !=
                STACK_ENV_FIRST_ARG+NArgs-1)
               Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
            Stack_Pointer = Simulate_Popping(STACK_ENV_FIRST_ARG);
		/* Remove the frame overhead, since the primitives
                   just expect arguments on the stack */
            Store_Expression(Function);
Repeat_External_Primitive:
	    /* Reinitialize Proc in case we "goto Repeat_External..." */
            Proc = Get_Integer(Fetch_Expression());
	    Export_Registers();
            Val = (*(Ext_Prim_Desc[Proc].proc))();
	    Set_Time_Zone(Zone_Working);
	    Import_Registers_Except_Val();
	    Pop_Primitive_Frame(Ext_Prim_Desc[Proc].arity);
	    goto Pop_Return;
	  }

/* Interpret() continues on the next page */

/* Interpret(), continued */

          case TC_EXTENDED_PROCEDURE:
          { Pointer Lambda_Expr, *List_Car, Temp;
            long NArgs, NParams, Formals, Params, Auxes,
                 Rest_Flag, Size, i;

/* Selectors for the various parts */

#define Get_Body_Elambda(Addr)  (Fast_Vector_Ref(Addr, ELAMBDA_SCODE))
#define Get_Names_Elambda(Addr) (Fast_Vector_Ref(Addr, ELAMBDA_NAMES))
#define Get_Count_Elambda(Addr) (Fast_Vector_Ref(Addr, ELAMBDA_ARG_COUNT))
#define Elambda_Formals_Count(Addr) \
     ((((long) Addr) & EL_FORMALS_MASK) >> EL_FORMALS_SHIFT)
#define Elambda_Opts_Count(Addr) \
     (((long) Addr) & EL_OPTS_MASK)
#define Elambda_Rest_Flag(Addr) \
     ((((long) Addr) & EL_REST_MASK) >> EL_REST_SHIFT)

            Apply_Future_Check(Lambda_Expr,
                               Fast_Vector_Ref(Function,
					       PROCEDURE_LAMBDA_EXPR));
	    Apply_Future_Check(Temp, Fast_Vector_Ref(Lambda_Expr,
						     ELAMBDA_NAMES));
            NParams = Vector_Length(Temp) - 1;
	    Apply_Future_Check(Temp, Get_Count_Elambda(Lambda_Expr));
            Formals = Elambda_Formals_Count(Temp);
            /* Formals DOES NOT include the name of the lambda */
            Params = Elambda_Opts_Count(Temp) + Formals;
            Rest_Flag = Elambda_Rest_Flag(Temp);
            NArgs = Get_Integer(Pop()) - 1;
            Auxes = NParams - (Params + Rest_Flag);
            if ((NArgs < Formals) ||
                (!Rest_Flag && (NArgs > Params)))
            { Push(STACK_FRAME_HEADER+NArgs);
              Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
            }

/* Interpret() continues on the next page */

/* Interpret(), continued */

            Size = Params + Rest_Flag + Auxes +
	           (HEAP_ENV_EXTRA_SLOTS + 1);
            List_Car = Free + Size;
            if (GC_Check(Size + ((NArgs > Params) ?
				 2 * (NArgs - Params) : 0)))
            { Push(STACK_FRAME_HEADER+NArgs);
              Prepare_Apply_Interrupt();
              Immediate_GC(Size + ((NArgs > Params) ?
				   2 * (NArgs - Params) : 0));
            }
            Store_Env(Make_Pointer(TC_ENVIRONMENT, Free));
	    *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, Size-1);
                                        /* Environment Header  */
	    *Free++ = NIL;              /* Aux list            */
	    *Free++ = NIL;              /* PD list             */
            Size = 1 + ((NArgs < Params) ? NArgs : Params);
            for (i = 0; i < Size; i++) *Free++ = Pop();
            for (i--;  i < Params; i++)
              *Free++ = UNASSIGNED_OBJECT;
            if (Rest_Flag)
              if (NArgs <= i) *Free++ = NIL;
              else
              { *Free++ = Make_Pointer(TC_LIST, List_Car);
                for (; i < NArgs; i++, List_Car++)
                { *List_Car++ = Pop();
                  *List_Car = Make_Pointer(TC_LIST, List_Car+1);
                }
                List_Car[-1] = NIL;
              }
            for (i = 0; i < Auxes; i++) *Free++ = UNASSIGNED_OBJECT;
            Free = List_Car;
            Reduces_To(Get_Body_Elambda(Lambda_Expr));
          }

/* Interpret() continues on the next page */

/* Interpret(), continued */

          case TC_PRIMITIVE:
          { long Number_Of_Args = N_Args_Primitive(Function);
            if (Get_Integer(Stack_Ref(STACK_ENV_HEADER)) !=
                STACK_ENV_FIRST_ARG+Number_Of_Args-1)
               Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
            Stack_Pointer = Simulate_Popping(STACK_ENV_FIRST_ARG);
		/* Remove the frame overhead, since the primitives
                   just expect arguments on the stack */
            Store_Expression(Function);
            goto Prim_No_Trap_Apply;
          }

/* Interpret() continues on the next page */

/* Interpret(), continued */

          case TC_COMPILED_PROCEDURE:
	  { apply_compiled_setup(STACK_ENV_EXTRA_SLOTS +
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
	    { compiled_error_backout();
	      Save_Cont();
	      Interrupt( (IntCode & IntEnb));
	    }

	    case ERR_WRONG_NUMBER_OF_ARGUMENTS:
	    { apply_compiled_backout();
	      Apply_Error( Which_Way);
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
	Stack_Ref(TRANSLATE_FROM_DISTANCE) = FIXNUM_0+(From_Count-1);
	Thunk = Fast_Vector_Ref(Current, STATE_POINT_AFTER_THUNK);
	New_Location = Fast_Vector_Ref(Current, STATE_POINT_NEARER_POINT);
	Stack_Ref(TRANSLATE_FROM_POINT) = New_Location;
	if ((From_Count == 1) &&
	    (Stack_Ref(TRANSLATE_TO_DISTANCE) == FIXNUM_0))
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
	Stack_Ref(TRANSLATE_TO_DISTANCE) = FIXNUM_0+To_Count;
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
      if (GC_Check(GC_Space_Needed))
      { printf("\nGC just ended.  The free pointer is at 0x%x, the top of this heap\n",
	       Free);
	printf("is at 0x%x, and we are trying to cons 0x%x objects.  Dead!\n",
	       MemTop, GC_Space_Needed);
	Microcode_Termination(TERM_EXIT);
      }
      GC_Space_Needed = 0;
      Val = Fetch_Expression();
      break;

    case RC_PCOMB1_APPLY:
      End_Subproblem();
      Push(Val);		/* Argument value */
      Finished_Eventual_Pushing();
      Store_Expression(Fast_Vector_Ref(Fetch_Expression(), PCOMB1_FN_SLOT));
      goto Primitive_Internal_Apply;

    case RC_PCOMB2_APPLY:
      End_Subproblem();
      Push(Val);		/* Value of arg. 1 */
      Finished_Eventual_Pushing();
      Store_Expression(Fast_Vector_Ref(Fetch_Expression(), PCOMB2_FN_SLOT));
      goto Primitive_Internal_Apply;

    case RC_PCOMB2_DO_1:
      Restore_Env();
      Push(Val);		/* Save value of arg. 2 */
      Do_Another_Then(RC_PCOMB2_APPLY, PCOMB2_ARG_1_SLOT);

    case RC_PCOMB3_APPLY:
      End_Subproblem();
      Push(Val);		/* Save value of arg. 1 */
      Finished_Eventual_Pushing();
      Store_Expression(Fast_Vector_Ref(Fetch_Expression(), PCOMB3_FN_SLOT));
      goto Primitive_Internal_Apply;

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_PCOMB3_DO_1:
    { Pointer Temp;
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
    { Pointer GC_Daemon_Proc, Result;
      Export_Registers();
      Result = Purify_Pass_2(Fetch_Expression());
      Import_Registers();
      if (Result == NIL)
      { /* The object does not fit in Constant space.
	   There is no need to run the daemons, and we should let the runtime
	   system know what happened.
	 */
	Val = NIL;
        break;
      }
      GC_Daemon_Proc = Get_Fixed_Obj_Slot(GC_Daemon);
      if (GC_Daemon_Proc==NIL)
      { Val = TRUTH;
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

    case RC_REPEAT_PRIMITIVE:
      if (Type_Code(Fetch_Expression()) == TC_PRIMITIVE_EXTERNAL)
        goto Repeat_External_Primitive;
      else goto Primitive_Internal_Apply;

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
    { Pointer Stacklet;
      Previous_Restore_History_Offset = Get_Integer(Pop());
      Stacklet = Pop();
      History = Get_Pointer(Fetch_Expression());
      if (Previous_Restore_History_Offset == 0)
	Previous_Restore_History_Stacklet = NULL;
      else if (Stacklet == NIL)
        Previous_Restore_History_Stacklet = NULL;
      else
	Previous_Restore_History_Stacklet = Get_Pointer(Stacklet);
      break;
    }

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_RESTORE_HISTORY:
    { Pointer Stacklet;
      Export_Registers();
      if (! Restore_History(Fetch_Expression()))
      { Import_Registers();
        Save_Cont();
       Will_Push(CONTINUATION_SIZE);
        Store_Expression(Val);
        Store_Return(RC_RESTORE_VALUE);
        Save_Cont();
       Pushed();
        Immediate_GC((Free > MemTop) ? 0 : ((MemTop-Free)+1));
      }
      Import_Registers();
      Previous_Restore_History_Offset = Get_Integer(Pop());
      Stacklet = Pop();
      if (Previous_Restore_History_Offset == 0)
	Previous_Restore_History_Stacklet = NULL;
      else
      { if (Stacklet == NIL)
        { Previous_Restore_History_Stacklet = NULL;
	  Get_End_Of_Stacklet()[-Previous_Restore_History_Offset] =
            Make_Non_Pointer(TC_RETURN_CODE, RC_RESTORE_HISTORY);
        }
        else
	{ Previous_Restore_History_Stacklet = Get_Pointer(Stacklet);
	  Previous_Restore_History_Stacklet[-Previous_Restore_History_Offset] =
            Make_Non_Pointer(TC_RETURN_CODE, RC_RESTORE_HISTORY);
        }
      }
      break;
    }

    case RC_RESTORE_FLUIDS:
      Fluid_Bindings = Fetch_Expression();
      New_Compiler_MemTop();
      break;

    case RC_RESTORE_INT_MASK: 
      IntEnb = Get_Integer(Fetch_Expression());
      New_Compiler_MemTop();
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

/*  case RC_RESTORE_VALUE is with RC_POP_RETURN_ERROR */

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

    default:
      Export_Registers();
      Microcode_Termination(TERM_NON_EXISTENT_CONTINUATION);
  };
  goto Pop_Return;
}
