/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/utils.c,v 9.44 1989/09/20 23:12:51 cph Exp $

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

/* This file contains utilities for interrupts, errors, etc. */

#include "scheme.h"
#include "prims.h"
#include "winder.h"
#include "history.h"
#include "cmpint.h"

/* Set_Up_Interrupt is called from the Interrupt
   macro to do all of the setup for calling the user's
   interrupt routines. */

void
Setup_Interrupt (Masked_Interrupts)
     long Masked_Interrupts;
{
  SCHEME_OBJECT Int_Vector, Handler;
  long i, Int_Number, The_Int_Code, New_Int_Enb;

  The_Int_Code = FETCH_INTERRUPT_CODE();
  Int_Vector = (Get_Fixed_Obj_Slot (System_Interrupt_Vector));

  /* The interrupt vector is normally of size (MAX_INTERRUPT_NUMBER + 1).
     We signal all normal interrupts though the first MAX_INTERRUPT_NUMBER
     slots, and any other (spurious) interrupts through the last slot. */

  Int_Number = 0;
  i = 1;
  while (true)
  {
    if (Int_Number > MAX_INTERRUPT_NUMBER)
    {
      New_Int_Enb = FETCH_INTERRUPT_MASK();
      break;
    }
    if ((Masked_Interrupts & i) != 0)
    {
      New_Int_Enb = ((1 << Int_Number) - 1);
      break;
    }
    Int_Number += 1;
    i = (i << 1);
  }

  /* Handle case where interrupt vector is too small. */
  if (Int_Number >= (VECTOR_LENGTH (Int_Vector)))
    {
      fprintf (stderr,
	       "\nInterrupt out of range: %ld (vector length = %ld)\n",
	       Int_Number, (VECTOR_LENGTH (Int_Vector)));
      fprintf (stderr,
	       "Interrupts = 0x%08lx, Mask = 0x%08lx, Masked = 0x%08lx\n",
	       FETCH_INTERRUPT_CODE(),
	       FETCH_INTERRUPT_MASK(),
	       Masked_Interrupts);
      Microcode_Termination (TERM_NO_INTERRUPT_HANDLER);
    }

  Global_Interrupt_Hook ();
  Handler = (VECTOR_REF (Int_Vector, Int_Number));

/* Setup_Interrupt continues on the next page */

/* Setup_Interrupt, continued */

Passed_Checks:	/* This label may be used in Global_Interrupt_Hook */
  Stop_History();
 Will_Push(CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS + 3);
  /* Return from interrupt handler will re-enable interrupts */
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(LONG_TO_FIXNUM(FETCH_INTERRUPT_MASK()));
  Save_Cont();
/*
  There used to be some code here for gc checks, but that is done
  uniformly now by RC_NORMAL_GC_DONE.
 */

/* Now make an environment frame for use in calling the
 * user supplied interrupt routine.  It will be given
 * two arguments: the UNmasked interrupt requests, and
 * the currently enabled interrupts.
 */

  Push(LONG_TO_FIXNUM(FETCH_INTERRUPT_MASK()));
  Push(LONG_TO_FIXNUM(The_Int_Code));
  Push(Handler);
  Push(STACK_FRAME_HEADER + 2);
 Pushed();
  /* Turn off interrupts */
  SET_INTERRUPT_MASK(New_Int_Enb);
  return;
}

/* Error processing utilities */

void
err_print (error_code, where)
     long error_code;
     FILE * where;
{
  extern char * Error_Names [];

  if (error_code > MAX_ERROR)
    fprintf (where, "Unknown error code 0x%x.\n", error_code);
  else
    fprintf (where, "Error code 0x%x (%s).\n",
	     error_code,
	     (Error_Names [error_code]));
  return;
}

extern long death_blow;
long death_blow;

void
error_death (code, message)
     long code;
     char * message;
{
  death_blow = code;
  fprintf (stderr, "\nMicrocode Error: %s.\n", message);
  err_print (code, stderr);
  fprintf (stderr, "\n**** Stack Trace ****\n\n");
  Back_Trace (stderr);
  Microcode_Termination (TERM_NO_ERROR_HANDLER);
  /*NOTREACHED*/
}

void
Stack_Death ()
{
  fprintf (stderr, "\nWill_Push vs. Pushed inconsistency.\n");
  Microcode_Termination (TERM_BAD_STACK);
  /*NOTREACHED*/
}

/* Back_Out_Of_Primitive sets the registers up so that the backout
   mechanism in interpret.c will cause the primitive to be
   restarted if the error/interrupt is proceeded. */

void
Back_Out_Of_Primitive ()
{
  long nargs;
  SCHEME_OBJECT primitive;

  /* Setup a continuation to return to compiled code if the primitive is
     restarted and completes successfully. */

  primitive = (Regs [REGBLOCK_PRIMITIVE]);
  if (! (PRIMITIVE_P (primitive)))
    {
      fprintf (stderr,
	       "\nBack_Out_Of_Primitive backing out when not in primitive!\n");
      Microcode_Termination (TERM_BAD_BACK_OUT);
    }
  nargs = (PRIMITIVE_N_ARGUMENTS (primitive));
  if (COMPILED_CODE_ADDRESS_P (Stack_Ref (nargs)))
    compiler_apply_procedure (nargs);
  Push (primitive);
  Push (STACK_FRAME_HEADER + nargs);
  Store_Env (MAKE_OBJECT (GLOBAL_ENV, END_OF_CHAIN));
  Val = SHARP_F;
  Store_Return (RC_INTERNAL_APPLY);
  Store_Expression (SHARP_F);
  (Regs [REGBLOCK_PRIMITIVE]) = SHARP_F;
  return;
}

/* canonicalize_primitive_context should be used by "unsafe" primitives
   to guarantee that their execution context is the expected one, ie.
   they are called from the interpreter.
   If they are called from compiled code, they should abort to the
   interpreter and reenter.
   Note: This is called only from the macro PRIMITIVE_CANONICALIZE_CONTEXT,
   so that the work can be divided between them if it is an issue. */

extern void canonicalize_primitive_context ();

void
canonicalize_primitive_context ()
{
  long nargs;
  SCHEME_OBJECT primitive;

  primitive = (Regs [REGBLOCK_PRIMITIVE]);
  if (! (PRIMITIVE_P (primitive)))
    {
      fprintf
	(stderr,
	 "\ncanonicalize_primitive_context invoked when not in primitive!\n");
      Microcode_Termination (TERM_BAD_BACK_OUT);
    }
  nargs = (PRIMITIVE_N_ARGUMENTS (primitive));
  if (! (COMPILED_CODE_ADDRESS_P (Stack_Ref (nargs))))
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
signal_error_from_primitive (error_code)
     long error_code;
{
  PRIMITIVE_ABORT (error_code);
  /*NOTREACHED*/
}

void
signal_interrupt_from_primitive ()
{
  PRIMITIVE_ABORT (PRIM_INTERRUPT);
  /*NOTREACHED*/
}

void
error_wrong_type_arg (n)
     int n;
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
error_bad_range_arg (n)
     int n;
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
error_external_return ()
{
  signal_error_from_primitive (ERR_EXTERNAL_RETURN);
}

long
arg_integer (arg_number)
     int arg_number;
{
  fast SCHEME_OBJECT object = (ARG_REF (arg_number));
  if (! (INTEGER_P (object)))
    error_wrong_type_arg (arg_number);
  if (! (integer_to_long_p (object)))
    error_bad_range_arg (arg_number);
  return (integer_to_long (object));
}

long
arg_nonnegative_integer (arg_number)
     int arg_number;
{
  fast long result = (arg_integer (arg_number));
  if (result < 0)
    error_bad_range_arg (arg_number);
  return (result);
}

long
arg_index_integer (arg_number, upper_limit)
     int arg_number;
     long upper_limit;
{
  fast long result = (arg_integer (arg_number));
  if ((result < 0) || (result >= upper_limit))
    error_bad_range_arg (arg_number);
  return (result);
}

long
arg_integer_in_range (arg_number, lower_limit, upper_limit)
     int arg_number;
     long lower_limit;
     long upper_limit;
{
  fast long result = (arg_integer (arg_number));
  if ((result < lower_limit) || (result >= upper_limit))
    error_bad_range_arg (arg_number);
  return (result);
}

Boolean
real_number_to_double_p (x)
     fast SCHEME_OBJECT x;
{
  return ((! (BIGNUM_P (x))) || (BIGNUM_TO_DOUBLE_P (x)));
}

double
real_number_to_double (x)
     fast SCHEME_OBJECT x;
{
  return
    ((FIXNUM_P (x))
     ? (FIXNUM_TO_DOUBLE (x))
     : (BIGNUM_P (x))
     ? (bignum_to_double (x))
     : (FLONUM_TO_DOUBLE (x)));
}

double
arg_real_number (arg_number)
     int arg_number;
{
  fast SCHEME_OBJECT number = (ARG_REF (arg_number));
  if (! (REAL_P (number)))
    error_wrong_type_arg (arg_number);
  if (! (real_number_to_double_p (number)))
    error_bad_range_arg (arg_number);
  return (real_number_to_double (number));
}

double
arg_real_in_range (arg_number, lower_limit, upper_limit)
     int arg_number;
     double lower_limit;
     double upper_limit;
{
  fast double result = (arg_real_number (arg_number));
  if ((result < lower_limit) || (result > upper_limit))
    error_bad_range_arg (arg_number);
  return (result);
}

Boolean
interpreter_applicable_p (object)
     fast SCHEME_OBJECT object;
{
  extern void compiled_entry_type ();
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
Do_Micro_Error (Err, From_Pop_Return)
     long Err;
     Boolean From_Pop_Return;
{
  SCHEME_OBJECT Error_Vector, Handler;

  if (Consistency_Check)
  {
    err_print(Err, stdout);
    Print_Expression(Fetch_Expression(), "Expression was");
    printf("\nEnvironment 0x%x (#%o).\n", Fetch_Env(), Fetch_Env());
    Print_Return("Return code");
    printf("\n");
  }

  Error_Exit_Hook();

  if (Trace_On_Error)
  {
    printf("\n\n**** Stack Trace ****\n\n");
    Back_Trace(stdout);
  }

#ifdef ENABLE_DEBUGGING_TOOLS
  {
    int *From = &(local_circle[0]), *To = &(debug_circle[0]), i;

    for (i = 0; i < local_nslots; i++)
    {
      *To++ = *From++;
    }
    debug_nslots = local_nslots;
    debug_slotno = local_slotno;
  }
#endif

/* Do_Micro_Error continues on the next page. */

/* Do_Micro_Error, continued */

  if ((!Valid_Fixed_Obj_Vector()) ||
      (OBJECT_TYPE ((Error_Vector =
		    Get_Fixed_Obj_Slot(System_Error_Vector))) !=
       TC_VECTOR))
  {
    error_death(Err, "Bad error handlers vector");
    /*NOTREACHED*/
  }

  if ((Err < 0) || (Err >= (VECTOR_LENGTH (Error_Vector))))
  {
    if (VECTOR_LENGTH (Error_Vector) == 0)
    {
      error_death(Err, "Empty error handlers vector");
      /*NOTREACHED*/
    }
    Handler = (VECTOR_REF (Error_Vector, ERR_BAD_ERROR_CODE));
  }
  else
  {
    Handler = (VECTOR_REF (Error_Vector, Err));
  }

  /* This can NOT be folded into the Will_Push below since we cannot
     afford to have the Will_Push put down its own continuation.
     There is guaranteed to be enough space for this one
     continuation; in fact, the Will_Push here is really unneeded!
   */

  if (From_Pop_Return)
  {
   Will_Push(CONTINUATION_SIZE);
    Save_Cont();
   Pushed();
  }
 Will_Push(STACK_ENV_EXTRA_SLOTS + 3 +
	   2 * CONTINUATION_SIZE +
	   HISTORY_SIZE +
           (From_Pop_Return ? 0 : 1));

  if (From_Pop_Return)
  {
    Store_Expression(Val);
  }
  else
  {
    Push(Fetch_Env());
  }

  Store_Return((From_Pop_Return) ?
	       RC_POP_RETURN_ERROR :
	       RC_EVAL_ERROR);
  Save_Cont();

  /* Return from error handler will re-enable interrupts & restore history */

  Stop_History();
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(LONG_TO_FIXNUM(FETCH_INTERRUPT_MASK()));
  Save_Cont();
  /* Arg 2:     Int. mask */
  Push(LONG_TO_FIXNUM(FETCH_INTERRUPT_MASK()));
  /* Arg 1:     Err. No   */
  if ((Err >= SMALLEST_FIXNUM) && (Err <= BIGGEST_FIXNUM))
  {
    Push (LONG_TO_FIXNUM(Err));
  }
  else
  {
    Push (LONG_TO_UNSIGNED_FIXNUM (ERR_BAD_ERROR_CODE));
  }
  /* Procedure: Handler   */
  Push(Handler);
  Push(STACK_FRAME_HEADER + 2);
 Pushed();

  /* Disable all interrupts */
  SET_INTERRUPT_MASK(0);
  return;
}

/* HISTORY manipulation */

SCHEME_OBJECT *
Make_Dummy_History ()
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
Stop_History ()
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

SCHEME_OBJECT *
Copy_Rib (Orig_Rib)
     SCHEME_OBJECT *Orig_Rib;
{
  SCHEME_OBJECT *Result, *This_Rib;

  for (This_Rib = NULL, Result = Free;
       (This_Rib != Orig_Rib) && (!GC_Check(0));
       This_Rib = OBJECT_ADDRESS (This_Rib[RIB_NEXT_REDUCTION]))
  {
    if (This_Rib == NULL)
    {
      This_Rib = Orig_Rib;
    }
    Free[RIB_EXP] = This_Rib[RIB_EXP];
    Free[RIB_ENV] = This_Rib[RIB_ENV];
    Free[RIB_NEXT_REDUCTION] =
      (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, Free+3));
    if (HISTORY_MARKED_P(This_Rib[RIB_MARK]))
    {
      HISTORY_MARK(Free[RIB_MARK]);
    }
    Free += 3;
  }
  ((Free - 3) [RIB_NEXT_REDUCTION]) =
    (OBJECT_NEW_ADDRESS (((Free - 3) [RIB_NEXT_REDUCTION]), Result));
  return (Result);
}

/* Restore_History pops a history object off the stack and
   makes a COPY of it the current history collection object.
   This is called only from the RC_RESTORE_HISTORY case in
   interpret.c .
*/

Boolean
Restore_History (Hist_Obj)
     SCHEME_OBJECT Hist_Obj;
{
  SCHEME_OBJECT *New_History, *Next_Vertebra, *Prev_Vertebra,
          *Orig_Vertebra;

  if (Consistency_Check)
  {
    if (!(HUNK3_P(Hist_Obj)))
    {
      fprintf(stderr, "Bad history to restore.\n");
      Microcode_Termination(TERM_EXIT);
      /*NOTREACHED*/
    }
  }
  Orig_Vertebra = OBJECT_ADDRESS (Hist_Obj);

  for (Next_Vertebra = NULL, Prev_Vertebra = NULL;
       Next_Vertebra != Orig_Vertebra;
       Next_Vertebra =
         OBJECT_ADDRESS (Next_Vertebra[HIST_NEXT_SUBPROBLEM]))
  {
    SCHEME_OBJECT *New_Rib;

    if (Prev_Vertebra == NULL)
    {
      Next_Vertebra = Orig_Vertebra;
    }
    New_Rib = Copy_Rib(OBJECT_ADDRESS (Next_Vertebra[HIST_RIB]));
    if (Prev_Vertebra == NULL)
    {
      New_History = Free;
    }
    else
    {
      Prev_Vertebra[HIST_NEXT_SUBPROBLEM] =
           MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, Free);
    }
    Free[HIST_RIB] = MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, New_Rib);
    Free[HIST_NEXT_SUBPROBLEM] = SHARP_F;
    Free[HIST_PREV_SUBPROBLEM] =
      MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, Prev_Vertebra);
    if (HISTORY_MARKED_P(Next_Vertebra[HIST_MARK]))
    {
      HISTORY_MARK(Free[HIST_MARK]);
    }
    Prev_Vertebra = Free;
    Free += 3;
    if (GC_Check(0))
    {
      return (false);
    }
  }
  (New_History [HIST_PREV_SUBPROBLEM]) =
    (OBJECT_NEW_ADDRESS ((New_History [HIST_PREV_SUBPROBLEM]), (Free - 3)));
  Prev_Vertebra[HIST_NEXT_SUBPROBLEM] =
    MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, New_History);
  if (HISTORY_MARKED_P(Orig_Vertebra[HIST_MARK]))
  {
    HISTORY_MARK(Prev_Vertebra[HIST_MARK]);
  }
  History = New_History;
  return (true);
}

/* If a debugging version of the interpreter is made, then this
 * procedure is called to actually invoke a primitive.  When a
 * 'production' version is made, all of the consistency checks are
 * omitted and a macro from DEFAULT.H is used to directly code the
 * call to the primitive function.  This is only used in INTERPRET.C.
 */

#ifdef ENABLE_DEBUGGING_TOOLS

SCHEME_OBJECT
Apply_Primitive (primitive)
     SCHEME_OBJECT primitive;
{
  SCHEME_OBJECT Result, *Saved_Stack;

  if (Primitive_Debug)
  {
    Print_Primitive(primitive);
  }
  Saved_Stack = Stack_Pointer;
  INTERNAL_APPLY_PRIMITIVE(Result, primitive);
  if (Saved_Stack != Stack_Pointer)
  {

    int NArgs;

    NArgs = PRIMITIVE_N_ARGUMENTS(primitive);
    Print_Expression(primitive, "Stack bad after ");
    fprintf(stderr,
	    "\nStack was 0x%x, now 0x%x, #args=%d.\n",
            Saved_Stack, Stack_Pointer, NArgs);
    Microcode_Termination(TERM_EXIT);
    /*NOTREACHED*/
  }
  if (Primitive_Debug)
  {
    Print_Expression(Result, "Primitive Result");
    fprintf(stderr, "\n");
  }
  return (Result);
}

#endif /* ENABLE_DEBUGGING_TOOLS */

#ifdef ENABLE_PRIMITIVE_PROFILING

/* The profiling mechanism is enabled by storing a vector in the fixed
   objects vector.  The vector should be initialized to contain all zeros
 */

void
record_primitive_entry (primitive)
     SCHEME_OBJECT primitive;
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
Allocate_New_Stacklet (N)
     long N;
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
      {
	Microcode_Termination(TERM_STACK_OVERFLOW);
      }
    }
    Free[STACKLET_LENGTH] = MAKE_OBJECT (TC_MANIFEST_VECTOR, (size - 1));
    Stack_Guard = &(Free[STACKLET_HEADER_SIZE]);
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
    Stack_Guard = &New_Stacklet[STACKLET_HEADER_SIZE];
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
Find_State_Space (State_Point)
     SCHEME_OBJECT State_Point;
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
      fprintf(stderr,
	      "\nState_Point 0x%x wrong: count was %d, #F at %d\n",
	     State_Point, How_Far, i);
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
Translate_To_Point (Target)
     SCHEME_OBJECT Target;
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
  {
    Current_Location = Current_State_Point;
  }
  else
  {
    Current_Location = MEMORY_REF (State_Space, STATE_SPACE_NEAREST_POINT);
  }

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
  {
    Path_Point = FAST_MEMORY_REF (Path_Point, STATE_POINT_NEARER_POINT);
  }

  for (Path_Ptr = (&(Path[Merge_Depth]));
       Merge_Depth >= 0;
       Merge_Depth--, Path_Ptr--)
  {
    if (*Path_Ptr == Path_Point)
    {
      break;
    }
    Path_Point = FAST_MEMORY_REF (Path_Point, STATE_POINT_NEARER_POINT);
  }

#ifdef ENABLE_DEBUGGING_TOOLS
  if (Merge_Depth < 0)
  {
    fprintf(stderr, "\nMerge_Depth went negative: %d\n", Merge_Depth);
    Microcode_Termination(TERM_EXIT);
  }
#endif /* ENABLE_DEBUGGING_TOOLS */

 Will_Push(2*CONTINUATION_SIZE + 4);
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(LONG_TO_FIXNUM(FETCH_INTERRUPT_MASK()));
  Save_Cont();
  Push(LONG_TO_UNSIGNED_FIXNUM((Distance - Merge_Depth)));
  Push(Target);
  Push(LONG_TO_UNSIGNED_FIXNUM((From_Depth - Merge_Depth)));
  Push(Current_Location);
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

extern SCHEME_OBJECT Compiler_Get_Fixed_Objects();

SCHEME_OBJECT
Compiler_Get_Fixed_Objects()
{
  if (Valid_Fixed_Obj_Vector())
  {
    return (Get_Fixed_Obj_Slot(Me_Myself));
  }
  else
  {
    return (SHARP_F);
  }
}
