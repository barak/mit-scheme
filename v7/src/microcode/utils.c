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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/utils.c,v 9.31 1987/06/11 21:51:44 cph Exp $ */

/* This file contains utilities for interrupts, errors, etc. */

#include "scheme.h"
#include "primitive.h"
#include "flonum.h"
#include "winder.h"

/* Set_Up_Interrupt is called from the Interrupt
 * macro to do all of the setup for calling the user's
 * interrupt routines.
 */

void
Setup_Interrupt (Masked_Interrupts)
     long Masked_Interrupts;
{
  Pointer Int_Vector, Handler;
  long i, Int_Number, The_Int_Code, New_Int_Enb;
  long Save_Space;

  The_Int_Code = IntCode;
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
	  New_Int_Enb = IntEnb;
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
  if (Int_Number >= (Vector_Length (Int_Vector)))
    {
      fprintf (stderr,
	       "\nInterrupt out of range: 0x%x (vector length = 0x%x)\n",
	       Int_Number, (Vector_Length (Int_Vector)));
      fprintf (stderr,
	       "Interrupts = 0x%x, Mask= 0x%x, Masked = 0x%x\n",
	       IntCode, IntEnb, Masked_Interrupts);
      Microcode_Termination (TERM_NO_INTERRUPT_HANDLER);
    }

  Global_Interrupt_Hook ();
  Handler = (User_Vector_Ref (Int_Vector, Int_Number));

/* Setup_Interrupt continues on the next page */

/* Setup_Interrupt, continued */

Passed_Checks:	/* This label may be used in Global_Interrupt_Hook */
  Stop_History();
  Save_Space = CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS+3;
  if (New_Int_Enb+1 == INT_GC) Save_Space += CONTINUATION_SIZE;
 Will_Push(Save_Space);
  /* Return from interrupt handler will re-enable interrupts */
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(Make_Unsigned_Fixnum(IntEnb));
  Save_Cont();
  if (New_Int_Enb+1 == INT_GC)
  { Store_Return(RC_GC_CHECK);
    Store_Expression(Make_Unsigned_Fixnum(GC_Space_Needed));
    Save_Cont();
  }

/* Now make an environment frame for use in calling the
 * user supplied interrupt routine.  It will be given
 * two arguments: the UNmasked interrupt requests, and
 * the currently enabled interrupts.
 */

  Push(Make_Unsigned_Fixnum(IntEnb));
  Push(Make_Unsigned_Fixnum(The_Int_Code));
  Push(Handler);
  Push(STACK_FRAME_HEADER+2);
 Pushed();
  IntEnb = New_Int_Enb;	/* Turn off interrupts */
  New_Compiler_MemTop();
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
Err_Print (Micro_Error)
     long Micro_Error;
{ switch (Micro_Error)
  { 
/*  case ERR_BAD_ERROR_CODE:
      printf("unknown error code.\n"); break;
*/
    case ERR_UNBOUND_VARIABLE:
      printf("unbound variable.\n"); break;
    case ERR_UNASSIGNED_VARIABLE:
      printf("unassigned variable.\n"); break;
    case ERR_INAPPLICABLE_OBJECT:
      printf("Inapplicable operator.\n"); break;
    case ERR_BAD_FRAME:
      printf("bad environment frame.\n"); break;
    case ERR_BROKEN_COMPILED_VARIABLE:
      printf("compiled variable invalid.\n"); break;
    case ERR_UNDEFINED_USER_TYPE:
      printf("undefined type code.\n"); break;
    case ERR_UNDEFINED_PRIMITIVE:
      printf("undefined primitive.\n"); break;
    case ERR_EXTERNAL_RETURN:
      printf("error during 'external' primitive.\n"); break;
    case ERR_EXECUTE_MANIFEST_VECTOR:
      printf("attempt to EVAL a vector.\n"); break;
    case ERR_WRONG_NUMBER_OF_ARGUMENTS:
      printf("wrong number of arguments.\n"); break;
    case ERR_ARG_1_WRONG_TYPE:
      printf("type error argument 1.\n"); break;
    case ERR_ARG_2_WRONG_TYPE:
      printf("type error argument 2.\n"); break;

/* Err_Print continues on the next page */

/* Err_Print, continued */

    case ERR_ARG_3_WRONG_TYPE:
      printf("type error argument 3.\n"); break;
    case ERR_ARG_1_BAD_RANGE:
      printf("range error argument 1.\n"); break;
    case ERR_ARG_2_BAD_RANGE:
      printf("range error, argument 2.\n"); break;
    case ERR_ARG_3_BAD_RANGE:
      printf("range error, argument 3.\n"); break;
    case ERR_FASL_FILE_TOO_BIG:
      printf("FASL file too large to load.\n"); break;
    case ERR_FASL_FILE_BAD_DATA:
      printf("No such file or not FASL format.\n"); break;
    case ERR_IMPURIFY_OUT_OF_SPACE:
      printf("Not enough room to impurify object.\n"); break;
    case ERR_WRITE_INTO_PURE_SPACE:
      printf("Write into pure area\n"); break;
    case ERR_BAD_SET:
      printf("Attempt to perform side-effect on 'self'.\n"); break;
    case ERR_ARG_1_FAILED_COERCION:
      printf("First argument couldn't be coerced.\n"); break;
    case ERR_ARG_2_FAILED_COERCION:
      printf("Second argument couldn't be coerced.\n"); break;
    case ERR_OUT_OF_FILE_HANDLES:
      printf("Too many open files.\n"); break;
    default:
      printf("Unknown error 0x%x occurred\n.", Micro_Error);
      break;
  }
  return;
}

void
Stack_Death ()
{ fprintf(stderr, "\nWill_Push vs. Pushed inconsistency.\n");
  Microcode_Termination(TERM_BAD_STACK);
}      

/* Back_Out_Of_Primitive sets the registers up so that the backout
 * mechanism in interpret.c will push the primitive number and
 * an appropriate return code so that the primitive can be
 * restarted.
 */

#if (TC_PRIMITIVE == 0) || (TC_PRIMITIVE_EXTERNAL == 0)
#include "Error: Some primitive type is 0"
#endif

void
Back_Out_Of_Primitive ()
{
  long nargs;
  Pointer expression = Fetch_Expression();

  /* When primitives are called from compiled code, the type code may
   * not be in the expression register.
   */

  if (Safe_Type_Code(expression) == 0)
  {
    expression = Make_Non_Pointer(TC_PRIMITIVE, expression);
    Store_Expression(expression);
  }

  /* Setup a continuation to return to compiled code if the primitive is
   * restarted and completes successfully.
   */

  nargs = N_Args_Primitive(Get_Integer(expression));
  if (Type_Code(Stack_Ref(nargs)) == TC_RETURN_ADDRESS)
  { 
    /* This clobbers the expression register. */
    compiler_apply_procedure(nargs);
    Store_Expression(expression);
  }

  /* When you come back to the primitive, the environment is
   * irrelevant .... primitives run with no real environment.
   * Similarly, the value register is meaningless. 
   */
  Store_Return(RC_REPEAT_PRIMITIVE);
  Store_Env(Make_Non_Pointer(GLOBAL_ENV, END_OF_CHAIN));
  Val = NIL;
}

/* Useful error procedures */

extern void
  signal_error_from_primitive(),
  signal_interrupt_from_primitive(),
  special_interrupt_from_primitive(),
  error_wrong_type_arg(),
  error_bad_range_arg(),
  error_external_return();

void
signal_error_from_primitive (error_code)
     long error_code;
{
  Back_Out_Of_Primitive ();
  longjmp (*Back_To_Eval, error_code);
  /*NOTREACHED*/
}

void
signal_interrupt_from_primitive ()
{
  Back_Out_Of_Primitive ();
  longjmp (*Back_To_Eval, PRIM_INTERRUPT);
  /*NOTREACHED*/
}

void
special_interrupt_from_primitive(local_mask)
     int local_mask;
{
  Back_Out_Of_Primitive();
  Save_Cont();
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(Make_Unsigned_Fixnum(IntEnb));
  IntEnb = (local_mask);
  longjmp(*Back_To_Eval, PRIM_INTERRUPT);
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
arg_nonnegative_integer (n)
     int n;
{
  fast Pointer argument;

  CHECK_ARG (n, FIXNUM_P);
  argument = (ARG_REF (n));
  if (FIXNUM_NEGATIVE_P (argument))
    error_bad_range_arg (n);
  return (UNSIGNED_FIXNUM_VALUE (argument));
}

long
arg_index_integer (n, upper_limit)
     int n;
     long upper_limit;
{
  fast Pointer argument;
  fast long result;

  CHECK_ARG (n, FIXNUM_P);
  argument = (ARG_REF (n));
  if (FIXNUM_NEGATIVE_P (argument))
    error_bad_range_arg (n);
  result = (UNSIGNED_FIXNUM_VALUE (argument));
  if (result >= upper_limit)
    error_bad_range_arg (n);
  return (result);
}

void
Do_Micro_Error (Err, From_Pop_Return)
     long Err;
     Boolean From_Pop_Return;
{
  Pointer Error_Vector, Handler;

  if (Consistency_Check)
  { Err_Print(Err);
    Print_Expression(Fetch_Expression(), "Expression was");
    printf("\nEnvironment 0x%x (#%o).\n", Fetch_Env(), Fetch_Env());
    Print_Return("Return code");
    printf( "\n");
  }

  Error_Exit_Hook();

  if (Trace_On_Error)
  {
    printf( "\n**** Stack Trace ****\n\n");
    Back_Trace();
  }

#ifdef ENABLE_DEBUGGING_TOOLS
  {
    int *From = &(local_circle[0]), *To = &(debug_circle[0]), i;

    for (i=0; i < local_nslots; i++) *To++ = *From++;
    debug_nslots = local_nslots;
    debug_slotno = local_slotno;
  }
#endif  

/* Do_Micro_Error continues on the next page. */

/* Do_Micro_Error, continued */

  if ((!Valid_Fixed_Obj_Vector()) ||
      (Type_Code((Error_Vector = 
		  Get_Fixed_Obj_Slot(System_Error_Vector))) !=
       TC_VECTOR))
  {
    fprintf(stderr,
	    "\nMicrocode Error: code = 0x%x; Bad error handlers vector.\n",
	    Err);
    printf("\n**** Stack Trace ****\n\n");
    Back_Trace();
    Microcode_Termination(TERM_NO_ERROR_HANDLER, Err);
  }

  if ((Err < 0) || (Err >= (Vector_Length (Error_Vector))))
    {
      if (Vector_Length(Error_Vector) == 0)
	{
	  fprintf(stderr,
		  "\nMicrocode Error: code = 0x%x; Empty error handlers vector.\n",
		  Err);
	  printf("\n**** Stack Trace ****\n\n");
	  Back_Trace();
	  Microcode_Termination(TERM_NO_ERROR_HANDLER, Err);
	}
      Handler = (User_Vector_Ref (Error_Vector, ERR_BAD_ERROR_CODE));
    }
  else
    Handler = (User_Vector_Ref (Error_Vector, Err));

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
 Will_Push(STACK_ENV_EXTRA_SLOTS+3+2*CONTINUATION_SIZE+HISTORY_SIZE+
           (From_Pop_Return ? 0 : 1));

  if (From_Pop_Return)
    Store_Expression(Val);
  else
    Push(Fetch_Env());

  Store_Return((From_Pop_Return) ?
	       RC_POP_RETURN_ERROR :
	       RC_EVAL_ERROR);
  Save_Cont();

  /* Return from error handler will re-enable interrupts & restore history */

  Stop_History();
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(Make_Unsigned_Fixnum(IntEnb));
  Save_Cont();
  Push(Make_Unsigned_Fixnum(IntEnb));	 /* Arg 2:     Int. mask */
  if ((Err >= SMALLEST_FIXNUM) && (Err <= BIGGEST_FIXNUM))
    Push(Make_Signed_Fixnum(Err));	 /* Arg 1:     Err. No   */
  else
    Push (Make_Unsigned_Fixnum (ERR_BAD_ERROR_CODE));
  Push(Handler);			 /* Procedure: Handler   */
  Push(STACK_FRAME_HEADER+2);
 Pushed();

  IntEnb = 0;				/* Turn off interrupts */
  New_Compiler_MemTop();
}

/* Make a Scheme string with the characters in C_String. */

Pointer
C_String_To_Scheme_String (C_String)
     fast char *C_String;
{
  fast char *Next;
  fast long Length, Max_Length;
  Pointer Result;

  Result = Make_Pointer( TC_CHARACTER_STRING, Free);
  Next = (char *) Nth_Vector_Loc( Result, STRING_CHARS);
  Max_Length = ((Space_Before_GC() - STRING_CHARS) *
                sizeof( Pointer));
  if (C_String == NULL)
    {
      Length = 0;
      if (Max_Length < 0)
	Primitive_GC(3);
    }
  else
    {
      for (Length = 0;
	   (*C_String != '\0') && (Length < Max_Length);
	   Length += 1)
	*Next++ = *C_String++;
      if (Length >= Max_Length)
	{
	  while (*C_String++ != '\0')
	    Length += 1;
	  Primitive_GC(2 +
		       (((Length + 1) + (sizeof( Pointer) - 1))
			/ sizeof( Pointer)));
	}
    }
  *Next = '\0';
  Free += (2 + ((Length + sizeof( Pointer)) / sizeof( Pointer)));
  Vector_Set(Result, STRING_LENGTH, Length);
  Vector_Set(Result, STRING_HEADER,
	     Make_Non_Pointer( TC_MANIFEST_NM_VECTOR,
			      ((Free - Get_Pointer( Result)) - 1)));
  return Result;
}

Boolean
Open_File (Name, Mode_String, Handle)
     Pointer Name;
     char *Mode_String;
     FILE **Handle;
{
  *Handle =
    ((FILE *)
     OS_file_open( Scheme_String_To_C_String( Name), (*Mode_String == 'w')));
  return ((Boolean) (*Handle != NULL));
}

void
Close_File (stream)
     FILE *stream;
{
  extern Boolean OS_file_close();

  if (!OS_file_close( stream))
    Primitive_Error( ERR_EXTERNAL_RETURN);
  return;
}

Pointer *
Make_Dummy_History ()
{
  Pointer *History_Rib = Free;
  Pointer *Result;

  Free[RIB_EXP] = NIL;
  Free[RIB_ENV] = NIL;
  Free[RIB_NEXT_REDUCTION] =
    Make_Pointer(TC_HUNK3, History_Rib);
  Free += 3;
  Result = Free;
  Free[HIST_RIB] = Make_Pointer(TC_HUNK3, History_Rib);
  Free[HIST_NEXT_SUBPROBLEM] =
    Make_Pointer(TC_HUNK3, Result);
  Free[HIST_PREV_SUBPROBLEM] =
    Make_Pointer(TC_HUNK3, Result);
  Free += 3;
  return Result;
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
  Pointer Saved_Expression = Fetch_Expression();
  long Saved_Return_Code = Fetch_Return();

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

Pointer *
Copy_Rib (Orig_Rib)
     Pointer *Orig_Rib;
{
  Pointer *Result, *This_Rib;

  for (This_Rib=NULL, Result=Free;
       (This_Rib != Orig_Rib) && (!GC_Check(0));
       This_Rib = Get_Pointer(This_Rib[RIB_NEXT_REDUCTION]))
  { if (This_Rib==NULL) This_Rib = Orig_Rib;
    Free[RIB_EXP] = This_Rib[RIB_EXP];
    Free[RIB_ENV] = This_Rib[RIB_ENV];
    Free[RIB_NEXT_REDUCTION] = Make_Pointer(TC_HUNK3, Free+3);
    if (Dangerous(This_Rib[RIB_MARK])) Free[RIB_MARK] |= DANGER_BIT;
    Free += 3;
  }
  Store_Address((Free-3)[RIB_NEXT_REDUCTION], C_To_Scheme(Result));
  return Result;
}

/* Restore_History pops a history object off the stack and
   makes a COPY of it the current history collection object.
   This is called only from the RC_RESTORE_HISTORY case in
   interpret.c .
*/

Boolean
Restore_History (Hist_Obj)
     Pointer Hist_Obj;
{
  Pointer *New_History, *Next_Vertebra, *Prev_Vertebra,
          *Orig_Vertebra;

  if (Consistency_Check)
    if (Type_Code(Hist_Obj) != TC_HUNK3)
    { printf("Bad history to restore.\n");
      Microcode_Termination(TERM_EXIT);
    }
  Orig_Vertebra = Get_Pointer(Hist_Obj);
  for (Next_Vertebra=NULL, Prev_Vertebra=NULL;
       Next_Vertebra != Orig_Vertebra;
       Next_Vertebra = 
         Get_Pointer(Next_Vertebra[HIST_NEXT_SUBPROBLEM]))
  { Pointer *New_Rib;
    if (Prev_Vertebra==NULL) Next_Vertebra = Orig_Vertebra;
    New_Rib = Copy_Rib(Get_Pointer(Next_Vertebra[HIST_RIB]));
    if (Prev_Vertebra==NULL) New_History = Free;
    else Prev_Vertebra[HIST_NEXT_SUBPROBLEM] =
           Make_Pointer(TC_HUNK3, Free);
    Free[HIST_RIB] = Make_Pointer(TC_HUNK3, New_Rib);
    Free[HIST_NEXT_SUBPROBLEM] = NIL;
    Free[HIST_PREV_SUBPROBLEM] =
      Make_Pointer(TC_HUNK3, Prev_Vertebra);
    if (Dangerous(Next_Vertebra[HIST_MARK]))
      Free[HIST_MARK] |= DANGER_BIT;
    Prev_Vertebra = Free;
    Free += 3;
    if (GC_Check(0)) return false;
  }
  Store_Address(New_History[HIST_PREV_SUBPROBLEM], C_To_Scheme(Free-3));
  Prev_Vertebra[HIST_NEXT_SUBPROBLEM] =
    Make_Pointer(TC_HUNK3, New_History); 
  if (Dangerous(Orig_Vertebra[HIST_MARK]))
    Prev_Vertebra[HIST_MARK] |= DANGER_BIT;
  History = New_History;
  return true;
}

CRLF ()
{
  printf( "\n");
}

/* If a debugging version of the interpreter is made, then this
 * procedure is called to actually invoke a primitive.  When a
 * 'production' version is made, all of the consistency checks are
 * omitted and a macro from DEFAULT.H is used to directly code the
 * call to the primitive function.  This is only used in INTERPRET.C.
 */

#ifdef ENABLE_DEBUGGING_TOOLS
Pointer
Apply_Primitive (Primitive_Number)
     long Primitive_Number;
{
  Pointer Result, *Saved_Stack;
  int NArgs;

  if (Primitive_Number > MAX_PRIMITIVE)
  {
    Primitive_Error(ERR_UNDEFINED_PRIMITIVE);
  }
  if (Primitive_Debug)
  {
    Print_Primitive(Primitive_Number);
  }
  NArgs = N_Args_Primitive(Primitive_Number);
  Saved_Stack = Stack_Pointer;
  Result = Internal_Apply_Primitive(Primitive_Number);
  if (Saved_Stack != Stack_Pointer)
  {
    Print_Expression(Make_Non_Pointer(TC_PRIMITIVE, Primitive_Number),
		     "Stack bad after ");
    fprintf(stderr,
	    "\nStack was 0x%x, now 0x%x, #args=%d.\n",
            Saved_Stack, Stack_Pointer, NArgs);
    Microcode_Termination(TERM_EXIT);
  }
  if (Primitive_Debug)
  {
    Print_Expression(Result, "Primitive Result");
    fprintf(stderr, "\n");
  }
  return Result;
}
#endif

#ifdef ENABLE_PRIMITIVE_PROFILING

/* The profiling mechanism is enabled by storing a cons of two vectors
   in the fixed objects vector.  The car will record the profiling for
   built-in primitives, and the cdr for user defined primitives.  Both
   vectors should be initialized to contain all zeros. */

void
record_primitive_entry (primitive)
     Pointer primitive;
{
  if ((Fixed_Objects != NIL) &&
      ((Get_Fixed_Obj_Slot (Primitive_Profiling_Table)) != NIL))
    {
      Pointer table;
      long index, old_value;

      /* Test for TC_PRIMITIVE_EXTERNAL rather than TC_PRIMITIVE here
	 because the compiled code interface will use 0 rather than
	 TC_PRIMITIVE. */
      table =
	(Vector_Ref
	 ((Get_Fixed_Obj_Slot (Primitive_Profiling_Table)),
	  (((pointer_type (primitive)) == TC_PRIMITIVE_EXTERNAL) ? 1 : 0)));
      index = (1 + (pointer_datum (primitive)));
      Scheme_Integer_To_C_Integer ((Vector_Ref (table, index)), &old_value);
      Vector_Set (table, index, (C_Integer_To_Scheme_Integer (1 + old_value)));
    }
}

#endif

Pointer
Allocate_Float (F)
     double F;
{
  Pointer Result;

  Align_Float(Free);
  Result = Make_Pointer(TC_BIG_FLONUM, Free);
  *Free = Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, FLONUM_SIZE);
  Get_Float(C_To_Scheme(Free)) = F;
  Primitive_GC_If_Needed(FLONUM_SIZE+1);
  Free += FLONUM_SIZE+1;
  return Result;
}

#ifdef USE_STACKLETS
                      /******************/
                      /*   STACKLETS    */
                      /******************/

void
Allocate_New_Stacklet (N)
     long N;
{
  Pointer Old_Expression, *Old_Stacklet, Old_Return;

  Old_Stacklet = Current_Stacklet;
  Terminate_Old_Stacklet();
  if ((Free_Stacklets == NULL) ||
      ((N+STACKLET_SLACK) > Get_Integer(Free_Stacklets[STACKLET_LENGTH])))
  { long size = New_Stacklet_Size(N);
    /* Room is set aside for the two header bytes of a stacklet plus
     * the two bytes required for the RC_JOIN_STACKLETS frame.
     */
    if (GC_Check(size))
    { Request_GC(size);
      if (Free+size >= Heap_Top)
	Microcode_Termination(TERM_STACK_OVERFLOW);
    }
    Free[STACKLET_LENGTH] = Make_Non_Pointer(TC_MANIFEST_VECTOR, size-1);
    Stack_Guard = &(Free[STACKLET_HEADER_SIZE]);
    Free += size;
    Stack_Pointer = Free;
  } 
  else /* Grab first one on the free list */
  { Pointer *New_Stacklet = Free_Stacklets;
    Free_Stacklets = ((Pointer *) Free_Stacklets[STACKLET_FREE_LIST_LINK]);
    Stack_Pointer =
      &New_Stacklet[1 + Get_Integer(New_Stacklet[STACKLET_LENGTH])];
    Stack_Guard = &New_Stacklet[STACKLET_HEADER_SIZE];
  }
  Old_Expression = Fetch_Expression();
  Old_Return = Fetch_Return();
  Store_Expression(Make_Pointer(TC_CONTROL_POINT, Old_Stacklet));
  Store_Return(RC_JOIN_STACKLETS);
/* Will_Push omitted because size calculation includes enough room. */
  Save_Cont();
  Store_Expression(Old_Expression);
  Store_Return(Old_Return);
  return;
}
#endif

/* Dynamic Winder support code */

Pointer
Find_State_Space (State_Point)
     Pointer State_Point;
{
  long How_Far = Get_Integer(Fast_Vector_Ref(State_Point,
					     STATE_POINT_DISTANCE_TO_ROOT));
  long i;
  fast Pointer Point = State_Point;

  for (i=0; i <= How_Far; i++)
  { 
#ifdef ENABLE_DEBUGGING_TOOLS
    if (Point == NIL)
    { printf("\nState_Point 0x%x wrong: count was %d, NIL at %d\n",
	     State_Point, How_Far, i);
      Microcode_Termination(TERM_EXIT);
    }
#endif
    Point = Fast_Vector_Ref(Point, STATE_POINT_NEARER_POINT);
  }
  return Point; 
}

/* ASSUMPTION: State points, which are created only by the interpreter,
   never contain FUTUREs except possibly as the thunks (which are handled
   by the apply code).

   Furthermore: 
     (1) On a single processor, things should work with multiple state
	 spaces.  The microcode variable Current_State_Point tracks
	 the location in the "boot" space (i.e. the one whose space is
	 NIL) and the state spaces themselves (roots of the space
	 trees) track the other spaces.
     (2) On multi-processors, multiple spaces DO NOT work.  Only the
	 initial space (NIL) is tracked by the microcode (it is
	 swapped on every task switch), but no association with trees
	 is kept.  This will work since the initial tree has no space
	 at the root, indicating that the microcode variable rather
	 than the state space contains the current state space
	 location.
*/

void
Translate_To_Point (Target)
     Pointer Target;
{
  Pointer State_Space = Find_State_Space(Target);
  Pointer Current_Location, *Path = Free;
  fast Pointer Path_Point, *Path_Ptr;
  long Distance, Merge_Depth, From_Depth, i;

  guarantee_state_point();
  Distance =
    Get_Integer(Fast_Vector_Ref(Target, STATE_POINT_DISTANCE_TO_ROOT));
  if (State_Space == NIL)
    Current_Location = Current_State_Point;
  else
    Current_Location = Vector_Ref(State_Space, STATE_SPACE_NEAREST_POINT);
  if (Target == Current_Location)
    longjmp(*Back_To_Eval, PRIM_POP_RETURN);
  for (Path_Ptr=(&(Path[Distance])), Path_Point=Target, i=0;
       i <= Distance;
       i++, Path_Point=Fast_Vector_Ref(Path_Point, STATE_POINT_NEARER_POINT))
    *Path_Ptr-- = Path_Point;
  From_Depth =
    Get_Integer(Fast_Vector_Ref(Current_Location, STATE_POINT_DISTANCE_TO_ROOT));
  for (Path_Point=Current_Location, Merge_Depth = From_Depth;
       Merge_Depth > Distance;
       Merge_Depth--)
    Path_Point = Fast_Vector_Ref(Path_Point, STATE_POINT_NEARER_POINT);
  for (Path_Ptr=(&(Path[Merge_Depth])); Merge_Depth >= 0;
       Merge_Depth--, Path_Ptr--,
       Path_Point=Fast_Vector_Ref(Path_Point, STATE_POINT_NEARER_POINT))
    if (*Path_Ptr == Path_Point)
      break;
#ifdef ENABLE_DEBUGGING_TOOLS
  if (Merge_Depth < 0)
  {
    fprintf(stderr, "\nMerge_Depth went negative: %d\n", Merge_Depth);
    Microcode_Termination(TERM_EXIT);
  }
#endif
 Will_Push(2*CONTINUATION_SIZE + 4); 
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(Make_Unsigned_Fixnum(IntEnb));
  Save_Cont();
  Push(Make_Unsigned_Fixnum((Distance-Merge_Depth)));
  Push(Target);
  Push(Make_Unsigned_Fixnum((From_Depth-Merge_Depth)));
  Push(Current_Location);
  Store_Expression(State_Space);
  Store_Return(RC_MOVE_TO_ADJACENT_POINT);
  Save_Cont();
 Pushed();
  IntEnb &= (INT_GC<<1) - 1;	/* Disable lower than GC level */
  longjmp(*Back_To_Eval, PRIM_POP_RETURN);
  /*NOTREACHED*/
}
