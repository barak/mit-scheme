/* -*-C-*-

Copyright (c) 1986 Massachusetts Institute of Technology

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

/* File: utils.c
 *
 * This file contains a number of utility routines for use
 * in the Scheme scode interpreter.
 */

#include "scheme.h"
#include "primitive.h"
#include "flonum.h"
#include "winder.h"

/* Set_Up_Interrupt is called from the Interrupt
 * macro to do all of the setup for calling the user's
 * interrupt routines.
 */

void
Setup_Interrupt(Masked_Interrupts)
long Masked_Interrupts;
{ Pointer Int_Vector, Handler;
  long i, Int_Number, The_Int_Code = IntCode, New_Int_Enb;
  long Save_Space;

  Int_Vector = Get_Fixed_Obj_Slot(System_Interrupt_Vector);
  for (Int_Number=0, i=1; Int_Number < MAX_INTERRUPT_NUMBER;
       i = i<<1, Int_Number++) if ((Masked_Interrupts & i) != 0) goto OK;
  printf("Int_Vector %x\n", Int_Vector);
  printf("\nInterrupts = 0x%x, Mask= 0x%x, Masked = 0x%x\n",
         IntCode, IntEnb, Masked_Interrupts);
  Microcode_Termination(TERM_NO_INTERRUPT_HANDLER);
OK:
  New_Int_Enb = (1<<Int_Number)-1;
  Global_Interrupt_Hook();
  if (Int_Number > Vector_Length(Int_Vector))
  { printf("\nInterrupt out of range: 0x%x (vector length = 0x%x)\n",
           Int_Number, Vector_Length(Int_Vector));
    printf("Interrupts = 0x%x, Mask= 0x%x, Masked = 0x%x\n",
           IntCode, IntEnb, Masked_Interrupts);
    Microcode_Termination(TERM_NO_INTERRUPT_HANDLER);
  }
  else Handler = User_Vector_Ref(Int_Vector, Int_Number);

/* Setup_Interrupt continues on the next page */

/* Setup_Interrupt, continued */

Passed_Checks:	/* This label may be used in Global_Interrupt_Hook */
  Stop_History();
  Save_Space = CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS+3;
  if (New_Int_Enb+1 == INT_GC) Save_Space += CONTINUATION_SIZE;
 Will_Push(Save_Space);
  /* Return from interrupt handler will re-enable interrupts */
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(FIXNUM_0 + IntEnb);
  Save_Cont();
  if (New_Int_Enb+1 == INT_GC)
  { Store_Return(RC_GC_CHECK);
    Store_Expression(FIXNUM_0 + GC_Space_Needed);
    Save_Cont();
  }

/* Now make an environment frame for use in calling the
 * user supplied interrupt routine.  It will be given
 * two arguments: the UNmasked interrupt requests, and
 * the currently enabled interrupts.
 */

  Push(FIXNUM_0+IntEnb);
  Push(FIXNUM_0+The_Int_Code);
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
Err_Print(Micro_Error)
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
    case ERR_NO_HASH_TABLE:
      printf("No hash table installed.\n"); break;
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
Stack_Death()
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
Back_Out_Of_Primitive()
{ long nargs;

  /* When primitives are called from compiled code, the type code may
   * not be in the expression register.
   */

  if (Safe_Type_Code(Fetch_Expression()) == 0)
    Store_Expression(Make_Non_Pointer(TC_PRIMITIVE, Fetch_Expression()));

  /* Setup a continuation to return to compiled code if the primitive is
   * restarted and completes successfully.
   */

  nargs = N_Args_Primitive(Fetch_Expression());
  if (Type_Code(Stack_Ref(nargs)) == TC_RETURN_ADDRESS)
  { Pointer expression = Fetch_Expression();
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

void
Do_Micro_Error(Err, From_Pop_Return)
long Err;
Boolean From_Pop_Return;
{ Pointer Error_Vector, Handler;

  if (Consistency_Check)
  { Err_Print(Err);
    Print_Expression(Fetch_Expression(), "Expression was");
    printf("\nEnvironment 0x%x (#%o).\n", Fetch_Env(), Fetch_Env());
    Print_Return("Return code");
    printf( "\n");
  }
  Error_Exit_Hook();
  if (Trace_On_Error)
  { printf( "\n\nStack trace:\n\n");
    Back_Trace();
  }

#ifdef ENABLE_DEBUGGING_TOOLS
{ int *From = &(local_circle[0]), *To = &(debug_circle[0]), i;
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
  { printf("\nBogus Error Vector! I'm terribly confused!\n");
    Microcode_Termination(TERM_NO_ERROR_HANDLER, Err);
  }
  if (Err >= Vector_Length(Error_Vector))
  { if (Vector_Length(Error_Vector) == 0)
    { printf("\nEmpty Error Vector! I'm terribly confused!\n");
      Microcode_Termination(TERM_NO_ERROR_HANDLER, Err);
    }
    Handler = User_Vector_Ref(Error_Vector, ERR_BAD_ERROR_CODE);
  }
  else Handler = User_Vector_Ref(Error_Vector, Err);
  if (From_Pop_Return)
  { /* This can NOT be folded into the Will_Push below since we cannot */
    /* afford to have the Will_Push put down its own continuation. */
    /* There is guaranteed to be enough space for this one */
    /* continuation; in fact, the Will_Push here is really unneeded! */ 
   Will_Push(CONTINUATION_SIZE);
    Save_Cont();
   Pushed();
  }
 Will_Push(STACK_ENV_EXTRA_SLOTS+3+2*CONTINUATION_SIZE+HISTORY_SIZE+
           (From_Pop_Return ? 0 : 1));
  if (From_Pop_Return) Store_Expression(Val);
  else Push(Fetch_Env());
  Store_Return(From_Pop_Return? RC_POP_RETURN_ERROR : RC_EVAL_ERROR);
  Save_Cont();
  /* Return from error handler will re-enable interrupts & restore history */
  Stop_History();
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(FIXNUM_0 + IntEnb);
  Save_Cont();
  Push(FIXNUM_0+IntEnb);		    /* Arg 2:    Int. mask */
  Push(FIXNUM_0+Err);			    /* Arg 1:    Err. No   */
  Push(Handler);			    /* Function: Handler   */
  Push(STACK_FRAME_HEADER+2);
 Pushed();
  IntEnb = 0;		/* Turn off interrupts */
  New_Compiler_MemTop();
}

/* Make a Scheme string with the characters in C_String. */

Pointer
C_String_To_Scheme_String( C_String)
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
    Length = 0;
  else
    for (Length = 0;
	 (*C_String != '\0') && (Length < Max_Length);
	 Length += 1)
      *Next++ = *C_String++;
  if (Length >= Max_Length)
    Primitive_GC( MemTop - Free);
  *Next = '\0';
  Free += (2 + ((Length + sizeof( Pointer)) / sizeof( Pointer)));
  Vector_Set(Result, STRING_LENGTH, Length);
  Vector_Set(Result, STRING_HEADER,
	     Make_Non_Pointer( TC_MANIFEST_NM_VECTOR,
			      ((Free - Get_Pointer( Result)) - 1)));
  return Result;
}

Boolean
Open_File( Name, Mode_String, Handle)
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
Close_File(stream)
     FILE *stream;
{
  extern Boolean OS_file_close();

  if (!OS_file_close( stream))
    Primitive_Error( ERR_EXTERNAL_RETURN);
  return;
}

Pointer
*Make_Dummy_History()
{ Pointer *History_Rib = Free;
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
Stop_History()
{ Pointer Saved_Expression = Fetch_Expression();
  long Saved_Return_Code = Fetch_Return();
Will_Push(HISTORY_SIZE);
  Save_History(RC_RESTORE_DONT_COPY_HISTORY);
Pushed();
  Previous_Restore_History_Stacklet = NULL;
  Previous_Restore_History_Offset   =
    (Get_End_Of_Stacklet() - Stack_Pointer) +
      CONTINUATION_RETURN_CODE;
  Store_Expression(Saved_Expression);
  Store_Return(Saved_Return_Code);
  return;
}

Pointer
*Copy_Rib(Orig_Rib)
Pointer *Orig_Rib;
{ Pointer *Result, *This_Rib;
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
   Basmod.
*/

Boolean
Restore_History(Hist_Obj)
Pointer Hist_Obj;
{ Pointer *New_History, *Next_Vertebra, *Prev_Vertebra,
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

CRLF()
{ printf( "\n");
}

/* If a debugging version of the interpreter is made, then this
 * procedure is called to actually invoke a primitive.  When a
 * 'production' version is made, all of the consistency checks are
 * omitted and a macro from DEFAULT.H is used to directly code the
 * call to the primitive function.  This is only used in INTERPRET.C.
 */

#ifdef ENABLE_DEBUGGING_TOOLS
Pointer
Apply_Primitive(Primitive_Number)
long Primitive_Number;
{ Pointer Result, *Saved_Stack;
  int NArgs;
  if (Primitive_Number > MAX_PRIMITIVE)
    Primitive_Error(ERR_UNDEFINED_PRIMITIVE);
  NArgs = (int) Arg_Count_Table[Primitive_Number];
  if (Primitive_Debug) Print_Primitive(Primitive_Number);
  Saved_Stack = Stack_Pointer;
  Result = (*(Primitive_Table[Primitive_Number]))();
  if (Saved_Stack != Stack_Pointer)
  { Print_Expression(Make_Non_Pointer(TC_PRIMITIVE, Primitive_Number),
		     "Stack bad after ");
    printf( "\nStack was 0x%x, now 0x%x, #args=%d.\n",
            Saved_Stack, Stack_Pointer, NArgs);
    Microcode_Termination(TERM_EXIT);
  }
  if (Primitive_Debug)
  { Print_Expression(Result, "Primitive Result");
    printf( "\n");
  }
  return Result;
}
#endif

Built_In_Primitive(Prim_Unused, 0, "Unimplemented Primitive Handler")
{ printf("Ignoring missing primitive. Expression = 0x%02x|%06x\n",
         Type_Code(Fetch_Expression()), Datum(Fetch_Expression()));
  Primitive_Error(ERR_UNDEFINED_PRIMITIVE);
}

Pointer
Allocate_Float(F)
double F;
{ Pointer Result;
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
Allocate_New_Stacklet(N)
long N;
{ Pointer Old_Expression, *Old_Stacklet, Old_Return;
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
Find_State_Space(State_Point)
Pointer State_Point;
{ long How_Far = Get_Integer(Fast_Vector_Ref(State_Point,
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
Translate_To_Point(Target)
Pointer Target;
{ Pointer State_Space = Find_State_Space(Target);
  Pointer Current_Location, *Path = Free;
  fast Pointer Path_Point, *Path_Ptr;
  long Distance =
    Get_Integer(Fast_Vector_Ref(Target, STATE_POINT_DISTANCE_TO_ROOT));
  long Merge_Depth, From_Depth, i;

  guarantee_state_point();
  if (State_Space == NIL) Current_Location = Current_State_Point;
  else Current_Location = Vector_Ref(State_Space, STATE_SPACE_NEAREST_POINT);
  if (Target == Current_Location) longjmp(*Back_To_Eval, PRIM_POP_RETURN);
  for (Path_Ptr=(&(Path[Distance])), Path_Point=Target, i=0;
       i <= Distance;
       i++, Path_Point=Fast_Vector_Ref(Path_Point, STATE_POINT_NEARER_POINT))
    *Path_Ptr-- = Path_Point;
  From_Depth =
    Get_Integer(Fast_Vector_Ref(Current_Location, STATE_POINT_DISTANCE_TO_ROOT));
  for (Path_Point=Current_Location, Merge_Depth=From_Depth;
       Merge_Depth > Distance; Merge_Depth--)
    Path_Point = Fast_Vector_Ref(Path_Point, STATE_POINT_NEARER_POINT);
  for (Path_Ptr=(&(Path[Merge_Depth])); Merge_Depth >= 0;
       Merge_Depth--, Path_Ptr--,
       Path_Point=Fast_Vector_Ref(Path_Point, STATE_POINT_NEARER_POINT))
    if (*Path_Ptr == Path_Point) break;
#ifdef ENABLE_DEBUGGING_TOOLS
  if (Merge_Depth < 0)
  { printf("\nMerge_Depth went negative: %d\n", Merge_Depth);
    Microcode_Termination(TERM_EXIT);
  }
#endif
 Will_Push(2*CONTINUATION_SIZE + 4); 
  Store_Return(RC_RESTORE_INT_MASK);
  Store_Expression(FIXNUM_0 + IntEnb);
  Save_Cont();
  Push(FIXNUM_0+(Distance-Merge_Depth));
  Push(Target);
  Push(FIXNUM_0+(From_Depth-Merge_Depth));
  Push(Current_Location);
  Store_Expression(State_Space);
  Store_Return(RC_MOVE_TO_ADJACENT_POINT);
  Save_Cont();
 Pushed();
  IntEnb &= (INT_GC<<1) - 1;	/* Disable lower than GC level */
  longjmp(*Back_To_Eval, PRIM_POP_RETURN);
}
