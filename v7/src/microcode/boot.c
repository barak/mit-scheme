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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/boot.c,v 9.25 1987/02/07 15:29:41 jinx Exp $
 *
 * This file contains the code to support startup of
 * the SCHEME interpreter.
  
 The command line (when not running a dumped executable version) may 
 take the following forms:

   scheme

   or

   scheme {band-name}

   or

   scheme {filespec}
          {-heap heap-size}
	  {-stack stack-size}
	  {-constant constant-size}
          {other arguments ignored by the core microcode}

   with filespec either {-band band-name} or {{-}fasl file-name}
   arguments are optional, numbers are in 1K units.  Default values
   are given above.  The arguments in the long for may appear in any
   order on the command line.  The allocation arguments (heap, stack,
   and constant) are ignored when scheme is an executable image.  A
   warning message is printed if the command line contains them.

   heap-size......number of cells to allocate for user heap; this will
                  be doubled to allow for 2 space GC.
   stack-size.....number of cells for control stack.  This primarily
                  controls maximum depth of recursion.  If the flag
		  USE_STACKLETS is defined, then this controls the
		  size of the stacklets (not the total stack) and
		  thus affects how often new stack segments must
		  be allocated.
   constant-size..number of cells for constant and pure space in the
                  system.

Additional arguments may exist for particular machines; see CONFIG.H
for details.  They are created by defining a macro Command_Line_Args.

*/

#include "scheme.h"
#include "primitive.h"
#include "prims.h"
#include "version.h"
#ifndef islower
#include <ctype.h>
#endif

#define STRING_SIZE 512
#define BLOCKSIZE 1024
#define blocks(n) ((n)*BLOCKSIZE)

/* Utilities for command line parsing */

#define upcase(c) ((islower(c)) ? (toupper(c)) : c)

void
uppercase(to_where, from_where)
fast char *to_where, *from_where;
{ fast char c;
  while((c = *from_where++) != '\0') *to_where++ = upcase(c);
  *to_where = '\0';
  return;
}

int 
Parse_Option(opt_key, nargs, args, casep)
char *opt_key, **args;
Boolean casep;
int nargs;
{ int i;
  char key[STRING_SIZE], current[STRING_SIZE];
  if (casep) uppercase(key, opt_key); else strcpy(key, opt_key);
  for(i = 0; i < nargs; i++)
  { if (casep) uppercase(current, args[i]); else strcpy(current, args[i]);
    if (strcmp(key, current) == 0) return i;
  }
  return NOT_THERE;
}

long
Def_Number(key, nargs, args, def)
char *key, **args;
long def;
int nargs;
{ int position = Parse_Option(key, nargs, args, true);
  if ((position == NOT_THERE) || (position == (nargs-1))) return def;
  else return atoi(args[position+1]);
}  

/* Obviously, the main program */

/* Used to test whether it is a dumped executable version */

extern Boolean Was_Scheme_Dumped;
Boolean Was_Scheme_Dumped = false;

/* Exit is done in a different way on some operating systems (eg. VMS)  */
Exit_Scheme_Declarations;

/* Main program */

forward void Start_Scheme();
extern void Clear_Memory(), Setup_Memory(), Reset_Memory();

void
main(argc, argv)
     int argc;
     char **argv;
{ Boolean FASL_It = false;
  char *File_Name = NULL;
  int Saved_Heap_Size, Saved_Stack_Size, Saved_Constant_Size;
  extern void compiler_initialize();

  Saved_argc = argc;
  Saved_argv = argv;
 
  Init_Exit_Scheme();

  if (argc > 2)
  { int position;
    if (((position = Parse_Option("-band", argc, argv, true))
	 != NOT_THERE) &&
	(position != (argc-1)))
      File_Name = argv[position+1];
    else if ((((position = Parse_Option("-fasl", argc, argv, true))
	      != NOT_THERE) ||
	      ((position = Parse_Option("fasl", argc, argv, true))
	      != NOT_THERE)) &&
	     (position != (argc-1)))
    { File_Name = argv[position + 1];
      FASL_It = true;
    }
  }
  else if ((argc == 2) && (argv[1][0] != '-')) File_Name = argv[1];

  if (!Was_Scheme_Dumped)
  { Heap_Size = HEAP_SIZE;
    Stack_Size = STACK_SIZE;
    Constant_Size = CONSTANT_SIZE;
  }
  else
  { Saved_Heap_Size = Heap_Size;
    Saved_Stack_Size = Stack_Size;
    Saved_Constant_Size = Constant_Size;
  }

  Heap_Size = Def_Number("-heap", argc, argv, Heap_Size);
  Stack_Size = Def_Number("-stack", argc, argv, Stack_Size);
  Constant_Size = Def_Number("-constant", argc, argv, Constant_Size);

  if (Was_Scheme_Dumped)
  { Boolean warned = false;
    printf("Executable Scheme");
    if ((Heap_Size != Saved_Heap_Size)		||
	(Stack_Size != Saved_Stack_Size)	||
	(Constant_Size != Saved_Constant_Size))
    { printf(".\n");
      fprintf(stderr,
"Warning: Allocation parameters (heap, stack, and constant) ignored.\n");
      Heap_Size = Saved_Heap_Size;
      Stack_Size = Saved_Stack_Size;
      Constant_Size = Saved_Constant_Size;
      warned = true;
    }
    if (File_Name == NULL)
    { if (!warned) printf("; ");
      printf("Microcode Version %d.%d\n", VERSION, SUBVERSION);
      OS_Init(true);
      Enter_Interpreter();
    }
    else
    { if (!warned) printf(".\n");
      Clear_Memory(blocks(Heap_Size), blocks(Stack_Size),
		   blocks(Constant_Size));
      /* We are reloading from scratch anyway. */
      Was_Scheme_Dumped = false;
      Start_Scheme(FASL_It ? PC_FASLOAD : PC_BAND_LOAD, File_Name);
    }
  }
  if (File_Name == NULL) File_Name = DEFAULT_BAND_NAME;
  Command_Line_Hook();

/* main continues on the next page */

/* main, continued */
	  
  Setup_Memory(blocks(Heap_Size), blocks(Stack_Size),
	       blocks(Constant_Size));
  compiler_initialize((long) FASL_It);
  Start_Scheme(FASL_It ? PC_FASLOAD : PC_BAND_LOAD, File_Name);
}

#define Default_Init_Fixed_Objects(Fixed_Objects)			\
{ Pointer Int_Vec, OB_Array, Error, Bad_Object,				\
          The_Queue, *Dummy_Hist, The_Utilities;			\
  fast long i;								\
	/* Interrupt vector */						\
  Int_Vec = Make_Pointer(TC_VECTOR, Free);				\
  *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR,			\
			     MAX_INTERRUPT_NUMBER + 1);			\
  for (i=0; i <= MAX_INTERRUPT_NUMBER; i++) *Free++ = NIL;		\
	/* Error vector is not needed at boot time */			\
  Error = NIL;								\
	/* Dummy History Structure */					\
  History = Make_Dummy_History();					\
  Dummy_Hist = Make_Dummy_History();					\
	/* OBArray */							\
  OB_Array = Make_Pointer(TC_VECTOR, Free);				\
  *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, OBARRAY_SIZE);		\
  for (i=0; i < OBARRAY_SIZE; i++) *Free++ = NIL;			\
        /* Non Object */						\
  Bad_Object = Make_Pointer(TC_LIST, Free);				\
  *Free++ = NIL;							\
  *Free++ = NIL;		    					\
        /* Initial empty work queue */					\
  The_Queue = Make_Pointer(TC_LIST, Free);				\
  *Free++ = NIL;							\
  *Free++ = NIL;							\
        /* Empty utilities vector */					\
  The_Utilities = Make_Pointer(TC_VECTOR, Free);			\
  *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, 0);			\
									\
	/* Now make the fixed objects vector */				\
  Fixed_Objects = Make_Pointer(TC_VECTOR, Free);			\
  *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, NFixed_Objects);	\
  for (i=1; i <= NFixed_Objects; i++) *Free++ = NIL;			\
  User_Vector_Set(Fixed_Objects, Non_Object, Bad_Object);		\
  User_Vector_Set(Fixed_Objects, System_Interrupt_Vector, Int_Vec);	\
  User_Vector_Set(Fixed_Objects, System_Error_Vector, Error);		\
  User_Vector_Set(Fixed_Objects, OBArray, OB_Array);			\
  User_Vector_Set(Fixed_Objects, Dummy_History,				\
                  Make_Pointer(TC_HUNK3, Dummy_Hist));			\
  User_Vector_Set(Fixed_Objects, State_Space_Tag, TRUTH);		\
  User_Vector_Set(Fixed_Objects, Bignum_One, Fix_To_Big(FIXNUM_0+1));	\
  User_Vector_Set(Fixed_Objects, Me_Myself, Fixed_Objects);		\
  User_Vector_Set(Fixed_Objects, The_Work_Queue, The_Queue);		\
  User_Vector_Set(Fixed_Objects, Utilities_Vector, The_Utilities);	\
}

/* Boot Scheme */

void
Start_Scheme(Start_Prim, File_Name)
int Start_Prim;
char *File_Name;
{ Pointer FName, Init_Prog, *Fasload_Call;
  fast long i;
  Boolean I_Am_Master = (Start_Prim != PC_GET_WORK);	/* Butterfly test */

  if (I_Am_Master)
    printf("Scheme Microcode Version %d.%d\n", VERSION, SUBVERSION);
  OS_Init(I_Am_Master);
  if (I_Am_Master)
  { for (i=0; i < FILE_CHANNELS; i++) Channels[i] = NULL;
    Init_Fixed_Objects();
  }

/* The initial program to execute is
        (SCODE-EVAL (FASLOAD <file-name>) SYSTEM-GLOBAL-ENVIRONMENT)
   if Start_Prim is FASLOAD.  Otherwise it is
	(BAND-LOAD <file-name>)     
*/

  FName = C_String_To_Scheme_String(File_Name);
  Fasload_Call = Free;
  switch (Start_Prim)
  { case PC_FASLOAD:	/* (SCODE-EVAL (FASLOAD <file>) GLOBAL-ENV) */
      *Free++ = Make_Non_Pointer(TC_PRIMITIVE, PC_FASLOAD);
      *Free++ = FName;
      Init_Prog = Make_Pointer(TC_PCOMB2, Free);
      *Free++ = Make_Non_Pointer(TC_PRIMITIVE, PC_SCODE_EVAL);
      *Free++ = Make_Pointer(TC_PCOMB1, Fasload_Call);
      *Free++ = Make_Non_Pointer(GLOBAL_ENV, GO_TO_GLOBAL);
      break;
    case PC_BAND_LOAD:	/* (BAND-LOAD <file>) */
      *Free++ = Make_Non_Pointer(TC_PRIMITIVE, PC_BAND_LOAD);
      *Free++ = FName;
      Init_Prog = Make_Pointer(TC_PCOMB1, Fasload_Call);
      break;
    case PC_GET_WORK:		/* ((GET-WORK)) */
      *Free++ = Make_Non_Pointer(TC_PRIMITIVE, PC_GET_WORK);
      *Free++ = NIL;
      Init_Prog = Make_Pointer(TC_COMBINATION, Free);
      *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, 1);
      *Free++ = Make_Non_Pointer(TC_PCOMB1, Fasload_Call);
      break;
  }

/* Start_Scheme continues on the next page */

/* Start_Scheme, continued */

	/* Setup registers */

  IntEnb = INT_Mask;
  IntCode = 0;
  Env = Make_Non_Pointer(GLOBAL_ENV, 0);
  Trapping = false;
  Return_Hook_Address = NULL;

	/* Give the interpreter something to chew on, and ... */

 Will_Push(CONTINUATION_SIZE);
  Store_Return(RC_END_OF_COMPUTATION);
  Store_Expression(NIL);
  Save_Cont();
 Pushed();
  Store_Expression(Init_Prog);

	/* Go to it! */

  if ((Stack_Pointer <= Stack_Guard) || (Free > MemTop))
  { fprintf(stderr, "Configuration won't hold initial data.\n");
    Microcode_Termination(TERM_EXIT);
  }
  Entry_Hook();
  Enter_Interpreter();
}

Enter_Interpreter()
{ jmp_buf Orig_Eval_Point;
  Back_To_Eval = (jmp_buf *) Orig_Eval_Point;

  Interpret(Was_Scheme_Dumped);
  fprintf(stderr, "\nThe interpreter returned to top level!\n");
  Microcode_Termination(TERM_EXIT);
}

#define IDENTITY_LENGTH 	20		/* Plenty of room */
#define ID_RELEASE		0		/* Scheme system release */
#define ID_MICRO_VERSION	1		/* Microcode version */
#define ID_MICRO_MOD		2		/* Microcode modification */
#define ID_PRINTER_WIDTH	3
#define ID_PRINTER_LENGTH	4
#define ID_NEW_LINE_CHARACTER	5
#define ID_FLONUM_PRECISION	6
#define ID_FLONUM_EXPONENT	7		/* Number of bits */

Built_In_Primitive(Prim_Microcode_Identify, 0, "MICROCODE-IDENTIFY")
{ Pointer *Result =  Free;
  long i;
  Primitive_0_Args();

  Primitive_GC_If_Needed(IDENTITY_LENGTH + VECTOR_DATA);
  *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, IDENTITY_LENGTH);
  for (i=IDENTITY_LENGTH; --i >= 0; ) *Free++ = NIL;
  Result[ID_RELEASE+VECTOR_DATA] =
    C_String_To_Scheme_String(RELEASE);
  Result[ID_MICRO_VERSION+VECTOR_DATA] =
    FIXNUM_0+VERSION;
  Result[ID_MICRO_MOD+VECTOR_DATA] =
    FIXNUM_0+SUBVERSION;
  Result[ID_PRINTER_WIDTH+VECTOR_DATA] =
    FIXNUM_0+NColumns();
  Result[ID_PRINTER_LENGTH+VECTOR_DATA] =
    FIXNUM_0+NLines();
  Result[ID_NEW_LINE_CHARACTER+VECTOR_DATA] =
    Make_Non_Pointer(TC_CHARACTER, '\n');
  Result[ID_FLONUM_PRECISION+VECTOR_DATA] =
    FIXNUM_0+FLONUM_MANTISSA_BITS;
  Result[ID_FLONUM_EXPONENT+VECTOR_DATA] =
    FIXNUM_0+FLONUM_EXPT_SIZE;
  return Make_Pointer(TC_VECTOR, Result);
}

Built_In_Primitive(Prim_Microcode_Tables_Filename,
		   0, "MICROCODE-TABLES-FILENAME")
{ char *From, *To,
       *Prefix=SCHEME_SOURCES_PATH,
       *Suffix=UCODE_TABLES_FILENAME;
  long Count=0;
  Pointer Result = Make_Pointer(TC_CHARACTER_STRING, Free);

  Primitive_0_Args();
  /* Might run out of room to do this, but not likely */
  for (From = &(Prefix[0]), To = (char *) &(Free[STRING_CHARS]);
       *From != '\0'; Count++) *To++ = *From++;
  for (From = &(Suffix[0]); *From != '\0'; Count++) *To++ = *From++;
  *To = '\0';
  Free += STRING_CHARS + (Count+sizeof(Pointer))/sizeof(Pointer);
  Vector_Set(Result, STRING_LENGTH, FIXNUM_0+Count);
  Vector_Set(Result, STRING_HEADER,
    Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, (Free-Get_Pointer(Result))-1));
  return Result;
}

/*VARARGS1*/
term_type
Microcode_Termination(Err, Micro_Error)
long Err, Micro_Error;
{ long value = 1;
  Pointer Term_Vector;
  if ((Err != TERM_HALT) &&
      (Valid_Fixed_Obj_Vector()) &&
      (Type_Code(Term_Vector =
		 Get_Fixed_Obj_Slot(Termination_Proc_Vector)) ==
       TC_VECTOR) &&
      (Vector_Length(Term_Vector) > Err))
  { Pointer Handler = User_Vector_Ref(Term_Vector, Err);
    if (Handler != NIL)
    {
     Will_Push(CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS +
 	       ((Err==TERM_NO_ERROR_HANDLER) ? 5 : 4));
      Store_Return(RC_HALT);
      Store_Expression(FIXNUM_0 + Err);
      Save_Cont();
      if (Err == TERM_NO_ERROR_HANDLER) Push(FIXNUM_0 + Micro_Error);
      Push(Val);			/* Arg 3 */
      Push(Fetch_Env());		/* Arg 2 */
      Push(Fetch_Expression());		/* Arg 1 */
      Push(Handler);			/* The handler function */
      Push(STACK_FRAME_HEADER + ((Err==TERM_NO_ERROR_HANDLER) ? 4 : 3));
     Pushed();
      longjmp(*Back_To_Eval, PRIM_NO_TRAP_APPLY);
    }
  }

/* Microcode_Termination continues on the next page */

/* Microcode_Termination, continued */

  switch(Err)
  { case TERM_BAD_PRIMITIVE:
      printf("\nBad primitive invoked.\n"); break;
    case TERM_BAD_PRIMITIVE_DURING_ERROR:
      printf("Error during unknown primitive.\n"); break;
    case TERM_BAD_ROOT:
      printf("Band file isn't a control point.\n"); break;
    case TERM_BAD_STACK:
      printf("Control stack messed up.\n"); break;
    case TERM_BROKEN_HEART:
      printf("Broken heart encountered.\n"); break;
    case TERM_COMPILER_DEATH:
      printf("Compiled code entered without compiler support.\n"); break;
    case TERM_DISK_RESTORE:
      printf("DISK restore.\n"); break;
    case TERM_EOF:
      printf("\nEnd of input stream reached.\n"); break;
    case TERM_END_OF_COMPUTATION:
      Print_Expression(Val, "End of computation; final result"); break;
    case TERM_EXIT:
      printf("Inconsistency detected.\n"); break;
    case TERM_GC_OUT_OF_SPACE:
      printf("Out of space after GC.  Needed %d, have %d\n",
	     Get_Integer(Fetch_Expression()), Space_Before_GC());
      break;
    case TERM_HALT:
      printf("User halt code.\n"); value = 0; break;
    case TERM_INVALID_TYPE_CODE:
      printf("Bad Type: check GC_Type map.\n"); break;
    case TERM_NO_ERROR_HANDLER:
      printf("\nNo handler for error code: %d\n", Micro_Error); break;
    case TERM_NO_INTERRUPT_HANDLER:
      printf("No interrupt handler.\n"); break;
    case TERM_NON_EXISTENT_CONTINUATION:
      printf("No such return code 0x%08x.\n", Fetch_Return()); break;
    case TERM_NON_POINTER_RELOCATION:
      printf("Non pointer relocation!?\n"); break;
    case TERM_STACK_ALLOCATION_FAILED:
      printf("No space for stack!?\n"); break;
    case TERM_STACK_OVERFLOW:
      printf("Recursion depth exceeded.\n"); break;
    case TERM_TERM_HANDLER:
      printf("Termination handler returned.\n"); break;
    case TERM_UNIMPLEMENTED_CONTINUATION:
      printf("Return code not implemented.\n"); break;
    case TERM_NO_SPACE:
      printf("Not enough memory.\n"); break;
    case TERM_SIGNAL:
      printf("Unhandled signal received.\n"); break;
    default: printf("Termination code 0x%x.\n", Err);
  }
  if ((Trace_On_Error) && (Err != TERM_HALT))
  { printf( "\n\nStack trace:\n\n");
    Back_Trace();
  }
  OS_Flush_Output_Buffer();
  OS_Quit();
  Reset_Memory();
  Exit_Hook();
  Exit_Scheme(value);
}

