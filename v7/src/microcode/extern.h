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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/extern.h,v 9.24 1987/04/16 02:21:28 jinx Exp $
 *
 * External declarations.
 *
 */

#ifdef ENABLE_DEBUGGING_TOOLS

extern Boolean Eval_Debug, Hex_Input_Debug, Cont_Debug,
               File_Load_Debug, Reloc_Debug, Intern_Debug,
               Primitive_Debug, Define_Debug, Lookup_Debug, GC_Debug,
               Upgrade_Debug, Trace_On_Error, Dump_Debug, Per_File,
               Bignum_Debug, Fluids_Debug;

extern sp_record_list SP_List;
extern void Pop_Return_Break_Point();
extern int debug_slotno, debug_nslots, local_slotno, local_nslots,
	   debug_circle[], local_circle[];
#else
#define Eval_Debug		false
#define Hex_Input_Debug		false
#define File_Load_Debug		false
#define Reloc_Debug		false
#define Intern_Debug		false
#define Cont_Debug		false
#define Primitive_Debug		false
#define Lookup_Debug		false
#define Define_Debug		false
#define GC_Debug		false
#define Upgrade_Debug		false
#define Trace_On_Error          false
#define Dump_Debug		false
#define Per_File		false
#define Bignum_Debug		false
#define Fluids_Debug		false
#endif

/* The register block */

extern Pointer Registers[];

extern Pointer
 *Ext_History,		/* History register */
 *Free,			/* Next free word in heap */
 *MemTop,		/* Top of heap space available */
 *Ext_Stack_Pointer,	/* Next available slot in control stack */
 *Stack_Top,		/* Top of control stack */
 *Stack_Guard,		/* Guard area at end of stack */
 *Free_Stacklets,	/* Free list of stacklets */
 *Constant_Space,	/* Bottom of constant+pure space */
 *Free_Constant,	/* Next free cell in constant+pure area */
 *Heap_Top,		/* Top of current heap space */
 *Heap_Bottom,		/* Bottom of current heap space */
 *Unused_Heap_Top,	/* Top of unused heap for GC */
 *Unused_Heap,		/* Bottom of unused heap for GC */
 *Local_Heap_Base,	/* Per-processor CONSing area */
 *Heap,			/* Bottom of all heap space */
  Current_State_Point,	/* Dynamic state point */
  Fluid_Bindings,	/* Fluid bindings AList */
  return_to_interpreter, /* Return address/code left by interpreter
			    when calling compiled code */
 *last_return_code;	/* Address of the most recent return code in the stack.
			   This is only meaningful while in compiled code.
			   *** This must be changed when stacklets are used. ***
			 */

extern Declare_Fixed_Objects();
		
extern long IntCode,	/* Interrupts requesting */
 	    IntEnb,	/* Interrupts enabled */
            GC_Reserve,	/* Scheme pointer overflow space in heap */
	    GC_Space_Needed, /* Amount of space needed when GC triggered */
	    /* Used to signal microcode errors from compiled code. */
	    compiled_code_error_code;

/* The lookup routines receive the slot location using these: */
extern Pointer Lookup_Base;
extern long Lookup_Offset;

extern char *Return_Names[];
extern long MAX_RETURN;

extern char *CONT_PRINT_RETURN_MESSAGE,
            *CONT_PRINT_EXPR_MESSAGE,
            *RESTORE_CONT_RETURN_MESSAGE,
            *RESTORE_CONT_EXPR_MESSAGE;

extern int GC_Type_Map[];

extern Boolean Photo_Open; /* Photo file open */
extern jmp_buf *Back_To_Eval;
extern Boolean Trapping;
extern Pointer Old_Return_Code, *Return_Hook_Address;

extern Pointer *Prev_Restore_History_Stacklet;
extern long Prev_Restore_History_Offset;

/* And file "channels" */

extern FILE *(Channels[FILE_CHANNELS]);
extern FILE *File_Handle;	/* Used by Fasload/Fasdump */
extern FILE *Photo_File_Handle;	/* Used by Photo */

extern int Saved_argc;
extern char **Saved_argv;
extern char *OS_Name, *OS_Variant;
extern long Heap_Size, Constant_Size, Stack_Size;
extern Pointer *Highest_Allocated_Address;

/* Environment lookup utilities. */

extern long Lex_Ref(), Local_Set(), Lex_Set(),
            Symbol_Lex_Ref(), Symbol_Lex_Set();

/* String utilities */

extern Pointer C_String_To_Scheme_String();

#define Scheme_String_To_C_String(Scheme_String) 		\
   ((char *) Nth_Vector_Loc(Scheme_String, STRING_CHARS))

/* Numeric utilities */

extern int Scheme_Integer_To_C_Integer();
extern Pointer C_Integer_To_Scheme_Integer(), Allocate_Float(), 
               Float_To_Big(), Big_To_Float(), Big_To_Fix(),
	       Fix_To_Big();

/* Random and OS utilities */

extern int Parse_Option();
extern Boolean Open_File(), Restore_History(), Open_Dump_File();
extern long NColumns(), NLines(), System_Clock();
extern void OS_Flush_Output_Buffer();
extern void Load_Data(), Write_Data(), OS_Re_Init();

/* Memory management utilities */

extern Pointer Purify_Pass_2(), Fasload();
extern Boolean Pure_Test();

/* Interpreter utilities */

extern term_type Microcode_Termination();
extern void Interpret(), Do_Micro_Error(), Setup_Interrupt(), 
	    Back_Out_Of_Primitive(), Translate_To_Point(),
	    Stop_History(), Stack_Death();

#ifdef USE_STACKLETS
extern void Allocate_New_Stacklet();
#endif

extern Pointer *Make_Dummy_History(), Find_State_Space();

/* Debugging utilities */

extern void Back_Trace(), Handle_Debug_Flags(),
            Find_Symbol(), Show_Env(), Show_Pure(), 
	    Print_Return(), Print_Expression(), Print_Primitive();

/* Conditional utilities */

#if false
extern void Clear_Perfinfo_Data();
#endif
