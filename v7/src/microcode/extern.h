/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/extern.h,v 9.33 1989/09/20 23:07:50 cph Exp $

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

/* External Declarations */

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

#else /* not ENABLE_DEBUGGING_TOOLS */

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

#endif /* ENABLE_DEBUGGING_TOOLS */

/* The register block */

extern SCHEME_OBJECT Registers [];

extern SCHEME_OBJECT
 * Ext_History,		/* History register */
 * Free,		/* Next free word in heap */
 * MemTop,		/* Top of heap space available */
 * Ext_Stack_Pointer,	/* Next available slot in control stack */
 * Stack_Top,		/* Top of control stack */
 * Stack_Guard,		/* Guard area at end of stack */
 * Free_Stacklets,	/* Free list of stacklets */
 * Constant_Space,	/* Bottom of constant+pure space */
 * Free_Constant,	/* Next free cell in constant+pure area */
 * Constant_Top,	/* Top of constant+pure space */
 * Heap_Top,		/* Top of current heap space */
 * Heap_Bottom,		/* Bottom of current heap space */
 * Unused_Heap_Top,	/* Top of unused heap for GC */
 * Unused_Heap,		/* Bottom of unused heap for GC */
 * Local_Heap_Base,	/* Per-processor CONSing area */
 * Heap,		/* Bottom of all heap space */
   Current_State_Point,	/* Dynamic state point */
   Fluid_Bindings,	/* Fluid bindings AList */

  /* Address of the most recent return code in the stack.  This is
     only meaningful while in compiled code.  *** This must be changed
     when stacklets are used. *** */
 * last_return_code,

  /* Return code/address used by the compiled code interface to make
     compiled code return to the interpreter.  */
   return_to_interpreter;

extern Declare_Fixed_Objects ();

extern long
  IntCode,		/* Interrupts requesting */
  IntEnb,		/* Interrupts enabled */
  temp_long,		/* temporary for sign extension */
  GC_Reserve,		/* Scheme pointer overflow space in heap */
  GC_Space_Needed, 	/* Amount of space needed when GC triggered */
  /* Used to signal microcode errors from compiled code. */
  compiled_code_error_code;

extern char * Return_Names [];
extern long MAX_RETURN;

extern char
  * CONT_PRINT_RETURN_MESSAGE,
  * CONT_PRINT_EXPR_MESSAGE,
  * RESTORE_CONT_RETURN_MESSAGE,
  * RESTORE_CONT_EXPR_MESSAGE;

extern int GC_Type_Map [];

extern FILE * (Channels [FILE_CHANNELS]);
extern Boolean Photo_Open;
extern FILE * Photo_File_Handle;

extern jmp_buf * Back_To_Eval;
extern Boolean Trapping;
extern SCHEME_OBJECT Old_Return_Code;
extern SCHEME_OBJECT * Return_Hook_Address;

extern SCHEME_OBJECT * Prev_Restore_History_Stacklet;
extern long Prev_Restore_History_Offset;

extern int Saved_argc;
extern char ** Saved_argv;

extern char * OS_Name;
extern char * OS_Variant;

extern long Heap_Size;
extern long Constant_Size;
extern long Stack_Size;
extern SCHEME_OBJECT * Highest_Allocated_Address;

/* Environment lookup utilities. */
extern long Lex_Ref ();
extern long Local_Set ();
extern long Lex_Set ();
extern long Symbol_Lex_Ref ();
extern long Symbol_Lex_Set ();

/* Arithmetic utilities */
extern long fixnum_to_long ();
extern SCHEME_OBJECT double_to_fixnum ();
extern SCHEME_OBJECT double_to_flonum ();
extern Boolean integer_to_long_p ();
extern long integer_to_long ();
extern SCHEME_OBJECT long_to_integer ();
extern Boolean integer_to_double_p ();
extern double integer_to_double ();
extern SCHEME_OBJECT double_to_integer ();
extern double double_truncate ();
extern Boolean real_number_to_double_p ();
extern double real_number_to_double ();
extern SCHEME_OBJECT bignum_to_fixnum ();
extern SCHEME_OBJECT bignum_to_integer ();
extern SCHEME_OBJECT bignum_to_flonum ();
extern SCHEME_OBJECT flonum_floor ();
extern SCHEME_OBJECT flonum_ceiling ();
extern SCHEME_OBJECT flonum_round ();
extern Boolean integer_zero_p ();
extern Boolean integer_negative_p ();
extern Boolean integer_positive_p ();
extern Boolean integer_equal_p ();
extern Boolean integer_less_p ();
extern SCHEME_OBJECT integer_negate ();
extern SCHEME_OBJECT integer_add ();
extern SCHEME_OBJECT integer_add_1 ();
extern SCHEME_OBJECT integer_subtract ();
extern SCHEME_OBJECT integer_subtract_1 ();
extern SCHEME_OBJECT integer_multiply ();
extern Boolean integer_divide ();
extern SCHEME_OBJECT integer_quotient ();
extern SCHEME_OBJECT integer_remainder ();

/* Character utilities */
extern long char_downcase ();
extern long char_upcase ();

/* Allocation utilities */
extern SCHEME_OBJECT cons ();
extern SCHEME_OBJECT system_pair_cons ();
extern SCHEME_OBJECT hunk3_cons ();
extern SCHEME_OBJECT allocate_non_marked_vector ();
extern SCHEME_OBJECT allocate_marked_vector ();
extern SCHEME_OBJECT make_vector ();
extern SCHEME_OBJECT allocate_string ();
extern SCHEME_OBJECT memory_to_string ();
extern SCHEME_OBJECT char_pointer_to_string ();

/* Random and OS utilities */
extern int Parse_Option ();
extern Boolean Restore_History ();
extern long OS_tty_x_size ();
extern long OS_tty_y_size ();
extern long OS_process_clock ();
extern void OS_tty_flush_output ();
extern void OS_reinitialize ();
extern Boolean interpreter_applicable_p ();

/* Memory management utilities */
extern SCHEME_OBJECT Purify_Pass_2 ();
extern SCHEME_OBJECT Fasload ();
extern Boolean Pure_Test ();

/* Interpreter utilities */

extern term_type Microcode_Termination ();
extern void
  Interpret (),
  Do_Micro_Error (),
  Setup_Interrupt (),
  Back_Out_Of_Primitive (),
  Translate_To_Point (),
  Stop_History (),
  Stack_Death ();

#ifdef USE_STACKLETS
extern void Allocate_New_Stacklet ();
#endif

extern SCHEME_OBJECT * Make_Dummy_History ();
extern SCHEME_OBJECT Find_State_Space ();

/* Debugging utilities */

extern void
  Back_Trace (),
  Handle_Debug_Flags (),
  Show_Env (),
  Show_Pure (),
  Print_Return (),
  Print_Expression (),
  Print_Primitive ();

/* Conditional utilities */

#if false
extern void Clear_Perfinfo_Data ();
#endif
