/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/extern.h,v 9.40 1991/03/01 00:54:24 cph Exp $

Copyright (c) 1987-91 Massachusetts Institute of Technology

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

extern Boolean Eval_Debug;
extern Boolean Hex_Input_Debug;
extern Boolean Cont_Debug;
extern Boolean File_Load_Debug;
extern Boolean Reloc_Debug;
extern Boolean Intern_Debug;
extern Boolean Primitive_Debug;
extern Boolean Define_Debug;
extern Boolean Lookup_Debug;
extern Boolean GC_Debug;
extern Boolean Upgrade_Debug;
extern Boolean Trace_On_Error;
extern Boolean Dump_Debug;
extern Boolean Per_File;
extern Boolean Bignum_Debug;
extern Boolean Fluids_Debug;

extern sp_record_list SP_List;
extern void Pop_Return_Break_Point ();
extern int debug_slotno;
extern int debug_nslots;
extern int local_slotno;
extern int local_nslots;
extern int debug_circle [];
extern int local_circle [];

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
   Fluid_Bindings;	/* Fluid bindings AList */

/* Address of the most recent return code in the stack.  This is
   only meaningful while in compiled code.  *** This must be changed
   when stacklets are used. *** */
extern SCHEME_OBJECT * last_return_code;

/* Return code/address used by the compiled code interface to make
   compiled code return to the interpreter.  */
extern SCHEME_OBJECT return_to_interpreter;

extern Declare_Fixed_Objects ();

extern long
  IntCode,		/* Interrupts requesting */
  IntEnb,		/* Interrupts enabled */
  temp_long,		/* temporary for sign extension */
  GC_Reserve,		/* Scheme pointer overflow space in heap */
  GC_Space_Needed; 	/* Amount of space needed when GC triggered */

extern char * Return_Names [];
extern long MAX_RETURN;

extern char
  * CONT_PRINT_RETURN_MESSAGE,
  * CONT_PRINT_EXPR_MESSAGE,
  * RESTORE_CONT_RETURN_MESSAGE,
  * RESTORE_CONT_EXPR_MESSAGE;

extern int GC_Type_Map [];

extern Boolean Trapping;
extern SCHEME_OBJECT Old_Return_Code;
extern SCHEME_OBJECT * Return_Hook_Address;

extern SCHEME_OBJECT * Prev_Restore_History_Stacklet;
extern long Prev_Restore_History_Offset;

extern CONST char * scheme_program_name;
extern CONST char * OS_Name;
extern CONST char * OS_Variant;
extern struct obstack scratch_obstack;

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
extern SCHEME_OBJECT flonum_normalize ();
extern SCHEME_OBJECT flonum_denormalize ();
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
extern Boolean Restore_History ();
extern Boolean interpreter_applicable_p ();
extern void EXFUN
  (add_reload_cleanup, (void EXFUN ((*cleanup_procedure), (void))));

/* Memory management utilities */
extern SCHEME_OBJECT Purify_Pass_2 ();
extern SCHEME_OBJECT Fasload ();
extern Boolean Pure_Test ();

/* Interpreter utilities */

extern void EXFUN (Microcode_Termination, (int code));
extern void EXFUN (termination_normal, (void));
extern void EXFUN (termination_init_error, (void));
extern void EXFUN (termination_end_of_computation, (void));
extern void EXFUN (termination_trap, (void));
extern void EXFUN (termination_no_error_handler, (void));
extern void EXFUN (termination_gc_out_of_space, (void));
extern void EXFUN (termination_eof, (void));
extern void EXFUN (termination_signal, (CONST char * signal_name));

extern void EXFUN (Setup_Interrupt, (long Masked_Interrupts));
extern void EXFUN (preserve_interrupt_mask, (void));
extern void EXFUN (back_out_of_primitive, (void));

extern void
  Interpret (),
  Do_Micro_Error (),
  Translate_To_Point (),
  Stop_History (),
  Stack_Death ();

#ifdef USE_STACKLETS
extern void Allocate_New_Stacklet ();
#endif

extern SCHEME_OBJECT * Make_Dummy_History ();
extern SCHEME_OBJECT Find_State_Space ();

/* Debugging utilities */

extern void EXFUN (debug_edit_flags, (void));

extern void
  Back_Trace (),
  Show_Env (),
  Show_Pure (),
  Print_Return (),
  Print_Expression (),
  Print_Primitive ();

/* Conditional utilities */

#if false
extern void Clear_Perfinfo_Data ();
#endif
