/* -*-C-*-

$Id: extern.h,v 9.63 2002/11/20 19:46:08 cph Exp $

Copyright (c) 1987-2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* External Declarations */

#ifndef SCM_EXTERN_H
#define SCM_EXTERN_H

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
extern void EXFUN (Pop_Return_Break_Point, (void));
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

#ifdef __WIN32__
extern SCHEME_OBJECT *RegistersPtr;
#define Registers RegistersPtr
#else
extern SCHEME_OBJECT Registers [];
#endif

extern SCHEME_OBJECT
  * MemTop,		/* Top of free space available */
  * Free,		/* Next free word in heap */
  * Heap_Top,		/* Top of current heap */
  * Heap_Bottom,	/* Bottom of current heap */
  * Unused_Heap_Top,	/* Top of unused heap */
  * Unused_Heap_Bottom,	/* Bottom of unused heap */
  * Stack_Guard,	/* Guard area at end of stack */
  * sp_register,	/* Next available slot in control stack */
  * Stack_Bottom,	/* Bottom of control stack */
  * Stack_Top,		/* Top of control stack */
  * Free_Constant,	/* Next free word in constant space */
  * Constant_Space,	/* Bottom of constant+pure space */
  * Constant_Top,	/* Top of constant+pure space */
  * Local_Heap_Base,	/* Per-processor CONSing area */
  * Free_Stacklets,	/* Free list of stacklets */
  * history_register,	/* History register */
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
extern SCHEME_OBJECT * Lowest_Allocated_Address, * Highest_Allocated_Address;

/* Arithmetic utilities */
extern long EXFUN (fixnum_to_long, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (double_to_fixnum, (double));
extern SCHEME_OBJECT EXFUN (double_to_flonum, (double));
extern Boolean EXFUN (integer_to_long_p, (SCHEME_OBJECT));
extern long EXFUN (integer_to_long, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (long_to_integer, (long));
extern Boolean EXFUN (integer_to_ulong_p, (SCHEME_OBJECT));
extern unsigned long EXFUN (integer_to_ulong, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (ulong_to_integer, (unsigned long));
extern Boolean EXFUN (integer_to_double_p, (SCHEME_OBJECT));
extern double EXFUN (integer_to_double, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (double_to_integer, (double));
extern double EXFUN (double_truncate, (double));
extern Boolean EXFUN (real_number_to_double_p, (SCHEME_OBJECT));
extern double EXFUN (real_number_to_double, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (bignum_to_fixnum, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (bignum_to_integer, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (bignum_to_flonum, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (flonum_floor, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (flonum_ceiling, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (flonum_round, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (flonum_normalize, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (flonum_denormalize,
			    (SCHEME_OBJECT, SCHEME_OBJECT));
extern Boolean EXFUN (integer_zero_p, (SCHEME_OBJECT));
extern Boolean EXFUN (integer_negative_p, (SCHEME_OBJECT));
extern Boolean EXFUN (integer_positive_p, (SCHEME_OBJECT));
extern Boolean EXFUN (integer_equal_p, (SCHEME_OBJECT, SCHEME_OBJECT));
extern Boolean EXFUN (integer_less_p, (SCHEME_OBJECT, SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (integer_negate, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (integer_add, (SCHEME_OBJECT, SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (integer_add_1, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (integer_subtract, (SCHEME_OBJECT, SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (integer_subtract_1, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (integer_multiply, (SCHEME_OBJECT, SCHEME_OBJECT));
extern Boolean EXFUN (integer_divide,
		      (SCHEME_OBJECT, SCHEME_OBJECT,
		       SCHEME_OBJECT *, SCHEME_OBJECT *));
extern SCHEME_OBJECT EXFUN (integer_quotient, (SCHEME_OBJECT, SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (integer_remainder, (SCHEME_OBJECT, SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (integer_length_in_bits, (SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN
  (integer_shift_left, (SCHEME_OBJECT, SCHEME_OBJECT));

/* Character utilities */
extern long EXFUN (char_downcase, (long));
extern long EXFUN (char_upcase, (long));

/* Allocation utilities */
extern SCHEME_OBJECT EXFUN (cons, (SCHEME_OBJECT, SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (system_pair_cons,
			    (long, SCHEME_OBJECT, SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (hunk3_cons,
			    (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (allocate_non_marked_vector, (int, long, Boolean));
extern SCHEME_OBJECT EXFUN (allocate_marked_vector, (int, long, Boolean));
extern SCHEME_OBJECT EXFUN (make_vector, (long, SCHEME_OBJECT, Boolean));
extern SCHEME_OBJECT EXFUN (allocate_string, (unsigned long));
extern SCHEME_OBJECT EXFUN (allocate_string_no_gc, (unsigned long));
extern SCHEME_OBJECT EXFUN
  (memory_to_string, (unsigned long, CONST unsigned char *));
extern SCHEME_OBJECT EXFUN
  (memory_to_string_no_gc, (unsigned long, CONST unsigned char *));
extern SCHEME_OBJECT EXFUN (char_pointer_to_string, (CONST unsigned char *));
extern SCHEME_OBJECT EXFUN
  (char_pointer_to_string_no_gc, (CONST unsigned char *));

/* Random and OS utilities */
extern Boolean EXFUN (Restore_History, (SCHEME_OBJECT));
extern Boolean EXFUN (interpreter_applicable_p, (SCHEME_OBJECT));
extern void EXFUN
  (add_reload_cleanup, (void EXFUN ((*cleanup_procedure), (void))));

/* Memory management utilities */

extern Boolean EXFUN (Pure_Test, (SCHEME_OBJECT *));

/* Interpreter utilities */

extern void EXFUN (Microcode_Termination, (int code));
extern void EXFUN (termination_normal, (CONST int));
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

extern void EXFUN (Interpret, (int));
extern void EXFUN (Do_Micro_Error, (long, Boolean));
extern void EXFUN (Translate_To_Point, (SCHEME_OBJECT));
extern void EXFUN (Stop_History, (void));
extern void EXFUN (Stack_Death, (void));

extern SCHEME_OBJECT * EXFUN (Make_Dummy_History, (void));
extern SCHEME_OBJECT EXFUN (Find_State_Space, (SCHEME_OBJECT));

/* Debugging utilities */

extern void EXFUN (debug_edit_flags, (void));

extern void EXFUN (Back_Trace, (outf_channel));
extern void EXFUN (Debug_Stack_Trace, (void));
extern void EXFUN (Debug_Print, (SCHEME_OBJECT, Boolean));
extern void EXFUN (Show_Env, (SCHEME_OBJECT));
extern void EXFUN (Show_Pure, (void));
extern void EXFUN (Print_Return, (char *));
extern void EXFUN (Print_Expression, (SCHEME_OBJECT, char *));
extern void EXFUN (Print_Primitive, (SCHEME_OBJECT));

/* Conditional utilities */

#ifdef USE_STACKLETS
extern void EXFUN (Allocate_New_Stacklet, (long));
#endif

#if FALSE
extern void EXFUN (Clear_Perfinfo_Data, (void));
#endif

#endif /* not SCM_EXTERN_H */
