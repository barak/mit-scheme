/* -*-C-*-

$Id: extern.h,v 9.71 2008/01/30 20:02:12 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* External Declarations */

#ifndef SCM_EXTERN_H
#define SCM_EXTERN_H 1

#include "outf.h"

/* The register block */

#ifdef __WIN32__
   extern SCHEME_OBJECT * RegistersPtr;
#  define Registers RegistersPtr
#else
   extern SCHEME_OBJECT Registers [];
#endif

#define GET_REG_O(i) (Registers[REGBLOCK_##i])
#define GET_REG_P(i) ((SCHEME_OBJECT *) (Registers[REGBLOCK_##i]))
#define GET_REG_N(i) ((unsigned long) (Registers[REGBLOCK_##i]))

#define SET_REG_O(i, v) ((Registers[REGBLOCK_##i]) = (v))
#define SET_REG_P(i, v) (set_ptr_register ((REGBLOCK_##i), (v)))
#define SET_REG_N(i, v) (set_ulong_register ((REGBLOCK_##i), (v)))

extern void set_ptr_register (unsigned int, SCHEME_OBJECT *);
extern void set_ulong_register (unsigned int, unsigned long);

#define GET_MEMTOP		GET_REG_P (MEMTOP)
#define GET_INT_MASK		GET_REG_N (INT_MASK)
#define GET_VAL			GET_REG_O (VAL)
#define GET_ENV			GET_REG_O (ENV)
#define GET_CC_TEMP		GET_REG_O (CC_TEMP)
#define GET_EXP			GET_REG_O (EXPR)
#define GET_RET			GET_REG_O (RETURN)
#define GET_LEXPR_ACTUALS	GET_REG_N (LEXPR_ACTUALS)
#define GET_PRIMITIVE		GET_REG_O (PRIMITIVE)
#define GET_CLOSURE_FREE	GET_REG_P (CLOSURE_FREE)
#define GET_CLOSURE_SPACE	GET_REG_P (CLOSURE_SPACE)
#define GET_STACK_GUARD		GET_REG_P (STACK_GUARD)
#define GET_INT_CODE		GET_REG_N (INT_CODE)
#define GET_REFLECTOR		GET_REG_O (REFLECT_TO_INTERFACE)

#define SET_MEMTOP(v)		SET_REG_P (MEMTOP, v)
#define SET_INT_MASK(v)		SET_REG_N (INT_MASK, v)
#define SET_VAL(v)		SET_REG_O (VAL, v)
#define SET_ENV(v)		SET_REG_O (ENV, v)
#define SET_CC_TEMP(v)		SET_REG_O (COMPILER_TEMP, v)
#define SET_EXP(v)		SET_REG_O (EXPR, v)
#define SET_RET(v)		SET_REG_O (RETURN, v)
#define SET_LEXPR_ACTUALS(v)	SET_REG_N (LEXPR_ACTUALS, v)
#define SET_PRIMITIVE(v)	SET_REG_O (PRIMITIVE, v)
#define SET_CLOSURE_FREE(v)	SET_REG_P (CLOSURE_FREE, v)
#define SET_CLOSURE_SPACE(v)	SET_REG_P (CLOSURE_SPACE, v)
#define SET_STACK_GUARD(v)	SET_REG_P (STACK_GUARD, v)
#define SET_INT_CODE(v)		SET_REG_N (INT_CODE, v)
#define SET_REFLECTOR(v)	SET_REG_O (REFLECT_TO_INTERFACE, v)

#define PUSH_ENV() STACK_PUSH (GET_ENV)
#define PUSH_VAL() STACK_PUSH (GET_VAL)
#define PUSH_EXP() STACK_PUSH (GET_EXP)
#define PUSH_RET() STACK_PUSH (GET_RET)

#define POP_ENV() SET_ENV (STACK_POP ())
#define POP_VAL() SET_VAL (STACK_POP ())
#define POP_EXP() SET_EXP (STACK_POP ())
#define POP_RET() SET_RET (STACK_POP ())

#define GET_RC (OBJECT_DATUM (GET_RET))
#define SET_RC(code) SET_RET (MAKE_OBJECT (TC_RETURN_CODE, (code)))
#define PUSH_RC(code) STACK_PUSH (MAKE_OBJECT (TC_RETURN_CODE, (code)))

#ifdef ENABLE_DEBUGGING_TOOLS
   extern bool Eval_Debug;
   extern bool Hex_Input_Debug;
   extern bool Cont_Debug;
   extern bool File_Load_Debug;
   extern bool Reloc_Debug;
   extern bool Intern_Debug;
   extern bool Primitive_Debug;
   extern bool Define_Debug;
   extern bool Lookup_Debug;
   extern bool GC_Debug;
   extern bool Upgrade_Debug;
   extern bool Trace_On_Error;
   extern bool Dump_Debug;
   extern bool Per_File;
   extern bool Bignum_Debug;

   extern void Pop_Return_Break_Point (void);
   extern unsigned int debug_slotno;
   extern unsigned int debug_nslots;
   extern unsigned int local_slotno;
   extern unsigned int local_nslots;
   extern unsigned int debug_circle [];
   extern unsigned int local_circle [];
#else
#  define Eval_Debug 0
#  define Hex_Input_Debug 0
#  define File_Load_Debug 0
#  define Reloc_Debug 0
#  define Intern_Debug 0
#  define Cont_Debug 0
#  define Primitive_Debug 0
#  define Lookup_Debug 0
#  define Define_Debug 0
#  define GC_Debug 0
#  define Upgrade_Debug 0
#  define Trace_On_Error 0
#  define Dump_Debug 0
#  define Per_File 0
#  define Bignum_Debug 0
#endif

extern SCHEME_OBJECT * Free;
extern SCHEME_OBJECT * heap_alloc_limit;
extern SCHEME_OBJECT * heap_start;
extern SCHEME_OBJECT * heap_end;

extern SCHEME_OBJECT * stack_pointer;
extern SCHEME_OBJECT * stack_guard;
extern SCHEME_OBJECT * stack_start;
extern SCHEME_OBJECT * stack_end;

extern SCHEME_OBJECT * constant_alloc_next;
extern SCHEME_OBJECT * constant_start;
extern SCHEME_OBJECT * constant_end;

extern SCHEME_OBJECT current_state_point;

/* Address of the most recent return code in the stack.  This is
   only meaningful while in compiled code.  */
extern SCHEME_OBJECT * last_return_code;
extern SCHEME_OBJECT fixed_objects;

extern char * CONT_PRINT_RETURN_MESSAGE;
extern char * CONT_PRINT_EXPR_MESSAGE;
extern char * RESTORE_CONT_RETURN_MESSAGE;
extern char * RESTORE_CONT_EXPR_MESSAGE;

extern unsigned long MAX_RETURN;

extern const char * Return_Names [];
extern const char * type_names [];
extern const char * Abort_Names [];
extern const char * Error_Names [];
extern const char * Term_Names [];
extern const char * term_messages [];

extern bool trapping;

extern const char * scheme_program_name;
extern const char * OS_Name;
extern const char * OS_Variant;
extern struct obstack scratch_obstack;

extern unsigned long n_heap_blocks;
extern unsigned long n_constant_blocks;
extern unsigned long n_stack_blocks;

extern SCHEME_OBJECT * memory_block_start;
extern SCHEME_OBJECT * memory_block_end;

extern unsigned long heap_reserved;

/* Amount of space needed when GC requested */
extern unsigned long gc_space_needed;

/* Arithmetic utilities */
extern SCHEME_OBJECT Mul (SCHEME_OBJECT, SCHEME_OBJECT);
extern long fixnum_to_long (SCHEME_OBJECT);
extern SCHEME_OBJECT double_to_fixnum (double);
extern bool integer_to_long_p (SCHEME_OBJECT);
extern long integer_to_long (SCHEME_OBJECT);
extern SCHEME_OBJECT long_to_integer (long);
extern bool integer_to_ulong_p (SCHEME_OBJECT);
extern unsigned long integer_to_ulong (SCHEME_OBJECT);
extern SCHEME_OBJECT ulong_to_integer (unsigned long);
extern bool integer_to_double_p (SCHEME_OBJECT);
extern double integer_to_double (SCHEME_OBJECT);
extern SCHEME_OBJECT double_to_integer (double);
extern double double_truncate (double);
extern double double_round (double);
extern SCHEME_OBJECT bignum_to_fixnum (SCHEME_OBJECT);
extern SCHEME_OBJECT bignum_to_integer (SCHEME_OBJECT);
extern SCHEME_OBJECT bignum_to_flonum (SCHEME_OBJECT);
extern bool flonum_integer_p (SCHEME_OBJECT);
extern SCHEME_OBJECT flonum_floor (SCHEME_OBJECT);
extern SCHEME_OBJECT flonum_ceiling (SCHEME_OBJECT);
extern SCHEME_OBJECT flonum_round (SCHEME_OBJECT);
extern SCHEME_OBJECT flonum_normalize (SCHEME_OBJECT);
extern SCHEME_OBJECT flonum_denormalize (SCHEME_OBJECT, SCHEME_OBJECT);
extern bool integer_zero_p (SCHEME_OBJECT);
extern bool integer_negative_p (SCHEME_OBJECT);
extern bool integer_positive_p (SCHEME_OBJECT);
extern bool integer_equal_p (SCHEME_OBJECT, SCHEME_OBJECT);
extern bool integer_less_p (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_negate (SCHEME_OBJECT);
extern SCHEME_OBJECT integer_add (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_add_1 (SCHEME_OBJECT);
extern SCHEME_OBJECT integer_subtract (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_subtract_1 (SCHEME_OBJECT);
extern SCHEME_OBJECT integer_multiply (SCHEME_OBJECT, SCHEME_OBJECT);
extern bool integer_divide
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *, SCHEME_OBJECT *);
extern SCHEME_OBJECT integer_quotient (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_remainder (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_length_in_bits (SCHEME_OBJECT);
extern SCHEME_OBJECT integer_shift_left (SCHEME_OBJECT, unsigned long);

extern SCHEME_OBJECT double_to_flonum (double);
extern bool real_number_to_double_p (SCHEME_OBJECT);
extern double real_number_to_double (SCHEME_OBJECT);

/* Character utilities */
extern long char_downcase (long);
extern long char_upcase (long);

/* Allocation utilities */
extern SCHEME_OBJECT cons (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT system_pair_cons (long, SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT hunk3_cons (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT allocate_vector
  (unsigned int, unsigned int, unsigned long, SCHEME_OBJECT **);
extern SCHEME_OBJECT allocate_non_marked_vector
  (unsigned int, unsigned long, bool);
extern SCHEME_OBJECT allocate_marked_vector
  (unsigned int, unsigned long, bool);
extern SCHEME_OBJECT make_vector (unsigned long, SCHEME_OBJECT, bool);
extern SCHEME_OBJECT allocate_string (unsigned long);
extern SCHEME_OBJECT allocate_string_no_gc (unsigned long);
extern SCHEME_OBJECT memory_to_string (unsigned long, const void *);
extern SCHEME_OBJECT memory_to_string_no_gc (unsigned long, const void *);
extern SCHEME_OBJECT char_pointer_to_string (const char *);
extern SCHEME_OBJECT char_pointer_to_string_no_gc (const char *);
extern SCHEME_OBJECT allocate_bit_string (unsigned long);
extern const char * arg_symbol (int);
extern const char * arg_interned_symbol (int);
extern SCHEME_OBJECT intern_symbol (SCHEME_OBJECT);
extern SCHEME_OBJECT string_to_symbol (SCHEME_OBJECT);
extern SCHEME_OBJECT char_pointer_to_symbol (const char *);
extern SCHEME_OBJECT memory_to_symbol (unsigned long, const void *);
extern SCHEME_OBJECT find_symbol (unsigned long, const char *);

/* Random and OS utilities */
extern int strcmp_ci (const char *, const char *);
extern bool interpreter_applicable_p (SCHEME_OBJECT);
extern void add_reload_cleanup (void (*) (void));
extern void execute_reload_cleanups (void);
extern void clear_bit_string (SCHEME_OBJECT);
extern void bit_string_set (SCHEME_OBJECT, long, int);
extern unsigned char * lookup_external_string (SCHEME_OBJECT, unsigned long *);

/* Memory management utilities */
extern bool object_in_constant_space_p (SCHEME_OBJECT);
extern SCHEME_OBJECT * copy_to_constant_space (SCHEME_OBJECT *, unsigned long);

extern void setup_memory (unsigned long, unsigned long, unsigned long);
extern void reset_memory (void);

/* Utilities for primitives */

typedef struct
{
  unsigned long * internal;
  unsigned long * external;
  unsigned long next_code;
} prim_renumber_t;

extern prim_renumber_t * make_prim_renumber (void);
extern SCHEME_OBJECT renumber_primitive (SCHEME_OBJECT, prim_renumber_t *);
extern unsigned long renumbered_primitives_export_length (prim_renumber_t *);
extern void export_renumbered_primitives (SCHEME_OBJECT *, prim_renumber_t *);
extern unsigned long primitive_table_export_length (void);
extern void export_primitive_table (SCHEME_OBJECT *);

extern void import_primitive_table
  (SCHEME_OBJECT *, unsigned long, SCHEME_OBJECT *);

extern void initialize_primitives (void);
extern SCHEME_OBJECT make_primitive (const char *, int);
extern SCHEME_OBJECT find_primitive (SCHEME_OBJECT, bool, bool, int);

/* Interpreter utilities */

extern void Microcode_Termination (int code) NORETURN;
extern void termination_normal (const int) NORETURN;
extern void termination_init_error (void) NORETURN;
extern void termination_end_of_computation (void) NORETURN;
extern void termination_trap (void) NORETURN;
extern void termination_no_error_handler (void) NORETURN;
extern void termination_gc_out_of_space (void) NORETURN;
extern void termination_eof (void) NORETURN;
extern void termination_signal (const char * signal_name) NORETURN;

extern void setup_interrupt (unsigned long);
extern void preserve_interrupt_mask (void);
extern void canonicalize_primitive_context (void);
extern void back_out_of_primitive (void);

extern void Interpret (void);
extern void Do_Micro_Error (long, bool);
extern void Translate_To_Point (SCHEME_OBJECT);
extern void Stack_Death (void) NORETURN;
extern SCHEME_OBJECT * control_point_start (SCHEME_OBJECT);
extern SCHEME_OBJECT * control_point_end (SCHEME_OBJECT);
extern void unpack_control_point (SCHEME_OBJECT);

extern SCHEME_OBJECT Find_State_Space (SCHEME_OBJECT);

/* Debugging utilities */

extern void Back_Trace (outf_channel);
extern void Debug_Stack_Trace (void);
extern void Debug_Print (SCHEME_OBJECT, bool);
extern void Show_Env (SCHEME_OBJECT);
extern void Print_Return (char *);
extern void Print_Expression (SCHEME_OBJECT, char *);
extern void Print_Primitive (SCHEME_OBJECT);

#endif /* not SCM_EXTERN_H */
