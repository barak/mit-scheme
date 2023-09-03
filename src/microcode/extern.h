/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

static inline SCHEME_OBJECT
get_register (unsigned int index)
{
  return Registers[index];
}

static inline SCHEME_OBJECT*
get_ptr_register (unsigned int index)
{
  return (SCHEME_OBJECT*) get_register (index);
}

static inline unsigned long
get_ulong_register (unsigned int index)
{
  return (unsigned long) get_register (index);
}

static inline void
set_register (unsigned int index, SCHEME_OBJECT object)
{
  Registers[index] = object;
}

static inline void
set_ptr_register (unsigned int index, SCHEME_OBJECT* p)
{
  set_register (index, (SCHEME_OBJECT) p);
}

static inline void
set_ulong_register (unsigned int index, unsigned long value)
{
  set_register (index, (SCHEME_OBJECT) value);
}

#define GET_MEMTOP		get_ptr_register (REGBLOCK_MEMTOP)
#define GET_INT_MASK		get_ulong_register (REGBLOCK_INT_MASK)
#define GET_VAL			get_register (REGBLOCK_VAL)
#define GET_ENV			get_register (REGBLOCK_ENV)
#define GET_CC_TEMP		get_register (REGBLOCK_CC_TEMP)
#define GET_EXP			get_register (REGBLOCK_EXPR)
#define GET_RET			get_register (REGBLOCK_RETURN)
#define GET_LEXPR_ACTUALS	get_ulong_register (REGBLOCK_LEXPR_ACTUALS)
#define GET_PRIMITIVE		get_register (REGBLOCK_PRIMITIVE)
#define GET_CLOSURE_FREE	get_ptr_register (REGBLOCK_CLOSURE_FREE)
#define GET_CLOSURE_SPACE	get_ptr_register (REGBLOCK_CLOSURE_SPACE)
#define GET_STACK_GUARD		get_ptr_register (REGBLOCK_STACK_GUARD)
#define GET_INT_CODE		get_ulong_register (REGBLOCK_INT_CODE)
#define GET_REFLECTOR		get_register (REGBLOCK_REFLECT_TO_INTERFACE)

#define SET_MEMTOP(v)		set_ptr_register (REGBLOCK_MEMTOP, (v))
#define SET_INT_MASK(v)		set_ulong_register (REGBLOCK_INT_MASK, (v))
#define SET_VAL(v)		set_register (REGBLOCK_VAL, (v))
#define SET_ENV(v)		set_register (REGBLOCK_ENV, (v))
#define SET_CC_TEMP(v)		set_register (REGBLOCK_COMPILER_TEMP, (v))
#define SET_EXP(v)		set_register (REGBLOCK_EXPR, (v))
#define SET_RET(v)		set_register (REGBLOCK_RETURN, (v))
#define SET_LEXPR_ACTUALS(v)	set_ulong_register (REGBLOCK_LEXPR_ACTUALS, (v))
#define SET_PRIMITIVE(v)	set_register (REGBLOCK_PRIMITIVE, (v))
#define SET_CLOSURE_FREE(v)	set_ptr_register (REGBLOCK_CLOSURE_FREE, (v))
#define SET_CLOSURE_SPACE(v)	set_ptr_register (REGBLOCK_CLOSURE_SPACE, (v))
#define SET_STACK_GUARD(v)	set_ptr_register (REGBLOCK_STACK_GUARD, (v))
#define SET_INT_CODE(v)		set_ulong_register (REGBLOCK_INT_CODE, (v))
#define SET_REFLECTOR(v)	set_register (REGBLOCK_REFLECT_TO_INTERFACE, (v))

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
   extern bool Dump_Debug;
   extern bool Trace_On_Error;
   extern bool Per_File;
   extern bool Bignum_Debug;
   extern bool Print_Errors;

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
#  define Print_Errors 0
#endif

extern SCHEME_OBJECT * Free;
extern SCHEME_OBJECT * Free_primitive;
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

/* Address of the most recent return code in the stack.  This is
   only meaningful while in compiled code.  */
extern SCHEME_OBJECT * last_return_code;
extern SCHEME_OBJECT fixed_objects;

extern SCHEME_OBJECT ephemeron_array;
extern unsigned long ephemeron_count;

extern const char * type_names [];
extern const char * Abort_Names [];
extern const char * Error_Names [];
extern const char * Term_Names [];
extern const char * term_messages [];
extern const char * term_halt_messages [];
extern const char * fixed_objects_names [];

extern bool trapping;

extern const char * scheme_program_name;
extern const char * OS_Name;
extern const char * OS_Variant;
extern struct obstack scratch_obstack;
extern struct obstack ffi_obstack;

extern unsigned long n_heap_blocks;
extern unsigned long n_constant_blocks;
extern unsigned long n_stack_blocks;

extern SCHEME_OBJECT * memory_block_start;
extern SCHEME_OBJECT * memory_block_end;

extern unsigned long heap_reserved;

/* Amount of space needed when GC requested */
extern unsigned long gc_space_needed;

/* Number of new ephemerons requested from the GC.  */
extern unsigned long n_ephemerons_requested;
extern bool ephemeron_request_hard_p;

/* Arithmetic utilities */
extern SCHEME_OBJECT Mul (SCHEME_OBJECT, SCHEME_OBJECT);
extern long fixnum_to_long (SCHEME_OBJECT);
extern SCHEME_OBJECT double_to_fixnum (double);
extern bool integer_to_long_p (SCHEME_OBJECT);
extern long integer_to_long (SCHEME_OBJECT);
extern bool integer_to_intmax_p (SCHEME_OBJECT);
extern intmax_t integer_to_intmax (SCHEME_OBJECT);
extern SCHEME_OBJECT long_to_integer (long);
extern SCHEME_OBJECT intmax_to_integer (intmax_t);
extern bool integer_to_ulong_p (SCHEME_OBJECT);
extern unsigned long integer_to_ulong (SCHEME_OBJECT);
extern bool integer_to_uintmax_p (SCHEME_OBJECT);
extern uintmax_t integer_to_uintmax (SCHEME_OBJECT);
extern SCHEME_OBJECT ulong_to_integer (unsigned long);
extern SCHEME_OBJECT uintmax_to_integer (uintmax_t);
extern bool integer_to_double_p (SCHEME_OBJECT);
extern double integer_to_double (SCHEME_OBJECT);
extern SCHEME_OBJECT double_to_integer (double);
extern double double_truncate (double);
extern double double_round (double);
extern SCHEME_OBJECT bignum_to_fixnum (SCHEME_OBJECT);
extern SCHEME_OBJECT bignum_to_integer (SCHEME_OBJECT);
extern SCHEME_OBJECT bignum_to_flonum (SCHEME_OBJECT);
extern bool finite_flonum_p (SCHEME_OBJECT);
extern bool flonum_is_finite_p (SCHEME_OBJECT);
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
extern SCHEME_OBJECT integer_length (SCHEME_OBJECT);
extern SCHEME_OBJECT integer_first_set_bit (SCHEME_OBJECT);
extern SCHEME_OBJECT integer_bit_count (SCHEME_OBJECT);
extern SCHEME_OBJECT integer_hamming_distance (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_bitwise_not (SCHEME_OBJECT);
extern SCHEME_OBJECT integer_bitwise_and (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_bitwise_andc2 (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_bitwise_andc1 (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_bitwise_xor (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_bitwise_ior (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_bitwise_nor (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_bitwise_eqv (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_bitwise_orc2 (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_bitwise_orc1 (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_bitwise_nand (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT integer_nonnegative_one_bits (unsigned long, unsigned long);
extern SCHEME_OBJECT integer_negative_zero_bits (unsigned long, unsigned long);
extern SCHEME_OBJECT integer_shift_left (SCHEME_OBJECT, unsigned long);
extern SCHEME_OBJECT integer_shift_right (SCHEME_OBJECT, unsigned long);

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
extern SCHEME_OBJECT allocate_non_marked_vector
  (unsigned int, unsigned long, bool);
extern SCHEME_OBJECT allocate_marked_vector
  (unsigned int, unsigned long, bool);
extern SCHEME_OBJECT allocate_vector (unsigned long, bool);
extern SCHEME_OBJECT make_vector (unsigned long, SCHEME_OBJECT, bool);
extern SCHEME_OBJECT record_applicator (SCHEME_OBJECT);
extern SCHEME_OBJECT allocate_string (unsigned long);
extern SCHEME_OBJECT allocate_string_no_gc (unsigned long);
extern SCHEME_OBJECT memory_to_string (unsigned long, const void *);
extern SCHEME_OBJECT memory_to_string_no_gc (unsigned long, const void *);
extern SCHEME_OBJECT char_pointer_to_string (const char *);
extern SCHEME_OBJECT char_pointer_to_string_no_gc (const char *);
extern unsigned char * string_to_char_pointer (SCHEME_OBJECT, unsigned long *);
extern uint8_t * arg_bytevector (int, unsigned long *);
extern SCHEME_OBJECT allocate_bytevector (unsigned long);
extern SCHEME_OBJECT memory_to_bytevector (unsigned long, const void *);
extern SCHEME_OBJECT allocate_bit_string (unsigned long);
extern const char * arg_symbol (int);
extern const char * arg_interned_symbol (int);
extern SCHEME_OBJECT intern_symbol (SCHEME_OBJECT);
extern SCHEME_OBJECT string_to_symbol (SCHEME_OBJECT);
extern SCHEME_OBJECT char_pointer_to_symbol (const char *);
extern SCHEME_OBJECT memory_to_symbol (unsigned long, const void *);
extern SCHEME_OBJECT find_symbol (unsigned long, const char *);
extern void strengthen_symbol (SCHEME_OBJECT);
extern void weaken_symbol (SCHEME_OBJECT);
extern unsigned long compute_extra_ephemeron_space (unsigned long);
extern void guarantee_extra_ephemeron_space (unsigned long);

/* Hashing */

extern uint32_t memory_hash (unsigned long, const void *);
extern bool hashable_object_p (SCHEME_OBJECT);
extern uint32_t hash_object (SCHEME_OBJECT);
extern uint32_t initial_hash (void);
extern uint32_t combine_hashes (uint32_t, uint32_t);

#if (FIXNUM_LENGTH >= 32)
#  define HASH_TO_FIXNUM(hash) (ULONG_TO_FIXNUM (hash))
#else
  /* Shorten the result using xor-folding.  */
#  define HASH_TO_FIXNUM(hash)						\
  (ULONG_TO_FIXNUM (((hash) >> FIXNUM_LENGTH) ^ ((hash) & FIXNUM_MASK)))
#endif

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
extern SCHEME_OBJECT find_primitive_cname (const char *, bool, bool, int);
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
extern SCHEME_OBJECT * control_point_start (SCHEME_OBJECT);
extern SCHEME_OBJECT * control_point_end (SCHEME_OBJECT);
extern void unpack_control_point (SCHEME_OBJECT);

/* Debugging utilities */

// Intended for use in debugger:
extern void debug_print (SCHEME_OBJECT);
extern void debug_print_stack (void);

// Intended for use in the code base:
extern bool debug_verify_heap (void);
extern void debug_print_env (outf_channel, SCHEME_OBJECT);
extern void debug_print_expr (SCHEME_OBJECT, const char*);
extern void debug_print_primitive (SCHEME_OBJECT);
extern void debug_stack_trace (outf_channel);

#endif /* not SCM_EXTERN_H */
