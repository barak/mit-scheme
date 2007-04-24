/* -*-C-*-

$Id: cmpint.h,v 10.16 2007/04/24 05:31:14 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

/* Compiled-code interface */

#ifndef SCM_CMPINT_H
#define SCM_CMPINT_H 1

#include "config.h"
#include "object.h"

#define COMPILER_INTERFACE_VERSION 3

typedef struct cc_entry_type_s cc_entry_type_t;
typedef struct cc_entry_offset_s cc_entry_offset_t;

#include "cmpintmd.h"

#ifdef NO_CC_SUPPORT_P
#  undef CC_SUPPORT_P
#else
#  define CC_SUPPORT_P 1

/* ASM_ENTRY_POINT is for OS/2, but it could also be used for any
   compiler that supports multiple calling conventions, such as GCC.

   The IBM C Set++/2 compiler has several different external calling
   conventions.  The default calling convention is called _Optlink,
   uses a combination of registers and the stack, and is complicated.
   The calling convention used for operating system interface
   procedures is called _System, uses only the stack, and is very
   similar to the calling conventions used with our DOS compilers.
   So, in order to simplify the changes to the assembly language, we
   use _System conventions for calling C procedures from the assembly
   language file.

   Since _Optlink is the default, we must somehow cause the relevant
   procedures to be compiled using _System.  The easiest way to do
   this is to force the use of _System everywhere, but that's
   undesirable since _Optlink is generally more efficient.  Instead,
   we use the ASM_ENTRY_POINT wrapper to cause each of the relevant
   procedures to be tagged with the compiler's _System keyword.  The
   relevant procedures are all of the SCHEME_UTILITY procedures,
   C_to_interface, interface_to_C, and interface_to_scheme.  */

#ifndef ASM_ENTRY_POINT
#  define ASM_ENTRY_POINT(name) name
#endif

/* The following code handles compiled entry points, where the
   addresses point to the "middle" of the code vector.  From the entry
   address, the offset word can be extracted, and this offset allows
   us to find the beginning of the block, so it can be copied as a
   whole.  The broken heart for the whole block lives in its usual
   place (first word in the vector).

   The offset word contains an encoding of the offset and an encoding
   of whether the resulting pointer points to the beginning of the
   block or is another entry, so the process may have to be repeated.  */

typedef enum
{
  CET_PROCEDURE,
  CET_CONTINUATION,
  CET_EXPRESSION,
  CET_INTERNAL_PROCEDURE,
  CET_INTERNAL_CONTINUATION,
  CET_TRAMPOLINE,
  CET_RETURN_TO_INTERPRETER,
  CET_CLOSURE
} cc_entry_type_marker_t;

struct cc_entry_type_s
{
  cc_entry_type_marker_t marker;
  union
    {
      struct
	{
	  unsigned int n_required;
	  unsigned int n_optional;
	  bool rest_p;
	} for_procedure;
      struct
	{
	  /* This number is in insn_t units.  A value of zero means
	     that the offset is unknown.  */
	  unsigned long offset;
	} for_continuation;
    } args;
};

extern void make_compiled_procedure_type
  (cc_entry_type_t *, unsigned int, unsigned int, bool);
extern void make_compiled_continuation_type (cc_entry_type_t *, unsigned long);
extern void make_cc_entry_type (cc_entry_type_t *, cc_entry_type_marker_t);

extern bool read_cc_entry_type (cc_entry_type_t *, insn_t *);
extern bool write_cc_entry_type (cc_entry_type_t *, insn_t *);

extern bool decode_old_style_format_word (cc_entry_type_t *, unsigned short);
extern bool encode_old_style_format_word (cc_entry_type_t *, unsigned short *);

/* If continued_p is false, then offset is the distance in insn_t
   units between the entry and the CC block.  Otherwise, offset is the
   distance in insn_t units between this entry and a preceding one.
   */
struct cc_entry_offset_s
{
  unsigned long offset;
  bool continued_p;
};

extern bool read_cc_entry_offset (cc_entry_offset_t *, insn_t *);
extern bool write_cc_entry_offset (cc_entry_offset_t *, insn_t *);

#define CC_ENTRY_ADDRESS(obj) ((insn_t *) (OBJECT_ADDRESS (obj)))
#define MAKE_CC_ENTRY(addr)						\
  (MAKE_POINTER_OBJECT (TC_COMPILED_ENTRY, ((SCHEME_OBJECT *) (addr))))

#define CC_ENTRY_NEW_ADDRESS(entry, address)				\
  (OBJECT_NEW_ADDRESS ((entry), ((insn_t *) (address))))

#define CC_ENTRY_NEW_BLOCK(entry, new_block, old_block)			\
  (CC_ENTRY_NEW_ADDRESS ((entry),					\
			 (((insn_t *) (new_block))			\
			  + ((CC_ENTRY_ADDRESS (entry))			\
			     - ((insn_t *) (old_block))))))

#define MAKE_CC_BLOCK(address)						\
  (MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, (address)))

#define MAKE_CC_STACK_ENV(address)					\
  (MAKE_POINTER_OBJECT (TC_STACK_ENVIRONMENT, (address)))

#define CC_BLOCK_LENGTH(block) (CC_BLOCK_ADDR_LENGTH (OBJECT_ADDRESS (block)))
#define CC_BLOCK_END(block) (CC_BLOCK_ADDR_END (OBJECT_ADDRESS (block)))
#define CC_BLOCK_ADDR_LENGTH(addr) ((OBJECT_DATUM (*addr)) + 1)
#define CC_BLOCK_ADDR_END(addr) ((addr) + (CC_BLOCK_ADDR_LENGTH (addr)))

#define CC_ENTRY_P(object) ((OBJECT_TYPE (object)) == TC_COMPILED_ENTRY)
#define CC_BLOCK_P(object) ((OBJECT_TYPE (object)) == TC_COMPILED_CODE_BLOCK)
#define CC_STACK_ENV_P(object) ((OBJECT_TYPE (object)) == TC_STACK_ENVIRONMENT)

extern unsigned long cc_entry_to_block_offset (SCHEME_OBJECT);
extern SCHEME_OBJECT cc_entry_to_block (SCHEME_OBJECT);
extern SCHEME_OBJECT * cc_entry_to_block_address (SCHEME_OBJECT);
extern SCHEME_OBJECT * cc_entry_address_to_block_address (insn_t *);
extern int plausible_cc_block_p (SCHEME_OBJECT *);

/* Linkage sections

   Linkage sections implement free-variable references in compiled
   code.  They are built to be very fast, and are customized to
   particular uses of the free variables.

   If a compiled-code block has linkage sections, they appear at the
   very beginning of the block's constants area (i.e. immediately
   following the non-marked code segment).  There are two basic kinds
   of sections: (1) a reference section is used to read or write a
   variable's value; and (2) an execution section is used to call a
   variable's value.

   Each linkage section has a header word, with type
   TC_LINKAGE_SECTION.  The bottom 16 bits of the header word's datum
   contains the number of words in the rest of the linkage section
   (i.e. its length, as if it were a vector).  The bits above that
   contain the linkage section's type, which must be one of the values
   listed below.

   Prior to linking, the header word has type TC_FIXNUM.  Each entry
   in a reference section is a symbol, which is the name of the free
   variable being referred to.  Each entry in an execution section has
   two words: (1) a non-negative fixnum, which is the number of
   arguments to be passed to the procedure; and (2) a symbol, which is
   the name of the variable.

   After linking, the header word has type TC_LINKAGE_SECTION.  Each
   entry in a reference section is the address (SCHEME_OBJECT*) of a
   TC_HUNK3 object.  Each entry in an execution section is an
   architecture-specific instruction sequence that jumps to the called
   procedure (a "UUO link").  */

typedef enum
{
  LINKAGE_SECTION_TYPE_OPERATOR,
  LINKAGE_SECTION_TYPE_REFERENCE,
  LINKAGE_SECTION_TYPE_ASSIGNMENT,
  LINKAGE_SECTION_TYPE_GLOBAL_OPERATOR,
  N_LINKAGE_SECTION_TYPES
} linkage_section_type_t;

extern linkage_section_type_t linkage_section_type (SCHEME_OBJECT);
extern unsigned long linkage_section_count (SCHEME_OBJECT);
extern SCHEME_OBJECT make_linkage_section_marker
  (linkage_section_type_t, unsigned long);

extern long make_uuo_link
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);
extern long coerce_to_compiled (SCHEME_OBJECT, unsigned int, SCHEME_OBJECT *);
extern SCHEME_OBJECT read_uuo_link (SCHEME_OBJECT, unsigned long);

extern SCHEME_OBJECT read_uuo_symbol (SCHEME_OBJECT *);
extern insn_t * read_uuo_target_no_reloc (SCHEME_OBJECT *);
extern void write_uuo_target (insn_t *, SCHEME_OBJECT *);

extern unsigned int read_uuo_frame_size (SCHEME_OBJECT *);

extern void write_variable_cache (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);

/* Compiled closures

   A manifest closure header is followed by a (positive) count N,
   which is followed by N closure entries.  Each entry consists of a
   GC offset and a type followed by the machine code for the closure
   (typically a call-like instruction).  The entries are tightly
   packed so that the end address of one entry is the start address of
   the next.  After the end address of the last entry there is
   optional padding, followed by the closure's marked objects.

   When EMBEDDED_CLOSURE_ADDRS_P is defined, the target address of a
   closure is embedded in the closure's code, and must be specially
   managed by the garbage collector.  When undefined, the targets are
   stored in the marked objects section of the closure, and so need no
   special treatment.

   When EMBEDDED_CLOSURE_ADDRS_P is defined, the following macros are
   used to read and write closure targets.

   READ_COMPILED_CLOSURE_TARGET(start, ref) returns the target of
   'start' as a SCHEME_OBJECT, using 'ref' as the relocation
   reference.  */

/* Given the address of the word past the manifest closure header,
   returns the number of closure entries in the block.  The returned
   value must be positive.  */
extern unsigned long compiled_closure_count (SCHEME_OBJECT *);

/* Given the address of the word past the manifest closure header,
   returns the address of the first closure entry in the block.  */
extern insn_t * compiled_closure_start (SCHEME_OBJECT *);

/* Given the address of the word past the manifest closure header,
   returns the address of the first marked object.  */
extern SCHEME_OBJECT * compiled_closure_objects (SCHEME_OBJECT *);

/* Given the address of a closure entry, returns the invocation
   address for the corresponding closure.  */
extern insn_t * compiled_closure_entry (insn_t *);

/* Given the address of a closure entry, returns the address of the
   next closure entry.  (Which is also the end address of the given
   closure entry.)  */
extern insn_t * compiled_closure_next (insn_t *);

/* Given the address of the end of the last closure entry, returns the
   address of the first marked object.  */
extern SCHEME_OBJECT * skip_compiled_closure_padding (insn_t *);

/* Given the address of a closure entry, returns the compiled-code
   entry that this closure invokes.  */
extern SCHEME_OBJECT compiled_closure_entry_to_target (insn_t *);

/* Given a compiled-code entry point and the address of a closure
   entry, modifies the closure to invoke the entry point.  */
extern void write_compiled_closure_target (insn_t *, insn_t *);

/* Given a compiled-code block, returns true iff it is a closure's
   block.  */
extern bool cc_block_closure_p (SCHEME_OBJECT);

/* Given a compiled-code entry, returns true iff it is a closure.  */
extern bool cc_entry_closure_p (SCHEME_OBJECT);

/* Given a compiled-code closure, returns the compiled-code entry that
   it calls.  */
extern SCHEME_OBJECT cc_closure_to_entry (SCHEME_OBJECT);

/* Trampolines

   Trampolines are "closures" that call code in the compiled-code
   interface rather than compiled code.  They have an
   architecture-specific calling sequence and a marked storage area.

   The architecture description must define TRAMPOLINE_ENTRY_SIZE to
   be the number of words occupied by the instruction sequence,
   including the compiled-code entry header.  The instruction sequence
   must be padded, if necessary, to use an integral number of words.

   Here is a diagram of a trampoline on a 32-bit machine:

   0x00		TC_FIXNUM | 2 + trampoline_entry_size() + n
   0x04		TC_MANIFEST_NM_VECTOR | trampoline_entry_size() == k
   0x08		trampoline entry (k words)
   0x08+k*4	trampoline storage (n words)

 */

/* Given the number of trampoline entries, returns the number of words
   needed to hold the instruction sequences for those entries.  */
extern unsigned long trampoline_entry_size (unsigned long);

/* Given the address of a trampoline block and an entry index, returns
   the address of the specified entry point.  */
extern insn_t * trampoline_entry_addr (SCHEME_OBJECT *, unsigned long);

/* Given the address of a trampoline entry and the code for the
   trampoline to be invoked, stores the appropriate instruction
   sequence in the trampoline.  */
extern bool store_trampoline_insns (insn_t *, byte_t);

/* Give the address of a trampoline block, returns a pointer to the
   start of the trampoline's storage area.  */
extern SCHEME_OBJECT * trampoline_storage (SCHEME_OBJECT *);

#ifndef UTILITY_RESULT_DEFINED
#ifdef CMPINT_USE_STRUCS

typedef struct
{
  void * interface_dispatch;
  union
    {
      long code_to_interpreter;
      insn_t * entry_point;
    } extra;
} utility_result_t;

#else

typedef insn_t * utility_result_t;
extern long C_return_value;

#endif
#endif

typedef void ASM_ENTRY_POINT (utility_proc_t)
  (utility_result_t *, unsigned long, unsigned long, unsigned long, 
   unsigned long);
extern utility_proc_t * utility_table [];

#ifndef FLUSH_I_CACHE
#  define FLUSH_I_CACHE() do {} while (false)
#endif

#if !defined(PUSH_D_CACHE_REGION) && defined(FLUSH_I_CACHE_REGION)
#  define PUSH_D_CACHE_REGION(addr, nwords) FLUSH_I_CACHE_REGION(addr, nwords)
#endif

extern unsigned int compiler_interface_version;
extern cc_arch_t compiler_processor_type;
extern unsigned long max_trampoline;

extern SCHEME_OBJECT compiler_utilities;
extern SCHEME_OBJECT return_to_interpreter;
extern SCHEME_OBJECT reflect_to_interface;

extern SCHEME_OBJECT cc_block_debugging_info (SCHEME_OBJECT);
extern SCHEME_OBJECT cc_block_environment (SCHEME_OBJECT);

extern long enter_compiled_expression (void);
extern void guarantee_cc_return (unsigned long);
extern void guarantee_interp_return (void);
extern long apply_compiled_procedure (void);
extern long return_to_compiled_code (void);

extern long apply_compiled_from_primitive (unsigned long, SCHEME_OBJECT);
extern void compiled_with_interrupt_mask
  (unsigned long, SCHEME_OBJECT, unsigned long);
extern void compiled_with_stack_marker (SCHEME_OBJECT);

extern void compiler_initialize (bool);
extern void compiler_reset (SCHEME_OBJECT);

extern void declare_compiled_code_block (SCHEME_OBJECT);

extern void compiler_interrupt_common
  (utility_result_t *, insn_t *, SCHEME_OBJECT);

extern long comp_link_caches_restart (void);
extern long comp_op_lookup_trap_restart (void);
extern long comp_interrupt_restart (void);
extern long comp_assignment_trap_restart (void);
extern long comp_cache_lookup_apply_restart (void);
extern long comp_lookup_trap_restart (void);
extern long comp_safe_lookup_trap_restart (void);
extern long comp_unassigned_p_trap_restart (void);
extern long comp_error_restart (void);

extern SCHEME_OBJECT bkpt_install (insn_t *);
extern SCHEME_OBJECT bkpt_closure_install (insn_t *);
extern bool bkpt_p (insn_t *);
extern SCHEME_OBJECT bkpt_proceed (insn_t *, SCHEME_OBJECT, SCHEME_OBJECT);
extern long do_bkpt_proceed (insn_t **);
extern void bkpt_remove (insn_t *, SCHEME_OBJECT);

extern int pc_to_utility_index (unsigned long);
extern const char * utility_index_to_name (unsigned int);
extern int pc_to_builtin_index (unsigned long);
extern const char * builtin_index_to_name (unsigned int);
extern void declare_builtin (unsigned long, const char *);

extern utility_proc_t comutil_return_to_interpreter;
extern utility_proc_t comutil_operator_apply_trap;
extern utility_proc_t comutil_operator_arity_trap;
extern utility_proc_t comutil_operator_entity_trap;
extern utility_proc_t comutil_operator_interpreted_trap;
extern utility_proc_t comutil_operator_lexpr_trap;
extern utility_proc_t comutil_operator_primitive_trap;
extern utility_proc_t comutil_operator_lookup_trap;
extern utility_proc_t comutil_operator_1_0_trap;
extern utility_proc_t comutil_operator_2_1_trap;
extern utility_proc_t comutil_operator_2_0_trap;
extern utility_proc_t comutil_operator_3_2_trap;
extern utility_proc_t comutil_operator_3_1_trap;
extern utility_proc_t comutil_operator_3_0_trap;
extern utility_proc_t comutil_operator_4_3_trap;
extern utility_proc_t comutil_operator_4_2_trap;
extern utility_proc_t comutil_operator_4_1_trap;
extern utility_proc_t comutil_operator_4_0_trap;
extern utility_proc_t comutil_primitive_apply;
extern utility_proc_t comutil_primitive_lexpr_apply;
extern utility_proc_t comutil_apply;
extern utility_proc_t comutil_error;
extern utility_proc_t comutil_lexpr_apply;
extern utility_proc_t comutil_link;
extern utility_proc_t comutil_interrupt_closure;
extern utility_proc_t comutil_interrupt_dlink;
extern utility_proc_t comutil_interrupt_procedure;
extern utility_proc_t comutil_interrupt_continuation;
extern utility_proc_t comutil_interrupt_ic_procedure;
extern utility_proc_t comutil_assignment_trap;
extern utility_proc_t comutil_cache_lookup_apply;
extern utility_proc_t comutil_lookup_trap;
extern utility_proc_t comutil_safe_lookup_trap;
extern utility_proc_t comutil_unassigned_p_trap;
extern utility_proc_t comutil_decrement;
extern utility_proc_t comutil_divide;
extern utility_proc_t comutil_equal;
extern utility_proc_t comutil_greater;
extern utility_proc_t comutil_increment;
extern utility_proc_t comutil_less;
extern utility_proc_t comutil_minus;
extern utility_proc_t comutil_multiply;
extern utility_proc_t comutil_negative;
extern utility_proc_t comutil_plus;
extern utility_proc_t comutil_positive;
extern utility_proc_t comutil_zero;
extern utility_proc_t comutil_primitive_error;
extern utility_proc_t comutil_quotient;
extern utility_proc_t comutil_remainder;
extern utility_proc_t comutil_modulo;
extern utility_proc_t comutil_reflect_to_interface;
extern utility_proc_t comutil_interrupt_continuation_2;
extern utility_proc_t comutil_compiled_code_bkpt;
extern utility_proc_t comutil_compiled_closure_bkpt;

#endif /* !NO_CC_SUPPORT_P */
#endif /* !SCM_CMPINT_H */
