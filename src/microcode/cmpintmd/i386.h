/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

/* Compiled code interface macros for Intel IA-32.  */

#ifndef SCM_CMPINTMD_H_INCLUDED
#define SCM_CMPINTMD_H_INCLUDED 1

/*

Problems with the IA-32 instruction set architecture
====================================================

1. Code space is separate from data space.  The only way to obtain a
   code space address is to do a CALL and use the return address on
   the stack.

Problem: References to the constants vector in compiled code.

Fix: Just as on RISC machines.  Use CALL when necessary, and cache the
   result in the assembly language.


2. Jumps are PC-relative.  There are absolute jumps, assuming the PC
   is in a data location, or with immediate destinations that include
   a segment descriptor (16 bits).  The short forms have a PC-relative
   offset defined with respect to the immediately following
   instruction.

Problem: Closures and execute caches need their address in old space
   in order to be relocated correctly.

Fix:

For execute caches we can define a new linker field, called
load-relocation-address which on every GC/relocation stores the new
address and the old contents into global variables and stores the new
address in the field.  Alternatively the difference between the new
address and the old contents can be stored into a single global
variable, and this can be used, together with the new address of each
cache, to find the old code.

For closures the code that reads the header (manifest closure) can do
the same.


3. The stack pointer register (ESP) cannot be used as the base in
   (base + displacement) addressing mode.

Problem: Common operation in the compiler, which assumes direct access
   to the stack.

Fix: Use base + indexed mode, which allows specification of ESP as
   base and nullification of the index (by using ESP again).  This is
   one byte longer than otherwise, but...


Register assignments
====================

EAX (0)		Unassigned
ECX (1)		Unassigned
EDX (2)		Unassigned
EBX (3)		Unassigned

ESP (4)		Stack Pointer
EBP (5)		Register Mask
ESI (6)		Pointer to register block, etc.
EDI (7)		Free Pointer

The dynamic link and value "registers" are not processor registers.
Slots in the register array must be reserved for them.

The Free Pointer is EDI because EDI is the implicit base register for
the memory-to-memory move instructions, and the string store
instruction.  Perhaps we can make use of it.

The pointer to register block is not held in EBP (the processor's
"frame" register is typically used) because its most common use, (EBP)
(address syllable for memory memtop) takes more bytes than (ESI).

Encodings and layout of various control features
================================================

Assumptions:

The processor will be in 32-bit address and operand mode.  Thus
instructions use 32-bit operands, and displacements for addressing
modes and jump instructions are all 32 bits by default.

	Offset		Contents		Encoding


- Execute cache entry encoding:

		Before linking

	0		16-bit arity	\
	2		0x00		  [TC_FIXNUM | arity]
entry	3		0x1A		/
	4		Symbol
	8		<next cache>

		After linking

	0		16-bit arity
	2		0x00
entry	3		JMP opcode		0x39
	4		32-bit offset
	8		<next cache>

Arity stays in place because the IA-32 is a little-endian architecture.


- Closure entry encoding:

entry	0		CALL opcode		0xE8
	1		32-bit offset
	5		<padding>		0x00
	6		<next entry or variables>


- Trampoline encoding:

entry	0		MOV	AL,code		0xB0, code-byte
	2		CALL	n(ESI)		0xFF 0x96 n-longword
	8		<trampoline dependent storage>


- GC & interrupt check at procedure/continuation entry:

gc_lab	-7		CALL	n(ESI)		0xFF 0x56 n-byte
	-4		<type/arity info>
	-2		<gc offset>
entry	0		CMP	EDI,(ESI)	0x39 0x3e
	2		JAE	gc_lab		0x73 -11
	4		<real code>


- GC & interrupt check at closure entry:

gc_lab	-11		ADD	(ESP),&offset	0x83 0x04 0x24 offset-byte
  	-7		JMP	n(ESI)		0xFF 0x66 n-byte
	-4		<type/arity info>
	-2		<gc offset>
entry	0		ADD	(ESP),&magic	0x81 0x04 0x24 magic-longword
	7		CMP	EDI,(ESI)	0x39 0x3e
	9		JAE	gc_lab		0x73 0xea (= -22)
	11		<real code>

The magic value depends on the closure because of canonicalization.

The ADD instruction at offset -11 is not present for the 0th closure
entry, since it is the canonical entry point.  Its format depends on
the value of offset, since the sign-extending forms often suffice.

offset = entry_number * entry_size
magic = ([TC_COMPILED_ENTRY | 0] - (offset + length_of_CALL_instruction))

*/

#define ASM_RESET_HOOK i386_reset_hook
#define FPE_RESET_TRAPS i386_interface_initialize

#define CMPINT_USE_STRUCS 1

/* These next definitions must agree with "cmpauxmd/i386.m4", which is
   where the register block is allocated.  */
#define COMPILER_REGBLOCK_N_FIXED 16
/* Big enough to hold 80-bit floating-point value: */
#define COMPILER_TEMP_SIZE 3
#define COMPILER_REGBLOCK_N_TEMPS 256
#define COMPILER_REGBLOCK_N_HOOKS 80
#define COMPILER_HOOK_SIZE 1

#define COMPILER_REGBLOCK_EXTRA_SIZE					\
  (COMPILER_REGBLOCK_N_HOOKS * COMPILER_HOOK_SIZE)

#define REGBLOCK_ALLOCATED_BY_INTERFACE true

typedef byte_t insn_t;

/* Number of insn_t units preceding entry address in which header
   (type and offset info) is stored.  */
#define CC_ENTRY_HEADER_SIZE (CC_ENTRY_TYPE_SIZE + CC_ENTRY_OFFSET_SIZE)
#define CC_ENTRY_TYPE_SIZE 2
#define CC_ENTRY_OFFSET_SIZE 2

/* Number of insn_t units preceding entry header in which GC trap
   instructions are stored.  */
#define CC_ENTRY_GC_TRAP_SIZE 3

#define EMBEDDED_CLOSURE_ADDRS_P 1

typedef struct
{
  insn_t * old_addr;
  insn_t * new_addr;
} reloc_ref_t;

#define DECLARE_RELOCATION_REFERENCE(name) reloc_ref_t name

#define START_CLOSURE_RELOCATION(scan, ref)				\
  start_closure_relocation ((scan), (&ref))

#define START_OPERATOR_RELOCATION(scan, ref) do				\
{									\
  start_operator_relocation ((scan), (&ref));				\
  (scan) += 1;								\
} while (false)

#define OPERATOR_RELOCATION_OFFSET 1

#define READ_COMPILED_CLOSURE_TARGET(a, r)				\
  read_compiled_closure_target ((a), (&r))

/* Size of execution cache in SCHEME_OBJECTS.  */
#define UUO_LINK_SIZE 2

#define UUO_WORDS_TO_COUNT(nw) (((nw) - 1) / UUO_LINK_SIZE)
#define UUO_COUNT_TO_WORDS(nc) (((nc) * UUO_LINK_SIZE) + 1)

#define READ_UUO_TARGET(a, r) read_uuo_target ((a), (&r))

#define FLUSH_I_CACHE() IA32_CACHE_SYNCHRONIZE ()
#define FLUSH_I_CACHE_REGION(address, nwords) IA32_CACHE_SYNCHRONIZE ()
#define PUSH_D_CACHE_REGION(address, nwords) IA32_CACHE_SYNCHRONIZE ()

#define IA32_CACHE_SYNCHRONIZE() do					\
{									\
  if (ia32_cpuid_needed)						\
    ia32_cache_synchronize ();						\
} while (false)

#if defined(__OS2__) && (defined(__IBMC__) || defined(__WATCOMC__))
#  define ASM_ENTRY_POINT(name) (_System name)
#elif defined(__WIN32__) && defined(__WATCOMC__)
#  define ASM_ENTRY_POINT(name) (__cdecl name)
#else
#  define ASM_ENTRY_POINT(name) name
#endif

extern int ASM_ENTRY_POINT (i386_interface_initialize) (void);
extern void ASM_ENTRY_POINT (within_c_stack) (void (*) (void *), void *);

extern void asm_assignment_trap (void);
extern void asm_dont_serialize_cache (void);
extern void asm_error (void);
extern void asm_fixnum_shift (void);
extern void asm_generic_add (void);
extern void asm_generic_decrement (void);
extern void asm_generic_divide (void);
extern void asm_generic_equal (void);
extern void asm_generic_greater (void);
extern void asm_generic_increment (void);
extern void asm_generic_less (void);
extern void asm_generic_modulo (void);
extern void asm_generic_multiply (void);
extern void asm_generic_negative (void);
extern void asm_generic_positive (void);
extern void asm_generic_quotient (void);
extern void asm_generic_remainder (void);
extern void asm_generic_subtract (void);
extern void asm_generic_zero (void);
extern void asm_interrupt_closure (void);
extern void asm_interrupt_continuation (void);
extern void asm_interrupt_continuation_2 (void);
extern void asm_interrupt_dlink (void);
extern void asm_interrupt_procedure (void);
extern void asm_link (void);
extern void asm_nofp_add (void);
extern void asm_nofp_decrement (void);
extern void asm_nofp_divide (void);
extern void asm_nofp_equal (void);
extern void asm_nofp_greater (void);
extern void asm_nofp_increment (void);
extern void asm_nofp_less (void);
extern void asm_nofp_modulo (void);
extern void asm_nofp_multiply (void);
extern void asm_nofp_negative (void);
extern void asm_nofp_positive (void);
extern void asm_nofp_quotient (void);
extern void asm_nofp_remainder (void);
extern void asm_nofp_subtract (void);
extern void asm_nofp_zero (void);
extern void asm_primitive_apply (void);
extern void asm_primitive_error (void);
extern void asm_primitive_lexpr_apply (void);
extern void asm_reference_trap (void);
extern void asm_safe_reference_trap (void);
extern void asm_set_interrupt_enables (void);
extern void asm_sc_apply (void);
extern void asm_sc_apply_size_1 (void);
extern void asm_sc_apply_size_2 (void);
extern void asm_sc_apply_size_3 (void);
extern void asm_sc_apply_size_4 (void);
extern void asm_sc_apply_size_5 (void);
extern void asm_sc_apply_size_6 (void);
extern void asm_sc_apply_size_7 (void);
extern void asm_sc_apply_size_8 (void);
extern void asm_scheme_to_interface (void);
extern void asm_scheme_to_interface_call (void);
extern void asm_serialize_cache (void);
extern void asm_short_primitive_apply (void);
extern void asm_trampoline_to_interface (void);

extern void ia32_cache_synchronize (void);
extern void start_closure_relocation (SCHEME_OBJECT *, reloc_ref_t *);
extern insn_t * read_compiled_closure_target (insn_t *, reloc_ref_t *);
extern void start_operator_relocation (SCHEME_OBJECT *, reloc_ref_t *);
extern insn_t * read_uuo_target (SCHEME_OBJECT *, reloc_ref_t *);
extern void i386_reset_hook (void);
extern int i387_read_fp_control_word (void);
extern void i387_write_fp_control_word (int);

extern int ia32_cpuid_needed;

#ifndef HAVE_FENV_H

#  define FE_TONEAREST 0
#  define FE_DOWNWARD 1
#  define FE_UPWARD 2
#  define FE_TOWARDZERO 3

#  define HAVE_FEGETROUND
#  define HAVE_FESETROUND

static inline int
fegetround (void)
{
  return (3 & ((i387_read_fp_control_word ()) >> 10));
}

static inline int
fesetround (int mode)
{
  switch (mode)
    {
    case 0: case 1: case 2: case 3: break;
    default: return (1);
    }
  i387_write_fp_control_word
    ((mode << 10) | (0xf3ff & (i387_read_fp_control_word ())));
  return (0);
}

#endif /* HAVE_FENV_H */

#endif /* !SCM_CMPINTMD_H_INCLUDED */
