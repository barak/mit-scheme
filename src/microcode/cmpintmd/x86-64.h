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

/* Compiled code interface macros for AMD x86-64.  */

#ifndef SCM_CMPINTMD_H_INCLUDED
#define SCM_CMPINTMD_H_INCLUDED 1

/*

Register assignments
====================

RAX (0)		Unassigned
RCX (1)		Unassigned
RDX (2)		Unassigned
RBX (3)		Unassigned

RSP (4)		Stack Pointer
RBP (5)		Register Mask
RSI (6)		Pointer to register block, etc.
RDI (7)		Free Pointer

R8-R15		Unassigned

The Free Pointer is RDI because RDI is the implicit base register for
the memory-to-memory move instructions, and the string store
instruction.  Perhaps we can make use of it.

The pointer to register block is not held in RBP (the processor's
"frame" register is typically used) because its most common use, (RBP)
(address syllable for memory memtop) takes more bytes than (RSI).

Encodings and layout of various control features
================================================

Assumptions:

The processor will be in 64-bit address and operand mode.  Thus
instructions use 64-bit operands, and displacements for addressing
modes and jump instructions are all 64 bits by default.

	Offset		Contents		Encoding


- Execute cache entry encoding:

		Before linking

	0		16-bit arity	\
	2		zero		  [TC_FIXNUM | arity]
	7		0x1A		/
entry	8		symbol
	16		<eight bytes of padding>
	24		<next cache>

		After linking

	0		16-bit arity
	2		zero
	7		0x1A
entry	8		MOV RAX,imm64		0x48 0xB8
	10		<address>
	18		JMP (RAX)		0xFF 0xE0
	19-23		<four bytes of padding>
	24		<next cache>


- Closures:

The only reason for a 32-bit entry count is to align everything
nicely.

	0		<closure manifest>
	8		<entry count>
	12		<type/arity info>       \__ format word
	14		<gc offset>             /
entry0	16		MOV RAX,imm64		0x48 0xB8
	18		<address>
	26		CALL (RAX)		0xFF 0xD0
	28		<four bytes of padding or next format word>
	...
	16*(n+1)	<variables>


- Trampoline encoding:

	-8		<padding>
	-4		<type/arity info>
	-2		<gc offset>
entry	0		MOV	AL,code		0xB0, code-byte
	2		CALL	n(RSI)		0xFF 0x96 n-longword
	8		<trampoline dependent storage>

*/

#define ASM_RESET_HOOK x86_64_reset_hook

#define CMPINT_USE_STRUCS 1

/* These next definitions must agree with "cmpauxmd/x86-64.m4", which is
   where the register block is allocated.  */
#define COMPILER_REGBLOCK_N_FIXED 16
/* Size in objects of the largest quantities that RTL registers can hold.  */
#define COMPILER_TEMP_SIZE 1
#define COMPILER_REGBLOCK_N_TEMPS 256
#define COMPILER_REGBLOCK_N_HOOKS 80
#define COMPILER_HOOK_SIZE 1

#define COMPILER_REGBLOCK_EXTRA_SIZE					\
  (COMPILER_REGBLOCK_N_HOOKS * COMPILER_HOOK_SIZE)

typedef byte_t insn_t;

/* Number of insn_t units preceding entry address in which header
   (type and offset info) is stored.  */
#define CC_ENTRY_HEADER_SIZE (CC_ENTRY_TYPE_SIZE + CC_ENTRY_OFFSET_SIZE)
#define CC_ENTRY_TYPE_SIZE 2
#define CC_ENTRY_OFFSET_SIZE 2

/* Number of insn_t units preceding entry header in which GC trap
   instructions are stored.  This is an approximation: it matches only
   those non-closure procedures for which LIAR has generated interrupt
   checks, in which case there is one CALL n(RSI), which is encoded as
   #xff #x96 <n>, where n is a longword (32 bits).  */
#define CC_ENTRY_GC_TRAP_SIZE 6

#define EMBEDDED_CLOSURE_ADDRS_P 1

#define DECLARE_RELOCATION_REFERENCE(name)

#define START_CLOSURE_RELOCATION(scan, ref)	do {} while (0)
#define START_OPERATOR_RELOCATION(scan, ref)	do {} while (0)

#define OPERATOR_RELOCATION_OFFSET 0

#define READ_COMPILED_CLOSURE_TARGET(a, r)				\
  read_compiled_closure_target (a)

/* Size of execution cache in SCHEME_OBJECTS.  */
#define UUO_LINK_SIZE 3

#define UUO_WORDS_TO_COUNT(nw) ((nw) / UUO_LINK_SIZE)
#define UUO_COUNT_TO_WORDS(nc) ((nc) * UUO_LINK_SIZE)

#define READ_UUO_TARGET(a, r) read_uuo_target (a)

#if defined(__WIN32__) && defined(__WATCOMC__)
#  define ASM_ENTRY_POINT(name) (__cdecl name)
#else
#  define ASM_ENTRY_POINT(name) name
#endif

extern void ASM_ENTRY_POINT (x86_64_interface_initialize) (void);
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
extern void asm_trampoline_to_interface (void);

extern insn_t * read_compiled_closure_target (insn_t *);
extern insn_t * read_uuo_target (SCHEME_OBJECT *);
extern void x86_64_reset_hook (void);

#ifndef HAVE_FENV_H
#  define CMPINTMD_EMULATES_FENV
#  define x87_p 1
#  define sse_p 1
#  include "cmpintmd/x86-fenv.h"
#endif

#endif /* !SCM_CMPINTMD_H_INCLUDED */
