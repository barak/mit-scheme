/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpintmd/i386.h,v 1.7 1992/02/12 15:29:26 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

/*
 *
 * Compiled code interface macros.
 *
 * See cmpint.txt for a description of these fields.
 *
 * Specialized for the Intel 386 (and successor) architecture.
 */

#ifndef CMPINT2_H_INCLUDED
#define CMPINT2_H_INCLUDED

#define COMPILER_NONE_TYPE			0
#define COMPILER_MC68020_TYPE			1
#define COMPILER_VAX_TYPE			2
#define COMPILER_SPECTRUM_TYPE			3
#define COMPILER_MIPS_TYPE			4
#define COMPILER_MC68040_TYPE			5
#define COMPILER_SPARC_TYPE			6
#define COMPILER_RS6000_TYPE			7
#define COMPILER_MC88K_TYPE			8
#define COMPILER_I386_TYPE			9
#define COMPILER_ALPHA_TYPE			10

/*

	Problems with i386 ISA (instruction set architecture)

<1> Code space is separate from data space.  The only way to obtain a
code space address is to do a CALL and use the return address on the
stack.

Problem: References to the constants vector in compiled code.

Fix: Just as on RISC machines.  Use CALL when necessary, and cache the
result in the assembly language.


<2> Jumps are PC-relative.  There are absolute jumps, assuming the PC
is in a data location, or with immediate destinations that include a
segment descriptor (16 bits).  The short forms have a PC-relative
offset defined with respect to the immediately following instruction.

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

  For closures the code that reads the header (manifest closure) can
do the same.


<3> The stack pointer register (ESP) cannot be used as the base in
(base + displacement) addressing mode.

Problem: Common operation in the compiler, which assumes direct access
to the stack.

Fix: Use base + indexed mode, which allows specification of ESP as
base and nullification of the index (by using ESP again).
This is one byte longer than otherwise, but...

	Register assignments

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

	Encodings and layout of various control features:

Assumptions:

  The processor will be in 32-bit address and operand mode.
Thus instructions use 32-bit operands, and displacements for
addressing modes and jump instructions are all 32-bits by default.

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

Arity stays in place because the i386 is a little-endian architecture.


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

#define COMPILER_PROCESSOR_TYPE			COMPILER_I386_TYPE

/* The i387 coprocessor and i486 use 80-bit extended format internally. */

#define COMPILER_TEMP_SIZE			3

typedef unsigned short format_word;

/* i386 instructions can be aligned on any byte boundary. */

#define PC_ZERO_BITS                    	0

/* See the encodings above. */

#define ENTRY_SKIPPED_CHECK_OFFSET 		4
#define ENTRY_PREFIX_LENGTH			3

#define CLOSURE_SKIPPED_CHECK_OFFSET 		11

#  define ADJUST_CLOSURE_AT_CALL(entry_point, location)			\
do {									\
  long magic_constant;							\
									\
  magic_constant = (* ((long *) (((char *) (entry_point)) + 3)));	\
  (location) = ((SCHEME_OBJECT)						\
		((((long) (OBJECT_ADDRESS (location))) + 5) +		\
		 magic_constant));					\
} while (0)

#  define COMPILED_CLOSURE_ENTRY_SIZE					\
  ((2 * (sizeof (format_word))) + 6)

/* *** GC and other relocators have to be changed to set this up. *** */

#define PC_RELATIVE_CLOSURES
#define PC_RELATIVE_UUO_LINKS

extern long pc_displacement_relocation;

#define EXTRACT_ADDRESS_FROM_DISPLACEMENT(loc, instr_address) do	\
{									\
  long displacement_address, new_displacement;				\
									\
  displacement_address = (((long) (instr_address)) + 1);		\
  new_displacement = ((* ((long *) displacement_address))		\
		      + pc_displacement_relocation);			\
  (* ((long *) displacement_address)) = new_displacement;		\
  (loc) = ((SCHEME_OBJECT)						\
	   ((displacement_address + 4) + new_displacement));		\
} while (0)

#define STORE_DISPLACEMENT_FROM_ADDRESS(target, instr_address) do	\
{									\
  long displacement_address = (((long) (instr_address)) + 1);		\
  (* ((long *) displacement_address)) =					\
    (((long) (target)) - (displacement_address + 4));			\
} while (0)

#define EXTRACT_CLOSURE_ENTRY_ADDRESS	EXTRACT_ADDRESS_FROM_DISPLACEMENT
#define STORE_CLOSURE_ENTRY_ADDRESS	STORE_DISPLACEMENT_FROM_ADDRESS

#define EXECUTE_CACHE_ENTRY_SIZE		2

#define EXTRACT_EXECUTE_CACHE_ARITY(target, address) do			\
{									\
  (target) = ((long) (* ((unsigned short *) (address))));		\
} while (0)

#define EXTRACT_EXECUTE_CACHE_SYMBOL(target, address) do		\
{									\
  (target) = (* (((SCHEME_OBJECT *) (address)) + 1));			\
} while (0)

#define EXTRACT_EXECUTE_CACHE_ADDRESS(target, address) do		\
{									\
  EXTRACT_ADDRESS_FROM_DISPLACEMENT (target,				\
				     (((long) (address)) + 3));		\
} while (0)

#define STORE_EXECUTE_CACHE_ADDRESS(address, entry_address) do		\
{									\
  STORE_DISPLACEMENT_FROM_ADDRESS (entry_address,			\
				   (((long) (address)) + 3));		\
} while (0)

#define STORE_EXECUTE_CACHE_CODE(address) do				\
{									\
  /* Store a <JMP rel32> opcode. */					\
  (* (((unsigned char *) (address)) + 3)) = 0xe9;			\
} while (0)

#define TRAMPOLINE_ENTRY_SIZE			3
#define TRAMPOLINE_BLOCK_TO_ENTRY		3 /* MNV to MOV instr. */

#define STORE_TRAMPOLINE_ENTRY(entry_address, index) do			\
{									\
  unsigned char *PC = ((unsigned char *) (entry_address));		\
									\
  *PC++ = 0xb0;			/* MOV	AL,byte */			\
  *PC++ = (index);		/* byte value */			\
  *PC++ = 0xff;			/* CALL */				\
  *PC++ = 0x96;			/* /2 disp32(ESI) */			\
  (* ((unsigned long *) PC)) = ESI_TRAMPOLINE_TO_INTERFACE_OFFSET;	\
} while (0)

#define TRAMPOLINE_ENTRY_POINT(tramp_block)				\
  (((SCHEME_OBJECT *) (tramp_block)) + TRAMPOLINE_BLOCK_TO_ENTRY)

#define TRAMPOLINE_STORAGE(tramp_entry)					\
  ((((SCHEME_OBJECT *) (tramp_entry)) - TRAMPOLINE_BLOCK_TO_ENTRY) +	\
   (2 + TRAMPOLINE_ENTRY_SIZE)) 

#define COMPILER_REGBLOCK_N_FIXED		16

#define COMPILER_REGBLOCK_N_HOOKS		80
	/* A hook is a 32-bit address for an indirect CALL/JMP instruction */
#define COMPILER_HOOK_SIZE			1

#define COMPILER_REGBLOCK_EXTRA_SIZE					\
  (COMPILER_REGBLOCK_N_HOOKS * COMPILER_HOOK_SIZE)

#define ESI_TRAMPOLINE_TO_INTERFACE_OFFSET				\
  ((COMPILER_REGBLOCK_N_FIXED + (2 * COMPILER_HOOK_SIZE)) *		\
   (sizeof (SCHEME_OBJECT)))

#ifdef IN_CMPINT_C

#define ASM_RESET_HOOK i386_reset_hook

#define SETUP_REGISTER(hook) do						\
{									\
  extern void hook ();							\
									\
  (* ((unsigned long *) (esi_value + offset))) =			\
    ((unsigned long) hook);						\
  offset += (COMPILER_HOOK_SIZE * (sizeof (SCHEME_OBJECT)));		\
} while (0)

void
DEFUN_VOID (i386_reset_hook)
{
  extern void interface_initialize ();
  unsigned char * esi_value = ((unsigned char *) (&Registers[0]));
  int offset = (COMPILER_REGBLOCK_N_FIXED * (sizeof (SCHEME_OBJECT)));

  /* These must match machines/i386/lapgen.scm */

  SETUP_REGISTER (asm_scheme_to_interface); 		/* 0 */
  SETUP_REGISTER (asm_scheme_to_interface_call);	/* 1 */

  if (offset != ESI_TRAMPOLINE_TO_INTERFACE_OFFSET)
  {
    fprintf (stderr,
	     "\ni386_reset_hook: ESI_TRAMPOLINE_TO_INTERFACE_OFFSET\n");
    Microcode_Termination (TERM_EXIT);
  }
  SETUP_REGISTER (asm_trampoline_to_interface);		/* 2 */

  SETUP_REGISTER (asm_interrupt_procedure);		/* 3 */
  SETUP_REGISTER (asm_interrupt_continuation);		/* 4 */
  SETUP_REGISTER (asm_interrupt_closure);		/* 5 */
  SETUP_REGISTER (asm_interrupt_dlink);			/* 6 */

#if 0
  SETUP_REGISTER (asm_primitive_apply);			/* 7 */
  SETUP_REGISTER (asm_primitive_lexpr_apply);		/* 8 */

  SETUP_REGISTER (asm_assignment_trap);			/* 9 */
  SETUP_REGISTER (asm_reference_trap);			/* 10 */
  SETUP_REGISTER (asm_safe_reference_trap);		/* 11 */

  SETUP_REGISTER (asm_generic_add);			/* 12 */
  SETUP_REGISTER (asm_generic_subtract);		/* 13 */
  SETUP_REGISTER (asm_generic_multiply);		/* 14 */
  SETUP_REGISTER (asm_generic_divide);			/* 15 */
  SETUP_REGISTER (asm_generic_equal);			/* 16 */
  SETUP_REGISTER (asm_generic_less);			/* 17 */
  SETUP_REGISTER (asm_generic_greater);			/* 18 */
  SETUP_REGISTER (asm_generic_increment);		/* 19 */
  SETUP_REGISTER (asm_generic_decrement);		/* 20 */
  SETUP_REGISTER (asm_generic_zero);			/* 21 */
  SETUP_REGISTER (asm_generic_positive);		/* 22 */
  SETUP_REGISTER (asm_generic_negative);		/* 23 */
  SETUP_REGISTER (asm_generic_quotient);		/* 24 */
  SETUP_REGISTER (asm_generic_remainder);		/* 25 */
  SETUP_REGISTER (asm_generic_modulo);			/* 26 */

  SETUP_REGISTER (asm_shortcircuit_apply);		/* 27 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_1);	/* 28 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_2);	/* 29 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_3);	/* 30 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_4);	/* 31 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_5);	/* 32 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_6);	/* 33 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_7);	/* 34 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_8);	/* 35 */

  /* Are these really necessary? */
  SETUP_REGISTER (asm_link);				/* 36 */
  SETUP_REGISTER (asm_error);				/* 37 */
  SETUP_REGISTER (asm_primitive_error);			/* 38 */
#endif

  interface_initialize ();
  return;
}

#endif /* IN_CMPINT_C */

/* Derived parameters and macros.
   These macros expect the above definitions to be meaningful.
   If they are not, the macros below may have to be changed as well.
 */

#define COMPILED_ENTRY_OFFSET_WORD(entry)                               \
  (((format_word *) (entry))[-1])
#define COMPILED_ENTRY_FORMAT_WORD(entry)                               \
  (((format_word *) (entry))[-2])

/* The next one assumes 2's complement integers....*/
#define CLEAR_LOW_BIT(word)                     ((word) & ((unsigned long) -2))
#define OFFSET_WORD_CONTINUATION_P(word)        (((word) & 1) != 0)

#if (PC_ZERO_BITS == 0)
/* Instructions aligned on byte boundaries */
#define BYTE_OFFSET_TO_OFFSET_WORD(offset)      ((offset) << 1)
#define OFFSET_WORD_TO_BYTE_OFFSET(offset_word)                         \
  ((CLEAR_LOW_BIT(offset_word)) >> 1)
#endif

#if (PC_ZERO_BITS == 1)
/* Instructions aligned on word (16 bit) boundaries */
#define BYTE_OFFSET_TO_OFFSET_WORD(offset)      (offset)
#define OFFSET_WORD_TO_BYTE_OFFSET(offset_word)                         \
  (CLEAR_LOW_BIT(offset_word))
#endif

#if (PC_ZERO_BITS >= 2)
/* Should be OK for =2, but bets are off for >2 because of problems
   mentioned earlier!
*/
#define SHIFT_AMOUNT                            (PC_ZERO_BITS - 1)
#define BYTE_OFFSET_TO_OFFSET_WORD(offset)      ((offset) >> (SHIFT_AMOUNT))
#define OFFSET_WORD_TO_BYTE_OFFSET(offset_word)                         \
  ((CLEAR_LOW_BIT(offset_word)) << (SHIFT_AMOUNT))
#endif

#define MAKE_OFFSET_WORD(entry, block, continue)                        \
  ((BYTE_OFFSET_TO_OFFSET_WORD(((char *) (entry)) -                     \
                               ((char *) (block)))) |                   \
   ((continue) ? 1 : 0))

#if (EXECUTE_CACHE_ENTRY_SIZE == 2)
#define EXECUTE_CACHE_COUNT_TO_ENTRIES(count)                           \
  ((count) >> 1)
#define EXECUTE_CACHE_ENTRIES_TO_COUNT(entries)				\
  ((entries) << 1)
#endif

#if (EXECUTE_CACHE_ENTRY_SIZE == 4)
#define EXECUTE_CACHE_COUNT_TO_ENTRIES(count)                           \
  ((count) >> 2)
#define EXECUTE_CACHE_ENTRIES_TO_COUNT(entries)				\
  ((entries) << 2)
#endif

#if (!defined(EXECUTE_CACHE_COUNT_TO_ENTRIES))
#define EXECUTE_CACHE_COUNT_TO_ENTRIES(count)                           \
  ((count) / EXECUTE_CACHE_ENTRY_SIZE)
#define EXECUTE_CACHE_ENTRIES_TO_COUNT(entries)				\
  ((entries) * EXECUTE_CACHE_ENTRY_SIZE)
#endif

/* The first entry in a cc block is preceeded by 2 headers (block and nmv),
   a format word and a gc offset word.   See the early part of the
   TRAMPOLINE picture, above.
 */

#define CC_BLOCK_FIRST_ENTRY_OFFSET                                     \
  (2 * ((sizeof(SCHEME_OBJECT)) + (sizeof(format_word))))

/* Format words */

#define FORMAT_BYTE_EXPR                0xFF
#define FORMAT_BYTE_COMPLR              0xFE
#define FORMAT_BYTE_CMPINT              0xFD
#define FORMAT_BYTE_DLINK               0xFC
#define FORMAT_BYTE_RETURN              0xFB

#define FORMAT_WORD_EXPR        (MAKE_FORMAT_WORD(0xFF, FORMAT_BYTE_EXPR))
#define FORMAT_WORD_CMPINT      (MAKE_FORMAT_WORD(0xFF, FORMAT_BYTE_CMPINT))
#define FORMAT_WORD_RETURN      (MAKE_FORMAT_WORD(0xFF, FORMAT_BYTE_RETURN))

/* This assumes that a format word is at least 16 bits,
   and the low order field is always 8 bits.
 */

#define MAKE_FORMAT_WORD(field1, field2)                                \
  (((field1) << 8) | ((field2) & 0xff))

#define SIGN_EXTEND_FIELD(field, size)                                  \
  (((field) & ((1 << (size)) - 1)) |                                    \
   ((((field) & (1 << ((size) - 1))) == 0) ? 0 :                        \
    ((-1) << (size))))

#define FORMAT_WORD_LOW_BYTE(word)                                      \
  (SIGN_EXTEND_FIELD((((unsigned long) (word)) & 0xff), 8))

#define FORMAT_WORD_HIGH_BYTE(word)					\
  (SIGN_EXTEND_FIELD((((unsigned long) (word)) >> 8),			\
		     (((sizeof (format_word)) * CHAR_BIT) - 8)))

#define COMPILED_ENTRY_FORMAT_HIGH(addr)                                \
  (FORMAT_WORD_HIGH_BYTE(COMPILED_ENTRY_FORMAT_WORD(addr)))

#define COMPILED_ENTRY_FORMAT_LOW(addr)                                 \
  (FORMAT_WORD_LOW_BYTE(COMPILED_ENTRY_FORMAT_WORD(addr)))

#define FORMAT_BYTE_FRAMEMAX            0x7f

#define COMPILED_ENTRY_MAXIMUM_ARITY    COMPILED_ENTRY_FORMAT_LOW
#define COMPILED_ENTRY_MINIMUM_ARITY    COMPILED_ENTRY_FORMAT_HIGH

#endif /* CMPINT2_H_INCLUDED */
