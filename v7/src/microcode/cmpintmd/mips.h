/* -*-C-*-

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpintmd/mips.h,v 1.1 1990/04/01 20:14:51 jinx Exp $
 *
 * Compiled code interface macros.
 *
 * See cmpint.txt for a description of these fields.
 *
 * Specialized for the MIPS R2000/R3000
 */

#ifndef CMPINT2_H_INCLUDED
#define CMPINT2_H_INCLUDED

#define COMPILER_NONE_TYPE			0
#define COMPILER_MC68020_TYPE			1
#define COMPILER_VAX_TYPE			2
#define COMPILER_SPECTRUM_TYPE			3
#define COMPILER_MIPS_TYPE			4

/* Machine parameters to be set by the user. */

/* Processor type.  Choose a number from the above list, or allocate your own. */

#define COMPILER_PROCESSOR_TYPE			COMPILER_MIPS_TYPE

/* Size (in long words) of the contents of a floating point register if
   different from a double.  For example, an MC68881 saves registers
   in 96 bit (3 longword) blocks.
   Default is fine for MIPS.
   define COMPILER_TEMP_SIZE			3
*/

/* Descriptor size.
   This is the size of the offset field, and of the format field.
   This definition probably does not need to be changed.
 */

typedef unsigned short format_word;

/* PC alignment constraint.
   Change PC_ZERO_BITS to be how many low order bits of the pc are
   guaranteed to be 0 always because of PC alignment constraints.
*/

#define PC_ZERO_BITS                    2

/* Utilities for manipulating absolute subroutine calls.
   On the MIPS this is done with:
   	JAL	destination
   The low 26 bits of the instruction form the low 28 bits of address,
   and the top 4 bits of the address of the JAL instruction form the
   top 4 bits of the address.
 */

#define EXTRACT_ABSOLUTE_ADDRESS(target, address)			\
{ unsigned long jal_instr, *addr;					\
									\
  addr = (unsigned long *) (address);					\
  jal_instr = *addr;							\
  (target) = (SCHEME_OBJECT) ((((long) address) & 0xF0000000) |		\
                              ((jal_instr & 0x03FFFFFF) <<2));		\
}

#define JAL_OP		(003 << 26)
#define JAL_INSTR(dest)	(JAL_OP | ((dest) >> 2))

#define STORE_ABSOLUTE_ADDRESS(entry_point, address)			\
{ unsigned long *addr, ep;						\
									\
  ep = ((unsigned long) (entry_point));					\
  addr = ((unsigned long *) (address));					\
  if (((((long) addr) & 0xF0000000) !=					\
       (((long) entry_point) & 0xF0000000)) ||                          \
      ((((long) addr) & 0x3) != 0))					\
    printf("\nSTORE_ABSOLUTE_ADDRESS: Bad addr in JAL 0x%x, 0x%x\n",	\
	   addr, ep);							\
  *addr = JAL_INSTR(ep & 0x0FFFFFF);					\
}

/* Compiled Code Register Conventions */
/* This must match the compiler and cmpaux-mips.s */

#define COMP_REG_TEMPORARY		1
#define	COMP_REG_RETURN			2
#define COMP_REG_STACK			3
#define COMP_REG_C_ARG_1		4
#define COMP_REG_C_ARG_2		5
#define COMP_REG_C_ARG_3		6
#define COMP_REG_C_ARG_4		7
#define COMP_REG_MEMTOP			8
#define COMP_REG_FREE			9
#define COMP_REG_SCHEME_TO_INTERFACE	10
#define COMP_REG_DYNAMIC_LINK		11

#define COMP_REG_ADDRESS_MASK		20
#define COMP_REG_REGISTERS		21
#define COMP_REG_QUAD_MASK		22

#define COMP_REG_TRAMP_INDEX		25
#define COMP_REG_KERNEL_RESERVED_1	26
#define COMP_REG_KERNEL_RESERVED_2	27
#define COMP_REG_C_GLOBALS		28
#define COMP_REG_C_STACK		29
#define COMP_REG_LINKAGE		31

/* Interrupt/GC polling. */

/* Skip over this many BYTES to bypass the GC check code (ordinary
procedures and continuations differ from closures) */

#define ENTRY_SKIPPED_CHECK_OFFSET 	8
#define CLOSURE_SKIPPED_CHECK_OFFSET 	32

/*
  The instructions for a normal entry should be something like

  SLT	$at,FREE,MEMTOP
  BEQ	$at,0,interrupt
  LW	MEMTOP,REG_BLOCK
  
  For a closure

  LUI	$at,TC_CLOSURE		; temp <- closure tag
  AND	31,QUAD,31		; 31 <- mask out top bits
  OR	31,31,$at	        ; 31 <- tagged value
  SW	31,-4(SP)		; push closure
  ADDI  SP,SP,-4
  SLT	$at,FREE,MEMTOP
  BEQ	$at,0,interrupt
  LW	MEMTOP,REG_BLOCK
 */

/* Compiled closures */

/* Manifest closure entry block size.
   Size in bytes of a compiled closure's header excluding the
   TC_MANIFEST_CLOSURE header.

   On the MIPS this is 2 format_words for the format word and gc
   offset words, and 8 more bytes for 2 instructions:

   JAL	destination
   ADDI LINKAGE,LINKAGE,-8
 */

#define COMPILED_CLOSURE_ENTRY_SIZE     12

/* Manifest closure entry destructuring.

   Given the entry point of a closure, extract the `real entry point'
   (the address of the real code of the procedure, ie. one indirection)
   from the closure.
   On the MIPS, the real entry point is constructed from the JAL
   instruction (low 26 bits) and the address of the closure (top 4
   bits).
*/

#define EXTRACT_CLOSURE_ENTRY_ADDRESS(real_entry_point, entry_point)	\
{									\
  EXTRACT_ABSOLUTE_ADDRESS(real_entry_point, entry_point);		\
}

/* This is the inverse of EXTRACT_CLOSURE_ENTRY_ADDRESS.
   Given a closure's entry point and a code entry point, store the
   code entry point in the closure.
 */

#define STORE_CLOSURE_ENTRY_ADDRESS(real_entry_point, entry_point)	\
{									\
  STORE_ABSOLUTE_ADDRESS(real_entry_point, entry_point);		\
}

/* Trampolines

   On the MIPS, here's a picture of a trampoline (offset in bytes from
   entry point)

     -12: MANIFEST vector header
     - 8: NON_MARKED header
     - 4: Format word
     - 2: 0x6 (GC Offset to start of block from .+2)
          Note the encoding -- divided by 2, low bit for
          extended distances (see OFFSET_WORD_TO_BYTE_OFFSET)
       0: ADDI  TEMP,SCHEME_TO_INTERFACE,-96
       4: JALR	LINKAGE,TEMP
       8: ADDI	TRAMP_INDEX,0,index
      12: trampoline dependent storage (0 - 3 longwords)

   TRAMPOLINE_ENTRY_SIZE is the size in longwords of the machine
   dependent portion of a trampoline, including the GC and format
   headers.  The code in the trampoline must store an index (used to
   determine which C SCHEME_UTILITY procedure to invoke) in a
   register, jump to "scheme_to_interface" and leave the address of
   the storage following the code in a standard location.

   TRAMPOLINE_BLOCK_TO_ENTRY is the number of longwords from the start
   of a trampoline to the first instruction.  Note that this aligns
   the first instruction to a longword boundary.

   WARNING: make_trampoline in cmpint.c will need to be changed if
   machine instructions must be aligned more strictly than just on
   longword boundaries (e.g. quad word alignment for instructions).

   TRAMPOLINE_STORAGE takes the address of the first instruction in a
   trampoline (not the start of the trampoline block) and returns the
   address of the first storage word in the trampoline.

   STORE_TRAMPOLINE_ENTRY gets the address of the first instruction in
   the trampoline and stores the instructions.  It also receives the
   index of the C SCHEME_UTILITY to be invoked.
*/

#define TRAMPOLINE_ENTRY_SIZE		4
#define TRAMPOLINE_BLOCK_TO_ENTRY	3
#define TRAMPOLINE_STORAGE(tramp)					\
((((SCHEME_OBJECT *) tramp) - TRAMPOLINE_BLOCK_TO_ENTRY) +		\
 (2 + TRAMPOLINE_ENTRY_SIZE)) 

#define SPECIAL_OPCODE	000
#define ADDI_OPCODE	010

#define OP(OPCODE)	(OPCODE << 26)
#define SPECIAL_OP	OP(SPECIAL_OPCODE)
#define ADDI_OP		OP(ADDI_OPCODE)

#define JALR_OP		(SPECIAL_OP | (011))
#define JALR_SRC(n)	((n & 0x1F) << 21)
#define JALR_DST(n)	((n & 0x1F) << 11)
#define JALR(d,s)	(JALR_OP|JALR_SRC(s)|JALR_DST(d))

#define ADDI_SRC(n)	((n & 0x1F) << 21)
#define ADDI_DST(n)	((n & 0x1F) << 16)
#define ADDI_IMMED(n)	(n & 0xFFFF)
#define ADDI(d,s,imm)	(ADDI_OP|ADDI_SRC(s)|ADDI_DST(d)|ADDI_IMMED(imm))

#define STORE_TRAMPOLINE_ENTRY(entry_address, index)			\
{ unsigned long *PC;							\
  PC = ((unsigned long *) (entry_address));				\
  *PC++ = ADDI(COMP_REG_TEMPORARY, COMP_REG_SCHEME_TO_INTERFACE, -96);	\
  *PC++ = JALR(COMP_REG_LINKAGE, COMP_REG_TEMPORARY);			\
  *PC = ADDI(COMP_REG_TRAMP_INDEX, 0, (4*index));			\
  /* assumes index fits in 16 bits */					\
}

/* Execute cache entries.

   Execute cache entry size size in longwords.  The cache itself
   contains both the number of arguments provided by the caller and
   code to jump to the destination address.  Before linkage, the cache
   contains the callee's name instead of the jump code.

   On MIPS: 4 instructions, the last being a NO-OP (ADDI with
   destination 0) containing a fixnum representing the number of
   arguments in the lower 16 bits.
 */

#define EXECUTE_CACHE_ENTRY_SIZE        2

/* Execute cache destructuring. */

/* Given a target location and the address of the first word of an
   execute cache entry, extract from the cache cell the number of
   arguments supplied by the caller and store it in target. */

/* For the MIPS (little endian), addresses in bytes from the start of
   the cache:

   Before linking
     +0: TC_SYMBOL || symbol address
     +4: number of supplied arguments, +1
     +6: TC_FIXNUM || 0

   After linking
     +0: JAL	destination
     +4: (unchanged)
     +6: ADDI 0, arg count
*/

#define EXTRACT_EXECUTE_CACHE_ARITY(target, address)			\
{ (target) = (long) (((unsigned short *) (address))[2]);		\
}

#define EXTRACT_EXECUTE_CACHE_SYMBOL(target, address)			\
{									\
  (target) = (* (((SCHEME_OBJECT *) (address))));			\
}

/* Extract the target address (not the code to get there) from an
   execute cache cell.
 */

#define EXTRACT_EXECUTE_CACHE_ADDRESS(target, address)			\
{									\
  EXTRACT_ABSOLUTE_ADDRESS(target, address);				\
}

/* This is the inverse of EXTRACT_EXECUTE_CACHE_ADDRESS. */

#define STORE_EXECUTE_CACHE_ADDRESS(address, entry)			\
{									\
  STORE_ABSOLUTE_ADDRESS(entry, address);				\
}

/* This stores the fixed part of the instructions leaving the
   destination address and the number of arguments intact.  These are
   split apart so the GC can call EXTRACT/STORE...ADDRESS but it does
   NOT need to store the instructions back.  On some architectures the
   instructions may change due to GC and then STORE_EXECUTE_CACHE_CODE
   should become a no-op and all of the work is done by
   STORE_EXECUTE_CACHE_ADDRESS instead.
 */

#define STORE_EXECUTE_CACHE_CODE(address)				\
{  char *opcode_addr = ((char *) (address))+7;				\
   *opcode_addr = ADDI_OPCODE<<2;					\
}

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

#define FORMAT_WORD_HIGH_BYTE(word)                                     \
  (SIGN_EXTEND_FIELD((((unsigned long) (word)) >> 8), (USHORT_SIZE - 8)))

#define COMPILED_ENTRY_FORMAT_HIGH(addr)                                \
  (FORMAT_WORD_HIGH_BYTE(COMPILED_ENTRY_FORMAT_WORD(addr)))

#define COMPILED_ENTRY_FORMAT_LOW(addr)                                 \
  (FORMAT_WORD_LOW_BYTE(COMPILED_ENTRY_FORMAT_WORD(addr)))

#define FORMAT_BYTE_FRAMEMAX            0x7f

#define COMPILED_ENTRY_MAXIMUM_ARITY    COMPILED_ENTRY_FORMAT_LOW
#define COMPILED_ENTRY_MINIMUM_ARITY    COMPILED_ENTRY_FORMAT_HIGH

#endif /* CMPINT2_H_INCLUDED */
