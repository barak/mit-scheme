/* -*-C-*-

Copyright (c) 1989 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpintmd/hppa.h,v 1.2 1989/11/27 16:12:48 jinx Exp $
 *
 * Compiled code interface macros.
 *
 * See cmpint.txt for a description of these fields.
 *
 * Specialized for the HP Precision Architecture (Spectrum)
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

#define COMPILER_PROCESSOR_TYPE			COMPILER_SPECTRUM_TYPE

/* Size of an assembly language hook if different from the executable
   portion of an execute cache.
   Not needed for PA.
   define COMPILER_HOOK_SIZE			17 
*/

/* Size (in long words) of the contents of a floating point register if
   different from a double.  For example, an MC68881 saves registers
   in 96 bit (3 longword) blocks.
   Default is fine for PA.
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
   On the PA the absolute address is "smeared out" over two
   instructions, an LDIL and a BLE instruction.

   Note: The following does not do a full decoding of the BLE instruction.
   It assumes that the overlap bits with the LDIL instruction are all
   zeroed.  Thus only 9 bits of w2 are needed (the rest, including
   w, w1, and the top and bottom bits of w2) are zero.
 */

/* This does the full decoding, unnecessary */

#if 0
#define EXTRACT_ABSOLUTE_ADDRESS(target, address)			\
{									\
  unsigned long ldil_inst, ble_inst, w, w1, w2, offset;			\
									\
  ldil_inst = ((long *) (address))[0];					\
  ble_inst = ((long *) (address))[1];					\
									\
  w = (ble_inst & 1);							\
  w1 = ((ble_inst >> 16) & ((1 << 6) - 1));				\
  w2 = ((ble_inst >> 2) & ((1 << 12) - 1));				\
									\
  offset = ((w << 16) | (w1 << 11) |					\
	    ((w2 & 1) << 10) | (w2 >> 1));				\
  if (w != 0)								\
  {									\
    offset |= (((1 << 16) - 1) << 17);					\
  }									\
									\
  ((long) (target)) =							\
    (((long) ((ldil_inst & ((1 << 22) - 1)) << 11)) +			\
     ((long) (offset << 2)));						\
}
#endif

/* This does the partial decoding needed. */

#define EXTRACT_ABSOLUTE_ADDRESS(target, address)			\
{									\
  unsigned long ldil_inst, ble_inst, offset;				\
									\
  ldil_inst = ((long *) (address))[0];					\
  ble_inst = ((long *) (address))[1];					\
									\
  offset = ((ble_inst >> 3) & ((1 << 10) - 1));				\
									\
  ((long) (target)) =							\
    (((ldil_inst & ((1 << 22) - 1)) << 11) +				\
     (offset << 2));							\
}

#define STORE_ABSOLUTE_ADDRESS(absadd, address, nullify_p)		\
{									\
  unsigned long actual_address, offset;					\
									\
  actual_address = ((long) (real_entry_point));				\
									\
  /* LDIL	L'actual_address,26 */					\
									\
  ((unsigned long *) (entry_point))[0] =				\
    ((0x8 << 26) | (26 << 21) | (actual_address >> 11));		\
									\
  offset = ((actual_address & ((1 << 12) - 1)) >> 2);			\
									\
  /* BLE	R'actual_address(5,26)					\
     The following instruction is nullified if nullify_p is true.	\
     The w and w1 fields are 0, and so are the top and bottom bits	\
     of w2.								\
   */									\
									\
  ((unsigned long *) (entry_point))[1] =				\
    ((0x39 << 26) | (26 << 21) | (5 << 13) | ((offset << 1) << 2) |	\
     ((nullify_p) ? 2 : 0));						\
}

/* Interrupt/GC polling. */

/* Skip over this many BYTES to bypass the GC check code (ordinary
procedures and continuations differ from closured procedures) */

#define ENTRY_SKIPPED_CHECK_OFFSET 	4
#define CLOSURE_SKIPPED_CHECK_OFFSET 	12

/*
  The instructions for a normal entry should be something like

  COMBT,>=,N	Rfree,Rmemtop,interrupt

  For a closure

  DEPI		tc_closure>>1,4,5,31
  STWS,MB	31,-4(0,Rstack)
  COMBT,>=,N	Rfree,Rmemtop,interrupt

  Where interrupt must be downstream so that the following instruction
  is nullified on a failing forward branch.

  After those instructions, we must have

  LDW		0(0,Regs),Rmemtop

  until the interrupt handler is changed to do this at interrupt time.
  While the nullification is currently spurious, it will not be later.

 */

/* Compiled closures */

/* Manifest closure entry block size.
   Size in bytes of a compiled closure's header excluding the
   TC_MANIFEST_CLOSURE header.

   On the PA this is 2 format_words for the format word and gc
   offset words, and 12 more bytes for 3 instructions:

   LDIL		L'target,26
   BLE		R'target(5,26)
   SUBI		12,31,31
 */

#define COMPILED_CLOSURE_ENTRY_SIZE     16

/* Manifest closure entry destructuring.

   Given the entry point of a closure, extract the `real entry point'
   (the address of the real code of the procedure, ie. one indirection)
   from the closure.
   On the PA, the real entry point is "smeared out" over the LDIL and
   the BLE instructions.
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
  STORE_ABSOLUTE_ADDRESS(real_entry_point, entry_point, false);		\
}

/* Trampolines

   On the PA, here's a picture of a trampoline (offset in bytes from
   entry point)

     -12: MANIFEST vector header
     - 8: NON_MARKED header
     - 4: Format word
     - 2: 0xFFF4 (GC Offset to start of block from .+2)
       0: BLE	0(4,3)		; call trampoline_to_interface
       4: LDI	index,28
       8: trampoline dependent storage (0 - 3 longwords)

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

#define TRAMPOLINE_ENTRY_SIZE		3
#define TRAMPOLINE_BLOCK_TO_ENTRY	3
#define TRAMPOLINE_STORAGE(tramp)					\
((((SCHEME_OBJECT *) tramp) - TRAMPOLINE_BLOCK_TO_ENTRY) +		\
 (2 + TRAMPOLINE_ENTRY_SIZE)) 

#define STORE_TRAMPOLINE_ENTRY(entry_address, index)			\
{									\
  unsigned long *PC;							\
									\
  PC = ((unsigned long *) (entry_address));				\
									\
  /*	BLE	0(4,3) */						\
									\
  *PC++ = ((unsigned long) 0xe4608000);					\
									\
  /*	LDO	index(0),28 */						\
									\
  *PC++ = (((unsigned long) 0x341c0000) + ((unsigned long) (index)));	\
}

/* Execute cache entries.

   Execute cache entry size size in longwords.  The cache itself
   contains both the number of arguments provided by the caller and
   code to jump to the destination address.  Before linkage, the cache
   contains the callee's name instead of the jump code.

   On PA: 2 instructions, and a fixnum representing the number of arguments.
 */

#define EXECUTE_CACHE_ENTRY_SIZE        3

/* Execute cache destructuring. */

/* Given a target location and the address of the first word of an
   execute cache entry, extract from the cache cell the number of
   arguments supplied by the caller and store it in target. */

/* For the HPPA, addresses in bytes from the start of the cache:
   Before linking
     +0: TC_SYMBOL || symbol address
     +4: #F
     +8: TC_FIXNUM || 0
    +10: number of supplied arguments, +1
   After linking
     +0: LDIL	L'target,26
     +4: BLE,n	R'target(5,26)
     +8: (unchanged)
    +10: (unchanged)
*/

#define EXTRACT_EXECUTE_CACHE_ARITY(target, address)			\
{									\
  (target) = ((unsigned short *) (address))[5];				\
}

#define EXTRACT_EXECUTE_CACHE_SYMBOL(target, address)			\
{									\
  ((long) (target)) = (* (((long *) (address))));			\
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
  STORE_ABSOLUTE_ADDRESS(entry, address, true);				\
}

/* This stores the fixed part of the instructions leaving the
   destination address and the number of arguments intact.  These are
   split apart so the GC can call EXTRACT/STORE...ADDRESS but it does
   NOT need to store the instructions back.  On some architectures the
   instructions may change due to GC and then STORE_EXECUTE_CACHE_CODE
   should become a no-op and all of the work is done by
   STORE_EXECUTE_CACHE_ADDRESS instead.
   On PA this is a NOP.
 */

#define STORE_EXECUTE_CACHE_CODE(address)				\
{									\
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

#define ASM_REGISTER_BLOCK	/* Registers temporarily allocated and 
				   laid out in cmpaux-mc68k.m4 */
#define ASM_RESET_HOOK()		\
do { extern void asm_reset_hook();	\
     asm_reset_hook();			\
   } while (0)

#endif /* CMPINT2_H_INCLUDED */
