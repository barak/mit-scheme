/* -*-C-*-

Copyright (c) 1989-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
 *
 * Compiled code interface macros.
 *
 * See cmpint.txt for a description of these fields.
 *
 * Specialized for the HP Precision Architecture (Spectrum)
 */

#ifndef CMPINTMD_H_INCLUDED
#define CMPINTMD_H_INCLUDED

#include "cmptype.h"
#include "hppacach.h"

/* Machine parameters to be set by the user. */

/* Until cmpaux-hppa.m4 is updated. */
#define CMPINT_USE_STRUCS

/* Processor type.  Choose a number from the above list, or allocate your own. */

#define COMPILER_PROCESSOR_TYPE			COMPILER_SPECTRUM_TYPE

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

/* C function pointers are pairs of instruction addreses and data segment
   pointers.  We don't want that for the assembly language entry points.
 */

#define C_FUNC_PTR_IS_CLOSURE

#ifndef C_FUNC_PTR_IS_CLOSURE
#  define interface_to_C ep_interface_to_C
#  define interface_to_scheme ep_interface_to_scheme
#  define interface_to_scheme_new ep_interface_to_scheme_new
#endif

/* Utilities for manipulating absolute subroutine calls.
   On the PA the absolute address is "smeared out" over two
   instructions, an LDIL and a BLE instruction.
 */

extern unsigned long
  EXFUN (hppa_extract_absolute_address, (unsigned long *));

extern void
  EXFUN (hppa_store_absolute_address,
	 (unsigned long *, unsigned long, unsigned long));

#define EXTRACT_ABSOLUTE_ADDRESS(target, address)			\
{									\
  (target) =								\
    ((SCHEME_OBJECT)							\
     (hppa_extract_absolute_address ((unsigned long *) (address))));	\
}

#define STORE_ABSOLUTE_ADDRESS(entry_point, address, nullify_p)		\
{									\
  hppa_store_absolute_address (((unsigned long *) (address)),		\
			       ((unsigned long) (entry_point)),		\
			       ((unsigned long) (nullify_p)));		\
}

/* OLD Interrupt/GC polling. */

/* OLD The length of the GC recovery code that precedes an entry.
   OLD On the HP-PA a "ble, ldi" instruction sequence.
 */

/* OLD #define ENTRY_PREFIX_LENGTH		8*/

#define ENTRY_PREFIX_LENGTH		0

/*

THIS IS OLD:

  The instructions for a normal entry should be something like

  COMBT,>=,N	Rfree,Rmemtop,interrupt
  LDW		0(0,Regs),Rmemtop

  For a closure

  DEPI		tc_closure>>1,4,5,25		; set type code
  STWM		25,-4(0,Rstack)			; push on stack
  COMB,>=	Rfree,Rmemtop,interrupt		; GC/interrupt check
  LDW		0(0,Regs),Rmemtop		; Recache memtop

  Notes:

  The LDW can be eliminated once the C interrupt handler is changed to
  update Rmemtop directly.  At that point, the instruction following the
  COMB instruction will have to be nullified whenever the interrupt
  branch is processed.

THIS IS NEW:

  The instructions for a normal entry should be something like

  entry_label
	COMBT,>=	rs_free,rs_memtop,interrupt_label
	LDW		REGBLOCK_MEMTOP(0,rs_regblock),rs_memtop
	...
  interrupt_label
	BLE	new_procedure_interrupt_hook(4,rs_ble)
	LDI	#args,1
	WORD    interrupt_label-entry_label-3    ; offset


  Continuations are similar, replacing new_procedure_interrupt_hook
  with new_continuation_interrupt_hook.  Closures are similar except
  that the entry point is in rs_closure so there is no need for an
  offest word, and the hook is new_closure_interrupt_hook.

 */

/* Compiled closures */

/* Manifest closure entry block size.
   Size in bytes of a compiled closure's header excluding the
   TC_MANIFEST_CLOSURE header.

   On the PA this is 2 format_words for the format word and gc
   offset words, and 12 more bytes for 3 instructions:

   LDIL		L'target,26
   BLE		R'target(5,26)
   ADDI		-15,31,25		; handle privilege bits
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
  EXTRACT_ABSOLUTE_ADDRESS (real_entry_point, entry_point);		\
}

/* This is the inverse of EXTRACT_CLOSURE_ENTRY_ADDRESS.
   Given a closure's entry point and a code entry point, store the
   code entry point in the closure.
 */

#define STORE_CLOSURE_ENTRY_ADDRESS(real_entry_point, entry_point)	\
{									\
  STORE_ABSOLUTE_ADDRESS (real_entry_point, entry_point, false);	\
}

/* Trampolines

   Here's a picture of a trampoline on the PA (offset in bytes from
   entry point)

     -12: MANIFEST vector header
     - 8: NON_MARKED header
     - 4: Format word
     - 2: 0xC (GC Offset to start of block from .+2)
       0: BLE	-16(4,3)		; call trampoline_to_interface
       4: LDI	index,28
       8: trampoline dependent storage (currently 1 - 4 longwords)

   TRAMPOLINE_ENTRY_SIZE is the size in longwords of the machine
   dependent portion of a trampoline, including the GC and format
   headers.  The code in the trampoline must store an index (used to
   determine which C SCHEME_UTILITY procedure to invoke) in a
   register, jump to "scheme_to_interface" and leave the address of
   the storage following the code in a standard location.

   TRAMPOLINE_ENTRY_POINT returns the address of the entry point of a
   trampoline when given the address of the word containing
   the manifest vector header.  According to the above picture,
   it would add 12 bytes to its argument.

   TRAMPOLINE_STORAGE takes the address of the first instruction in a
   trampoline (not the start of the trampoline block) and returns the
   address of the first storage word in the trampoline.

   STORE_TRAMPOLINE_ENTRY gets the address of the first instruction in
   the trampoline and stores the instructions.  It also receives the
   index of the C SCHEME_UTILITY to be invoked.

   Note: this flushes both caches because the words may fall in a cache
   line that already has an association in the i-cache because a different
   trampoline or a closure are in it.

   Note: The first storage word ALWAYS has the number of actual
   operands that must be stored on the stack when the trampoline is
   invoked.  This is fixed and known for each kind of trampoline.  It
   is required so that the trampoline can save these registers on the
   stack before invoking the (possibly C written) handler.
*/

#define TRAMPOLINE_ENTRY_SIZE		3
#define TRAMPOLINE_BLOCK_TO_ENTRY	3 /* longwords from MNV to BLE */

#define TRAMPOLINE_ENTRY_POINT(tramp_block)				\
  (((SCHEME_OBJECT *) (tramp_block)) + TRAMPOLINE_BLOCK_TO_ENTRY)

#define TRAMPOLINE_STORAGE(tramp_entry)					\
  ((((SCHEME_OBJECT *) (tramp_entry)) - TRAMPOLINE_BLOCK_TO_ENTRY) +	\
   (2 + TRAMPOLINE_ENTRY_SIZE)) 

#define STORE_TRAMPOLINE_ENTRY(entry_address, index) do			\
{									\
  extern void								\
    EXFUN (cache_flush_region, (PTR, long, unsigned int));		\
									\
  unsigned long *PC;							\
									\
  PC = ((unsigned long *) (entry_address));				\
									\
  /*	was: BLE	4(4,3)		*/				\
  /* *PC = ((unsigned long) 0xe4602008);	*/			\
									\
  /*	BLE	-16(4,3)	*/					\
									\
  *PC = ((unsigned long) 0xe47f3fe5);					\
									\
  /*	LDO	index(0),28	*/					\
  /*    This assumes that index is >= 0. */				\
									\
  *(PC + 1) = (((unsigned long) 0x341c0000) +				\
	       (((unsigned long) (index)) << 1));			\
  cache_flush_region (PC, (TRAMPOLINE_ENTRY_SIZE - 1),			\
		      (I_CACHE | D_CACHE));				\
} while (0)

/* Execute cache entries.

   Execute cache entry size size in longwords.  The cache itself
   contains both the number of arguments provided by the caller and
   code to jump to the destination address.  Before linkage, the cache
   contains the callee's name instead of the jump code.

   On PA: 2 instructions, and a fixnum representing the number of arguments.
 */

#define EXECUTE_CACHE_ENTRY_SIZE        3

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

   Important:

     Currently the code below unconditionally nullifies the delay-slot
     instruction for the BLE instruction.  This is wasteful and
     unnecessary.  An EXECUTE_CACHE_ENTRY could be one word longer to
     accomodate a delay-slot instruction, and the linker could do the
     following:

     - If the target instruction is not a branch instruction, use 4 +
     the address of the target instruction, and copy the target
     instruction to the delay slot.  Note that branch instructions are
     those with opcodes (6 bits) in the range #b1xy0zw, for any bit
     value for x, y, z, w.

     - If the target instruction is the COMBT instruction of an
     interrupt/gc check, use 4 + the address of the target
     instruction, and insert a similar COMBT instruction in the delay
     slot.  This COMBT instruction would then branch to an instruction
     shared by all the cache cells in the same block.  This shared
     instruction would be a BE instruction used to jump to an assembly
     language handler.  This handler would recover the target address
     from the link address left in register 31 by the BLE instruction
     in the execute cache cell, and use it to compute the address of
     and branch to the interrupt code for the entry.

     - Otherwise use the address of the target instruction and insert
     a NOP in the delay slot.
*/

/* Execute cache destructuring. */

/* Given a target location and the address of the first word of an
   execute cache entry, extract from the cache cell the number of
   arguments supplied by the caller and store it in target.
 */

#define EXTRACT_EXECUTE_CACHE_ARITY(target, address)			\
{									\
  (target) = ((long) (* (((unsigned short *) (address)) + 5)));		\
}

/* Given a target location and the address of the first word of an
   execute cache entry, extract from the cache cell the name
   of the variable whose value is being invoked.
   This is valid only before linking.
 */

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

#define STORE_EXECUTE_CACHE_CODE(address) do				\
{									\
} while (0)

/* This is supposed to flush the Scheme portion of the I-cache.
   It flushes the entire I-cache instead, since it is easier.
   It is used after a GC or disk-restore.
   It's needed because the GC has moved code around, and closures
   and execute cache cells have absolute addresses that the
   processor might have old copies of.
 */

#define FLUSH_I_CACHE() do						\
{									\
  extern void								\
    EXFUN (flush_i_cache, (void));					\
									\
  flush_i_cache ();							\
} while (0)

/* This flushes a region of the I-cache.
   It is used after updating an execute cache while running.
   Not needed during GC because FLUSH_I_CACHE will be used.
 */   

#define FLUSH_I_CACHE_REGION(address, nwords) do			\
{									\
  extern void								\
    EXFUN (cache_flush_region, (PTR, long, unsigned int));		\
									\
  cache_flush_region (((PTR) (address)), ((long) (nwords)),		\
		      (D_CACHE | I_CACHE));				\
} while (0)

/* This pushes a region of the D-cache back to memory.
   It is (typically) used after loading (and relocating) a piece of code
   into memory.
 */   

#define PUSH_D_CACHE_REGION(address, nwords) do				\
{									\
  extern void								\
    EXFUN (push_d_cache_region, (PTR, unsigned long));			\
									\
  push_d_cache_region (((PTR) (address)),				\
		       ((unsigned long) (nwords)));			\
} while (0)

extern void EXFUN (hppa_update_primitive_table, (int, int));
extern Boolean EXFUN (hppa_grow_primitive_table, (int));

#define UPDATE_PRIMITIVE_TABLE_HOOK hppa_update_primitive_table
#define GROW_PRIMITIVE_TABLE_HOOK hppa_grow_primitive_table

/* This is not completely true.  Some models (eg. 850) have combined caches,
   but we have to assume the worst.
 */

#define SPLIT_CACHES

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

#ifndef FORMAT_BYTE_CLOSURE
#define FORMAT_BYTE_CLOSURE			0xFA
#endif

#ifndef FORMAT_WORD_CLOSURE
#define FORMAT_WORD_CLOSURE	(MAKE_FORMAT_WORD (0xFF, FORMAT_BYTE_CLOSURE))
#endif

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

#define COMPILED_ENTRY_MAXIMUM_ARITY    COMPILED_ENTRY_FORMAT_LOW
#define COMPILED_ENTRY_MINIMUM_ARITY    COMPILED_ENTRY_FORMAT_HIGH

#ifdef IN_CMPINT_C

/* Definitions of the utility procedures.
   Procedure calls of leaf procedures on the HPPA are pretty fast,
   so there is no reason not to do this out of line.
   In this way compiled code can use them too.
 */

union ldil_inst
{
  unsigned long inst;
  struct
  {
    unsigned opcode	: 6;
    unsigned base	: 5;
    unsigned D		: 5;
    unsigned C		: 2;
    unsigned E		: 2;
    unsigned B		: 11;
    unsigned A		: 1;
  } fields;
};

union branch_inst
{
  unsigned long inst;
  struct
  {
    unsigned opcode	: 6;
    unsigned t_or_b	: 5;
    unsigned x_or_w1	: 5;
    unsigned s		: 3;
    unsigned w2b	: 10;
    unsigned w2a	: 1;
    unsigned n		: 1;
    unsigned w0		: 1;
  } fields;
};

union short_pointer
{
  unsigned long address;
  struct
  {
    unsigned A		: 1;
    unsigned B		: 11;
    unsigned C		: 2;
    unsigned D		: 5;
    unsigned w2a	: 1;
    unsigned w2b	: 10;
    unsigned pad	: 2;
  } fields;
};

union assemble_17_u
{
  long value;
  struct
  {
    int sign_pad	: 13;
    unsigned w0		: 1;
    unsigned w1		: 5;
    unsigned w2a	: 1;
    unsigned w2b	: 10;
    unsigned pad	: 2;
  } fields;
};

union assemble_12_u
{
  long value;
  struct
  {
    int sign_pad	: 18;
    unsigned w0		: 1;
    unsigned w2a	: 1;
    unsigned w2b	: 10;
    unsigned pad	: 2;
  } fields;
};

long
DEFUN (assemble_17, (inst), union branch_inst inst)
{
  union assemble_17_u off;

  off.fields.pad = 0;
  off.fields.w2b = inst.fields.w2b;
  off.fields.w2a = inst.fields.w2a;
  off.fields.w1  = inst.fields.x_or_w1;
  off.fields.w0  = inst.fields.w0;
  off.fields.sign_pad = ((inst.fields.w0 == 0) ? 0 : -1);
  return (off.value);
}

long
DEFUN (assemble_12, (inst), union branch_inst inst)
{
  union assemble_12_u off;

  off.fields.pad = 0;
  off.fields.w2b = inst.fields.w2b;
  off.fields.w2a = inst.fields.w2a;
  off.fields.w0  = inst.fields.w0;
  off.fields.sign_pad = ((inst.fields.w0 == 0) ? 0 : -1);
  return (off.value);
}

static unsigned long hppa_closure_hook = 0;

static unsigned long
DEFUN (C_closure_entry_point, (closure), unsigned long C_closure)
{
  if ((C_closure & 0x3) != 0x2)
    return (C_closure);
  else
  {
    long offset;
    extern int etext;
    unsigned long entry_point;
    char * blp = (* ((char **) (C_closure - 2)));

    blp = ((char *) (((unsigned long) blp) & ~3));
    offset = (assemble_17 (* ((union branch_inst *) blp)));
    entry_point = ((unsigned long) ((blp + 8) + offset));
    return ((entry_point < ((unsigned long) &etext))
	    ? entry_point
	    : hppa_closure_hook);
  }
}

#define HAVE_BKPT_SUPPORT

static unsigned short branch_opcodes[] =
{
  0x20, 0x21, 0x22, 0x23, 0x28, 0x29, 0x2a, 0x2b,
  0x30, 0x31, 0x32, 0x33, 0x38, 0x39, 0x3a
};

static Boolean
  branch_opcode_table[64];

static unsigned long
  bkpt_instruction,
  closure_bkpt_instruction,
  closure_entry_bkpt_instruction,
  * bkpt_normal_proceed_thunk,
  * bkpt_plus_proceed_thunk,
  * bkpt_minus_proceed_thunk_start,
  * bkpt_minus_proceed_thunk,
  * bkpt_closure_proceed_thunk,
  * bkpt_closure_proceed_thunk_end;

#define FAHRENHEIT 451

static void
DEFUN_VOID (bkpt_init)
{
  int i;
  union branch_inst instr;
  extern void EXFUN (bkpt_normal_proceed, (void));
  extern void EXFUN (bkpt_plus_proceed, (void));
  extern void EXFUN (bkpt_minus_proceed_start, (void));
  extern void EXFUN (bkpt_minus_proceed, (void));
  extern void EXFUN (bkpt_closure_proceed, (void));
  extern void EXFUN (bkpt_closure_proceed_end, (void));

  for (i = 0;
       i < ((sizeof (branch_opcode_table)) / (sizeof (Boolean)));
       i++)
    branch_opcode_table[i] = FALSE;

  for (i = 0;
       i < ((sizeof (branch_opcodes)) / (sizeof (short)));
       i++)
    branch_opcode_table[branch_opcodes[i]] = TRUE;

  instr.fields.opcode	= 0x39;	/* BLE opcode */
  instr.fields.t_or_b	= 03;	/* scheme_to_interface_ble */
  instr.fields.n      	= 01;	/* nullify */
  instr.fields.s      	= 01;	/* C code space, rotated illegibly */
  instr.fields.w0     	= 00;
  instr.fields.x_or_w1	= 00;
  instr.fields.w2a	= 00;
  instr.fields.w2b	= ((FAHRENHEIT + 1) >> 2);

  bkpt_instruction = instr.inst;

  instr.fields.w2b	= ((FAHRENHEIT + 33) >> 2);
  closure_entry_bkpt_instruction = instr.inst;

  instr.fields.opcode	= 0x38;	/* BE opcode */
  instr.fields.w2b	= ((FAHRENHEIT + 9) >> 2);
  closure_bkpt_instruction = instr.inst;

  bkpt_normal_proceed_thunk
    = ((unsigned long *)
       (C_closure_entry_point ((unsigned long) bkpt_normal_proceed)));
  bkpt_plus_proceed_thunk
    = ((unsigned long *)
       (C_closure_entry_point ((unsigned long) bkpt_plus_proceed)));
  bkpt_minus_proceed_thunk_start
    = ((unsigned long *)
       (C_closure_entry_point ((unsigned long) bkpt_minus_proceed_start)));
  bkpt_minus_proceed_thunk
    = ((unsigned long *)
       (C_closure_entry_point ((unsigned long) bkpt_minus_proceed)));
  bkpt_closure_proceed_thunk
    = ((unsigned long *)
       (C_closure_entry_point ((unsigned long) bkpt_closure_proceed)));
  bkpt_closure_proceed_thunk_end
    = ((unsigned long *)
       (C_closure_entry_point ((unsigned long) bkpt_closure_proceed_end)));
  return;
}

#define BKPT_KIND_CLOSURE		0
#define BKPT_KIND_NORMAL		1
#define BKPT_KIND_PC_REL_BRANCH		2
#define BKPT_KIND_BL_INST		3
#define BKPT_KIND_BLE_INST		4
#define BKPT_KIND_CLOSURE_ENTRY		5

extern void EXFUN (cache_flush_region, (PTR, long, unsigned int));

static SCHEME_OBJECT
DEFUN (alloc_bkpt_handle, (kind, first_instr, entry_point),
       int kind AND unsigned long first_instr AND PTR entry_point)
{
  SCHEME_OBJECT * handle;
  Primitive_GC_If_Needed (5);
  handle = Free;
  Free += 5;

  handle[0] = (MAKE_OBJECT (TC_MANIFEST_VECTOR, 4));
  handle[1] = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 2));
  handle[2] = ((SCHEME_OBJECT) (FIXNUM_ZERO + kind));
  handle[3] = ((SCHEME_OBJECT) first_instr);
  handle[4] = (ENTRY_TO_OBJECT (entry_point));
  
  return (MAKE_POINTER_OBJECT (TC_NON_MARKED_VECTOR, handle));
}

SCHEME_OBJECT
DEFUN (bkpt_install, (entry_point), PTR entry_point)
{
  unsigned long kind;
  SCHEME_OBJECT handle;
  unsigned long first_instr = (* ((unsigned long *) entry_point));
  unsigned short opcode = ((first_instr >> 26) & 0x3f);
  unsigned long new_instr = bkpt_instruction;

  if ((COMPILED_ENTRY_FORMAT_WORD (entry_point)) == FORMAT_WORD_CLOSURE)
  {
    /* This assumes that the first instruction is normal */ 
    kind = BKPT_KIND_CLOSURE_ENTRY;
    new_instr = closure_entry_bkpt_instruction;
  }
  else if ((! (branch_opcode_table[opcode])) || (opcode == 0x38))
    kind = BKPT_KIND_NORMAL;	/* BE instr included */
  else if (opcode == 0x39)
#if 0
    kind = BKPT_KIND_BLE_INST;
#else /* for now */
    return (SHARP_F);
#endif
  else if (opcode != 0x3a)
  {
    unsigned long second_instr = (* (((unsigned long *) entry_point) + 1));
    unsigned long second_opcode = ((second_instr >> 26) & 0x3f);

    /* We can't handle breakpoints to a branch instruction
       with another branch instruction in its delay slot.
       This could be nullification sensitive, but not
       currently worthwhile.
     */

    if (branch_opcode_table[second_opcode])
      return (SHARP_F);

    kind = BKPT_KIND_PC_REL_BRANCH;
  }

  else
  {
    union branch_inst finstr;

    finstr.inst = first_instr;
    switch (finstr.fields.s)	/* minor opcode */
    {
      case 0:			/* BL instruction */
#if 0
        kind = BKPT_KIND_BL_INST;
	break;
#endif /* for now, fall through */

      case 1:			/* GATE instruction */
      case 2:			/* BLR  instruction */
      default:			/* ?? */
	return (SHARP_F);

      case 6:
	kind = BKPT_KIND_NORMAL;
	break;
    }
  }

  handle = (alloc_bkpt_handle (kind, first_instr, entry_point));

  (* ((unsigned long *) entry_point)) = new_instr;
  cache_flush_region (((PTR) entry_point), 1, (D_CACHE | I_CACHE));

  return (handle);
}

SCHEME_OBJECT
DEFUN (bkpt_closure_install, (entry_point), PTR entry_point)
{
  unsigned long * instrs = ((unsigned long *) entry_point);
  SCHEME_OBJECT handle;

  handle = (alloc_bkpt_handle (BKPT_KIND_CLOSURE, instrs[2], entry_point));
  instrs[2] = closure_bkpt_instruction;
  cache_flush_region (((PTR) &instrs[2]), 1, (D_CACHE | I_CACHE));
  return (handle);
}

void
DEFUN (bkpt_remove, (entry_point, handle),
       PTR entry_point AND SCHEME_OBJECT handle)
{
  int offset;
  unsigned long * instrs = ((unsigned long *) entry_point);

  if ((instrs[0] == bkpt_instruction)
      || (instrs[0] == closure_entry_bkpt_instruction))
    offset = 0;
  else if (instrs[2] == closure_bkpt_instruction)
    offset = 2;
  else
    error_external_return ();

  instrs[offset] = ((unsigned long) (FAST_MEMORY_REF (handle, 3)));
  cache_flush_region (((PTR) &instrs[offset]), 1, (D_CACHE | I_CACHE));
  return;
}

Boolean
DEFUN (bkpt_p, (entry_point), PTR entry_point)
{
  unsigned long * instrs = ((unsigned long *) entry_point);

  return ((instrs[0] == bkpt_instruction)
	  || (instrs[0] == closure_entry_bkpt_instruction)
	  || (instrs[2] == closure_bkpt_instruction));
}

Boolean
DEFUN (do_bkpt_proceed, (value), unsigned long * value)
{
  SCHEME_OBJECT ep = (STACK_POP ());
  SCHEME_OBJECT handle = (STACK_POP ());
  SCHEME_OBJECT state = (STACK_POP ());

  STACK_POP ();			/* Pop duplicate entry point. */

  switch (OBJECT_DATUM (FAST_MEMORY_REF (handle, 2)))
  {
    case BKPT_KIND_CLOSURE:
    {
      int i, len;
      unsigned long * buffer = ((unsigned long *) Constant_Top);
      unsigned long * clos_entry
	= (OBJECT_ADDRESS (FAST_MEMORY_REF (handle, 4)));
      SCHEME_OBJECT real_entry_point;

      EXTRACT_CLOSURE_ENTRY_ADDRESS (real_entry_point, clos_entry);
      len = (bkpt_closure_proceed_thunk_end - bkpt_closure_proceed_thunk);
      for (i = 0; i < (len - 2); i++)
	buffer[i] = bkpt_closure_proceed_thunk[i];
      cache_flush_region (((PTR) buffer), (len - 2), (D_CACHE | I_CACHE));

      buffer[len - 2] = ((unsigned long) clos_entry);
      buffer[len - 1] = real_entry_point;

      Val = SHARP_F;
      * value = ((unsigned long) buffer);
      return (TRUE);
    }

    case BKPT_KIND_NORMAL:
    {
      int i, len;
      unsigned long * buffer = ((unsigned long *) Constant_Top);

      len = (bkpt_plus_proceed_thunk - bkpt_normal_proceed_thunk);
      for (i = 0; i < (len - 2); i++)
	buffer[i] = bkpt_normal_proceed_thunk[i];
      buffer[len - 2] = ((unsigned long) (FAST_MEMORY_REF (handle, 3)));

      cache_flush_region (((PTR) buffer), (len - 1), (D_CACHE | I_CACHE));
      buffer[len - 1] = (((unsigned long) (OBJECT_ADDRESS (ep))) + 4);

      Val = state;
      * value = ((unsigned long) buffer);
      return (TRUE);
    }

    case BKPT_KIND_CLOSURE_ENTRY:
    {
      STACK_PUSH (state);	/* closure object */
      * value = ((unsigned long) ((OBJECT_ADDRESS (ep)) + 2));
      return (TRUE);
    }

    case BKPT_KIND_BL_INST:
    case BKPT_KIND_BLE_INST:
    default:
      STACK_PUSH (ep);
      * value = ((unsigned long) ERR_EXTERNAL_RETURN);
      return (FALSE);

    case BKPT_KIND_PC_REL_BRANCH:
    {
      long offset;
      int i, len, clobber;
      union branch_inst new, old;
      unsigned long * buffer = ((unsigned long *) Constant_Top);
      unsigned long * instrs = ((unsigned long *) (OBJECT_ADDRESS (ep)));
      unsigned long * block;

      old.inst = ((unsigned long) (FAST_MEMORY_REF (handle, 3)));
      offset = (assemble_12 (old));
      if (offset >= 0)
      {
	block = bkpt_plus_proceed_thunk;
	len = (bkpt_minus_proceed_thunk_start - block);
	clobber = 0;
      }
      else
      {
	block = bkpt_minus_proceed_thunk_start;
	len = (bkpt_closure_proceed_thunk - block);
	clobber = (bkpt_minus_proceed_thunk - block);
      }
      
      for (i = 0; i < (len - 2); i++)
	buffer[i] = block[i];

      new.inst = buffer[clobber];
      old.inst = ((unsigned long) (FAST_MEMORY_REF (handle, 3)));
      old.fields.w2b = new.fields.w2b;
      old.fields.w2a = new.fields.w2a;
      old.fields.w0 = new.fields.w0;
      buffer[clobber] = old.inst;
      buffer[clobber + 1] = instrs[1];
      cache_flush_region (((PTR) buffer), (len - 2), (D_CACHE | I_CACHE));

      buffer[len - 2] = (((unsigned long) instrs) + 8);
      buffer[len - 1] = ((((unsigned long) instrs) + 8)
			 + offset);
      
      Val = state;
      * value = ((unsigned long) &buffer[clobber]);
      return (TRUE);
    }
  }
}

static void
DEFUN (transform_procedure_entries, (len, otable, ntable),
       long len AND PTR * otable AND PTR * ntable)
{
  long counter;
  
  for (counter = 0; counter < len; counter++)
    ntable[counter] =
      ((PTR) (C_closure_entry_point ((unsigned long) (otable [counter]))));
  return;
}       

static PTR *
DEFUN (transform_procedure_table, (table_length, old_table),
       long table_length AND PTR * old_table)
{
  PTR * new_table;

  new_table = ((PTR *) (malloc (table_length * (sizeof (PTR)))));
  if (new_table == ((PTR *) NULL))
  {
    outf_fatal ("transform_procedure_table: malloc (%d) failed.\n",
		(table_length * (sizeof (PTR))));
    exit (1);
  }
  transform_procedure_entries (table_length, old_table, new_table);
  return (new_table);
}

#define UTIL_TABLE_PC_REF(index)					\
  (C_closure_entry_point (UTIL_TABLE_PC_REF_REAL (index)))

#ifdef _BSD4_3
#  include <sys/mman.h>
#  define VM_PROT_SCHEME (PROT_READ | PROT_WRITE | PROT_EXEC)
#endif

void
DEFUN_VOID (change_vm_protection)
{
#if 0
  /* Thought I needed this under _BSD4_3 */

  unsigned long pagesize = (getpagesize ());
  unsigned long heap_start_page;
  unsigned long size;

  heap_start_page = (((unsigned long) Heap) & (pagesize - 1));
  size = (((((unsigned long) Highest_Allocated_Address) + (pagesize - 1))
	   & (pagesize - 1))
	  - heap_start_page);
  if ((mprotect (((caddr_t) heap_start_page), size, VM_PROT_SCHEME))
      == -1)
  {
    perror ("\nchange_vm_protection");
    outf_fatal ("mprotect (0x%lx, 0x%lx, 0x%lx)\n",
		heap_start_page, size, VM_PROT_SCHEME);
    outf_fatal ("ASM_RESET_HOOK: Unable to change VM protection of Heap.\n");
    termination_init_error ();
  }
#endif
  return;
}

#include "option.h"

#ifndef MODELS_FILENAME
#define MODELS_FILENAME "hppacach.mod"
#endif

static struct pdc_cache_dump cache_info;

static void
DEFUN_VOID (flush_i_cache_initialize)
{
  extern char * EXFUN (getenv, (const char *));
  CONST char * models_filename =
    (search_path_for_file (0, MODELS_FILENAME, 1, 1));
  char * model;

  model = (getenv ("MITSCHEME_HPPA_MODEL"));

#ifdef _HPUX
  if (model == ((char *) NULL))
  {
    struct utsname sysinfo;
    if ((uname (&sysinfo)) < 0)
    {
      outf_fatal ("\nflush_i_cache: uname failed.\n");
      goto loser;
    }
    model = &sysinfo.machine[0];
  }
#endif /* _HPUX */
  if (model == ((char *) NULL))
  {
    outf_fatal
      ("\nflush_i_cache: MITSCHEME_HPPA_MODEL not set in environment.\n");
    goto loser;
  }
  {
    int fd = (open (models_filename, O_RDONLY));
    if (fd < 0)
      {
	outf_fatal ("\nflush_i_cache: open (%s) failed.\n",
		    models_filename);
	goto loser;
      }
    while (1)
      {
	int read_result =
	  (read (fd,
		 ((char *) (&cache_info)),
		 (sizeof (struct pdc_cache_dump))));
	if (read_result == 0)
	  {
	    close (fd);
	    break;
	  }
	if (read_result != (sizeof (struct pdc_cache_dump)))
	  {
	    close (fd);
	    outf_fatal ("\nflush_i_cache: read (%s) failed.\n",
			models_filename);
	    goto loser;
	  }
	if ((strcmp (model, (cache_info . hardware))) == 0)
	  {
	    close (fd);
	    return;
	  }
      }
  }
  outf_fatal (
	      "The cache parameters database has no entry for the %s model.\n",
	      model);
  outf_fatal ("Please make an entry in the database;\n");
  outf_fatal ("the installation notes contain instructions for doing so.\n");
 loser:
  outf_fatal ("\nASM_RESET_HOOK: Unable to read cache parameters.\n");
  termination_init_error ();
}

/* This loads the cache information structure for use by flush_i_cache,
   sets the floating point flags correctly, and accommodates the c
   function pointer closure format problems for utilities for HP-UX >= 8.0 .
   It also changes the VM protection of the heap, if necessary.
 */

extern PTR * hppa_utility_table;
extern PTR * hppa_primitive_table;

PTR * hppa_utility_table = ((PTR *) NULL);

static void
DEFUN (hppa_reset_hook, (utility_length, utility_table),
       long utility_length AND PTR * utility_table)
{
  extern void EXFUN (interface_initialize, (void));
  extern void EXFUN (cross_segment_call, (void));

  flush_i_cache_initialize ();
  interface_initialize ();
  change_vm_protection ();
  hppa_closure_hook
    = (C_closure_entry_point ((unsigned long) cross_segment_call));
  hppa_utility_table
    = (transform_procedure_table (utility_length, utility_table));
  return;
}

#define ASM_RESET_HOOK() do						\
{									\
  bkpt_init ();								\
  hppa_reset_hook (((sizeof (utility_table)) / (sizeof (PTR))),		\
		   ((PTR *) (&utility_table[0])));			\
} while (0)

PTR * hppa_primitive_table = ((PTR *) NULL);

void
DEFUN (hppa_update_primitive_table, (low, high), int low AND int high)
{
  transform_procedure_entries ((high - low),
			       ((PTR *) (Primitive_Procedure_Table + low)),
			       (hppa_primitive_table + low));
  return;
}

Boolean 
DEFUN (hppa_grow_primitive_table, (new_size), int new_size)
{
  PTR * new_table
    = ((PTR *) (realloc (hppa_primitive_table, (new_size * (sizeof (PTR))))));
  if (new_table != ((PTR *) NULL))
    hppa_primitive_table = new_table;
  return (new_table != ((PTR *) NULL));
}

/*
   Note: The following does not do a full decoding of the BLE instruction.
   It assumes that the bits have been set by STORE_ABSOLUTE_ADDRESS below,
   which decomposes an absolute address according to the `short_pointer'
   structure above, and thus certain fields are 0.

   The sequence inserted by STORE_ABSOLUTE_ADDRESS is approximately
   (the actual address decomposition is given above).
   LDIL		L'ep,26
   BLE		R'ep(5,26)
 */

unsigned long
DEFUN (hppa_extract_absolute_address, (addr), unsigned long * addr)
{
  union short_pointer result;
  union branch_inst ble;
  union ldil_inst ldil;

  ldil.inst = *addr++;
  ble.inst = *addr;

  /* Fill the padding */
  result.address = 0;

  result.fields.A = ldil.fields.A;
  result.fields.B = ldil.fields.B;
  result.fields.C = ldil.fields.C;
  result.fields.D = ldil.fields.D;
  result.fields.w2a = ble.fields.w2a;
  result.fields.w2b = ble.fields.w2b;

  return (result.address);
}

void
DEFUN (hppa_store_absolute_address, (addr, sourcev, nullify_p),
       unsigned long * addr AND unsigned long sourcev
       AND unsigned long nullify_p)
{
  union short_pointer source;
  union ldil_inst ldil;
  union branch_inst ble;

  source.address = sourcev;

#if 0
  ldil.fields.opcode = 0x08;
  ldil.fields.base = 26;
  ldil.fields.E = 0;
#else
  ldil.inst = ((0x08 << 26) | (26 << 21));
#endif

  ldil.fields.A = source.fields.A;
  ldil.fields.B = source.fields.B;
  ldil.fields.C = source.fields.C;
  ldil.fields.D = source.fields.D;

#if 0
  ble.fields.opcode = 0x39;
  ble.fields.t_or_b = 26;
  ble.fields.x_or_w1 = 0;
  ble.fields.s = 3;
  ble.fields.w0 = 0;
#else
  ble.inst = ((0x39 << 26) | (26 << 21) | (3 << 13));
#endif

  ble.fields.w2a = source.fields.w2a;
  ble.fields.w2b = source.fields.w2b;
  ble.fields.n = (nullify_p & 1);

  *addr++ = ldil.inst;
  *addr = ble.inst;
  return;
}

/* Cache flushing/pushing code.
   Uses routines from cmpaux-hppa.m4.
 */

extern void
  EXFUN (flush_i_cache, (void)),
  EXFUN (push_d_cache_region, (PTR, unsigned long));

void
DEFUN_VOID (flush_i_cache)
{
  extern void
    EXFUN (cache_flush_all, (unsigned int, struct pdc_cache_result *));

  struct pdc_cache_result * cache_desc;
  
  cache_desc = ((struct pdc_cache_result *) &(cache_info.cache_format));

  /* The call can be interrupted in the middle of a set, so do it twice.
     Probability of two interrupts in the same cache line is
     exceedingly small, so this is likely to win.
     On the other hand, if the caches are directly mapped, a single
     call can't lose.
     In addition, if the cache is shared, there is no need to flush at all.
   */

  if (((cache_desc->I_info.conf.bits.fsel & 1) == 0)
      || ((cache_desc->D_info.conf.bits.fsel & 1) == 0))
  {
    unsigned int flag = 0;

    if (cache_desc->I_info.loop != 1)
      flag |= I_CACHE;
    if (cache_desc->D_info.loop != 1)
      flag |= D_CACHE;

    if (flag != 0)
      cache_flush_all (flag, cache_desc);
    cache_flush_all ((D_CACHE | I_CACHE), cache_desc);
  }
}

void
DEFUN (push_d_cache_region, (start_address, block_size),
       PTR start_address AND unsigned long block_size)
{
  extern void
    EXFUN (cache_flush_region, (PTR, long, unsigned int));

  struct pdc_cache_result * cache_desc;
  
  cache_desc = ((struct pdc_cache_result *) &(cache_info.cache_format));

  /* Note that the first and last words are also flushed from the I-cache
     in case this object is adjacent to another that has already caused
     the cache line to be copied into the I-cache.
   */

  if (((cache_desc->I_info.conf.bits.fsel & 1) == 0)
      || ((cache_desc->D_info.conf.bits.fsel & 1) == 0))
  {
    cache_flush_region (start_address, block_size, D_CACHE);
    cache_flush_region (start_address, 1, I_CACHE);
    cache_flush_region (((PTR)
			 (((unsigned long *) start_address)
			  + (block_size - 1))),
			1,
			I_CACHE);
  }
  return;
}

#define DECLARE_CMPINTMD_UTILITIES()					\
  UTLD (assemble_17),							\
  UTLD (assemble_12),							\
  UTLD (C_closure_entry_point),						\
  UTLD (bkpt_init),							\
  UTLD (alloc_bkpt_handle),						\
  UTLD (bkpt_install),							\
  UTLD (bkpt_closure_install),						\
  UTLD (bkpt_remove),							\
  UTLD (bkpt_p),							\
  UTLD (do_bkpt_proceed),						\
  UTLD (transform_procedure_entries),					\
  UTLD (transform_procedure_table),					\
  UTLD (change_vm_protection),						\
  UTLD (hppa_reset_hook),						\
  UTLD (hppa_update_primitive_table),					\
  UTLD (hppa_grow_primitive_table),					\
  UTLD (hppa_extract_absolute_address),					\
  UTLD (hppa_store_absolute_address),					\
  UTLD (flush_i_cache),							\
  UTLD (push_d_cache_region),						\
  UTLD (flush_i_cache_initialize)

#endif /* IN_CMPINT_C */

#endif /* CMPINTMD_H_INCLUDED */
