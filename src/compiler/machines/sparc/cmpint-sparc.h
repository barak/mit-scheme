/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

/*
 *
 * Compiled code interface macros.
 *
 * See cmpint.txt for a description of these fields.
 *
 * Specialized for the MIPS R2000/R3000
 */

#ifndef CMPINT2_H_INCLUDED
#define CMPINT2_H_INCLUDED

#define ICACHEFLUSH(addr, nbytes) flushrange ((addr), (nbytes))

#define COMPILER_NONE_TYPE			0
#define COMPILER_MC68020_TYPE			1
#define COMPILER_VAX_TYPE			2
#define COMPILER_SPECTRUM_TYPE			3
#define COMPILER_OLD_MIPS_TYPE			4
#define COMPILER_MC68040_TYPE			5
#define COMPILER_SPARC_TYPE			6
#define COMPILER_RS6000_TYPE			7
#define COMPILER_MC88K_TYPE			8
#define COMPILER_I386_TYPE			9
#define COMPILER_ALPHA_TYPE			10
#define COMPILER_MIPS_TYPE			11

/* Machine parameters to be set by the user. */

/* Processor type.  Choose a number from the above list, or allocate your own. */

#define COMPILER_PROCESSOR_TYPE			COMPILER_SPARC_TYPE

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
   On the SPARC this is done with:
     CALL destination

	The low 30 bits of the instruction form the address. This will
	automatically be shifted over 2 bits to adjust for alignment.
 */

#define EXTRACT_FROM_JAL_INSTR(target, address)				\
{									\
  unsigned long * addr = ((unsigned long *) (address));			\
  unsigned long jal_instr = (*addr);					\
  (target) =								\
    ((SCHEME_OBJECT)							\
     ((((long) (address)) & 0x3FFFFFFF))); \
}

#define CALL_OP		(0x1 << 30)
#define CALL_INSTR(dest)	(CALL_OP | (dest >> 2))

#define STORE_JAL_INSTR(entry_point, address)				\
{									\
  unsigned long ep = ((unsigned long) (entry_point));			\
  unsigned long * addr = ((unsigned long *) (address));			\
  if ((((long) addr) & 0x3) != 0)					\
  {									\
    fprintf (stderr,							\
	     "\nSTORE_JAL_INSTR: Bad addr in CALL 0x%x, 0x%x\n",		\
	     addr, ep);							\
  }									\
  (*addr) = CALL_INSTR (ep);				\
}

/* Compiled Code Register Conventions */
/* This must match the compiler and cmpaux-sparc.s */

#define COMP_REG_TEMPORARY	1
#define COMP_REG_RETURN		16
#define COMP_REG_STACK		17
#define COMP_REG_C_ARG_1		8
#define COMP_REG_C_ARG_2		9
#define COMP_REG_C_ARG_3		10
#define COMP_REG_C_ARG_4		11
#define COMP_REG_MEMTOP		18
#define COMP_REG_FREE		19
#define COMP_REG_SCHEME_TO_INTERFACE 20
#define COMP_REG_DYNAMIC_LINK		21
#define COMP_REG_TRAMP_INDEX       13

#define COMP_REG_CLOSURE_FREE		22
#define COMP_REG_ADDRESS_MASK		25
#define COMP_REG_REGISTERS		26
#define COMP_REG_QUAD_MASK		27
#define COMP_REG_CLOSURE_HOOK		28

#define COMP_REG_KERNEL_RESERVED_1	2
#define COMP_REG_KERNEL_RESERVED_2	3
#define COMP_REG_KERNEL_RESERVED_3	4
#define COMP_REG_C_GLOBALS		
#define COMP_REG_C_STACK		30
#define COMP_REG_LINKAGE		31

/* Interrupt/GC polling. */

/* Skip over this many BYTES to bypass the GC check code (ordinary
procedures and continuations differ from closures) */

#define ENTRY_SKIPPED_CHECK_OFFSET 	12
#define CLOSURE_SKIPPED_CHECK_OFFSET 	40

/* The length of the GC recovery code that precedes an entry.
   On the SPARC a "addi, jalr, addi" instruction sequence.
 */

#define ENTRY_PREFIX_LENGTH		12

/*
  The instructions for a normal entry should be something like

  ADDICC  $at,$FREE,$MEMTOP
  BGE     interrupt
  LD      $MEMTOP,REG_BLOCK
  
  For a closure

  LUI	$at,FROB(TC_CLOSURE)	; temp <- closure tag
  XOR	$1,$1,$at	        ; 1 <- tagged value
  ADDI    $SP,$SP,-4		; push closure
  ST	     $1,0($SP)
  ADDICC  $at,$FREE,$MEMTOP
  BGE     interrupt
  LD      $MEMTOP,REG_BLOCK
*/

/* A NOP on machines where instructions are longword-aligned. */

#define ADJUST_CLOSURE_AT_CALL(entry_point, location)			\
do {									\
} while (0)

/* Compiled closures */

/* Manifest closure entry block size.
   Size in bytes of a compiled closure's header excluding the
   TC_MANIFEST_CLOSURE header.

   On the SPARC this is 2 format_words for the format word and gc offset
   words, and 12 more bytes for 3 instructions.

   The three instructions are:

   SETHI %HI(TARGET), GLOBAL_TEMP
   JMPL [GLOBAL_TEMP + %LO(TARGET)], GLOBAL_TEMP
   ADDI 8,GLOBAL_TEMP,GLOBAL_TEMP
 */

#define SETHI_GLOBAL_TEMP_TEMPLATE  0x03000000
#define NOP_INSTRUCTION 0x01000000
#define JMPL_TEMPLATE 0x81c06000
#define CLOSURE_JMPL_TEMPLATE 0x83c06000

#define COMPILED_CLOSURE_ENTRY_SIZE     16

/* Manifest closure entry destructuring.

   Given the entry point of a closure, extract the `real entry point'
   (the address of the real code of the procedure, ie. one indirection)
   from the closure.

   On the SPARC we have to extract from a SETHI/JMPL_OFFSET sequence.
   
*/

#define EXTRACT_CLOSURE_ENTRY_ADDRESS(extracted_ep, clos_addr) do	\
{									\
  unsigned long * addr = ((unsigned long*)(clos_addr)); \
  unsigned long sethi_instr = addr[0]; \
  unsigned long jmpl_instr = addr[1]; \
  (extracted_ep) = \
   ((SCHEME_OBJECT) \
    (((sethi_instr & 0x3fffff) << 10) | (jmpl_instr & 0x3ff))); \
} while (0)

/* This is the inverse of EXTRACT_CLOSURE_ENTRY_ADDRESS.
   Given a closure's entry point and a code entry point, store the
   code entry point in the closure.
 */

/* The following is a SPARC ADDI 8,G1,G1 */
#define CLOSURE_BUMP_LINKAGE_TO_DATA_INSTR 0x82006008

#define STORE_CLOSURE_ENTRY_ADDRESS(ep_to_store, clos_addr) do		\
{									\
  unsigned long * addr = (unsigned long *)(clos_addr); \
  unsigned long target = (unsigned long)(ep_to_store); \
  addr[0] = (addr[0] & SETHI_GLOBAL_TEMP_TEMPLATE) | (target >> 10); \
  addr[1] = (addr[1] & CLOSURE_JMPL_TEMPLATE) | (target & 0x000003ff); \
  addr[2] = CLOSURE_BUMP_LINKAGE_TO_DATA_INSTR; \
} while (0)

/* Trampolines

   On the SPARC, here's a picture of a trampoline (offset in bytes from
   entry point)

     -12: MANIFEST vector header
     - 8: NON_MARKED header
     - 4: Format word
     - 2: 0x6 (GC Offset to start of block from .+2)
          Note the encoding -- divided by 2, low bit for
          extended distances (see OFFSET_WORD_TO_BYTE_OFFSET)
       0: ADDI  TEMP,SCHEME_TO_INTERFACE,MAGIC_CONSTANT
       4: JALR	LINKAGE,TEMP
       8: ADDI	TRAMP_INDEX,0,index
      12: trampoline dependent storage (0 - 3 longwords)

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
*/

#define TRAMPOLINE_ENTRY_SIZE		5
#define TRAMPOLINE_BLOCK_TO_ENTRY	3

#define TRAMPOLINE_ENTRY_POINT(tramp_block)				\
  (((SCHEME_OBJECT *) (tramp_block)) + TRAMPOLINE_BLOCK_TO_ENTRY)

#define TRAMPOLINE_STORAGE(tramp_entry)					\
  ((((SCHEME_OBJECT *)(tramp_entry)) + 3))

#define SPECIAL_OPCODE	000
#define ADDI_OPCODE	010

#define OP(OPCODE)	(OPCODE << 18)
#define SPECIAL_OP	OP(SPECIAL_OPCODE)
#define ADDI_OP		OP(ADDI_OPCODE)

#define JALR_TEMPLATE 0x81c02000
#define JALR_SRC(n)	((n & 0x1F) << 14)
#define JALR_DST(n)	((n & 0x1F) << 25)
#define JALR(d,s)	(JALR_TEMPLATE|JALR_SRC(s)|JALR_DST(d))

#define ADDI_TEMPLATE 0x80002000
#define ADDI_SRC(n)	((n & 0x1F) << 14)
#define ADDI_DST(n)	((n & 0x1F) << 25)
#define ADDI_IMMED(n)	(n & 0x1FFF)
#define ADDI(d,s,imm)	(ADDI_TEMPLATE|ADDI_DST(d)|ADDI_SRC(s)|ADDI_IMMED(imm))

#define STORE_TRAMPOLINE_ENTRY(entry_address, index)			\
{ unsigned long *PC;							\
  PC = ((unsigned long *) (entry_address));				\
  *PC++ = ADDI(COMP_REG_TEMPORARY, COMP_REG_SCHEME_TO_INTERFACE, -8);	\
  *PC++ = JALR(COMP_REG_C_ARG_1, COMP_REG_TEMPORARY);			\
  *PC = ADDI(COMP_REG_TRAMP_INDEX, 0, (4*index));			\
  /* assumes index fits in 13 bits */					\
}

/* Execute cache entries.

   Execute cache entry size size in longwords.  The cache itself
   contains both the number of arguments provided by the caller and
   code to jump to the destination address.  Before linkage, the cache
   contains the callee's name instead of the jump code.

   On SPARC: 3 instructions, the last being a NO-OP (SETHI with
   constant 0, destination 0)
 */

#define EXECUTE_CACHE_ENTRY_SIZE        3

/* Execute cache destructuring. */

/* Given a target location and the address of the first word of an
   execute cache entry, extract from the cache cell the number of
   arguments supplied by the caller and store it in target. */

/* For the SPARC (big endian), addresses in bytes from the start of
   the cache:

   Before linking
     +0: TC_SYMBOL || symbol address
     +4: TC_FIXNUM || 0
     +6: number of supplied arguments, +1
	+8: ???
	
   After linking
     +0: SETHI global_temp (top 22 bits) 
	+4: JMPL global_temp (low 10 bits)
     +8: NOP

*/

#define SPARC_CACHE_ARITY_OFFSET 5
#define SPARC_CACHE_CODE_OFFSET 8


#define EXTRACT_EXECUTE_CACHE_ARITY(target, address)			\
{									\
  (target) =								\
    ((long)								\
     (((unsigned short *) (address)) [SPARC_CACHE_ARITY_OFFSET]) & 0x0fff);\
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
  unsigned long * addr = ((unsigned long*)(address)); \
  unsigned long sethi_instr = addr[0]; \
  unsigned long jmpl_instr = addr[1]; \
  (target) = \
   ((SCHEME_OBJECT) \
    (((sethi_instr & 0x3fffff) << 10) | (jmpl_instr & 0x3ff))); \
}

/* This is the inverse of EXTRACT_EXECUTE_CACHE_ADDRESS.
   On the SPARC it must flush the I-cache, but there is no
   need to flush the following ADDI instruction, which is a NOP.
 */

#define STORE_EXECUTE_CACHE_ADDRESS(address, entry)			\
{									\
  unsigned long * addr = (unsigned long *)(address); \
  unsigned long target = (unsigned long)(entry); \
  addr[0] = (addr[0] & SETHI_GLOBAL_TEMP_TEMPLATE) | (target >> 10); \
  addr[1] = (addr[1] & JMPL_TEMPLATE) | (target & 0x000003ff); \
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
{									\
  unsigned long* nop_addr = (((unsigned long *)(address)) + 2); \
  unsigned long  nop_val; \
  *((unsigned long *)address) = (SETHI_GLOBAL_TEMP_TEMPLATE); \
  *(((unsigned long *)(address))+1) = JMPL_TEMPLATE; \
  nop_val = (*nop_addr); \
  (*nop_addr) = ADDI(0,0,nop_val); \
}

/* This flushes the Scheme portion of the I-cache.
   It is used after a GC or disk-restore.
   It's needed because the GC has moved code around, and closures
   and execute cache cells have absolute addresses that the
   processor might have old copies of.
 */

#define FLUSH_I_CACHE() do						\
{									\
  ICACHEFLUSH (Heap_Bottom,						\
	       ((sizeof(SCHEME_OBJECT)) *				\
		(Heap_Top - Heap_Bottom)));				\
  ICACHEFLUSH (Constant_Space,						\
	       ((sizeof(SCHEME_OBJECT)) *				\
		(Constant_Top - Constant_Space)));			\
  ICACHEFLUSH (Stack_Pointer,						\
	       ((sizeof(SCHEME_OBJECT)) *				\
		(Stack_Top - Stack_Pointer)));				\
} while (0)


/* This flushes a region of the I-cache.
   It is used after updating an execute cache while running.
   Not needed during GC because FLUSH_I_CACHE will be used.
 */   

#define FLUSH_I_CACHE_REGION(address, nwords) do			\
{									\
  ICACHEFLUSH ((address), ((sizeof (long)) * (nwords)));		\
} while (0)

#define PUSH_D_CACHE_REGION FLUSH_I_CACHE_REGION

/* The following is misnamed.
   It should really be called STORE_BACK_D_CACHE.
   Neither the R2000 nor the R3000 systems have them.
   I don't know about the R4000 or R6000.
 */

/* #define SPLIT_CACHES */

#ifdef IN_CMPINT_C


#define CLOSURE_ENTRY_WORDS			\
  (COMPILED_CLOSURE_ENTRY_SIZE / (sizeof (SCHEME_OBJECT)))

static long closure_chunk = (1024 * CLOSURE_ENTRY_WORDS);

#define REGBLOCK_CLOSURE_LIMIT	REGBLOCK_CLOSURE_SPACE

/* The apparently random instances of the number 3 below arise from
   the convention that free_closure always points to a JAL instruction
   with (at least) 3 unused words preceding it.
   In this way, if there is enough space, we can use free_closure
   as the address of a new uni- or multi-closure.
   
   The code below (in the initialization loop) depends on knowing that
   CLOSURE_ENTRY_WORDS is 3.
   
   Random hack: ADDI instructions look like TC_TRUE objects, thus of the
   pre-initialized words, only the JALR looks like a pointer object
   (an SCODE-QUOTE).  Since there is exactly one JALR of waste between
   closures, and it is always 3 words before free_closure,
   the code for uni-closure allocation (in mips.m4) bashes that word
   with 0 (SHARP_F) to make the heap parseable.
 */

/* size in Scheme objects of the block we need to allocate. */

void
DEFUN (allocate_closure, (size), long size)
{
  long space;
  SCHEME_OBJECT * free_closure, * limit;

  free_closure = ((SCHEME_OBJECT *) Registers[REGBLOCK_CLOSURE_FREE]);
  limit = ((SCHEME_OBJECT *) Registers[REGBLOCK_CLOSURE_LIMIT]);
  space =  ((limit - free_closure) + 3);

  /* Bump up to a multiple of CLOSURE_ENTRY_WORDS.
     Otherwise clearing by the allocation code may clobber
     a different word.
   */
  size = (CLOSURE_ENTRY_WORDS
	  * ((size + (CLOSURE_ENTRY_WORDS - 1))
	     / CLOSURE_ENTRY_WORDS));
  if (size > space)
  {
    long chunk_size;
    SCHEME_OBJECT *ptr;

    /* Make the heap be parseable forward by protecting the waste
       in the last chunk.
     */
       
    if ((space > 0) && (free_closure != ((SCHEME_OBJECT) NULL)))
      free_closure[-3] = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (space - 1)));

    free_closure = Free;
    if ((size <= closure_chunk) && (!(GC_Check (closure_chunk))))
      limit = (free_closure + closure_chunk);
    else
    {
      if (GC_Check (size))
      {
	if ((Heap_Top - Free) < size)
	{
	  /* No way to back out -- die. */
	  fprintf (stderr, "\nC_allocate_closure (%d): No space.\n", size);
	  Microcode_Termination (TERM_NO_SPACE);
	  /* NOTREACHED */
	}
	Request_GC (0);
      }
      else if (size <= closure_chunk)
	Request_GC (0);
      limit = (free_closure + size);
    }
    Free = limit;
    chunk_size = (limit - free_closure);

    ptr = free_closure;
    while (ptr < limit)
    {
      *ptr++ = (JALR (COMP_REG_LINKAGE, COMP_REG_CLOSURE_HOOK));
      *ptr++ = (ADDI (COMP_REG_LINKAGE, COMP_REG_LINKAGE, -8));
      *ptr++ = SHARP_F;
    }
    PUSH_D_CACHE_REGION (free_closure, chunk_size);
    Registers[REGBLOCK_CLOSURE_LIMIT] = ((SCHEME_OBJECT) limit);
    Registers[REGBLOCK_CLOSURE_FREE] = ((SCHEME_OBJECT) (free_closure + 3));
  }
  return;
}

#endif /* IN_CMPINT_C */

/* Derived parameters and macros.

   These macros expect the above definitions to be meaningful.
   If they are not, the macros below may have to be changed as well.
 */

#define COMPILED_ENTRY_OFFSET_WORD(entry) (((format_word *) (entry)) [-1])
#define COMPILED_ENTRY_FORMAT_WORD(entry) (((format_word *) (entry)) [-2])

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
  (SIGN_EXTEND_FIELD ((((unsigned long) (word)) & 0xff), 8))

#define FORMAT_WORD_HIGH_BYTE(word)					\
  (SIGN_EXTEND_FIELD							\
   ((((unsigned long) (word)) >> 8),					\
    (((sizeof (format_word)) * CHAR_BIT) - 8)))

#define COMPILED_ENTRY_FORMAT_HIGH(addr)                                \
  (FORMAT_WORD_HIGH_BYTE (COMPILED_ENTRY_FORMAT_WORD (addr)))

#define COMPILED_ENTRY_FORMAT_LOW(addr)                                 \
  (FORMAT_WORD_LOW_BYTE (COMPILED_ENTRY_FORMAT_WORD (addr)))

#define FORMAT_BYTE_FRAMEMAX            0x7f

#define COMPILED_ENTRY_MAXIMUM_ARITY    COMPILED_ENTRY_FORMAT_LOW
#define COMPILED_ENTRY_MINIMUM_ARITY    COMPILED_ENTRY_FORMAT_HIGH

#endif /* CMPINT2_H_INCLUDED */
