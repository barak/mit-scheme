/* -*-C-*-

$Id: a044c6bc1803ae05183ce26b59485bba77e379e6 $

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

/*
 *
 * Compiled code interface macros.
 *
 * See cmpint.txt for a description of these fields.
 *
 * Specialized for the MIPS R2000/R3000
 */

#ifndef SCM_CMPINTMD_H_INCLUDED
#define SCM_CMPINTMD_H_INCLUDED 1

#ifdef _IRIX

#include <sys/cachectl.h>
#include <unistd.h>

/* Define this to use the official method of flushing the cache: the
   `mprotect' system call.  When not defined, we use `cacheflush',
   which is more efficient. The mprotect method is known to work on
   IRIX 6.3.  */
/* #define USE_MPROTECT_CACHE_FLUSH */

#else /* not _IRIX */
#ifdef sonyrisc

#include <sys/syscall.h>
#include <sys/sysmips.h>
#include <sys/cachectl.h>

extern void syscall ();

#define cacheflush(addr, nbytes, cache)					\
  syscall (SYS_sysmips, FLUSH_CACHE, (addr), (nbytes), cache)

#else /* not sonyrisc */

#if 0

/* advertised, but not provided */
extern void cacheflush();

#else /* not 0 */

#include <sys/syscall.h>
#include <sys/sysmips.h>
#include <mips/cachectl.h>

extern void syscall();

#define cacheflush(addr,nbytes,cache)					\
  syscall (SYS_sysmips, MIPS_CACHEFLUSH, (addr), (nbytes), (cache))

#endif /* not 0 */

#endif /* not sonyrisc */
#endif /* not _IRIX */

#ifdef USE_MPROTECT_CACHE_FLUSH
#define FLUSH_BOTH call_mprotect
#else
#define FLUSH_BOTH(addr, size) cacheflush ((addr), (size), BCACHE)
#endif

/* Machine parameters to be set by the user. */

/* Until cmpaux-mips.m4 is updated. */
#define CMPINT_USE_STRUCS

/* Processor type.  Choose a number from the above list, or allocate your own. */

#define COMPILER_PROCESSOR_TYPE COMPILER_MIPS_TYPE

/* Size (in long words) of the contents of a floating point register if
   different from a double.  For example, an MC68881 saves registers
   in 96 bit (3 longword) blocks.
   Default is fine for MIPS.
   define COMPILER_TEMP_SIZE			3
*/

#define COMPILER_REGBLOCK_N_TEMPS 256

/* Descriptor size.
   This is the size of the offset field, and of the format field.
   This definition probably does not need to be changed.
 */

typedef unsigned short format_word;

/* Utilities for manipulating absolute subroutine calls.
   On the MIPS this is done with:
   	JAL	destination
   The low 26 bits of the instruction form the low 28 bits of address,
   and the top 4 bits of the address of the JAL instruction form the
   top 4 bits of the address.
 */

#define EXTRACT_FROM_JAL_INSTR(target, address)				\
{									\
  unsigned long * addr = ((unsigned long *) (address));			\
  unsigned long jal_instr = (*addr);					\
  (target) =								\
    ((SCHEME_OBJECT)							\
     ((((long) (address)) & 0xF0000000) |				\
      ((jal_instr & 0x03FFFFFF) << 2)));				\
}

#define JAL_OP		(003 << 26)
#define JAL_INSTR(dest)	(JAL_OP | ((dest) >> 2))

#define STORE_JAL_INSTR(entry_point, address)				\
{									\
  unsigned long ep = ((unsigned long) (entry_point));			\
  unsigned long * addr = ((unsigned long *) (address));			\
  if (((((long) addr) & 0xF0000000)					\
       != (((long) entry_point) & 0xF0000000))				\
      || ((((long) addr) & 0x3) != 0))					\
  {									\
    fprintf (stderr,							\
	     "\nSTORE_JAL_INSTR: Bad addr in JAL 0x%x, 0x%x\n",		\
	     addr, ep);							\
  }									\
  (*addr) = JAL_INSTR (ep & 0x0FFFFFFF);				\
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

#define COMP_REG_CLOSURE_FREE		19
#define COMP_REG_ADDRESS_MASK		20
#define COMP_REG_REGISTERS		21
#define COMP_REG_QUAD_MASK		22
#define COMP_REG_CLOSURE_HOOK		23

#define COMP_REG_TRAMP_INDEX		25
#define COMP_REG_KERNEL_RESERVED_1	26
#define COMP_REG_KERNEL_RESERVED_2	27
#define COMP_REG_C_GLOBALS		28
#define COMP_REG_C_STACK		29
#define COMP_REG_LINKAGE		31

/* Interrupt/GC polling. */

/* The length of the GC recovery code that precedes an entry.
   On the MIPS a "addi, jalr, addi" instruction sequence.
 */

#define ENTRY_PREFIX_LENGTH		12

/*
  The instructions for a normal entry should be something like

  SLT	$at,$FREE,$MEMTOP
  BEQ	$at,$0,interrupt
  LW	$MEMTOP,REG_BLOCK

  For a closure

  LUI	$at,FROB(TC_CLOSURE)	; temp <- closure tag
  XOR	$31,$31,$at	        ; 31 <- tagged value
  ADDI  $SP,$SP,-4		; push closure
  SW	$31,0($SP)
  SLT	$at,$FREE,$MEMTOP
  BEQ	$at,$0,interrupt
  LW	$MEMTOP,REG_BLOCK
*/

/* A NOP on machines where instructions are longword-aligned. */

#define ADJUST_CLOSURE_AT_CALL(entry_point, location)			\
do {									\
} while (0)

/* Compiled closures */

/* Manifest closure entry block size.
   Size in bytes of a compiled closure's header excluding the
   TC_MANIFEST_CLOSURE header.

   On the MIPS this is 2 format_words for the format word and gc
   offset words, and 8 more bytes for 2 instructions.

   The two instructions are

   JAL	destination
   ADDI LINKAGE,LINKAGE,-8

   However, there is some trickery involved.  Because of cache-line
   sizes and prefetch buffers, the straight-forward allocation does
   not always work, thus closures are allocated from a pre-initialized
   pool where the entries have been initialized to contain
   the following instructions.

   JALR LINKAGE,CLOSURE_HOOK
   ADDI LINKAGE,LINKAGE,-8

   Note that the JALR instruction is overwritten with the JAL
   instruction, thus although the I-cache may have a stale instruction,
   execution will be correct, since the stale instruction will jump
   to an out-of-line handler which will fetch the correct destination
   from the return-address (through the D cache) and jump there.
 */

#define COMPILED_CLOSURE_ENTRY_SIZE     12

/* Manifest closure entry destructuring.

   Given the entry point of a closure, extract the `real entry point'
   (the address of the real code of the procedure, ie. one indirection)
   from the closure.
   On the MIPS, the real entry point is stored directly 8 bytes from
   the closure's address (address of JAL or JALR instruction).
   When using the JAL format, it is also the target address encoded
   in the instruction.
*/

#define EXTRACT_CLOSURE_ENTRY_ADDRESS(extracted_ep, clos_addr) do	\
{									\
  EXTRACT_FROM_JAL_INSTR (extracted_ep, clos_addr);			\
} while (0)

/* This is the inverse of EXTRACT_CLOSURE_ENTRY_ADDRESS.
   Given a closure's entry point and a code entry point, store the
   code entry point in the closure.
 */

#define STORE_CLOSURE_ENTRY_ADDRESS(ep_to_store, clos_addr) do		\
{									\
  STORE_JAL_INSTR (ep_to_store, clos_addr);				\
} while (0)

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

#define TRAMPOLINE_ENTRY_SIZE		4
#define TRAMPOLINE_BLOCK_TO_ENTRY	3

#define TRAMPOLINE_ENTRY_POINT(tramp_block)				\
  (((SCHEME_OBJECT *) (tramp_block)) + TRAMPOLINE_BLOCK_TO_ENTRY)

#define TRAMPOLINE_STORAGE(tramp_entry)					\
  ((((SCHEME_OBJECT *) (tramp_entry)) - TRAMPOLINE_BLOCK_TO_ENTRY) +	\
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
  PC[0] = ADDI(COMP_REG_TEMPORARY, COMP_REG_SCHEME_TO_INTERFACE, -96);	\
  PC[1] = JALR(COMP_REG_LINKAGE, COMP_REG_TEMPORARY);			\
  PC[2] = ADDI(COMP_REG_TRAMP_INDEX, 0, (4*index));			\
  /* assumes index fits in 16 bits */					\
  FLUSH_BOTH (PC, (3 * sizeof (unsigned long)));			\
}

/* Execute cache entries.

   Execute cache entry size size in longwords.  The cache itself
   contains both the number of arguments provided by the caller and
   code to jump to the destination address.  Before linkage, the cache
   contains the callee's name instead of the jump code.

   On MIPS: 2 instructions, the last being a NO-OP (ADDI with
   destination 0) containing a fixnum representing the number of
   arguments in the lower 16 bits.
 */

#define EXECUTE_CACHE_ENTRY_SIZE 2

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

   (big endian):

   Before linking
     +0: TC_SYMBOL || symbol address
     +4: TC_FIXNUM || 0
     +6: number of supplied arguments, +1

   After linking
     +0: JAL	destination
     +4: ADDI 0, arg count
     +6: (unchanged)

*/

#ifdef MIPSEL

/* Little-endian MIPS, i.e. DecStations. */

#define MIPS_CACHE_ARITY_OFFSET 2
#define MIPS_CACHE_CODE_OFFSET 7

#else /* not MIPSEL */

/* Big-endian MIPS, e.g. SGI and Sony. */

#define MIPS_CACHE_ARITY_OFFSET 3
#define MIPS_CACHE_CODE_OFFSET 4

#endif /* not MIPSEL */

#define EXTRACT_EXECUTE_CACHE_ARITY(target, address)			\
{									\
  (target) =								\
    ((long)								\
     (((unsigned short *) (address)) [MIPS_CACHE_ARITY_OFFSET]));	\
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
  EXTRACT_FROM_JAL_INSTR (target, address);				\
}

/* This is the inverse of EXTRACT_EXECUTE_CACHE_ADDRESS.
   On the MIPS it must flush the I-cache, but there is no
   need to flush the ADDI instruction, which is a NOP.
 */

#define STORE_EXECUTE_CACHE_ADDRESS(address, entry)			\
{									\
  STORE_JAL_INSTR (entry, address);					\
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
  char * opcode_addr = (((char *) (address)) + MIPS_CACHE_CODE_OFFSET);	\
  (*opcode_addr) = (ADDI_OPCODE << 2);					\
}

/* This flushes the Scheme portion of the I-cache.
   It is used after a GC or disk-restore.
   It's needed because the GC has moved code around, and closures
   and execute cache cells have absolute addresses that the
   processor might have old copies of.
 */

#define FLUSH_I_CACHE() do						\
{									\
  FLUSH_BOTH (constant_start,						\
	      (((unsigned long) heap_end)				\
	       - ((unsigned long) constant_start)));			\
} while (0)

/* This flushes a region of the I-cache.
   It is used after updating an execute cache while running.
   Not needed during GC because FLUSH_I_CACHE will be used.
 */

#define FLUSH_I_CACHE_REGION(address, nwords) do			\
{									\
  FLUSH_BOTH ((address), ((sizeof (long)) * (nwords)));			\
} while (0)

/* This guarantees that a newly-written section of address space
   has its values propagated to main memory so that i-stream fetches
   will see the new values.
   The first and last byte are flushed from the i-cache in case
   the written region overlaps with already-executed areas.
 */

#ifdef USE_MPROTECT_CACHE_FLUSH

#define PUSH_D_CACHE_REGION(address, nwords) do				\
{									\
  FLUSH_BOTH ((address), ((sizeof (long)) * (nwords)));			\
} while (0)

#else /* not USE_MPROTECT_CACHE_FLUSH */

#define PUSH_D_CACHE_REGION(address, nwords) do				\
{									\
  unsigned long _addr = ((unsigned long) (address));			\
  unsigned long _nbytes = ((sizeof (long)) * (nwords));			\
  cacheflush (((void *) _addr), _nbytes, DCACHE);			\
  cacheflush (((void *) _addr), 1, ICACHE);				\
  cacheflush (((void *) (_addr + (_nbytes - 1))), 1, ICACHE);		\
} while (0)

#endif /* not USE_MPROTECT_CACHE_FLUSH */

#ifdef IN_CMPINT_C

static void
interface_initialize_C (void)
{
  extern void interface_initialize (void);

  /* Prevent the OS from "fixing" unaligned accesses.
     Within Scheme, they are a BUG, and should fault.

     Is this defined for all the OSs?
   */
#ifdef MIPSEL
  syscall (SYS_sysmips, MIPS_FIXADE, 0);
#endif
  interface_initialize ();
  return;
}

#ifdef _IRIX6

#include <sys/mman.h>
#include <sys/types.h>

#define VM_PROT_SCHEME (PROT_READ | PROT_WRITE | PROT_EXEC)

static void * mprotect_start;
static unsigned long mprotect_size;

static void
call_mprotect_1 (void * start, unsigned long size)
{
  if ((mprotect (start, size, VM_PROT_SCHEME)) != 0)
    {
      perror ("unable to change memory protection");
      fprintf (stderr, "mprotect(0x%lx, %d (0x%lx), 0x%lx)\n",
	       start, size, size, VM_PROT_SCHEME);
      Microcode_Termination (TERM_EXIT);
      /*NOTREACHED*/
    }
}

#ifdef USE_MPROTECT_CACHE_FLUSH
void
call_mprotect (void * start, unsigned long size)
{
  unsigned long pagesize = (getpagesize ());
  unsigned long istart = ((unsigned long) start);
  unsigned long pstart = ((istart / pagesize) * pagesize);
  call_mprotect_1 (((void *) pstart), (istart - pstart));
}
#endif /* USE_MPROTECT_CACHE_FLUSH */

void *
irix_heap_malloc (long size)
{
  int pagesize = (getpagesize ());
  void * area = (malloc (size + pagesize));
  if (area == 0)
    return (0);
  mprotect_start
    = ((void *)
       (((((unsigned long) area) + (pagesize - 1)) / pagesize) * pagesize));
  mprotect_size = size;
  call_mprotect_1 (mprotect_start, mprotect_size);
  return (mprotect_start);
}

#endif /* _IRIX6 */

#define ASM_RESET_HOOK interface_initialize_C

#define CLOSURE_ENTRY_WORDS			\
  (COMPILED_CLOSURE_ENTRY_SIZE / (sizeof (SCHEME_OBJECT)))

static long closure_chunk = (1024 * CLOSURE_ENTRY_WORDS);

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
allocate_closure (long size)
{
  long space;
  SCHEME_OBJECT * free_closure, * limit;

  free_closure = GET_CLOSURE_FREE;
  limit = GET_CLOSURE_SPACE;
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

    if ((space > 0) && (free_closure != ((SCHEME_OBJECT *) NULL)))
      free_closure[-3] = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (space - 1)));

    free_closure = Free;
    if ((size <= closure_chunk) && (!GC_NEEDED_P (closure_chunk)))
      limit = (free_closure + closure_chunk);
    else
    {
      if (GC_NEEDED_P (size))
      {
	if ((heap_end - Free) < size)
	{
	  /* No way to back out -- die. */
	  fprintf (stderr, "\nC_allocate_closure (%d): No space.\n", size);
	  Microcode_Termination (TERM_NO_SPACE);
	  /* NOTREACHED */
	}
	REQUEST_GC (0);
      }
      else if (size <= closure_chunk)
	REQUEST_GC (0);
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
    SET_CLOSURE_SPACE (limit);
    SET_CLOSURE_FREE (free_closure + 3);
  }
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

#define BYTE_OFFSET_TO_OFFSET_WORD(offset) ((offset) >> 1)
#define OFFSET_WORD_TO_BYTE_OFFSET(word) ((CLEAR_LOW_BIT (word)) << 1)

#define MAKE_OFFSET_WORD(entry, block, continue)                        \
  ((BYTE_OFFSET_TO_OFFSET_WORD(((char *) (entry)) -                     \
                               ((char *) (block)))) |                   \
   ((continue) ? 1 : 0))

#define EXECUTE_CACHE_COUNT_TO_ENTRIES(count)                           \
  ((count) >> 1)
#define EXECUTE_CACHE_ENTRIES_TO_COUNT(entries)				\
  ((entries) << 1)

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

#endif /* !SCM_CMPINTMD_H_INCLUDED */
