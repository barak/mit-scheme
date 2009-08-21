/* -*-C-*-

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
 * Specialized for the Motorola 68K family.
 */

#ifndef SCM_CMPINTMD_H_INCLUDED
#define SCM_CMPINTMD_H_INCLUDED 1

/* Machine parameters to be set by the user. */

/* Until cmpaux-mc68k.m4 is updated. */
#define CMPINT_USE_STRUCS

/* Processor type.
   Choose a number from the above list, or allocate your own. */

#ifndef COMPILER_PROCESSOR_TYPE
#define COMPILER_PROCESSOR_TYPE		COMPILER_MC68040_TYPE
#endif

/* Size (in long words) of the contents of a floating point register if
   different from a double.  For example, an MC68881 saves registers
   in 96 bit (3 longword) blocks.
*/
#define COMPILER_TEMP_SIZE			3

/* Descriptor size.
   This is the size of the offset field, and of the format field.
   This definition probably does not need to be changed.
 */

typedef unsigned short format_word;

/* The length of the GC recovery code that precedes an entry.
   On the 68K a "jsr n(a6)" instruction.
 */

#define ENTRY_PREFIX_LENGTH		4

/* Cache flushing. */

#ifdef _NEXTOS

extern void NeXT_cacheflush (void);

#  ifdef IN_CMPINT_C

/* This is not inlined because trap #2 clobbers %d0.
   Since %d0 is a compiler supertemporary, doing the trap
   out of line wins.
   Perhaps this should be more paranoid and preserve all, but...
 */

void
NeXT_cacheflush (void)
{
  asm ("trap #2");
  return;
}

#  endif /* IN_CMPINT_C */

#  define SPLIT_CACHES
#  define FLUSH_I_CACHE()			NeXT_cacheflush ()
#  define FLUSH_I_CACHE_REGION(addr,nwords)	FLUSH_I_CACHE()

#endif /* _NEXTOS */

#ifdef __hpux

/* The following is a test for HP-UX >= 7.05 */

#  include <sys/time.h>
#  include <sys/resource.h>
#  include <sys/proc.h>

#  if defined(S2DATA_WT) || defined(SWITZERLAND)

/* This only works in HP-UX >= 7.05 */

#    include <sys/cache.h>

#    ifdef SWITZERLAND

extern void swiss_cachectl (int, void *, unsigned long);

#      define FLUSH_CACHE_INITIALIZE() swiss_cachectl_init_p = 0

#      ifdef IN_CMPINT_C

static int
  swiss_cachectl_init_p = 0,
  swiss_cachectl_flush_p = 0;

void
swiss_cachectl (int mode, void * base, unsigned long count)
{
  if (swiss_cachectl_init_p == 0)
  {
    int length;
    char *string, *posn;
    extern char * strstr (char *, char *);
    extern int getcontext (char *, int);

    string = ((char *) Free);
    length = (getcontext (string,
			  ((heap_alloc_limit - Free)
			   * (sizeof (SCHEME_OBJECT)))));
    swiss_cachectl_flush_p =
      (((strstr (string, "HP-MC68040")) == ((char *) NULL)) ? 0 : 1);
    swiss_cachectl_init_p = 1;
  }
  if (swiss_cachectl_flush_p == 1)
  {
    (void) (cachectl (mode, base, count));
  }
  return;
}

#      endif /* IN_CMPINT_C */

#      define cachectl(m,b,l) swiss_cachectl(m,b,l)
#    endif /* SWITZERLAND */

extern void
  operate_on_cache_region (int, char *, unsigned long);

#    define SPLIT_CACHES

#    define FLUSH_I_CACHE()						\
  (void) (cachectl (CC_IPURGE, 0, 0))

#    define FLUSH_I_CACHE_REGION(addr, nwords)				\
  (operate_on_cache_region (CC_IPURGE, ((char *) (addr)), (nwords)))

#    define PUSH_D_CACHE_REGION(addr, nwords)				\
do									\
{									\
  char *base = ((char *) (addr));					\
  unsigned long len = (nwords);						\
									\
  operate_on_cache_region (CC_FLUSH, base, len);			\
  operate_on_cache_region (CC_IPURGE, base, 1);				\
  operate_on_cache_region (CC_IPURGE,					\
			   ((char *)					\
			    (((unsigned long *) base) + (len - 1))),	\
			    1);						\
} while (0)

#    ifdef IN_CMPINT_C

void
operate_on_cache_region (int cachecmd, char * bptr, unsigned long nwords)
{
  char * eptr;
  unsigned long nbytes, quantum;

  if (nwords == 0)
    return;

  nbytes = (nwords * (sizeof (long)));
  eptr = (bptr + (nbytes - 1));
  quantum = ((nbytes <= 0x40) ? 0x10 : 0x1000);

  for (bptr = ((char *) (((unsigned long) bptr) & (~(quantum - 1)))),
       eptr = ((char *) (((unsigned long) eptr) & (~(quantum - 1))));
       (bptr <= eptr);
       bptr += quantum)
    (void) (cachectl (cachecmd, bptr, quantum));
  return;
}

#    endif /* IN_CMPINT_C */
#  else  /* S2DATA_WT */
#    define FLUSH_I_CACHE() do {} while (0)
#  endif /* S2DATA_WT */
#endif /* __hpux */

#ifndef FLUSH_CACHE_INITIALIZE
#  define FLUSH_CACHE_INITIALIZE() do {} while (0)
#endif /* FLUSH_CACHE_INITIALIZE */

#ifndef FLUSH_I_CACHE_REGION
#  define FLUSH_I_CACHE_REGION(addr, nwords) do {} while (0)
#endif /* not FLUSH_I_CACHE_REGION */

#ifndef PUSH_D_CACHE_REGION
#  define PUSH_D_CACHE_REGION(addr, nwords) FLUSH_I_CACHE_REGION(addr, nwords)
#endif /* not PUSH_D_CACHE_REGION */

#if (COMPILER_PROCESSOR_TYPE == COMPILER_MC68020_TYPE)

/* 68k magic.
   On the 68k, when closures are invoked, the closure corresponding
   to the first entry point is what's needed on the top of the stack.
   Note that it is needed for environment only, not for code.
   The closure code does an
   ADDI.L	&magic-constant,(SP)
   on entry, to bump the current entry point (after the JSR instruction)
   to the correct place.
   This code emulates that operation by extracting the magic constant
   from the closure code, and adjusting the address by 6 as if the
   JSR instruction had just been executed.
   It is used when interrupts are disabled, in order not to get into a loop.
   Note that if closure entry points were always longword-aligned, there
   would be no need for this nonsense.
 */

#  define ADJUST_CLOSURE_AT_CALL(entry_point, location)			\
do {									\
  long magic_constant;							\
									\
  magic_constant = (* ((long *) (((char *) (entry_point)) + 2)));	\
  (location) = ((SCHEME_OBJECT)						\
		((((long) (OBJECT_ADDRESS (location))) + 6) +		\
		 magic_constant));					\
} while (0)

/* Manifest closure entry block size.
   Size in bytes of a compiled closure's header excluding the
   TC_MANIFEST_CLOSURE header.

   On the 68k, this is the format word and gc offset word and 6 bytes
   more for the jsr instruction.
*/

#  define COMPILED_CLOSURE_ENTRY_SIZE					\
  ((2 * (sizeof (format_word))) + 6)

/* Manifest closure entry destructuring.

   Given the entry point of a closure, extract the `real entry point'
   (the address of the real code of the procedure, ie. one indirection)
   from the closure.
   Note that on some machines this address may be "smeared out" over
   multiple instructions.
*/

#  define EXTRACT_CLOSURE_ENTRY_ADDRESS(real_entry_point, entry_point)	\
{									\
  (real_entry_point) =							\
    (* ((SCHEME_OBJECT *) (((char *) (entry_point)) + 2)));		\
}

/* This is the inverse of EXTRACT_CLOSURE_ENTRY_ADDRESS.
   Given a closure's entry point and a code entry point, store the
   code entry point in the closure.
 */

#  define STORE_CLOSURE_ENTRY_ADDRESS(real_entry_point, entry_point)	\
{									\
  (* ((SCHEME_OBJECT *) (((char *) (entry_point)) + 2))) =		\
    ((SCHEME_OBJECT) (real_entry_point));				\
}

#endif /* (COMPILER_PROCESSOR_TYPE == COMPILER_MC68020_TYPE) */

#if (COMPILER_PROCESSOR_TYPE == COMPILER_MC68040_TYPE)

/* On the MC68040, closure entry points are aligned, so this is a NOP. */

#  define ADJUST_CLOSURE_AT_CALL(entry_point, location) do {} while (0)

/* Manifest closure entry block size.
   Size in bytes of a compiled closure's header excluding the
   TC_MANIFEST_CLOSURE header.

   On the 68040, this is the format word and gc offset word a 4-byte-long
   jsr instruction, and 4 bytes for the target address.
*/

#  define COMPILED_CLOSURE_ENTRY_SIZE					\
  ((2 * (sizeof (format_word))) + 4 + 4)

/* Manifest closure entry destructuring.

   EXTRACT_CLOSURE_ENTRY_ADDRESS(real_entry_point, entry_point)
   Given the entry point of a closure, extract the `real entry point'
   (the address of the real code of the procedure, ie. one indirection)
   from the closure.
   Note that on some machines this address may be "smeared out" over
   multiple instructions.

   STORE_CLOSURE_ENTRY_ADDRESS(real_entry_point, entry_point)
   is the inverse of EXTRACT_CLOSURE_ENTRY_ADDRESS.
   Given a closure's entry point and a code entry point, store the
   code entry point in the closure.
*/

#  ifndef GC_ELIMINATES_CLOSURE_HOOK

#    define EXTRACT_CLOSURE_ENTRY_ADDRESS(real_ep, entry_point) do	\
{									\
  (real_ep) =								\
    (* ((SCHEME_OBJECT *) (((char *) (entry_point)) + 4)));		\
} while (0)

#    define STORE_CLOSURE_ENTRY_ADDRESS(real_ep, entry_point) do	\
{									\
  (* ((SCHEME_OBJECT *) (((char *) (entry_point)) + 4))) =		\
    ((SCHEME_OBJECT) (real_ep));					\
} while (0)


#  else /* GC_ELIMINATES_CLOSURE_HOOK */


#    define EXTRACT_CLOSURE_ENTRY_ADDRESS(real_ep, entry_point) do	\
{									\
  unsigned short *pc = ((unsigned short *) (entry_point));		\
									\
  (real_ep) =								\
    (((*pc) == 0x4eae)							\
     ? (* ((SCHEME_OBJECT *) (((char *) pc) + 4)))			\
     : (* ((SCHEME_OBJECT *) (((char *) pc) + 2))));			\
} while (0)

/* This version changes the instructions to a more efficient version.
   It is assumed that this is done only by the GC or other processes
   that flush the I-cache at the end.
 */

#    define STORE_CLOSURE_ENTRY_ADDRESS(real_ep, entry_point) do	\
{									\
  unsigned short *pc = ((unsigned short *) (entry_point));		\
									\
  *pc++ = 0x4eb9;			/* JSR absolute */		\
  (* ((SCHEME_OBJECT *) pc)) = ((SCHEME_OBJECT) (real_ep));		\
} while (0)

#  endif /* GC_ELIMINATES_CLOSURE_HOOK */


#endif /* (COMPILER_PROCESSOR_TYPE == COMPILER_MC68040_TYPE) */


#ifndef ADJUST_CLOSURE_AT_CALL

#  include "ERROR: COMPILER_PROCESSOR_TYPE unknown"

#endif /* ADJUST_CLOSURE_AT_CALL */

/* Execute cache entry size size in longwords.  The cache itself
   contains both the number of arguments provided by the caller and
   code to jump to the destination address.  Before linkage, the cache
   contains the callee's name instead of the jump code.
 */

#define EXECUTE_CACHE_ENTRY_SIZE        2

/* Execute cache destructuring. */

/* Given a target location and the address of the first word of an
   execute cache entry, extract from the cache cell the number of
   arguments supplied by the caller and store it in target. */

/* For the 68K, addresses in bytes from start of cache:
   Before linking
     +0: TC_SYMBOL || symbol address
     +4: TC_FIXNUM || 0
     +6: number of supplied arguments, + 1
   After linking
     +0: jmp $xxx
     +2:  xxx
     +6: (unchanged)
*/

#define EXTRACT_EXECUTE_CACHE_ARITY(target, address) do			\
{									\
  (target) =								\
    ((long) (* ((unsigned short *) (((char *) (address)) + 6))));	\
} while (0)

#define EXTRACT_EXECUTE_CACHE_SYMBOL(target, address) do		\
{									\
  (target) = (* ((SCHEME_OBJECT *) (address)));				\
} while (0)

/* Extract the target address (not the code to get there) from an
   execute cache cell.
 */

#define EXTRACT_EXECUTE_CACHE_ADDRESS(target, address) do		\
{									\
  (target) = (* ((SCHEME_OBJECT *) (((char *) (address)) + 2)));	\
} while (0)

/* This is the inverse of EXTRACT_EXECUTE_CACHE_ADDRESS. */

#define STORE_EXECUTE_CACHE_ADDRESS(address, entry_address) do		\
{									\
  (* ((SCHEME_OBJECT *) (((char *) (address)) + 2))) =			\
    ((SCHEME_OBJECT) (entry_address));					\
} while (0)

/* This stores the fixed part of the instructions leaving the
   destination address and the number of arguments intact.  These are
   split apart so the GC can call EXTRACT/STORE...ADDRESS but it does
   NOT need to store the instructions back.  On some architectures the
   instructions may change due to GC and then STORE_EXECUTE_CACHE_CODE
   should become a no-op and all of the work is done by
   STORE_EXECUTE_CACHE_ADDRESS instead.
 */

#define STORE_EXECUTE_CACHE_CODE(address) do				\
{									\
  (* ((unsigned short *) (address))) = ((unsigned short) 0x4ef9);	\
} while (0)

/* This overrides the definition in cmpint.c because the code below
   depends on knowing it, and is inserted before the definition in
   "cmpint.c". */

#define COMPILER_REGBLOCK_N_FIXED	16
#define COMPILER_REGBLOCK_N_TEMPS	256

#define COMPILER_REGBLOCK_START_HOOKS	COMPILER_REGBLOCK_N_FIXED
#define COMPILER_REGBLOCK_N_HOOKS	80
#define COMPILER_HOOK_SIZE		2	/* absolute jsr instruction */

#define COMPILER_REGBLOCK_EXTRA_SIZE					\
  (COMPILER_REGBLOCK_N_HOOKS * COMPILER_HOOK_SIZE)

#define A6_TRAMPOLINE_TO_INTERFACE_OFFSET				\
  ((COMPILER_REGBLOCK_START_HOOKS + (2 * COMPILER_HOOK_SIZE)) *		\
   (sizeof (SCHEME_OBJECT)))

#define A6_CLOSURE_HOOK_OFFSET						\
  ((COMPILER_REGBLOCK_START_HOOKS + (37 * COMPILER_HOOK_SIZE)) *	\
   (sizeof (SCHEME_OBJECT)))

#ifdef IN_CMPINT_C

#define ASM_RESET_HOOK mc68k_reset_hook

#ifdef CAST_FUNCTION_TO_INT_BUG

#define SETUP_REGISTER(hook) do						\
{									\
  extern unsigned long hook;						\
  (* ((unsigned short *) (a6_value + offset))) = 0x4ef9;		\
  (* ((unsigned long *)							\
      (((unsigned short *) (a6_value + offset)) + 1))) =		\
    ((unsigned long) (&hook));						\
  offset += (COMPILER_HOOK_SIZE * (sizeof (SCHEME_OBJECT)));		\
} while (0)

#else /* not CAST_FUNCTION_TO_INT_BUG */

#define SETUP_REGISTER(hook) do						\
{									\
  extern void hook, (void);						\
  (* ((unsigned short *) (a6_value + offset))) = 0x4ef9;		\
  (* ((unsigned long *)							\
      (((unsigned short *) (a6_value + offset)) + 1))) =		\
	((unsigned long) hook);						\
  offset += (COMPILER_HOOK_SIZE * (sizeof (SCHEME_OBJECT)));		\
} while (0)

#endif

void
mc68k_reset_hook (void)
{
  extern void interface_initialize (void);

  unsigned char * a6_value = ((unsigned char *) (&Registers[0]));
  int offset = (COMPILER_REGBLOCK_START_HOOKS * (sizeof (SCHEME_OBJECT)));

  /* These must match machines/bobcat/lapgen.scm */

  SETUP_REGISTER (asm_scheme_to_interface);		/* 0 */
  SETUP_REGISTER (asm_scheme_to_interface_jsr);		/* 1 */

  if (offset != A6_TRAMPOLINE_TO_INTERFACE_OFFSET)
  {
    fprintf (stderr,
	     "\nmc68k_reset_hook: A6_TRAMPOLINE_TO_INTERFACE_OFFSET\n");
    Microcode_Termination (TERM_EXIT);
  }

  SETUP_REGISTER (asm_trampoline_to_interface);		/* 2 */
  SETUP_REGISTER (asm_shortcircuit_apply);		/* 3 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_1);	/* 4 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_2);	/* 5 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_3);	/* 6 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_4);	/* 7 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_5);	/* 8 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_6);	/* 9 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_7);	/* 10 */
  SETUP_REGISTER (asm_shortcircuit_apply_size_8);	/* 11 */
  SETUP_REGISTER (asm_primitive_apply);			/* 12 */
  SETUP_REGISTER (asm_primitive_lexpr_apply);		/* 13 */
  SETUP_REGISTER (asm_error);				/* 14 */
  SETUP_REGISTER (asm_link);				/* 15 */
  SETUP_REGISTER (asm_interrupt_closure);		/* 16 */
  SETUP_REGISTER (asm_interrupt_dlink);			/* 17 */
  SETUP_REGISTER (asm_interrupt_procedure);		/* 18 */
  SETUP_REGISTER (asm_interrupt_continuation);		/* 19 */
  SETUP_REGISTER (asm_assignment_trap);			/* 20 */
  SETUP_REGISTER (asm_reference_trap);			/* 21 */
  SETUP_REGISTER (asm_safe_reference_trap);		/* 22 */
  SETUP_REGISTER (asm_generic_add);			/* 23 */
  SETUP_REGISTER (asm_generic_subtract);		/* 24 */
  SETUP_REGISTER (asm_generic_multiply);		/* 25 */
  SETUP_REGISTER (asm_generic_divide);			/* 26 */
  SETUP_REGISTER (asm_generic_equal);			/* 27 */
  SETUP_REGISTER (asm_generic_less);			/* 28 */
  SETUP_REGISTER (asm_generic_greater);			/* 29 */
  SETUP_REGISTER (asm_generic_increment);		/* 30 */
  SETUP_REGISTER (asm_generic_decrement);		/* 31 */
  SETUP_REGISTER (asm_generic_zero);			/* 32 */
  SETUP_REGISTER (asm_generic_positive);		/* 33 */
  SETUP_REGISTER (asm_generic_negative);		/* 34 */
  SETUP_REGISTER (asm_primitive_error);			/* 35 */
  SETUP_REGISTER (asm_allocate_closure);		/* 36 */

  if (offset != A6_CLOSURE_HOOK_OFFSET)
  {
    fprintf (stderr, "\nmc68k_reset_hook: A6_CLOSURE_HOOK_OFFSET\n");
    Microcode_Termination (TERM_EXIT);
  }
  else
  {							/* 37 */
    unsigned short *pc;

    pc = ((unsigned short *) (a6_value + offset));
    *pc++ = 0x2057;		/* MOVEA.L	(%sp),%a0 */
    *pc++ = 0x2050;		/* MOVEA.L	(%a0),%a0 */
    *pc++ = 0x5497;		/* ADDQ.L	&2,(%sp) */
    *pc++ = 0x4ed0;		/* JMP		(%a0) */

    offset += (COMPILER_HOOK_SIZE * (sizeof (SCHEME_OBJECT)));
  }

  SETUP_REGISTER (asm_generic_quotient);		/* 38 */
  SETUP_REGISTER (asm_generic_remainder);		/* 39 */
  SETUP_REGISTER (asm_generic_modulo);			/* 40 */
  SETUP_REGISTER (asm_stack_and_interrupt_check_12);	/* 41 */
  SETUP_REGISTER (asm_stack_and_interrupt_check_14);	/* 42 */
  SETUP_REGISTER (asm_stack_and_interrupt_check_18);	/* 43 */
  SETUP_REGISTER (asm_stack_and_interrupt_check_22);	/* 44 */
  SETUP_REGISTER (asm_stack_and_interrupt_check_24);	/* 45 */
  SETUP_REGISTER (asm_set_interrupt_enables);		/* 46 */

  FLUSH_CACHE_INITIALIZE ();
  FLUSH_I_CACHE_REGION (&Registers[COMPILER_REGBLOCK_START_HOOKS],
			(COMPILER_REGBLOCK_N_HOOKS * COMPILER_HOOK_SIZE));

  interface_initialize ();
  return;
}

#define CLOSURE_ENTRY_WORDS						\
  (COMPILED_CLOSURE_ENTRY_SIZE / (sizeof (SCHEME_OBJECT)))

static long closure_chunk = (1024 * CLOSURE_ENTRY_WORDS);
static long last_chunk_size;

SCHEME_OBJECT *
allocate_closure (long size)
{
  long space;
  SCHEME_OBJECT *result;

#if (COMPILER_PROCESSOR_TYPE != COMPILER_MC68040_TYPE)

  fprintf (stderr, "\nallocate_closure should not be invoked!\n");
  Microcode_Termination (TERM_COMPILER_DEATH);

#else /* (COMPILER_PROCESSOR_TYPE == COMPILER_MC68040_TYPE) */

  space = ((long) GET_CLOSURE_SPACE);
  result = GET_CLOSURE_FREE;

  if (size > space)
  {
    SCHEME_OBJECT *start, *ptr, *eptr;

    /* Clear remaining words from last chunk so that the heap can be scanned
       forward.
       Do not clear if there was no last chunk (ie. CLOSURE_FREE was NULL).
     */

    if (result != (((SCHEME_OBJECT *) NULL) + space))
    {
      start = result;
      if (space < 0)
	start -= size;
      eptr = (result + space);
      for (ptr = start; ptr < eptr; ptr++)
	*ptr = SHARP_F;

      /* We can reformat the closures here using last_chunk_size.
	 The start of the area is (eptr - last_chunk_size), and all
	 closures are contiguous and have appropriate headers.
       */
    }

    if ((size <= closure_chunk) && (!GC_NEEDED_P (closure_chunk)))
    {
      start = Free;
      eptr = (start + closure_chunk);
    }
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
      {
	REQUEST_GC (0);
      }
      start = Free;
      eptr = (start + size);
    }

    Free = eptr;
    result = start;
    space = (eptr - start);
    last_chunk_size = space;	/* To be used next time, maybe. */

    for (ptr = start; ptr < eptr; ptr++)
    {
      unsigned short *wptr;

      wptr = ((unsigned short *) ptr);
      *wptr++ = 0x4eae;			/* JSR n(a6) */
      *wptr = A6_CLOSURE_HOOK_OFFSET;	/* n */
    }

    PUSH_D_CACHE_REGION (start, space);
  }

  SET_CLOSURE_FREE (result + size);
  SET_CLOSURE_SPACE (space - size);
  return (result);

#endif /* (COMPILER_PROCESSOR_TYPE == COMPILER_MC68040_TYPE) */
}

#endif /* IN_CMPINT_C */

/* On the 68K, here's a  picture of a trampoline (offset in bytes from
   entry point)
     -12: MANIFEST vector header
     - 8: NON_MARKED header
     - 4: Format word
     - 2: 0xFFF4 (GC Offset to start of block from .+2)
       0: mov.w	#index,%d0
       4: jsr	A6_TRAMPOLINE_TO_INTERFACE_OFFSET(a6)
       8: trampoline dependent storage (0 - 3 longwords)

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

#define TRAMPOLINE_ENTRY_SIZE		3
#define TRAMPOLINE_BLOCK_TO_ENTRY	3 /* longwords from MNV to MOV */

#define TRAMPOLINE_ENTRY_POINT(tramp_block)				\
  (((SCHEME_OBJECT *) (tramp_block)) + TRAMPOLINE_BLOCK_TO_ENTRY)

#define TRAMPOLINE_STORAGE(tramp_entry)					\
  ((((SCHEME_OBJECT *) (tramp_entry)) - TRAMPOLINE_BLOCK_TO_ENTRY) +	\
   (2 + TRAMPOLINE_ENTRY_SIZE))

#define STORE_TRAMPOLINE_ENTRY(entry_address, index) do			\
{									\
  unsigned short *start_address, *PC;					\
  /* D0 will get the index.  JSR will be used to call the assembly	\
     language to C SCHEME_UTILITY handler:				\
	mov.w	#index,%d0						\
	jsr	n(a6)							\
  */									\
  start_address = ((unsigned short *) (entry_address));			\
  PC = start_address;							\
  *PC++ = ((unsigned short) 0x303C);	/* mov.w #???,%d0 */		\
  *PC++ = ((unsigned short) index); 	/* ??? */			\
  *PC++ = ((unsigned short) 0x4EAE);	/* jsr n(a6) */			\
  *PC++ = ((unsigned short) A6_TRAMPOLINE_TO_INTERFACE_OFFSET);		\
  PUSH_D_CACHE_REGION (start_address, 2);				\
} while (0)

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

/* Instructions aligned on word (16 bit) boundaries */
#define BYTE_OFFSET_TO_OFFSET_WORD(offset) (offset)
#define OFFSET_WORD_TO_BYTE_OFFSET(word) (CLEAR_LOW_BIT (word))

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

#endif /* !SCM_CMPINTMD_H_INCLUDED */
