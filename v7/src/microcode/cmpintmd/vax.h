/* -*-C-*-

$Id: vax.h,v 1.14 2007/04/22 16:31:24 cph Exp $

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

/*
 *
 * Compiled code interface macros.
 *
 * See cmpint.txt for a description of these fields.
 *
 * Specialized for the Vax architecture.
 */

#ifndef SCM_CMPINTMD_H_INCLUDED
#define SCM_CMPINTMD_H_INCLUDED 1

/* Machine parameters to be set by the user. */

/* Until cmpaux-vax.m4 is updated. */
#define CMPINT_USE_STRUCS

/* Processor type.  Choose a number from the above list, or allocate your own. */

#define COMPILER_PROCESSOR_TYPE COMPILER_VAX_TYPE

/* Size (in long words) of the contents of a floating point register if
   different from a double.  Default is fine.

   #define COMPILER_TEMP_SIZE			2

*/

/* Descriptor size.
   This is the size of the offset field, and of the format field.
   This definition probably does not need to be changed.
 */

typedef unsigned short format_word;

/* The length of the GC recovery code that precedes an entry.
   On the Vax a "movl s^code,r0; jsb b^n(r10)" sequence.
 */

#define ENTRY_PREFIX_LENGTH		6

/* Multi-closure magic
   On the Vax, when closures are invoked, the closure corresponding
   to the first entry point is what's needed on the top of the stack.
   Note that it is needed for environment only, not for code.
   The closure code does an
   ADDL2	&magic-constant,(SP)
   on entry, to bump the current entry point (after the JSB instruction)
   to the correct place.
   This code emulates that operation by extracting the magic constant
   from the closure code, and adjusting the address by 6 as if the
   JSB instruction had just been executed.
   It is used when interrupts are disabled, in order not to get into a loop.
   Note that if closure entry points were always longword-aligned, there
   would be no need for this nonsense.
 */

#define ADJUST_CLOSURE_AT_CALL(entry_point, location)			\
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

   On the Vax, this is the format word and gc offset word and 6 bytes
   more for the jsb instruction.
*/

#define COMPILED_CLOSURE_ENTRY_SIZE					\
((2 * (sizeof (format_word))) + 6)

/* Manifest closure entry destructuring.

   Given the entry point of a closure, extract the `real entry point'
   (the address of the real code of the procedure, ie. one indirection)
   from the closure.
   Note that on some machines this address may be "smeared out" over
   multiple instructions.
*/

#define EXTRACT_CLOSURE_ENTRY_ADDRESS(real_entry_point, entry_point)	\
{									\
  (real_entry_point) =							\
    (* ((SCHEME_OBJECT *) (((char *) (entry_point)) + 2)));		\
}

/* This is the inverse of EXTRACT_CLOSURE_ENTRY_ADDRESS.
   Given a closure's entry point and a code entry point, store the
   code entry point in the closure.
 */

#define STORE_CLOSURE_ENTRY_ADDRESS(real_entry_point, entry_point)	\
{									\
  (* ((SCHEME_OBJECT *) (((char *) (entry_point)) + 2))) =		\
    ((SCHEME_OBJECT) (real_entry_point));				\
}

/* Execute cache entry size size in longwords.  The cache itself
   contains both the number of arguments provided by the caller and
   code to jump to the destination address.  Before linkage, the cache
   contains the callee's name instead of the jump code.
 */

#define EXECUTE_CACHE_ENTRY_SIZE 2

/* Execute cache destructuring. */

/* Given a target location and the address of the first word of an
   execute cache entry, extract from the cache cell the number of
   arguments supplied by the caller and store it in target. */

/* For the Vax, addresses in bytes from start of cache:
   Before linking
     +0: TC_FIXNUM || arity
     +4: TC_SYMBOL || symbol address
   After linking
     +0: arity
     +2: jmp @&
     +4: xxx
   Note that arity stays in the same place since Vaxen are little-endian.
*/

#define EXTRACT_EXECUTE_CACHE_ARITY(target, address)			\
{									\
  (target) = ((long) (* ((unsigned short *) (address))));		\
}

#define EXTRACT_EXECUTE_CACHE_SYMBOL(target, address)			\
{									\
  (target) = (* (((SCHEME_OBJECT *) (address)) + 1));			\
}

/* Extract the target address (not the code to get there) from an
   execute cache cell.
 */

#define EXTRACT_EXECUTE_CACHE_ADDRESS(target, address)			\
{									\
  (target) = (* (((SCHEME_OBJECT *) (address)) + 1));			\
}

/* This is the inverse of EXTRACT_EXECUTE_CACHE_ADDRESS. */

#define STORE_EXECUTE_CACHE_ADDRESS(address, entry_address)		\
{									\
  (* (((SCHEME_OBJECT *) (address)) + 1)) =				\
    ((SCHEME_OBJECT) (entry_address));					\
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
  (* (((unsigned short *) (address)) + 1)) =				\
    ((unsigned short) 0x9f17);						\
}

/* This overrides the definition in cmpint.c because the code below
   depends on knowing it, and is inserted before the definition in
   cmpint.c
 */

#define COMPILER_REGBLOCK_N_FIXED	16
#define COMPILER_REGBLOCK_N_TEMPS	256

#define COMPILER_REGBLOCK_N_HOOKS	40
#define COMPILER_HOOK_SIZE		2	/* jsb @& + pad */

#define COMPILER_REGBLOCK_EXTRA_SIZE					\
(COMPILER_REGBLOCK_N_HOOKS * COMPILER_HOOK_SIZE)

#define R10_TRAMPOLINE_TO_INTERFACE_OFFSET				\
((COMPILER_REGBLOCK_N_FIXED + (2 * COMPILER_HOOK_SIZE)) *		\
 (sizeof (SCHEME_OBJECT)))

#ifdef IN_CMPINT_C

#define ASM_RESET_HOOK vax_reset_hook

#ifdef CAST_FUNCTION_TO_INT_BUG

#define SETUP_REGISTER(hook)						\
{									\
  extern unsigned long hook;						\
  (* ((unsigned short *) (r10_value + offset))) =			\
    ((unsigned short) 0x9f17);						\
  (* ((unsigned long *)							\
      (((unsigned short *) (r10_value + offset)) + 1))) =		\
    ((unsigned long) (&hook));						\
  offset += (COMPILER_HOOK_SIZE * (sizeof (SCHEME_OBJECT)));		\
}

#else /* not CAST_FUNCTION_TO_INT_BUG */

#define SETUP_REGISTER(hook)						\
{									\
  extern void hook (void);						\
  (* ((unsigned short *) (r10_value + offset))) =			\
    ((unsigned short) 0x9f17);						\
  (* ((unsigned long *)							\
      (((unsigned short *) (r10_value + offset)) + 1))) =		\
	((unsigned long) hook);						\
  offset += (COMPILER_HOOK_SIZE * (sizeof (SCHEME_OBJECT)));		\
}

#endif

void
vax_reset_hook (void)
{
  unsigned char * r10_value = ((unsigned char *) (&Registers[0]));
  int offset = (COMPILER_REGBLOCK_N_FIXED * (sizeof (SCHEME_OBJECT)));

  /* These must match machines/vax/lapgen.scm */
  SETUP_REGISTER (asm_scheme_to_interface);		/* 0 */
  SETUP_REGISTER (asm_scheme_to_interface_jsb);		/* 1 */
  SETUP_REGISTER (asm_trampoline_to_interface);		/* 2 */
#if 0
  /* Not yet written for the Vax */
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
#endif /* 0 */
  return;
}

#endif /* IN_CMPINT_C */

/* On the Vax, here's a  picture of a trampoline (offset in bytes from
   entry point)
     -12: MANIFEST vector header
     - 8: NON_MARKED header
     - 4: Format word
     - 2: 0x12 (GC Offset to start of block from .+2)
       0: movl	S^code,r0
       3: jsb	B^R10_TRAMPOLINE_TO_INTERFACE_OFFSET(r10)
       6: 0
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
#define TRAMPOLINE_BLOCK_TO_ENTRY	3 /* longwords from MNV to MOVL */

#define TRAMPOLINE_ENTRY_POINT(tramp_block)				\
  (((SCHEME_OBJECT *) (tramp_block)) + TRAMPOLINE_BLOCK_TO_ENTRY)

#define TRAMPOLINE_STORAGE(tramp_entry)					\
  ((((SCHEME_OBJECT *) tramp_entry) - TRAMPOLINE_BLOCK_TO_ENTRY) +	\
   (2 + TRAMPOLINE_ENTRY_SIZE))

#define STORE_TRAMPOLINE_ENTRY(entry_address, code)			\
{									\
  unsigned long *PC;							\
  /* r0 will get the code.  JSB will be used to call the assembly	\
     language to C SCHEME_UTILITY handler:				\
        movl	S^code,r0						\
	jsb	B^R10_TRAMPOLINE_TO_INTERFACE_OFFSET(R10)		\
  */									\
  PC = ((unsigned long *) entry_address);				\
  *PC++ = (((unsigned long) 0x165000d0) +				\
	   (((unsigned long) (code)) << 8));				\
  *PC++ = (((unsigned long) 0x000000aa) +				\
	   (((unsigned long) R10_TRAMPOLINE_TO_INTERFACE_OFFSET)	\
	    << 8));							\
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

/* Instructions aligned on byte boundaries */
#define BYTE_OFFSET_TO_OFFSET_WORD(offset) ((offset) << 1)
#define OFFSET_WORD_TO_BYTE_OFFSET(word) ((CLEAR_LOW_BIT (word)) >> 1)

#define MAKE_OFFSET_WORD(entry, block, continue)                        \
  ((BYTE_OFFSET_TO_OFFSET_WORD(((char *) (entry)) -                     \
                               ((char *) (block)))) |                   \
   ((continue) ? 1 : 0))

#define EXECUTE_CACHE_COUNT_TO_ENTRIES(count) ((count) >> 1)
#define EXECUTE_CACHE_ENTRIES_TO_COUNT(entries) ((entries) << 1)

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
