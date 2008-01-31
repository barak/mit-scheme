/* -*-C-*-

$Id: i386.c,v 1.3 2008/01/30 20:02:24 cph Exp $

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

/* Compiled code interface for Intel IA-32.  */

#include "cmpint.h"
#include "extern.h"
#include "outf.h"
#include "errors.h"

extern void * tospace_to_newspace (void *);

bool
read_cc_entry_type (cc_entry_type_t * cet, insn_t * address)
{
  return (decode_old_style_format_word (cet, (((uint16_t *) address) [-2])));
}

bool
write_cc_entry_type (cc_entry_type_t * cet, insn_t * address)
{
  return (encode_old_style_format_word (cet, ((uint16_t *) address) - 2));
}

bool
read_cc_entry_offset (cc_entry_offset_t * ceo, insn_t * address)
{
  uint16_t n = (((uint16_t *) address) [-1]);
  (ceo->offset) = (n >> 1);
  (ceo->continued_p) = ((n & 1) != 0);
  return (false);
}

bool
write_cc_entry_offset (cc_entry_offset_t * ceo, insn_t * address)
{
  if (! ((ceo->offset) < 0x4000))
    return (true);
  (((uint16_t *) address) [-1])
    = (((ceo->offset) << 1) | ((ceo->continued_p) ? 1 : 0));
  return (false);
}

/* Compiled closures */

void
start_closure_relocation (SCHEME_OBJECT * scan, reloc_ref_t * ref)
{
  (ref->old_addr) = (CC_ENTRY_ADDRESS (* ((CC_BLOCK_ADDR_END (scan)) - 1)));
  (ref->new_addr)
    = (tospace_to_newspace
       (compiled_closure_entry (compiled_closure_start (scan + 1))));
}

insn_t *
read_compiled_closure_target (insn_t * start, reloc_ref_t * ref)
{
  insn_t * addr = (start + CC_ENTRY_HEADER_SIZE + 1);
  return (((((insn_t *) (tospace_to_newspace (addr + 4)))
	    - (ref->new_addr))
	   + (ref->old_addr))
	  + (* ((long *) addr)));
}

void
write_compiled_closure_target (insn_t * target, insn_t * start)
{
  insn_t * addr = (start + CC_ENTRY_HEADER_SIZE + 1);
  (* ((long *) addr))
    = (target - ((insn_t *) (tospace_to_newspace (addr + 4))));
}

#define SINGLE_CLOSURE_OFFSET						\
  (((sizeof (SCHEME_OBJECT)) + CC_ENTRY_HEADER_SIZE) << 1)

unsigned long
compiled_closure_count (SCHEME_OBJECT * block)
{
  unsigned short * addr = ((unsigned short *) block);
  return (((addr[1]) == SINGLE_CLOSURE_OFFSET) ? 1 : (addr[0]));
}

insn_t *
compiled_closure_start (SCHEME_OBJECT * block)
{
  unsigned short * addr = ((unsigned short *) block);
  return
    (((insn_t *) block)
     + (((addr[1]) == SINGLE_CLOSURE_OFFSET)
	? 0
	: CC_ENTRY_HEADER_SIZE));
}

insn_t *
compiled_closure_entry (insn_t * start)
{
  return (start + CC_ENTRY_HEADER_SIZE);
}

insn_t *
compiled_closure_next (insn_t * start)
{
  return (start + CC_ENTRY_HEADER_SIZE + 6);
}

SCHEME_OBJECT *
skip_compiled_closure_padding (insn_t * start)
{
  return
    ((SCHEME_OBJECT *)
     ((((unsigned long) start) + ((sizeof (SCHEME_OBJECT)) - 1))
      &~ ((sizeof (SCHEME_OBJECT)) - 1)));
}

SCHEME_OBJECT
compiled_closure_entry_to_target (insn_t * entry)
{
  insn_t * addr = (entry + 1);
  return (MAKE_CC_ENTRY ((addr + 4) + (* ((long *) addr))));
}

/* Execution caches (UUO links)

   An execution cache is a region of memory that lives in the
   constants section of a compiled-code block.  It is an indirection
   for calling external procedures that allows the linker to control
   the calling process without having to find and change all the
   places in the compiled code that refer to it.

   Prior to linking, the execution cache has two pieces of
   information: (1) the name of the procedure being called (a symbol),
   and (2) the number of arguments that will be passed to the
   procedure.  */

SCHEME_OBJECT
read_uuo_symbol (SCHEME_OBJECT * saddr)
{
  return (saddr[1]);
}

unsigned int
read_uuo_frame_size (SCHEME_OBJECT * saddr)
{
  return (* ((unsigned short *) saddr));
}

void
start_operator_relocation (SCHEME_OBJECT * saddr, reloc_ref_t * ref)
{
  insn_t * nsaddr = (tospace_to_newspace (saddr));
  (ref->old_addr) = (* ((insn_t **) saddr));
  (ref->new_addr) = nsaddr;
  (* ((insn_t **) saddr)) = nsaddr;
}

insn_t *
read_uuo_target (SCHEME_OBJECT * saddr, reloc_ref_t * ref)
{
  insn_t * addr = (((insn_t *) saddr) + 4);
  insn_t * base = (tospace_to_newspace (addr + 4));
  return (((ref == 0)
	   ? base
	   : ((base - (ref->new_addr)) + (ref->old_addr)))
	  + (* ((long *) addr)));
}

insn_t *
read_uuo_target_no_reloc (SCHEME_OBJECT * saddr)
{
  return (read_uuo_target (saddr, 0));
}

void
write_uuo_target (insn_t * target, SCHEME_OBJECT * saddr)
{
  insn_t * addr = (((insn_t *) saddr) + 3);
  (*addr++) = 0xE9;		/* JMP rel32 */
  (* ((long *) addr))
    = (target - ((insn_t *) (tospace_to_newspace (addr + 4))));
}

#define TRAMPOLINE_ENTRY_SIZE 3

#define ESI_TRAMPOLINE_TO_INTERFACE_OFFSET				\
  ((COMPILER_REGBLOCK_N_FIXED + (2 * COMPILER_HOOK_SIZE))		\
   * SIZEOF_SCHEME_OBJECT)

unsigned long
trampoline_entry_size (unsigned long n_entries)
{
  return (n_entries * TRAMPOLINE_ENTRY_SIZE);
}

insn_t *
trampoline_entry_addr (SCHEME_OBJECT * block, unsigned long index)
{
  return (((insn_t *) (block + 2 + (index * TRAMPOLINE_ENTRY_SIZE)))
	  + CC_ENTRY_HEADER_SIZE);
}

bool
store_trampoline_insns (insn_t * entry, byte_t code)
{
  (*entry++) = 0xB0;		/* MOV AL,code */
  (*entry++) = code;
  (*entry++) = 0xFF;		/* CALL /2 disp32(ESI) */
  (*entry++) = 0x96;
  (* ((unsigned long *) entry)) = ESI_TRAMPOLINE_TO_INTERFACE_OFFSET;
  IA32_CACHE_SYNCHRONIZE ();
  return (false);
}

#ifdef _MACH_UNIX
#  include <mach.h>
#  define VM_PROT_SCHEME (VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE)
#endif

#define SETUP_REGISTER(hook) do						\
{									\
  (* ((unsigned long *) (esi_value + offset)))				\
    = ((unsigned long) (hook));						\
  offset += (COMPILER_HOOK_SIZE * (sizeof (SCHEME_OBJECT)));		\
  declare_builtin (((unsigned long) hook), #hook);			\
} while (0)

void
i386_reset_hook (void)
{
  unsigned int offset = (COMPILER_REGBLOCK_N_FIXED * (sizeof (SCHEME_OBJECT)));
  unsigned char * esi_value = ((unsigned char *) Registers);
  bool fp_support_present = (i386_interface_initialize ());

  /* These must match machines/i386/lapgen.scm */

  SETUP_REGISTER (asm_scheme_to_interface); 		/* 0 */
  SETUP_REGISTER (asm_scheme_to_interface_call);	/* 1 */

  if (offset != ESI_TRAMPOLINE_TO_INTERFACE_OFFSET)
    {
      outf_fatal ("\ni386_reset_hook: ESI_TRAMPOLINE_TO_INTERFACE_OFFSET\n");
      Microcode_Termination (TERM_EXIT);
    }
  SETUP_REGISTER (asm_trampoline_to_interface);		/* 2 */

  SETUP_REGISTER (asm_interrupt_procedure);		/* 3 */
  SETUP_REGISTER (asm_interrupt_continuation);		/* 4 */
  SETUP_REGISTER (asm_interrupt_closure);		/* 5 */
  SETUP_REGISTER (asm_interrupt_dlink);			/* 6 */
  SETUP_REGISTER (asm_primitive_apply);			/* 7 */
  SETUP_REGISTER (asm_primitive_lexpr_apply);		/* 8 */
  SETUP_REGISTER (asm_assignment_trap);			/* 9 */
  SETUP_REGISTER (asm_reference_trap);			/* 10 */
  SETUP_REGISTER (asm_safe_reference_trap);		/* 11 */
  SETUP_REGISTER (asm_link);				/* 12 */
  SETUP_REGISTER (asm_error);				/* 13 */
  SETUP_REGISTER (asm_primitive_error);			/* 14 */
  SETUP_REGISTER (asm_short_primitive_apply);		/* 15 */

  /* No more room for positive offsets without going to 32-bit
     offsets!  */

  /* This is a hack to make all the hooks be addressable with byte
     offsets (instead of longword offsets).  The register block
     extends to negative offsets as well, so all the following hooks
     are accessed with negative offsets, and all fit in a byte.  */

  offset = -128;
  if (fp_support_present)
    {
      SETUP_REGISTER (asm_generic_add);			/* -32 */
      SETUP_REGISTER (asm_generic_subtract);		/* -31 */
      SETUP_REGISTER (asm_generic_multiply);		/* -30 */
      SETUP_REGISTER (asm_generic_divide);		/* -29 */
      SETUP_REGISTER (asm_generic_equal);		/* -28 */
      SETUP_REGISTER (asm_generic_less);		/* -27 */
      SETUP_REGISTER (asm_generic_greater);		/* -26 */
      SETUP_REGISTER (asm_generic_increment);		/* -25 */
      SETUP_REGISTER (asm_generic_decrement);		/* -24 */
      SETUP_REGISTER (asm_generic_zero);		/* -23 */
      SETUP_REGISTER (asm_generic_positive);		/* -22 */
      SETUP_REGISTER (asm_generic_negative);		/* -21 */
      SETUP_REGISTER (asm_generic_quotient);		/* -20 */
      SETUP_REGISTER (asm_generic_remainder);		/* -19 */
      SETUP_REGISTER (asm_generic_modulo);		/* -18 */
    }
  else
    {
      SETUP_REGISTER (asm_nofp_add);			/* -32 */
      SETUP_REGISTER (asm_nofp_subtract);		/* -31 */
      SETUP_REGISTER (asm_nofp_multiply);		/* -30 */
      SETUP_REGISTER (asm_nofp_divide);			/* -29 */
      SETUP_REGISTER (asm_nofp_equal);			/* -28 */
      SETUP_REGISTER (asm_nofp_less);			/* -27 */
      SETUP_REGISTER (asm_nofp_greater);		/* -26 */
      SETUP_REGISTER (asm_nofp_increment);		/* -25 */
      SETUP_REGISTER (asm_nofp_decrement);		/* -24 */
      SETUP_REGISTER (asm_nofp_zero);			/* -23 */
      SETUP_REGISTER (asm_nofp_positive);		/* -22 */
      SETUP_REGISTER (asm_nofp_negative);		/* -21 */
      SETUP_REGISTER (asm_nofp_quotient);		/* -20 */
      SETUP_REGISTER (asm_nofp_remainder);		/* -19 */
      SETUP_REGISTER (asm_nofp_modulo);			/* -18 */
    }

  SETUP_REGISTER (asm_sc_apply);			/* -17 */
  SETUP_REGISTER (asm_sc_apply_size_1);			/* -16 */
  SETUP_REGISTER (asm_sc_apply_size_2);			/* -15 */
  SETUP_REGISTER (asm_sc_apply_size_3);			/* -14 */
  SETUP_REGISTER (asm_sc_apply_size_4);			/* -13 */
  SETUP_REGISTER (asm_sc_apply_size_5);			/* -12 */
  SETUP_REGISTER (asm_sc_apply_size_6);			/* -11 */
  SETUP_REGISTER (asm_sc_apply_size_7);			/* -10 */
  SETUP_REGISTER (asm_sc_apply_size_8);			/* -9 */
  SETUP_REGISTER (asm_interrupt_continuation_2);	/* -8 */
  if (ia32_cpuid_needed)
    SETUP_REGISTER (asm_serialize_cache);		/* -7 */
  else
    SETUP_REGISTER (asm_dont_serialize_cache);		/* -7 */

#ifdef _MACH_UNIX
  {
    vm_address_t addr;
    vm_size_t size;
    vm_prot_t prot;
    vm_prot_t max_prot;
    vm_inherit_t inheritance;
    boolean_t shared;
    port_t object;
    vm_offset_t offset;

    addr = ((vm_address_t) Heap);
    if ((vm_region ((task_self ()), &addr, &size, &prot, &max_prot,
		    &inheritance, &shared, &object, &offset))
	!= KERN_SUCCESS)
      {
	outf_fatal ( "compiler_reset: vm_region() failed.\n");
	Microcode_Termination (TERM_EXIT);
	/*NOTREACHED*/
      }
    if ((prot & VM_PROT_SCHEME) != VM_PROT_SCHEME)
      {
	if ((max_prot & VM_PROT_SCHEME) != VM_PROT_SCHEME)
	  {
	    outf_fatal (
			"compiler_reset: inadequate protection for Heap.\n");
	    outf_fatal ( "maximum = 0x%lx; desired = 0x%lx\n",
			((unsigned long) (max_prot & VM_PROT_SCHEME)),
			((unsigned long) VM_PROT_SCHEME));
	    Microcode_Termination (TERM_EXIT);
	    /*NOTREACHED*/
	  }
	if ((vm_protect ((task_self ()), ((vm_address_t) Heap),
			 (((char *) constant_end) - ((char *) Heap)),
			 0, VM_PROT_SCHEME))
	    != KERN_SUCCESS)
	  {
	    outf_fatal ("Unable to change protection for Heap.\n");
	    outf_fatal ("actual = 0x%lx; desired = 0x%lx\n",
			((unsigned long) (prot & VM_PROT_SCHEME)),
			((unsigned long) VM_PROT_SCHEME));
	    Microcode_Termination (TERM_EXIT);
	    /*NOTREACHED*/
	  }
      }
  }
#endif /* _MACH_UNIX */
}
