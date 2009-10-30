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

/* Compiled code interface for AMD x86-64.  */

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

/* MOV RAX,imm64 has two bytes of opcode cruft before the imm64.  */

insn_t *
read_compiled_closure_target (insn_t * start)
{
  return (* ((insn_t **) (start + CC_ENTRY_HEADER_SIZE + 2)));
}

void
write_compiled_closure_target (insn_t * target, insn_t * start)
{
  (* ((insn_t **) (start + CC_ENTRY_HEADER_SIZE + 2))) = target;
}

unsigned long
compiled_closure_count (SCHEME_OBJECT * block)
{
  /* `block' is a pointer to the first object after the manifest.  The
     first object following it is the entry count.  */
  return ((unsigned long) (* ((uint32_t *) block)));
}

insn_t *
compiled_closure_start (SCHEME_OBJECT * block)
{
  /* Skip the 32-bit entry count.  */
  return (((insn_t *) block) + 4);
}

insn_t *
compiled_closure_entry (insn_t * start)
{
  return (start + CC_ENTRY_HEADER_SIZE);
}

insn_t *
compiled_closure_next (insn_t * start)
{
  return (start + CC_ENTRY_HEADER_SIZE + 12);
}

SCHEME_OBJECT *
skip_compiled_closure_padding (insn_t * start)
{
  /* The padding is the same size as the entry header (format word).  */
  return ((SCHEME_OBJECT *) (start + CC_ENTRY_HEADER_SIZE));
}

SCHEME_OBJECT
compiled_closure_entry_to_target (insn_t * entry)
{
  /* `entry' points to the start of the MOV RAX,imm64 instruction,
     which has two bytes of opcode cruft before the imm64.  */
  return (MAKE_CC_ENTRY (* ((long *) (entry + 2))));
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
   procedure.  `saddr' points to the arity at the beginning of the
   execution cache.  */

SCHEME_OBJECT
read_uuo_symbol (SCHEME_OBJECT * saddr)
{
  return (saddr[1]);
}

unsigned int
read_uuo_frame_size (SCHEME_OBJECT * saddr)
{
  return (* ((uint16_t *) saddr));
}

insn_t *
read_uuo_target (SCHEME_OBJECT * saddr)
{
  insn_t * mov_addr = ((insn_t *) (saddr + 1));
  return (* ((insn_t **) (mov_addr + 2)));
}

insn_t *
read_uuo_target_no_reloc (SCHEME_OBJECT * saddr)
{
  return (read_uuo_target (saddr));
}

void
write_uuo_target (insn_t * target, SCHEME_OBJECT * saddr)
{
  /* Skip the arity. */
  insn_t * addr = ((insn_t *) (saddr + 1));
  (*addr++) = 0x48;		/* REX.W (64-bit operand size prefix) */
  (*addr++) = 0xB8;		/* MOV RAX,imm64 */
  (* ((insn_t **) addr)) = target;
  addr += 8;
  (*addr++) = 0xFF;		/* JMP reg/mem64 */
  (*addr++) = 0xE0;		/* ModR/M for RAX */
}

#define BYTES_PER_TRAMPOLINE_ENTRY_PADDING 4
#define OBJECTS_PER_TRAMPOLINE_ENTRY 2

#define RSI_TRAMPOLINE_TO_INTERFACE_OFFSET				\
  ((COMPILER_REGBLOCK_N_FIXED + (2 * COMPILER_HOOK_SIZE))		\
   * SIZEOF_SCHEME_OBJECT)

unsigned long
trampoline_entry_size (unsigned long n_entries)
{
  return (n_entries * OBJECTS_PER_TRAMPOLINE_ENTRY);
}

insn_t *
trampoline_entry_addr (SCHEME_OBJECT * block, unsigned long index)
{
  return (((insn_t *) (block + 2 + (index * OBJECTS_PER_TRAMPOLINE_ENTRY)))
	  + BYTES_PER_TRAMPOLINE_ENTRY_PADDING + CC_ENTRY_HEADER_SIZE);
}

bool
store_trampoline_insns (insn_t * entry, byte_t code)
{
  (*entry++) = 0xB0;		/* MOV AL,code */
  (*entry++) = code;
  (*entry++) = 0xFF;		/* CALL /2 disp32(ESI) */
  (*entry++) = 0x96;
  (* ((uint32_t *) entry)) = RSI_TRAMPOLINE_TO_INTERFACE_OFFSET;
  X86_64_CACHE_SYNCHRONIZE ();
  return (false);
}

#ifdef _MACH_UNIX
#  include <mach.h>
#  define VM_PROT_SCHEME (VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE)
#endif

#define SETUP_REGISTER(hook) do						\
{									\
  (* ((unsigned long *) (rsi_value + offset)))				\
    = ((unsigned long) (hook));						\
  offset += (COMPILER_HOOK_SIZE * (sizeof (SCHEME_OBJECT)));		\
  declare_builtin (((unsigned long) hook), #hook);			\
} while (0)

void
x86_64_reset_hook (void)
{
  int offset = (COMPILER_REGBLOCK_N_FIXED * (sizeof (SCHEME_OBJECT)));
  unsigned char * rsi_value = ((unsigned char *) Registers);
  bool fp_support_present = (x86_64_interface_initialize ());

  /* These must match machines/x86-64/lapgen.scm */

  SETUP_REGISTER (asm_scheme_to_interface); 		/* 0 */
  SETUP_REGISTER (asm_scheme_to_interface_call);	/* 1 */

  if (offset != RSI_TRAMPOLINE_TO_INTERFACE_OFFSET)
    {
      outf_fatal ("\nx86_64_reset_hook: RSI_TRAMPOLINE_TO_INTERFACE_OFFSET\n");
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
  /* [TRC 20091025: This was an i386 hack for when the PC is not
     available, which on x86-64 it always is. */
  /* SETUP_REGISTER (asm_short_primitive_apply); */		/* 15 */

  /* No more room for positive offsets without going to 32-bit
     offsets!  */

  /* This is a hack to make all the hooks be addressable with byte
     offsets (instead of longword offsets).  The register block
     extends to negative offsets as well, so all the following hooks
     are accessed with negative offsets, and all fit in a byte.  */

  /* [TRC 20091029: This hack doesn't work any longer; this code
     should be cleaned up, since we must use longword offsets anyway.]
     */

  offset = -256;
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
  /* [TRC 20091025: The cache synchronization bug does not occur in any
      x86-64 machines of which I am aware.]

  if (x86_64_cpuid_needed)
    SETUP_REGISTER (asm_serialize_cache);		/\* -7 *\/
  else
    SETUP_REGISTER (asm_dont_serialize_cache);		/\* -7 *\/
  */

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
