/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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
extern void * newspace_to_tospace (void *);

bool
read_cc_entry_type (cc_entry_type_t * cet, insn_t * address)
{
  return (decode_old_style_format_word (cet, (((uint16_t *) address) [-6])));
}

bool
write_cc_entry_type (cc_entry_type_t * cet, insn_t * address)
{
  return (encode_old_style_format_word (cet, (((uint16_t *) address) - 6)));
}

bool
read_cc_entry_offset (cc_entry_offset_t * ceo, insn_t * address)
{
  uint16_t n = (((uint16_t *) address) [-5]);
  (ceo->offset) = (n >> 1);
  (ceo->continued_p) = ((n & 1) != 0);
  return (false);
}

bool
write_cc_entry_offset (cc_entry_offset_t * ceo, insn_t * address)
{
  if (! ((ceo->offset) < 0x4000))
    return (true);
  (((uint16_t *) address) [-5])
    = (((ceo->offset) << 1) | ((ceo->continued_p) ? 1 : 0));
  return (false);
}

insn_t *
cc_return_address_to_entry_address (insn_t * pc)
{
  if ((pc[0]) == 0xeb)		/* JMP rel8 */
    return ((pc + 2) + (* ((int8_t *) &pc[1])));
  else if ((pc[0]) == 0xe9)	/* JMP rel32 */
    return ((pc + 5) + (* ((int32_t *) &pc[1])));
  else
    return (pc);
}

/* Compiled closures */

/* start_closure_reloation(scan, ref)

   `scan' points at the manifest of a compiled closure.  Initialize
   `ref' with whatever we need to relocate the entries in it.  */

void
start_closure_relocation (SCHEME_OBJECT * scan, reloc_ref_t * ref)
{
  /* The last element of the block is always the tagged first entry of
     the closure, which tells us where the closure was in oldspace.  */
  (ref->old_addr) = (CC_ENTRY_ADDRESS (* ((CC_BLOCK_ADDR_END (scan)) - 1)));
  /* Find the address of the first entry in newspace.  */
  (ref->new_addr)
    = (tospace_to_newspace
       (compiled_closure_entry (compiled_closure_start (scan + 1))));
}

/* read_compiled_closure_target(start, ref)

   `start' points to the start of a closure entry in tospace, beginning
   with the format word and block offset.  `ref' was initialized with
   `start_closure_relocation'.  Return the untagged compiled entry
   address in oldspace that the closure entry points to.  */

insn_t *
read_compiled_closure_target (insn_t * start, reloc_ref_t * ref)
{
  insn_t * addr = (start + CC_ENTRY_HEADER_SIZE);
  insn_t * base = (tospace_to_newspace (addr));
  /* If we're relocating, find where base was in the oldspace.  */
  if (ref)
    base += (ref->old_addr - ref->new_addr);
  return (base + (((int64_t *) addr)[-1]));
}

/* write_compiled_closure_target(target, start)

   `target' is an untagged compiled entry address in newspace.  `start'
   points to the start of a closure entry in tospace, beginning with
   the format word and block offset.  Set the closure entry at `start'
   to go to `target'.  */

void
write_compiled_closure_target (insn_t * target, insn_t * start)
{
  insn_t * addr = (start + CC_ENTRY_HEADER_SIZE);
  (((int64_t *) addr)[-1]) =
    (target - ((insn_t *) (tospace_to_newspace (addr))));
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
  return (start + CC_ENTRY_HEADER_SIZE + 4);
}

SCHEME_OBJECT *
skip_compiled_closure_padding (insn_t * start)
{
  /* The last entry is _not_ padded, so undo the padding skip.  */
  return ((SCHEME_OBJECT *) (start - 4));
}

SCHEME_OBJECT
compiled_closure_entry_to_target (insn_t * entry)
{
  return (MAKE_CC_ENTRY (entry + (((int64_t *) entry)[-1])));
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
  /* Skip the arity.  */
  insn_t * addr = ((insn_t *) (saddr + 1));
  assert ((addr[0]) == 0x48);
  assert ((addr[1]) == 0xb9);
  /* 0x48 0xb9 <addr> */
  return (* ((insn_t **) (&addr[2])));
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
  (addr[0]) = 0x48;		/* MOV RCX,imm64 */
  (addr[1]) = 0xb9;
  (* ((insn_t **) (&addr[2]))) = target;
  /* If the target PC is right after the target offset, then the PC
     requires no further relocation and we can jump to a fixed address.
     But if the target is a compiled closure pointing into a block
     somewhere else, the block may not have been relocated yet and so
     we don't know where the PC will be in the newspace.  */
  if ((((int64_t *) (newspace_to_tospace (target)))[-1]) == 0)
    {
      ptrdiff_t jmprel32_offset =
	(target - ((const insn_t *) (tospace_to_newspace (&addr[15]))));
      if ((INT32_MIN <= jmprel32_offset) && (jmprel32_offset <= INT32_MAX))
	{
	  (addr[10]) = 0xe9;	/* JMP rel32 */
	  (* ((int32_t *) (&addr[11]))) = jmprel32_offset;
	}
      else
	{
	  (addr[10]) = 0x48;	/* MOV RAX,imm64 */
	  (addr[11]) = 0xb8;
	  (* ((insn_t **) (&addr[12]))) = target;
	  (addr[20]) = 0xff;	/* JMP RAX */
	  (addr[21]) = 0xe0;
	}
    }
  else
    {
      (addr[10]) = 0x48;	/* MOV RAX,-8(RCX) */
      (addr[11]) = 0x8b;
      (addr[12]) = 0x41;
      (addr[13]) = 0xf8;
      (addr[14]) = 0x48;	/* ADD RAX,RCX */
      (addr[15]) = 0x01;
      (addr[16]) = 0xc8;
      (addr[17]) = 0xff;	/* JMP RAX */
      (addr[18]) = 0xe0;
    }
}

#define BYTES_PER_TRAMPOLINE_ENTRY_PADDING 4
#define OBJECTS_PER_TRAMPOLINE_ENTRY 4

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

insn_t *
trampoline_return_addr (SCHEME_OBJECT * block, unsigned long index)
{
  return (trampoline_entry_addr (block, index));
}

bool
store_trampoline_insns (insn_t * entry, uint8_t code)
{
  (((int64_t *) entry)[-1]) = 0;
  (entry[0]) = 0x41;		/* MOVB R9,imm8 */
  (entry[1]) = 0xb1;
  (entry[2]) = code;
  (entry[3]) = 0xff;		/* JMP r/m64 */
  (entry[4]) = 0xa6;		/* disp32(RSI) */
  (* ((uint32_t *) (&entry[5]))) = RSI_TRAMPOLINE_TO_INTERFACE_OFFSET;
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
  SETUP_REGISTER (asm_generic_add);			/* 15 */
  SETUP_REGISTER (asm_generic_subtract);		/* 16 */
  SETUP_REGISTER (asm_generic_multiply);		/* 17 */
  SETUP_REGISTER (asm_generic_divide);			/* 18 */
  SETUP_REGISTER (asm_generic_equal);			/* 19 */
  SETUP_REGISTER (asm_generic_less);			/* 20 */
  SETUP_REGISTER (asm_generic_greater);			/* 21 */
  SETUP_REGISTER (asm_generic_increment);		/* 22 */
  SETUP_REGISTER (asm_generic_decrement);		/* 23 */
  SETUP_REGISTER (asm_generic_zero);			/* 24 */
  SETUP_REGISTER (asm_generic_positive);		/* 25 */
  SETUP_REGISTER (asm_generic_negative);		/* 26 */
  SETUP_REGISTER (asm_generic_quotient);		/* 27 */
  SETUP_REGISTER (asm_generic_remainder);		/* 28 */
  SETUP_REGISTER (asm_generic_modulo);			/* 29 */
  SETUP_REGISTER (asm_sc_apply);			/* 30 */
  SETUP_REGISTER (asm_sc_apply_size_1);			/* 31 */
  SETUP_REGISTER (asm_sc_apply_size_2);			/* 32 */
  SETUP_REGISTER (asm_sc_apply_size_3);			/* 33 */
  SETUP_REGISTER (asm_sc_apply_size_4);			/* 34 */
  SETUP_REGISTER (asm_sc_apply_size_5);			/* 35 */
  SETUP_REGISTER (asm_sc_apply_size_6);			/* 36 */
  SETUP_REGISTER (asm_sc_apply_size_7);			/* 37 */
  SETUP_REGISTER (asm_sc_apply_size_8);			/* 38 */

  /* Logically, this should be up by the other interrupt routines, but
     I just wrote all those numbers above by hand and am too exhausted
     by that gruelling effort to be inclined to go to the trouble to
     renumber them now.  */
  SETUP_REGISTER (asm_interrupt_continuation_2);	/* 39 */

  SETUP_REGISTER (asm_fixnum_shift);			/* 40 */
  SETUP_REGISTER (asm_apply_setup);			/* 41 */
  SETUP_REGISTER (asm_apply_setup_size_1);		/* 42 */
  SETUP_REGISTER (asm_apply_setup_size_2);		/* 43 */
  SETUP_REGISTER (asm_apply_setup_size_3);		/* 44 */
  SETUP_REGISTER (asm_apply_setup_size_4);		/* 45 */
  SETUP_REGISTER (asm_apply_setup_size_5);		/* 46 */
  SETUP_REGISTER (asm_apply_setup_size_6);		/* 47 */
  SETUP_REGISTER (asm_apply_setup_size_7);		/* 48 */
  SETUP_REGISTER (asm_apply_setup_size_8);		/* 49 */

  SETUP_REGISTER (asm_set_interrupt_enables);		/* 50 */

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

#ifndef HAVE_FENV_H
#  define x87_p 1
#  define sse_p 1
#  include "cmpintmd/x86-fenv.c"
#endif
