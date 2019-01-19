/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

/* Compiled code interface for AArch64.  */

#include "cmpint.h"
#include "prims.h"

extern void * tospace_to_newspace (void *);
extern void * newspace_to_tospace (void *);

#define TYPE_ARITY_MASK	(UINT32_C (0x0000ffff))
#define TYPE_ARITY_SHIFT 0
#define BLOCK_OFFSET_MASK (UINT32_C (0xffff0000))
#define BLOCK_OFFSET_SHIFT 16

bool
read_cc_entry_type (cc_entry_type_t * cet, insn_t * address)
{
  uint32_t word = (address[-3]);
  uint16_t type_arity = ((word & TYPE_ARITY_MASK) >> TYPE_ARITY_SHIFT);
  return (decode_old_style_format_word (cet, type_arity));
}

bool
write_cc_entry_type (cc_entry_type_t * cet, insn_t * address)
{
  uint16_t type_arity;
  bool error = (encode_old_style_format_word (cet, (&type_arity)));
  if (error)
    return (error);
  (address[-3]) &=~ TYPE_ARITY_MASK;
  (address[-3]) |= (type_arity << TYPE_ARITY_SHIFT);
  return (false);
}

bool
read_cc_entry_offset (cc_entry_offset_t * ceo, insn_t * address)
{
  const size_t units = ((sizeof (SCHEME_OBJECT)) / (sizeof (insn_t)));
  assert (units == 2);
  uint32_t word = (address[-3]);
  uint16_t n = ((word & BLOCK_OFFSET_MASK) >> BLOCK_OFFSET_SHIFT);
  /* Block offsets are stored in units of Scheme objects.  */
  (ceo->offset) = (units * (n >> 1));
  (ceo->continued_p) = ((n & 1) != 0);
  return (false);
}

bool
write_cc_entry_offset (cc_entry_offset_t * ceo, insn_t * address)
{
  const size_t units = ((sizeof (SCHEME_OBJECT)) / (sizeof (insn_t)));
  assert (units == 2);
  assert (((ceo->offset) % units) == 0);
  if (! ((ceo->offset) < 0x4000))
    return (true);
  (address[-3]) &=~ BLOCK_OFFSET_MASK;
  (address[-3]) |=
    (((((ceo->offset) / units) << 1) | ((ceo->continued_p) ? 1 : 0))
     << BLOCK_OFFSET_SHIFT);
  return (false);
}

insn_t *
cc_return_address_to_entry_address (insn_t * pc)
{
  insn_t insn = (pc[0]);
  if ((insn & 0xfc000000UL) == 0x14000000UL) /* B */
    return (pc + (insn & 0x3fffffff));
  else
    /* XXX What if it got branch-tensioned?  */
    error_external_return ();
}

/* Compiled closures */

/* start_closure_reloation (scan, ref)

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

/* read_compiled_closure_target (start, ref)

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
  return ((insn_t *) block);
}

insn_t *
compiled_closure_entry (insn_t * start)
{
  return (start + CC_ENTRY_PADDING_SIZE + CC_ENTRY_HEADER_SIZE);
}

insn_t *
compiled_closure_next (insn_t * start)
{
  return (start + CC_ENTRY_PADDING_SIZE + CC_ENTRY_HEADER_SIZE);
}

SCHEME_OBJECT *
skip_compiled_closure_padding (insn_t * start)
{
  return ((SCHEME_OBJECT *) start);
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
  return (saddr[0]);
}

unsigned int
read_uuo_frame_size (SCHEME_OBJECT * saddr)
{
#ifdef WORDS_BIGENDIAN
  return ((saddr[1]) & 0xffff);
#else
  return ((saddr[2]) & 0xffff);
#endif
}

insn_t *
read_uuo_target (SCHEME_OBJECT * saddr)
{
  return ((insn_t *) (saddr[0]));
}

insn_t *
read_uuo_target_no_reloc (SCHEME_OBJECT * saddr)
{
  return (read_uuo_target (saddr));
}

static void
write_uuo_insns (insn_t * target, insn_t * iaddr, int pcrel)
{
  /* ldr x1, pc-pcrel */
  (iaddr[0]) = (0x58000001UL | ((((unsigned) pcrel) & 0x7ffff) << 5));

  /* If the target PC is right after the target offset, then the PC
     requires no further relocation and we can jump to a fixed address.
     But if the target is a compiled closure pointing into a block
     somewhere else, the block may not have been relocated yet and so
     we don't know where the PC will be in the newspace.  */
  if ((((const int64_t *) (newspace_to_tospace (target)))[-1]) == 0)
    {
      ptrdiff_t offset = (((uintptr_t) target) - ((uintptr_t) (&iaddr[1])));
      assert ((offset & 3) == 0);
      if ((-0x10000000 <= offset) && (offset <= 0xfffffff))
	{
	  unsigned imm26 = ((offset >> 2) & 0x03ffffff);
	  /* b target */
	  (iaddr[1]) = (0x14000000UL | imm26);
	}
      else if (((- (INT64_C (0x200000000))) <= offset) &&
	       (offset <= (INT64_C (0x1ffffffff))))
	{
	  unsigned long lo12 = (offset & 0xfff);
	  unsigned long pglo2 = ((((unsigned long) offset) >> 12) & 3);
	  unsigned long pghi19 = ((((unsigned long) offset) >> 14) & 0x1ffff);
	  assert
	    (offset == ((ptrdiff_t) ((pghi19 << 14) | (pglo2 << 12) | lo12)));
	  /* adrp x1, target */
	  (iaddr[1]) = (0x90000001UL | (pglo2 << 29) | (pghi19 << 5));
	  /* add x17, x17, #off */
	  (iaddr[2]) = (0x91000031UL | (lo12 << 10));
	  /* br x17 */
	  (iaddr[3]) = 0xd61f0022UL;
	}
      else
	/* You have too much memory.  */
	error_external_return ();
    }
  else
    {
      (iaddr[1]) = 0xd1002031UL; /* sub x17, x1, #8 */
      (iaddr[2]) = 0xf9400231UL; /* ldr x17, [x17] */
      (iaddr[3]) = 0x8b010231UL; /* add x17, x17, x1 */
      (iaddr[4]) = 0xd61f0022UL; /* br x17 */
    }
}

void
write_uuo_target (insn_t * target, SCHEME_OBJECT * saddr)
{
  insn_t * iaddr;
  int ioff;

#ifdef WORDS_BIGENDIAN
  ioff = 2;
#else
  ioff = 3;
#endif

  (saddr[0]) = ((SCHEME_OBJECT) target);
  iaddr = (((insn_t *) saddr) + ioff);
  write_uuo_insns (target, iaddr, -ioff);
}

#define TRAMPOLINE_ENTRY_PADDING_SIZE 1
#define OBJECTS_PER_TRAMPOLINE_ENTRY 4

unsigned long
trampoline_entry_size (unsigned long n_entries)
{
  return (n_entries * OBJECTS_PER_TRAMPOLINE_ENTRY);
}

insn_t *
trampoline_entry_addr (SCHEME_OBJECT * block, unsigned long index)
{
  return (((insn_t *) (block + 2 + (index * OBJECTS_PER_TRAMPOLINE_ENTRY)))
	  + TRAMPOLINE_ENTRY_PADDING_SIZE + CC_ENTRY_HEADER_SIZE);
}

insn_t *
trampoline_return_addr (SCHEME_OBJECT * block, unsigned long index)
{
  return (trampoline_entry_addr (block, index));
}

bool
store_trampoline_insns (insn_t * entry, uint8_t code)
{
  (entry[-2]) = 0;		/* PC offset, first half */
  (entry[-1]) = 0;		/* PC offset, other half */
  /* movz x17, #code */
  (entry[0]) = (0xd2800011UL | (((unsigned) code) << 5));
  /* adr x1, storage (pc + 12) */
  (entry[1]) = 0x10000061UL;
  /* br x23 (scheme-to-interface) */
  (entry[2]) = 0xd61f02e0UL;
  return (false);		/* no error */
}

void
aarch64_reset_hook (void)
{
  /* XXX Make sure we're mapped write and execute.  (Such is the state...)  */
}
