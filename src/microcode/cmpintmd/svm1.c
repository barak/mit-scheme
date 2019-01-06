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

/* Compiled code interface for SVM v1. */

#include "cmpint.h"
#include "extern.h"
#include "errors.h"
#include "svm1-defns.h"

static void write_u16 (unsigned int, insn_t *);

bool
read_cc_entry_type (cc_entry_type_t * cet, insn_t * address)
{
  unsigned int n;

  if ((*address) == SVM1_INST_ENTER_CLOSURE)
    {
      n = read_u16 (address - 2);
      make_compiled_procedure_type
	(cet, (n & 0x007F), ((n & 0x3F80) >> 7), ((n & 0x4000) != 0));
      return (false);
    }
  n = (read_u16 (address - 4));
  if (n < 0x8000)
    make_compiled_procedure_type
      (cet,
       (n & 0x007F),
       ((n & 0x3F80) >> 7),
       ((n & 0x4000) != 0));
  else if (n < 0xFFF8)
    make_compiled_continuation_type (cet, (n - 0x8000));
  else
    switch (n - 0xFFF8)
      {
      case 6:
	make_cc_entry_type (cet, CET_EXPRESSION);
	break;

      case 5:
	make_cc_entry_type (cet, CET_INTERNAL_PROCEDURE);
	break;

      case 4:
	make_cc_entry_type (cet, CET_INTERNAL_CONTINUATION);
	break;

      case 3:
	make_cc_entry_type (cet, CET_TRAMPOLINE);
	break;

      case 2:
	make_cc_entry_type (cet, CET_RETURN_TO_INTERPRETER);
	break;

      default:
	return (true);
      }
  return (false);
}

/* This is used only for creating trampolines.  */

bool
write_cc_entry_type (cc_entry_type_t * cet, insn_t * address)
{
  unsigned int n;

  switch (cet->marker)
    {
    case CET_PROCEDURE:
      if (! (((cet->args.for_procedure.n_required) < 0x80)
	     && ((cet->args.for_procedure.n_optional) < 0x80)))
	return (true);
      n = ((cet->args.for_procedure.n_required)
	   | ((cet->args.for_procedure.n_optional) << 7)
	   | ((cet->args.for_procedure.rest_p) ? 0x4000 : 0));
      break;

    case CET_CONTINUATION:
      if (! ((cet->args.for_continuation.offset) < 0x7FF8))
	return (true);
      n = ((cet->args.for_continuation.offset) + 0x8000);
      break;

    case CET_EXPRESSION:
      n = (0xFFF8 + 6);
      break;

    case CET_INTERNAL_PROCEDURE:
      n = (0xFFF8 + 5);
      break;

    case CET_INTERNAL_CONTINUATION:
      n = (0xFFF8 + 4);
      break;

    case CET_TRAMPOLINE:
      n = (0xFFF8 + 3);
      break;

    case CET_RETURN_TO_INTERPRETER:
      n = (0xFFF8 + 2);
      break;

    default:
      return (true);
    }
  write_u16 (n, (address - 4));
  return (false);
}

bool
read_cc_entry_offset (cc_entry_offset_t * ceo, insn_t * address)
{
  if ((*address) == SVM1_INST_ENTER_CLOSURE)
    {
      unsigned int index = (read_u16 (address + 1));
      (ceo->offset)
	= ((sizeof (SCHEME_OBJECT))
	   + CLOSURE_ENTRY_OFFSET + (index * CLOSURE_ENTRY_SIZE));
      (ceo->continued_p) = false;
    }
  else
    {
      unsigned int n = (read_u16 (address - 2));
      ceo->offset = (n >> 1);
      ceo->continued_p = ((n & 1) != 0);
    }
  return (false);
}

/* This is used only for creating trampolines.  */

bool
write_cc_entry_offset (cc_entry_offset_t * ceo, insn_t * address)
{
  unsigned int code = (ceo->offset) << 1;
  write_u16 (code + (ceo->continued_p ? 1 : 0), address - 2);
  return (false);
}

unsigned int
read_u16 (insn_t * address)
{
  return
    ((((unsigned int) (address[1])) << 8)
     | ((unsigned int) (address[0])));
}

static void
write_u16 (unsigned int n, insn_t * address)
{
  (address[0]) = (n & 0x00FF);
  (address[1]) = (n >> 8);
}

/* Compiled closures

   A compiled-closure block starts with a single GC header
   (TC_MANIFEST_CLOSURE), followed by a 2-byte count, followed by the
   closure entries (as specified by the count).  The closure entries
   refer to their targets indirectly: the targets appear in sequence
   after all of the entries and are stored as Scheme objects.

   For example, on a 32-bit machine with count == 3 and 4 value cells:

   0x00    TC_MANIFEST_CLOSURE | n_words == 12

   0x04    count == 3
   0x06    2 cc-entry type bytes (next address must be word-aligned)

   0x08    SVM1_INST_ENTER_CLOSURE
   0x09    index == 0

   0x0B    2 cc-entry type (arity) bytes
   0x0D    SVM1_INST_ENTER_CLOSURE
   0x0E    index == 1

   0x10    2 cc-entry type (arity) bytes
   0x12    SVM1_INST_ENTER_CLOSURE
   0x13    index == 2

   0x15    3 padding bytes (next address must be word-aligned)

   0x18    target 0
   0x1C    target 1
   0x20    target 2

   0x24    value cell 0
   0x28    value cell 1
   0x2C    value cell 2
   0x30    value cell 3

   On a 64-bit machine:

   0x00    TC_MANIFEST_CLOSURE | n_words == 10

   0x08    count == 3
   0x0A    4 unused bytes
   0x0E    2 cc-entry type (arity) bytes

   0x10    SVM1_INST_ENTER_CLOSURE (this first entry must be word aligned)
   0x11    index == 0
   0x13    2 cc-entry type (arity) bytes
   0x15    SVM1_INST_ENTER_CLOSURE
   0x16    index == 1

   0x18    2 cc-entry type (arity) bytes
   0x1A    SVM1_INST_ENTER_CLOSURE
   0x1B    index == 2
   0x1D    3 padding bytes

   0x20    target 0
   0x28    target 1
   0x30    target 2

   0x38    value cell 0
   0x40    value cell 1
   0x48    value cell 2
   0x50    value cell 3

   */

unsigned long
compiled_closure_count (SCHEME_OBJECT * block)
{
  return (read_u16 ((insn_t *) block));
}

insn_t *
compiled_closure_start (SCHEME_OBJECT * block)
{
  return (((insn_t *) block) + CLOSURE_ENTRY_START);
}

insn_t *
compiled_closure_next (insn_t * start)
{
  return (start + CLOSURE_ENTRY_SIZE);
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
  unsigned int index = (read_u16 (entry + 1));
  insn_t * block
    = (entry - (CLOSURE_ENTRY_OFFSET + (index * CLOSURE_ENTRY_SIZE)));
  unsigned int count = (read_u16 (block));
  SCHEME_OBJECT * targets
    = (skip_compiled_closure_padding
       (block + (CLOSURE_ENTRY_START + (count * CLOSURE_ENTRY_SIZE))));
  return (targets[index]);
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
   procedure.  It is laid out in memory like this (on a 32-bit
   machine):

   0x00    frame-size (fixnum)
   0x04    name encoded as symbol

   After linking, the cache is changed as follows:

   0x00    frame-size (u16)
   0x02    SVM1_INST_IJUMP_U8
   0x03    offset = 0
   0x04    32-bit address

   On a 64-bit machine, the post-linking layout is:

   0x00    frame-size (u16)
   0x02    4 padding bytes
   0x06    SVM1_INST_IJUMP_U8
   0x07    offset = 0
   0x08    64-bit address

   */

unsigned int
read_uuo_frame_size (SCHEME_OBJECT * saddr)
{
  return (read_u16 ((insn_t *) saddr));
}

SCHEME_OBJECT
read_uuo_symbol (SCHEME_OBJECT * saddr)
{
  return (saddr[1]);
}

insn_t *
read_uuo_target (SCHEME_OBJECT * saddr)
{
  insn_t * addr = ((insn_t *) (saddr + 2));
  insn_t * end = ((insn_t *) (saddr + 1));
  unsigned long eaddr = 0;

  while (true)
    {
      eaddr |= (*--addr);
      if (addr == end)
	return ((insn_t *) eaddr);
      eaddr <<= 8;
    }
}

insn_t *
read_uuo_target_no_reloc (SCHEME_OBJECT * saddr)
{
  return (read_uuo_target (saddr));
}

void
write_uuo_target (insn_t * target, SCHEME_OBJECT * saddr)
{
  unsigned long eaddr = ((unsigned long) target);
  unsigned long frame_size = (OBJECT_DATUM (saddr[0]));
  insn_t * addr = ((insn_t *) saddr);
  insn_t * end = ((insn_t *) (saddr + 1));

  (*addr++) = (frame_size & 0x00FF);
  (*addr++) = ((frame_size & 0xFF00) >> 8);
  while (addr < (end - 2))
    (*addr++) = 0;
  (*addr++) = SVM1_INST_IJUMP_U8;
  (*addr++) = 0;

  end = ((insn_t *) (saddr + 2));
  while (true)
    {
      (*addr++) = (eaddr & 0xFF);
      if (addr == end)
	break;
      eaddr >>= 8;
    }
}

unsigned long
trampoline_entry_size (unsigned long n_entries)
{
  return (BYTES_TO_WORDS (n_entries * (CC_ENTRY_HEADER_SIZE + 2)));
}

insn_t *
trampoline_entry_addr (SCHEME_OBJECT * block, unsigned long index)
{
  return (((insn_t *) (block + 2))
	  + (index * (CC_ENTRY_HEADER_SIZE + 2))
	  + CC_ENTRY_HEADER_SIZE);
}

bool
store_trampoline_insns (insn_t * entry, uint8_t code)
{
  (entry[0]) = SVM1_INST_TRAP_TRAP_0;
  switch (code)
    {
    case TRAMPOLINE_K_RETURN_TO_INTERPRETER:
      entry[1] = SVM1_TRAP_0_RETURN_TO_INTERPRETER; break;
    case TRAMPOLINE_K_APPLY:
      entry[1] = SVM1_TRAP_0_OPERATOR_APPLY; break;
    case TRAMPOLINE_K_LEXPR_PRIMITIVE:
      entry[1] = SVM1_TRAP_0_OPERATOR_LEXPR; break;
    case TRAMPOLINE_K_PRIMITIVE:
      entry[1] = SVM1_TRAP_0_OPERATOR_PRIMITIVE; break;
    case TRAMPOLINE_K_LOOKUP:
      entry[1] = SVM1_TRAP_0_OPERATOR_LOOKUP; break;
    case TRAMPOLINE_K_1_0:
      entry[1] = SVM1_TRAP_0_OPERATOR_1_0; break;
    case TRAMPOLINE_K_2_1:
      entry[1] = SVM1_TRAP_0_OPERATOR_2_1; break;
    case TRAMPOLINE_K_2_0:
      entry[1] = SVM1_TRAP_0_OPERATOR_2_0; break;
    case TRAMPOLINE_K_3_2:
      entry[1] = SVM1_TRAP_0_OPERATOR_3_2; break;
    case TRAMPOLINE_K_3_1:
      entry[1] = SVM1_TRAP_0_OPERATOR_3_1; break;
    case TRAMPOLINE_K_3_0:
      entry[1] = SVM1_TRAP_0_OPERATOR_3_0; break;
    case TRAMPOLINE_K_4_3:
      entry[1] = SVM1_TRAP_0_OPERATOR_4_3; break;
    case TRAMPOLINE_K_4_2:
      entry[1] = SVM1_TRAP_0_OPERATOR_4_2; break;
    case TRAMPOLINE_K_4_1:
      entry[1] = SVM1_TRAP_0_OPERATOR_4_1; break;
    case TRAMPOLINE_K_4_0:
      entry[1] = SVM1_TRAP_0_OPERATOR_4_0; break;
    case TRAMPOLINE_K_REFLECT_TO_INTERFACE:
      entry[1] = SVM1_TRAP_0_REFLECT_TO_INTERFACE; break;
    default:
      return (true);
    }
  return (false);
}
