/* -*-C-*-

$Id: svm1.c,v 1.2 2007/04/22 16:31:24 cph Exp $

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

/* Compiled code interface for SVM v1. */

#include "cmpint.h"
#include "extern.h"
#include "errors.h"
#include "svm1-defns.h"

static unsigned int cc_entry_reference_offset (insn_t *);

bool
read_cc_entry_type (cc_entry_type_t * cet, insn_t * address)
{
  unsigned int n
    = ((((unsigned int) (address[-3])) << 8)
       | ((unsigned int) (address[-4])));
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
      case 7:
	make_cc_entry_type (cet, CET_EXPRESSION);
	break;

      case 6:
	make_cc_entry_type (cet, CET_INTERNAL_PROCEDURE);
	break;

      case 5:
	make_cc_entry_type (cet, CET_INTERNAL_CONTINUATION);
	break;

      case 4:
	make_cc_entry_type (cet, CET_TRAMPOLINE);
	break;

      case 3:
	make_cc_entry_type (cet, CET_RETURN_TO_INTERPRETER);
	break;

      case 2:
	make_cc_entry_type (cet, CET_CLOSURE);
	break;

      default:
	return (true);
      }
  return (false);
}

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
      n = (0xFFF8 + 7);
      break;

    case CET_INTERNAL_PROCEDURE:
      n = (0xFFF8 + 6);
      break;

    case CET_INTERNAL_CONTINUATION:
      n = (0xFFF8 + 5);
      break;

    case CET_TRAMPOLINE:
      n = (0xFFF8 + 4);
      break;

    case CET_RETURN_TO_INTERPRETER:
      n = (0xFFF8 + 3);
      break;

    case CET_CLOSURE:
      n = (0xFFF8 + 2);
      break;

    default:
      return (true);
    }
  (address[-4]) = (n & 0x00FF);
  (address[-3]) = (n >> 8);
  return (false);
}

/* The offset is encoded as two bytes.  It's relative to its own
   address, _not_ relative to the entry address.  In the case of a
   closure, the offset points to the start of the first entry in the
   block.  For other entry types, it points to the first non-marked
   word in the block.  */

bool
read_cc_entry_offset (cc_entry_offset_t * ceo, insn_t * address)
{
  unsigned int n
    = ((((unsigned int) (address[-1])) << 8)
       | ((unsigned int) (address[-2])));
  if (n < 0x8000)
    {
      (ceo->offset) = (n + (cc_entry_reference_offset (address)));
      (ceo->continued_p) = false;
    }
  else
    {
      (ceo->offset) = (n - 0x8000);
      (ceo->continued_p) = true;
    }
  return (false);
}

bool
write_cc_entry_offset (cc_entry_offset_t * ceo, insn_t * address)
{
  unsigned long offset = (ceo->offset);
  if (ceo->continued_p)
    {
      offset -= (cc_entry_reference_offset (address));
      if (! (offset < 0x8000))
	return (true);
    }
  else
    {
      if (! (offset < 0x8000))
	return (true);
      offset += 0x8000;
    }
  (address[-2]) = (offset & 0x00FF);
  (address[-1]) = (offset >> 8);
  return (false);
}

static unsigned int
cc_entry_reference_offset (insn_t * address)
{
  cc_entry_type_t cet;
#ifdef ENABLE_DEBUGGING_TOOLS
  bool ok_p = (read_cc_entry_type ((&cet), address));
  assert (ok_p);
#else
  read_cc_entry_type ((&cet), address);
#endif
  return
    (CC_ENTRY_OFFSET_SIZE
     + (((cet.marker) == CET_CLOSURE)
	? ((sizeof (SCHEME_OBJECT)) + 2)
	: (2 * (sizeof (SCHEME_OBJECT)))));
}

/* Compiled closures

   A compiled-closure block starts with a single GC header
   (TC_MANIFEST_CLOSURE), followed by a 2-byte positive count,
   followed by the closure entries (as specified by the count).  For
   example, on a 32-bit machine with count == 2 and 4 value cells:

   0x00    TC_MANIFEST_CLOSURE | n_words == 10
   0x04    (count - 1) == 1

   0x06    type == CET_CLOSURE
   0x08    (offset - 6) == 2
   0x0A    SVM1_INST_ICALL_U8
   0x0B    offset == 8

   0x0C    type == CET_CLOSURE
   0x0E    (offset - 6) == 8
   0x10    SVM1_INST_ICALL_U8
   0x11    offset == 6

   0x12    2 padding bytes

   0x14    code entry 0
   0x18    code entry 1

   0x1C    value cell 0
   0x20    value cell 1
   0x24    value cell 2
   0x28    value cell 3

   */

unsigned long
compiled_closure_count (SCHEME_OBJECT * block)
{
  return
    (((((unsigned long) (((byte_t *) block) [1])) << 8)
      | ((unsigned long) (((byte_t *) block) [0])))
     + 1);
}

insn_t *
compiled_closure_start (SCHEME_OBJECT * block)
{
  return (((insn_t *) block) + 2);
}

insn_t *
compiled_closure_entry (insn_t * start)
{
  return (start + CC_ENTRY_HEADER_SIZE);
}

insn_t *
compiled_closure_next (insn_t * start)
{
  insn_t * entry = (compiled_closure_entry (start));
  switch (*entry)
    {
    case SVM1_INST_ICALL_U8: return (entry + 2);
    case SVM1_INST_ICALL_U16: return (entry + 3);
    case SVM1_INST_ICALL_U32: return (entry + 5);
    default:
      Microcode_Termination (TERM_COMPILER_DEATH);
      return (0);
    }
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
  unsigned long offset;
  switch (entry[0])
    {
    case SVM1_INST_ICALL_U8:
      offset = (((unsigned long) (entry[1])) + 2);
      break;

    case SVM1_INST_ICALL_U16:
      offset
	= (((((unsigned long) (entry[2])) << 8)
	    | ((unsigned long) (entry[1])))
	   + 3);
      break;

    case SVM1_INST_ICALL_U32:
      offset
	= (((((unsigned long) (entry[4])) << 24)
	    | (((unsigned long) (entry[3])) << 16)
	    | (((unsigned long) (entry[2])) << 8)
	    | ((unsigned long) (entry[1])))
	   + 5);
      break;

    default:
      Microcode_Termination (TERM_COMPILER_DEATH);
      return (0);
    }
  return (* ((SCHEME_OBJECT *) (entry + offset)));
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

   0x00    n-args encoded as fixnum
   0x04    name encoded as symbol

   After linking, the cache is changed as follows:

   0x00    n-args
   0x02    SVM1_INST_IJUMP_U8
   0x03    offset = 0
   0x04    32-bit address

   On a 64-bit machine, the post-linking layout is:

   0x00    n-args
   0x02    4 padding bytes
   0x06    SVM1_INST_IJUMP_U8
   0x07    offset = 0
   0x08    64-bit address

   */

unsigned int
read_uuo_frame_size (SCHEME_OBJECT * saddr)
{
  insn_t * address = ((insn_t *) saddr);
  return
    ((((unsigned int) (address[1])) << 8)
     | ((unsigned int) (address[0])));
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
	return (eaddr);
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
store_trampoline_insns (insn_t * entry, byte_t code)
{
  (entry[0]) = SVM1_INST_TRAP_TRAP_0;
  (entry[1]) = code;
  return (false);
}
