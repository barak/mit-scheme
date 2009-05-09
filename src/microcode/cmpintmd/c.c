/* -*-C-*-

$Id: c.c,v 1.3 2008/01/30 20:02:24 cph Exp $

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

/* Compiled code interface for C "native" code. */

#include "cmpint.h"
#include "extern.h"
#include "errors.h"

bool
read_cc_entry_type (cc_entry_type_t * cet, insn_t * address)
{
  uint32_t n = ((uint32_t) (address[-1]));
  return (decode_old_style_format_word (cet, (n & 0x0000FFFF)));
}

bool
write_cc_entry_type (cc_entry_type_t * cet, insn_t * address)
{
  uint16_t m;
  if (!encode_old_style_format_word (cet, (&m)))
    {
      uint32_t n = ((uint32_t) (address[-1]));
      (address[-1]) = ((insn_t) ((n & 0xFFFF0000) | m));
      return (false);
    }
  return (true);
}

bool
read_cc_entry_offset (cc_entry_offset_t * ceo, insn_t * address)
{
  uint32_t n = ((uint32_t) (address[-1]));
  uint16_t m = (n >> 16);
  (ceo->offset) = (m >> 1);
  (ceo->continued_p) = ((m & 1) == 1);
  return (false);
}

bool
write_cc_entry_offset (cc_entry_offset_t * ceo, insn_t * address)
{
  if ((ceo->offset) < 0x4000)
    {
      uint16_t m = (((ceo->offset) << 1) | ((ceo->continued_p) ? 1 : 0));
      uint32_t n = ((uint32_t) (address[-1]));
      (address[-1]) = ((insn_t) ((n & 0x0000FFFF) | (m << 16)));
      return (false);
    }
  return (true);
}

insn_t *
read_compiled_closure_target (insn_t * start)
{
  return ((insn_t *) (start [CC_ENTRY_HEADER_SIZE + 1]));
}

void
write_compiled_closure_target (insn_t * target, insn_t * start)
{
  (start [CC_ENTRY_HEADER_SIZE + 1]) = ((insn_t) target);
}

SCHEME_OBJECT
compiled_closure_entry_to_target (insn_t * entry)
{
  return (MAKE_CC_ENTRY ((insn_t *) (entry[1])));
}

unsigned long
compiled_closure_count (SCHEME_OBJECT * block)
{
  uint32_t n = ((uint32_t) (*block));
  return (((n & 0xFFFF0000) == 0) ? (n & 0x0000FFFF) : 1);
}

insn_t *
compiled_closure_start (SCHEME_OBJECT * block)
{
  uint32_t n = ((uint32_t) (*block));
  return (block + (((n & 0xFFFF0000) == 0) ? 1 : 0));
}

insn_t *
compiled_closure_entry (insn_t * start)
{
  return (start + CC_ENTRY_HEADER_SIZE);
}

insn_t *
compiled_closure_next (insn_t * start)
{
  return (start + CC_ENTRY_HEADER_SIZE + 2);
}

SCHEME_OBJECT *
skip_compiled_closure_padding (insn_t * start)
{
  return ((SCHEME_OBJECT *) start);
}

SCHEME_OBJECT
read_uuo_symbol (SCHEME_OBJECT * saddr)
{
  return (saddr[0]);
}

unsigned int
read_uuo_frame_size (SCHEME_OBJECT * saddr)
{
  return ((unsigned int) (saddr[1]));
}

insn_t *
read_uuo_target (SCHEME_OBJECT * saddr)
{
  return ((insn_t *) (saddr[0]));
}

insn_t *
read_uuo_target_no_reloc (SCHEME_OBJECT * saddr)
{
  return ((insn_t *) (saddr[0]));
}

void
write_uuo_target (insn_t * target, SCHEME_OBJECT * saddr)
{
  (saddr[0]) = ((SCHEME_OBJECT) target);
}

unsigned long
trampoline_entry_size (unsigned long n_entries)
{
  return (n_entries * 2);
}

insn_t *
trampoline_entry_addr (SCHEME_OBJECT * block, unsigned long index)
{
  return (((insn_t *) (block + 2 + (index * 2))) + CC_ENTRY_HEADER_SIZE);
}
