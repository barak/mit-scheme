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

/* Compiled code interface macros for AArch64.  */

#ifndef SCM_CMPINTMD_H_INCLUDED
#define SCM_CMPINTMD_H_INCLUDED 1

/*

- Execute cache, little-endian:

	(before linking)
	0x00	8 [symbol: <name>]
	0x08	8 [fixnum: <frame size>]
	0x10	16 (padding)
	0x20 end

	(after linking, pointing to near open procedure)
target	0x00	8 <entry address>
	0x08	4 <frame size>
uuo	0x0c	4 ldr x1, target		; Load entry address.
	0x10	4 b target_pc			; Branch to PC-relative.
	0x14	12 (padding)
	0x20

	(after linking, pointing to far open procedure)
target	0x00	8 <entry address>
	0x08	4 <frame size>
uuo	0x0c	4 ldr x1, target		; Load entry address.
	0x10	4 adrp x17, target_pc		; Load PC-relative page addr.
	0x14	4 add x17, x17, #page_offset	: Add page offset.
	0x18	4 br x17
	0x1c	4 (padding)
	0x20

	(after linking, pointing to closure)
target	0x00	8 <entry address>
	0x08	4 <frame size>
uuo	0x0c	4 ldr x1, target		; Load entry address.
	0x10	4 sub x17, x1, #8		; Get address of PC offset.
	0x14	4 ldr x17, [x17]		; Load PC offset.
	0x18	4 add x17, x17, x1		; Compute PC = entry + offset.
	0x1c	4 br x17
	0x20

- Execute cache, big-endian:

	(before linking)
	0x00	8 [symbol: <name>]
	0x08	16 (padding)
	0x18	8 [fixnum: <frame size>]
	0x20

	(after linking, pointing to near open procedure)
target	0x00	8 <entry address>
uuo	0x08	4 ldr x1, target		; Load entry address.
	0x0c	4 b target_pc			; Branch PC-relative.
	0x10	12 (padding)
	0x1c	4 <frame size>
	0x20

	(after linking, pointing to far open procedure)
target	0x00	8 <entry address>
uuo	0x08	4 ldr x1, target		; Load entry address.
	0x0c	4 adrp x17, target_pc		; Load PC-relative page addr.
	0x10	4 add x17, x17, #page_offset	; Add page offset.
	0x14	4 br x17
	0x18	4 (padding)
	0x1c	4 <frame size>
	0x20

	(after linking, pointing to closure)
target	0x00	8 <entry address>
uuo	0x08	4 ldr x1, target		; Load entry address.
	0x0c	4 sub x17, x1, #8		; Get address of PC offset.
	0x10	4 ldr x17, [x17]		; Load PC offset.
	0x14	4 add x17, x17, x1		; Compute PC = entry + offset.
	0x18	4 br x17
	0x1c	4 <frame size>
	0x20

- Closure format:

start	0x00	8 [manifest-closure: <nwords>]
	0x08	4 <entry count>
	0x0c	2 <type/arity for entry0>
	0x0e	2 <block offset for entry0: 2*(entry0 - start)>
	0x10	8 <PC offset for entry0: pc0 - entry0>
entry0	0x18	4 (padding)
	0x1c	2 <type/arity for entry1>
	0x1e	2 <block offset for entry1: 2*(entry1 - start)>
	0x20	8 <PC offset for entry1: pc1 - entry1>
entry1	0x28	4 (padding)
	0x2c	2 <type/arity for entry2>
	0x2e	2 <block offset for entry2: 2*(entry2 - start)>
	0x30	8 <PC offset for entry2: pc2 - entry2>
entry2
slots	0x38	8 [tag: first object]
	0x40	8 [tag: second object]
	...

  Note the block offsets are all multiplied by two.  The low bit
  specifies whether the offset is from the start of the block, or from
  another offset, which is relevant to large compiled blocks but not
  relevant to closures unless you use gargantuan multiclosures, and we
  don't even generate multiclosures, so.

- Trampoline encoding:

	-0x10	4 (padding)
	-0x0c	2 <type/arity info>
	-0x0a	2 <block offset>
	-0x08	8 <PC offset = 0>		00 00 00 00 00 00 00 00
entry	0x00	4 movz x17, #<code>		; Set utility index.
	0x04	4 adr x1, storage		; Set x1 to storage pointer.
	0x08	4 br x23			; Jump to scheme-to-interface.
	0x0c	4 (padding)
storage	0x10	8 [tag: first trampoline datum]
	0x18	8 [tag: second trampoline datum]
	...

*/

#define ASM_RESET_HOOK aarch64_reset_hook

void aarch64_reset_hook (void);

#define CMPINT_USE_STRUCS 1

/* Must agree with cmpauxmd/aarch64.m4 and aarch64/machine.scm.  */
#define COMPILER_REGBLOCK_N_FIXED 16 /* XXX why? */
#define COMPILER_TEMP_SIZE 1 /* size in objects of largest RTL registers */
#define COMPILER_REGBLOCK_N_TEMPS 256
#define COMPILER_REGBLOCK_N_HOOKS 0 /* we'll use a machine register instead */
#define COMPILER_HOOK_SIZE (-1)

/* All aarch64 instructions are 32-bit-aligned.  */
typedef uint32_t insn_t;

/* Number of insn_t units for padding before entry.  */
#define CC_ENTRY_PADDING_SIZE 1

/* Number of insn_t units for type/arity, block offset, and PC offset.  */
#define CC_ENTRY_HEADER_SIZE 3

/* Use of this struct no doubt constitutes a strict-aliasing violation,
   but it is a well-known fact that if you write a comment about the
   undefined behaviour you're invoking, the C compiler is obligated to
   do what you meant.  */
struct cc_entry
{
  uint32_t padding;
  uint16_t type_arity;
  uint16_t block_offset;
  int64_t pc_offset;
};

/* We don't put GC trap code before an entry any more.  */
#define CC_ENTRY_GC_TRAP_SIZE 0

/* A compiled entry address points to _after_ the PC offset that, when
   added to the entry address, gives the address of instructions for
   the CPU to execute.

   XXX This is suboptimal because aarch64 does not have immediate
   negative load offsets, but putting the offset after the label causes
   other annoying issues.  */

#define CC_ENTRY_ADDRESS_PTR(e)		(e)
#define CC_ENTRY_ADDRESS_PC(e)		((e) + (((const int64_t *) (e))[-1]))

/* A compiled return address points to a jump instruction that jumps to
   the continuation's body.  */

#define CC_RETURN_ADDRESS_PTR(r)	(r)
#define CC_RETURN_ADDRESS_PC(r)		((insn_t *) interface_to_scheme_return)

insn_t * cc_return_address_to_entry_address (insn_t *);

#define CC_RETURN_ADDRESS_TO_ENTRY_ADDRESS cc_return_address_to_entry_address

#define EMBEDDED_CLOSURE_ADDRS_P 1

typedef struct
{
  insn_t * old_addr;
  insn_t * new_addr;
} reloc_ref_t;

#define DECLARE_RELOCATION_REFERENCE(name) reloc_ref_t name

#define START_CLOSURE_RELOCATION(scan, ref)				\
  start_closure_relocation ((scan), (&ref))

#define START_OPERATOR_RELOCATION(scan, ref)	do {(void)ref;} while (0)

#define OPERATOR_RELOCATION_OFFSET 0

#define READ_COMPILED_CLOSURE_TARGET(a, r)				\
  read_compiled_closure_target ((a), (&r))

void start_closure_relocation (SCHEME_OBJECT *, reloc_ref_t *);
insn_t * read_compiled_closure_target (insn_t *, reloc_ref_t *);

/* Number of objects in an execute cache.  Must match aarch64/rules3.scm.  */
#define UUO_LINK_SIZE 4

#define UUO_WORDS_TO_COUNT(nw) ((nw) / UUO_LINK_SIZE)
#define UUO_COUNT_TO_WORDS(nc) ((nc) * UUO_LINK_SIZE)

#define READ_UUO_TARGET(a, r) read_uuo_target (a)

insn_t * read_uuo_target (SCHEME_OBJECT *);

/* C stack is completely separate.  */
#define within_c_stack(fn, cookie) (fn)(cookie)

#endif /* SCM_CMPINTMD_H_INCLUDED */
