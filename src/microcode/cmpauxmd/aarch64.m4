// -*- Asm -*-
//
// Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
//     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
//     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
//     2015, 2016, 2017, 2018, 2019, 2020 Massachusetts Institute of
//     Technology
//
// This file is part of MIT/GNU Scheme.
//
// MIT/GNU Scheme is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// MIT/GNU Scheme is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with MIT/GNU Scheme; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
// 02110-1301, USA.

///////////////////////////////////////////////////////////////////////////////
// Scheme compiled code support for AArch64
///////////////////////////////////////////////////////////////////////////////

ifdef(`SUPPRESS_LEADING_UNDERSCORE',
	`define(SYMBOL,`$1')',
	`define(SYMBOL,`_$1')')

	// Symbol definitions.
	//
	// XXX Use .def/.endef or .func/.endfunc?
define(GLOBAL,`	.globl $1
SYMBOL($1):')
define(LOCAL,`
SYMBOL($1):')
define(END,`	.size SYMBOL($1),.-SYMBOL($1)')

	// gas has this for arm32 but not for aarch64, no idea why.
define(ADRL,`
	adrp	$1, :pg_hi21:$2
	add	$1, $1, #:lo12:$2')

	// For some reason these are not automatically defined in gas?
	ip0	.req x16
	ip1	.req x17
	fp	.req x29
	lr	.req x30

	// Scheme machine registers.  Must agree with
	// aarch64/machine.scm, aarch64/lapgen.scm.
	VAL		.req x0
	UARG1		.req x1
	UARG2		.req x2
	UARG3		.req x3
	UARG4		.req x4
	UIDX		.req x17
	APPLICAND	.req x1
	APPLICAND_PC	.req x17
	REGS		.req x19
	FREE		.req x20
	DYNLINK		.req x21
	HOOKS		.req x23
	SSP		.req x28// Note: Scheme and C use separate stacks!

	// Interpreter register block offsets.  Must agree with
	// const.h.
	.equiv	REGBLOCK_VAL,		2

///////////////////////////////////////////////////////////////////////////////
// Entering Scheme from C
///////////////////////////////////////////////////////////////////////////////

	// long C_to_interface (insn_t * addr@x0, insn_t * pc@x1)
	//
	//	From C, call the compiled Scheme code with the
	//	specified entry address and PC.  addr is an untagged
	//	compiled-entry address; pc is the pointer to actual
	//	instructions.
	//
	//	Steps:
	//
	//	1. Save the return address, frame pointer, and
	//	   callee-saves registers.
	//	2. Set up the Scheme registers.
	//	3. Defer to interface_to_scheme.
	//
GLOBAL(C_to_interface)
	// Push frame and save frame pointer and return address.
	stp	fp, lr, [sp,#-96]!

	// Set our own frame pointer for fun.
	mov	fp, sp

	// Save callee-saves registers.
	stp	x19, x20, [sp,#16]
	stp	x21, x22, [sp,#32]
	stp	x23, x24, [sp,#48]
	stp	x25, x26, [sp,#64]
	stp	x27, x28, [sp,#80]

	// Set up Scheme registers:
	// - interpreter registers
	// - hook table
	// The others -- VAL, DYNLINK, FREE, SSP -- are set up by
	// interface_to_scheme.
	ADRL(REGS,Registers)		// address of register block
	ADRL(HOOKS,hooks)		// address of hook table

	// Set parameters for interface_to_scheme.  Note
	// APPLICAND_PC=x17, APPLICAND=x0, so ordering is important
	// here.
	mov	APPLICAND_PC, x1
	mov	APPLICAND, x0

	b	SYMBOL(interface_to_scheme)
END(C_to_interface)

///////////////////////////////////////////////////////////////////////////////
// Returning to C from Scheme
///////////////////////////////////////////////////////////////////////////////

	// void interface_to_C (long code@x1, void * garbage@x17)
	//
	//	When a utility returns and it needs to fall back to the
	//	interpreter, it directs scheme_to_interface_return to
	//	jump here to return to C, making control come flying
	//	back out of the last C_to_interface.
	//
	//	Steps:
	//
	//	1. Restore the return address, frame pointer, and
	//	   callee-saves registers.
	//	2. Return.
	//
	//	The mutable Scheme registers -- FREE, SSP -- will have
	//	alreay been saved by scheme_to_interface on earlier
	//	entry to C.
	//
GLOBAL(interface_to_C)
	// Set return value.
	mov	x0, x1

	// Restore callee-saves registers.
	ldp	x19, x20, [sp,#16]
	ldp	x21, x22, [sp,#32]
	ldp	x23, x24, [sp,#48]
	ldp	x25, x26, [sp,#64]
	ldp	x27, x28, [sp,#80]

	// Restore frame pointer and return address and pop frame.
	ldp	fp, lr, [sp],#96

	// And we're done.
	ret
END(interface_to_C)

///////////////////////////////////////////////////////////////////////////////
// Entering a C subroutine from Scheme
///////////////////////////////////////////////////////////////////////////////

	// scheme_to_interface
	//
	//	Compiled Scheme code needs help from the microcode.
	//	Possible return value or dynamic link is in x0;
	//	arguments are in x1,x2,x3,x4; utility index is in ip1 =
	//	x17.  ip0 = x16 is free as a temporary.
	//
	//	Steps:
	//
	//	1. Save value, Free, and stack_pointer.
	//	   => No need to save REGS or HOOKS because callee-saves.
	//	   => If DYNLINK is active, will be utility argument 2.
	//	2. Allocate a struct on the stack for return values in x0.
	//	3. Call the function in utility_table.
	//	4. Go to wherever the microcode directed us.
	//
GLOBAL(scheme_to_interface)
	// Save value, Free, and stack_pointer.
	str	VAL, [REGS,#(REGBLOCK_VAL*8)]
	ADRL(ip0,Free)			// address of Free pointer
	str	FREE, [ip0]		// store current Free pointer
	ADRL(ip0,stack_pointer)		// address of stack pointer
	str	SSP, [ip0]		// store current stack pointer

	// Allocate a struct on the C stack for return values in x0,
	// first argument to utility function.  Keep the stack 32-byte
	// aligned just in case.
	sub	sp, sp, #32
	mov	x0, sp

	// Call the function in utility_table.
	ADRL(ip0,utility_table)		// address of utility table
	ldr	ip0, [ip0,UIDX,lsl #3]	// load utility function pointer
	blr	ip0			// call

scheme_to_interface_return:
	// Pop the utility_result_t contents:
	//	ip0 := interface_dispatch (x16)
	//	x1 := interpreter code / compiled applicand
	//	ip1 := interpreter garbage / compiled applicand PC (x17)
	ldp	ip0, APPLICAND, [sp],#16
	ldr	APPLICAND_PC, [sp],#16

	// Jump to interface_dispatch.
	br	ip0
END(scheme_to_interface)

///////////////////////////////////////////////////////////////////////////////
// Returning from a C subroutine back into Scheme
///////////////////////////////////////////////////////////////////////////////

	// void interface_to_scheme (insn_t * entry@x1, insn_t * pc@x17)
	//
	//	Set up a transition to compiled Scheme code after a
	//	utility return, whether we are jumping to a Scheme
	//	entry or returning to a Scheme return address.
	//
	//	- Set x0 to be the preserved return value, if any.
	//	- Set x21 to be the preserved dynamic link, if any.
	//	  (Both were in REGBLOCK_VAL.)
	//	- Set up x20 (FREE) and x28 (Scheme SP).
	//	- Preserve x1 (APPLICAND).
	//	- Preserve REGS (x19) and HOOKS (x23) because those are
	//	  callee-saves and unmodified by C.
	//
	//	Finally, jump to pc, x17, which is either the first PC
	//	of a compiled entry, or interface_to_scheme_return if
	//	we are returning to a Scheme continuation.
	//
GLOBAL(interface_to_scheme)
	// Restore value if it was in use, dynamic link if it was in
	// use, Free, and stack_pointer.
	ldr	VAL, [REGS,#(REGBLOCK_VAL*8)]
	mov	DYNLINK, VAL
	ADRL(FREE,Free)			// address of Free pointer
	ldr	FREE, [FREE]		// load current Free pointer
	ADRL(SSP,stack_pointer)		// address of stack pointer
	ldr	SSP, [SSP]		// load current stack pointer

	// Jump to Scheme, or to scheme_to_interface.
	br	APPLICAND_PC
END(interface_to_scheme)

	// void interface_to_scheme_return (insn_t * entry@x1)
	//
	//	Issue a RET to entry, x1.
	//
GLOBAL(interface_to_scheme_return)
	mov	lr, APPLICAND
	ret
END(interface_to_scheme_return)

///////////////////////////////////////////////////////////////////////////////
// Scheme unknown procedure application setup
///////////////////////////////////////////////////////////////////////////////

	// apply_setup(applicand@x1, frame_size@x2)
	//
	//	If applicand is a compiled entry of exactly the correct
	//	arity, load its PC into APPLICAND_PC=x17.  Otherwise,
	//	load apply_setup_fail into APPLICAND_PC=x17 to defer to
	//	microcode.  Then return to link register.  Caller is
	//	expected to jump to APPLICAND_PC=x17.
	//
	//	Not yet implemented fully.
	//
LOCAL(apply_setup)
	ADRL(APPLICAND_PC,apply_setup_fail)
	ret
END(apply_setup)

	// apply_setup_fail(applicand@x1, frame_size@x2)
	//
	//	Enter the microcode to apply applicand.  Note that the
	//	arguments are already in the correct places for a
	//	utility.
	//
LOCAL(apply_setup_fail)
	mov	UIDX, #0x14	// comutil_apply
	b	SYMBOL(scheme_to_interface)
END(apply_setup_fail)

///////////////////////////////////////////////////////////////////////////////
// Scheme miscellaneous primitive subroutine hooks
///////////////////////////////////////////////////////////////////////////////

	// fixnum_shift
	//
	//	Compute a left shift, handling all possible signs of
	//	both inputs.  Not yet implemented.
	//
LOCAL(fixnum_shift)
	hlt	#0
END(fixnum_shift)

	// set_interrupt_enables
	//
	//	Set the interrupt mask, and adjust stack_guard and
	//	memtop accordingly.  Not yet implemented.
LOCAL(set_interrupt_enables)
	hlt	#0
END(set_interrupt_enables)

///////////////////////////////////////////////////////////////////////////////
// The hook table
///////////////////////////////////////////////////////////////////////////////

	// JUMP_HOOK(name, target)
	//
	//	Hook that just jumps to target, no questions asked.
	//
define(JUMP_HOOK, `
LOCAL($1)
	b	SYMBOL($2)
	nop
	nop
	nop
END($1)')

	// UTILITY_HOOK(name, number)
	//
	//	Hook that jumps to the utility with the specified
	//	number.  Does not reduce caller code size, so use this
	//	only as an interim for cases where we are likely to
	//	add some extra logic here soon that would help to
	//	reduce caller code size.  The number must match
	//	utility_table in cmpint.c.
	//
define(UTILITY_HOOK, `
LOCAL($1)
	mov	UIDX, #$2
	b	SYMBOL(scheme_to_interface)
	nop
	nop
END($1)')

	// APPLY_HOOK(name, label, n)
	//
	//	Application setup hook, to be implemented at label.
	//	Currently not implemented, so just loads n (frame size,
	//	i.e. number of arguments + 1) into UARG2 and defers to
	//	apply_setup.  Caller ensures UARG1 already has the
	//	callee.
	//
define(APPLY_HOOK, `
LOCAL($1)
	mov	UARG2, #$3
	b	SYMBOL(apply_setup)
	nop
	nop
END($1)')

	// hooks
	//
	//	Table of hooks for support routines used by compiled
	//	Scheme code.  The first one, scheme_to_interface, is
	//	needed to call the C utilities.  The remainder are
	//	mainly to reduce compiled code size while avoiding
	//	unnecessary costly calls to C.
	//
	//	Each entry must be exactly four instructions long,
	//	which is enough to load a far PC-relative address (up
	//	to two instructions) and branch to it (one more) and
	//	another instruction just for good measure in case we
	//	find a reason to need one.
	//
	//	The order must match DEFINE-ENTRIES in
	//	aarch64/lapgen.scm.
	//
LOCAL(hooks)
	JUMP_HOOK(hook_scheme_to_interface, scheme_to_interface)	// 00
	UTILITY_HOOK(hook_generic_add, 0x2b)				// 01
	UTILITY_HOOK(hook_generic_sub, 0x28)				// 02
	UTILITY_HOOK(hook_generic_mul, 0x29)				// 03
	UTILITY_HOOK(hook_generic_div, 0x23)				// 04
	UTILITY_HOOK(hook_generic_eq, 0x24)				// 05
	UTILITY_HOOK(hook_generic_lt, 0x27)				// 06
	UTILITY_HOOK(hook_generic_gt, 0x25)				// 07
	UTILITY_HOOK(hook_generic_add1, 0x26)				// 08
	UTILITY_HOOK(hook_generic_sub1, 0x22)				// 09
	UTILITY_HOOK(hook_generic_zero_p, 0x2d)				// 0a
	UTILITY_HOOK(hook_generic_positive_p, 0x2c)			// 0b
	UTILITY_HOOK(hook_generic_negative_p, 0x2a)			// 0c
	UTILITY_HOOK(hook_generic_quotient, 0x37)			// 0d
	UTILITY_HOOK(hook_generic_remainder, 0x38)			// 0e
	UTILITY_HOOK(hook_generic_modulo, 0x39)				// 0f
	JUMP_HOOK(hook_fixnum_shift, fixnum_shift)			// 10
	JUMP_HOOK(hook_apply_setup, apply_setup)			// 11
	APPLY_HOOK(hook_apply_setup_1, apply_setup_1, 1)		// 12
	APPLY_HOOK(hook_apply_setup_2, apply_setup_2, 2)		// 13
	APPLY_HOOK(hook_apply_setup_3, apply_setup_3, 3)		// 14
	APPLY_HOOK(hook_apply_setup_4, apply_setup_4, 4)		// 15
	APPLY_HOOK(hook_apply_setup_5, apply_setup_5, 5)		// 16
	APPLY_HOOK(hook_apply_setup_6, apply_setup_6, 6)		// 17
	APPLY_HOOK(hook_apply_setup_7, apply_setup_7, 7)		// 18
	APPLY_HOOK(hook_apply_setup_8, apply_setup_8, 8)		// 19
	JUMP_HOOK(hook_set_interrupt_enables, set_interrupt_enables)	// 1a
END(hooks)

// Local Variables:
// comment-start: "//"
// asm-comment-char: ?/
// End:
