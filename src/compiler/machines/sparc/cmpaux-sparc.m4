/* #define DEBUG_INTERFACE */ /* -*-Midas-*- */
 !###
 !### Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993,
 !###     1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
 !###     2004, 2005, 2006, 2007, 2008 Massachusetts Institute of
 !###     Technology
 !###
 !### This file is part of MIT/GNU Scheme.
 !###
 !### MIT/GNU Scheme is free software; you can redistribute it and/or
 !### modify it under the terms of the GNU General Public License as
 !### published by the Free Software Foundation; either version 2 of
 !### the License, or (at your option) any later version.
 !###
 !### MIT/GNU Scheme is distributed in the hope that it will be
 !### useful, but WITHOUT ANY WARRANTY; without even the implied
 !### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 !### See the GNU General Public License for more details.
 !###
 !### You should have received a copy of the GNU General Public
 !### License along with MIT/GNU Scheme; if not, write to the Free
 !### Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
 !### MA 02110-1301, USA.
 !###

 !#### SPARC Architecture assembly language part of the compiled
 !#### code interface. See cmpint.txt, cmpint.c, cmpint-mips.h, and
 !#### cmpgc.h for more documentation.
 !####
 !#### NOTE:
 !####	Assumptions:
 !####
 !####	1) All registers (except double floating point registers) and
 !####	stack locations hold a C long object.
 !####
 !####	2) The C compiler divides registers into four categories:
 !####		in: (%i7-%i0 or %r31-%r24) incoming parameters
 !####		note: %fp is in this group
 !####		note: %i7 holds the C return address, don't bash this.
 !####
 !####		out: (%o7-%o0 or %r15-%r8) outgoing parameters
 !####		note: %sp is in this group
 !####		
 !####		locals: (%l7-%l0 or %r23-%r16)
 !####		
 !####		globals: (%g7-%g0 or %r7-%r0), reserved, essentially useless
 !####
 !####	The ins and locals are callee save through the standard SPARC save
 !####	and restore instructions. This has the added effect of cleaning
 !####	up the stack and frame pointers correctly. Globals are callee save.
 !####	Note that save and restore also pose as simulataneous add
 !####	instructions. This comes in handy for allocating the stack frame.
 !####
 !####	3) On SPARC the floating point registers are totally ungoverned.
 !####	The de-facto standard is caller save.


 !#### Compiled Scheme code uses the following register convention.
 !####	- g0 is the 0 constant				(hardwired)
 !####	- g1 is the designated temporary		(scheme available)
 !####	- g2-g4 are available for globals		(scheme available)
 !####	- g5-g7 are off limits super globals.		(don't touch!)
 !####	< Start of C callee saves >
 !####   - l0 is the return value register.		(scheme available)
 !####	- l1 contains the Scheme stack pointer.		(scheme available)
 !####	- l2 contains a cached version of MemTop.	(scheme available)
 !####	- l3 contains the Scheme free pointer.		(scheme available)	
 !####   - l4 contains the address of scheme_to_interface.	(scheme available)
 !####	- l5 contains the dynamic link when needed.	(scheme available)
 !####   - l6 contains the closure free pointer.		(scheme available)
 !####	- l7 is leftover (used for tramp index)		(scheme available)
 !####	- i0 is the C return value / first parameter	(scheme available)
 !####	- i1 contains the address mask for machine pointers.	(scheme available)
 !####	- i2 contains a pointer to the Scheme interpreter's	(scheme available)
 !####	       "register" block.  This block contains the compiler's
 !####          copy of MemTop, the interpreter's registers (val, env,
 !####          exp, etc), temporary locations for compiled code.
 !####	- i3 contains the top 6 address bits for heap pointers.	(scheme available)
 !####	- i4 contains the closure hook.				(scheme available)
 !####	- i5 is leftover.					(scheme available)
 !####	- i6 is the C frame pointer, alternatively the old C sp.(don't touch!)
 !####	- i7 is the C return address.				(don't touch!)
 !####	< End of C callee saves >
 !####	- o7 is the target of call instructions, ie next pc.	(scheme available)
 !####	- o6 is the current C stack pointer.			(scheme available)
 !####	- o5-o1 are outgoing parameters to the C world.		(scheme available)
 !####	- o0 is an outgoing parameter to the C world, and the return value
 !####	  from there						(scheme available)
 !####	

 !#	.verstamp	1 31

define(value, l0)
define(stack, l1)
define(C_arg1, o0)
define(C_arg2, o1)
define(C_arg3, o2)
define(C_arg4, o3)
define(utility_index, o5)

define(memtop, l2)
define(free, l3)
define(s_to_i, l4)
define(dynlink, l5)

define(closure_free, l6)
define(addr_mask, i1)
define(registers, i2)
define(heap_bits, i3)
define(closure_reg, i4)

	.global _Free
	.global _Registers
	.global _Ext_Stack_Pointer

	.text	
	.align	4


 !# Argument (in $C_arg1) is a compiled Scheme entry point
 !# but save C registers first
	.align 	4
	.global	_C_to_interface
	.proc	020
_C_to_interface:
	save 	%sp,-104,%sp

	!# Make space for interface return structs and stick a pointer to
	!# on the stack. SPARC C calling conventions require this.

	add	%fp, -24, %o0
	st	%o0,[%sp+64]

	!# Now stick the right interpreter registers into the right machine
	!# registers.

	sethi	%hi(_Free), %g1
	ld	[%g1+%lo(_Free)], %heap_bits
	sethi	%hi(0xfc000000), %addr_mask
	sethi	%hi(_Registers), %g1
	or	%g1, %lo(_Registers), %registers
	and 	%heap_bits, %addr_mask, %heap_bits
	xnor 	%g0, %addr_mask, %addr_mask
	
	.align 4
	.global	_interface_to_scheme
_interface_to_scheme:
	
	sethi	%hi(_Free), %g1
	ld	[%g1+%lo(_Free)], %free
	sethi	%hi(_Ext_Stack_Pointer), %g1
	ld	[%g1+%lo(_Ext_Stack_Pointer)], %stack

	ld 	[%registers + 36],%closure_free
	ld 	[%registers + 8],%value
	ld 	[%registers],%memtop

	and	%value,%addr_mask,%dynlink
	or 	%dynlink,%heap_bits,%dynlink
	jmpl	%i0 + 0, %o7
	add	%o7,264,%s_to_i
	
!# Don't rearrange the following procedures. The compiler backend knows their offsets
!# from scheme_to_interface and uses this knowledge to jump to them.

	.align 4
	.global _cons_multi_closure
	!# arg1 -> linkage data start address
	!# arg2 -> number of entries
	!# arg3 -> contains contents of %free
	!# %s_to_1 -256
	!# C_arg1 points to a manifest closure header word, followed by
  	!# nentries two-word structures, followed by the actual
  	!# instructions to return to.
  	!# The first word of each descriptor is the format+gc-offset word of
  	!# the corresponding entry point of the generated closure.
  	!# The second word is the offset from the entry address to the real
	!# code of the closure.
_cons_multi_closure:
	save %sp, -96, %sp
	add %i0, 0, %l0

	!# Stuff the tag word and length into the beginning of the multi-closure
	!# also write in the number of entries word.
	ld [%l0], %g1
	st %g1, [%i2]
	add %l0, 4, %l0

	sll %i1, 16, %g1
	st %g1, [%i2 + 4]

	!# Setup a template for the Addi part of each entry
	sethi %hi(0x82006008), %l1
	add %lo(0x82006008), %l1, %l1

	!# Calcualate the first offset to the closed var.
	add %i1, -1, %l2
	umul %l2, 16, %l2

	!# Copy free and bump it up two words
	add %i2, 8, %l3

cmc_l2:
	!# Copy the format+gc-offset word into the start of the entry
	ld [%l0], %g1
	st %g1, [%l3]

	!# Construct the sethi(target) part of the entry
	ld [%l0+4], %g1
	add %i0, %g1, %g1
	srl %g1, 10, %l4
	sethi %hi(0x03000000), %l5
	or %l4, %l5, %l5
	st %l5, [%l3+4]

	!# Construct the jmpl(lo(target)) part of the entry
	and %g1, 0x3ff, %l4
	sethi %hi(0x83c06000), %l5
	or %l4, %l5, %l5
	st %l5, [%l3+8]

	!# Construct the addi offset-to-data part of the entry
	add %l2, %l1, %l5
	st %l5, [%l3+12]

	!# Flush the instruction cache
	iflush %l3 + 4
	iflush %l3 + 8
	iflush %l3 + 12

	!# Bump to the next entry, next set of data

	add %l3, 16, %l3
	add %l0, 8, %l0
	subcc %l2, 16, %l2
	bge cmc_l2
	nop

	add %l0, 0, %g1
	jmpl %g1, %g0
	restore

	.align 4
	.global _cons_closure		
	!# arg1 -> return address
	!# arg2 -> delta from return address
	!# arg3 -> closure size (in bytes)
	!# arg4 -> using as an extra temp
	!# s_to_i -108
_cons_closure:
	ld [%C_arg1], %g1
	st %g1, [%free]
	ld [%C_arg1 + 4], %g1
	st %g1, [%free + 4]
	add %g0, %g0, %C_arg4
	add %C_arg2, %C_arg1, %C_arg2
	sethi %hi(0x03000000), %C_arg4
	srl %C_arg2, 10, %g1
	add %g1, %C_arg4, %C_arg4
	st %C_arg4, [%free + 8]
	sethi %hi(0x83c06000), %C_arg4
	and 0x3ff, %C_arg2, %g1
	add %g1, %C_arg4, %C_arg4
	st %C_arg4, [%free + 12]
	sethi %hi(0x82006008), %C_arg4
	add %lo(0x82006008), %C_arg4, %C_arg4
	st %C_arg4, [%free + 16]
	iflush %free + 8
	iflush %free + 12
	iflush %free + 16
	add %free, 8, %C_arg2
	add %C_arg3, %free, %free
	add %C_arg1, 8, %C_arg1
	jmpl %C_arg1, %g0
	nop
	
	.align 4
	.global _trampoline_to_interface	
	!# s_to_i - 8
_trampoline_to_interface:
	add	%C_arg1, -4, %C_arg1

	.align	4
	.global _link_to_interface	
	!# s_to_i - 4
_link_to_interface:
	add	%C_arg1, 12, %C_arg1
		
	.align 	4
	.global	_scheme_to_interface
	.proc	020
_scheme_to_interface:
	st	%value,[%registers + 8]
	st	%closure_free,[%registers + 36]

	sethi	%hi(_utility_table), %g1
	or	%g1, %lo(_utility_table), %g1	!# Find table
	add	%g1,%utility_index,%g1		!# Address of entry
	ld	[%g1],%l7			!# l7 <- Entry
	nop
	sethi	%hi(_Ext_Stack_Pointer), %g1
	st	%stack,[%g1+%lo(_Ext_Stack_Pointer)]	!# Save Scheme stack pointer
	nop
	sethi	%hi(_Free), %g1
	st	%free,[%g1+%lo(_Free)]		!# Save Free
	nop
	jmpl	%l7 + 0, %o7		!# Off to interface code
	nop
	unimp	8
	ld	[%o0 + 4],%i0		!# Get dispatch address
	ld	[%o0],%C_arg1		!# Arg1 <- value component
	jmpl	%C_arg1,%o7		!# Redispatch ...
	nop				!# Branch delay
	
	.align	4
	.global	_interface_to_C
	.proc	020
_interface_to_C:
	add	%i0,%g0,%C_arg1		!# Return value to C
	ret				!# Return to the C universe
	restore				!# Restore callee save regs

	.align	4
	.global	_flushrange
	.proc	020
_flushrange:
	save 	%sp,-96,%sp
	!# arg1: address base, arg2: byte count
	add	%g0, %g0, %l0
flush_l:
	iflush	%i0 + %l0
	add	4, %l0, %l0
	subcc	%l0,%i1,%g0
	bl	flush_l			!# Continue if address < address + count
	nop
	nop				!# flush pipeline
	nop
	nop
	nop
	nop
	ret				!# Return to caller
	restore				!# Restore callee save regs
