/* #define DEBUG_INTERFACE */ /* -*-Midas-*- */
 ###
 ### $Id$
 ###
 ### Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993,
 ###     1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
 ###     2004, 2005, 2006, 2007, 2008 Massachusetts Institute of
 ###     Technology
 ###
 ### This file is part of MIT/GNU Scheme.
 ###
 ### MIT/GNU Scheme is free software; you can redistribute it and/or
 ### modify it under the terms of the GNU General Public License as
 ### published by the Free Software Foundation; either version 2 of
 ### the License, or (at your option) any later version.
 ###
 ### MIT/GNU Scheme is distributed in the hope that it will be useful,
 ### but WITHOUT ANY WARRANTY; without even the implied warranty of
 ### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 ### General Public License for more details.
 ###
 ### You should have received a copy of the GNU General Public License
 ### along with MIT/GNU Scheme; if not, write to the Free Software
 ### Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 ### 02110-1301, USA.

 #### MIPS Architecture assembly language part of the compiled
 #### code interface. See cmpint.txt, cmpint.c, cmpint-mips.h, and
 #### cmpgc.h for more documentation.
 ####
 #### NOTE:
 ####	Assumptions:
 ####
 ####	1) All registers (except double floating point registers) and
 ####	stack locations hold a C long object.
 ####
 ####	2) The C compiler divides registers into three groups:
 ####	- Linkage registers, used for procedure calls and global
 ####	references.  On MIPS: 0 (always 0), 31 (return address),
 ####	28 (global data pointer), and 29 (C stack pointer).
 ####	- super temporaries, not preserved accross procedure calls and
 ####	always usable. On MIPS: 1-15, and 24-25.
 ####	  4-7 are argument registers,
 ####	  2 and 3 are return registers.
 ####	- preserved registers saved by the callee if they are written.
 ####	On MIPS: 16-23, and 30.
 ####
 ####	3) Arguments, if passed on a stack, are popped by the caller
 ####	or by the procedure return instruction (as on the VAX).  Thus
 ####	most "leaf" procedures need not worry about them. On MIPS: All
 ####	arguments have slots in the stack, allocated and popped by the
 ####	caller, but the first four words are actually passed in
 ####	registers 4 through 7, unless they are floating point
 ####	arguments, in which case they are passed in floating point
 ####	registers.
 ####
 ####	4) There is a hardware or software maintained stack for
 ####	control.  The procedure calling sequence may leave return
 ####	addresses in registers, but they must be saved somewhere for
 ####	nested calls and recursive procedures.  On MIPS: Passed in a
 ####	register, but a slot on the stack exists, allocated by the
 ####	caller.  The return link is in 31.  The stack pointer is in
 ####	29.
 ####
 ####	5) C procedures return long values in a super temporary
 ####   register.  MIPS only: two word structures are returned in a
 ####   location specified by the contents of the first argument
 ####   register, and all other arguments are shifted over one
 ####   location (i.e.  apparent argument 1 is passed in the register
 ####   usually used for argument 2, etc.)
 ####
 ####	6) On MIPS the floating point registers fr20-fr31 are
 ####	callee-saves registers, fr12-fr15 are parameter registers, and
 ####	fr4-fr11 and fr16-fr19 are caller-saves registers.  fr0-3 are
 ####	return result registers.  Only the even numbered registers are
 ####	used (odd registers contain second 32 bits of 64 bit values).

 #### Compiled Scheme code uses the following register convention.
 #### Note that scheme_to_interface and the register block are
 #### preserved by C calls, but the others are not, since they change
 #### dynamically.  scheme_to_interface_linked and
 #### trampoline_to_interface can be reached at fixed offsets from
 #### scheme_to_interface.
 ####	- gr1  is the assembler temporary.
 ####   - gr2  is the returned value register.
 ####	- gr3  contains the Scheme stack pointer.
 ####   - gr4 - gr7 are used by C for passing arguments.
 ####	- gr8  contains a cached version of MemTop.
 ####	- gr9  contains the Scheme free pointer.
 ####	- gr10 contains the address of scheme_to_interface.
 ####	- gr11 contains the dynamic link when needed.
 ####	- gr12 - gr15 have no special uses.
 ####   <CALLEE SAVES REGISTERS BELOW HERE>
 ####   - gr16 - gr18 have no special uses.
 ####   - gr19 contains the closure free pointer.
 ####	- gr20 contains the address mask for machine pointers.
 ####	- gr21 contains a pointer to the Scheme interpreter's
 ####	       "register" block.  This block contains the compiler's
 ####          copy of MemTop, the interpreter's registers (val, env,
 ####          exp, etc), temporary locations for compiled code.
 ####   - gr22 contains the top 6 address bits for heap pointers.
 ####	- gr23 contains the closure hook.
 ####   <CALLEE SAVES REGISTERS ABOVE HERE>
 ####	- gr24 has no special use.
 ####   - gr25 is used a an index for dispatch into C.
 ####   - gr26 and 27 are reserved for the OS.
 ####   - gr28 contains the pointer to C static variables.
 ####   - gr29 contains the C stack pointer.
 ####   <CALLEE SAVES REGISTERS BELOW HERE>
 ####   - gr30 has no special use.
 ####   <CALLEE SAVES REGISTERS ABOVE HERE>
 ####	- gr31 is used for linkage (JALR, JAL, BGEZAL, and BLTZAL write it).
 ####
 ####	All other registers are available to the compiler.  A
 ####	caller-saves convention is used, so the registers need not be
 ####	preserved by subprocedures.
 ####
 ####   Notice that register gr25 is used for the index used to
 ####   dispatch into the trampolines and interface routines.

 #	.verstamp	1 31
	.text	
	.align	2
	.set	noat
	.set	noreorder

 # This is required to work around a bug in the IRIX 6.3 assembler.
 # The bug caused an incorrect reference to be generated in the
 # "la	$closure_reg,closure_hook" instruction.
	.globl	closure_hook

define(value, 2)
define(stack, 3)
define(C_arg1, 4)
define(C_arg2, 5)
define(C_arg3, 6)
define(C_arg4, 7)
define(memtop, 8)
define(free, 9)
define(s_to_i, 10)
define(dynlink, 11)

define(closure_free, 19)
define(addr_mask, 20)
define(registers, 21)
define(heap_bits, 22)
define(closure_reg, 23)
	
define(tramp_index, 25)

define(TC_ENTITY, 0x10)
define(TC_FIXNUM, 0x1A)
define(TC_CCENTRY, 0x28)

 # Argument (in $C_arg1) is a compiled Scheme entry point
 # but save C registers first
	.globl	C_to_interface
	.ent	C_to_interface
C_to_interface:
	addi	$sp,$sp,-120
	.frame	$sp,120,$0
	.mask	0x80ff0000,0
	sw	$31,116($sp)		# Save return address
	sw	$30,112($sp)
	sw	$23,108($sp)
	sw	$22,104($sp)
	sw	$21,100($sp)
	sw	$20,96($sp)
	sw	$19,92($sp)
	sw	$18,88($sp)
	sw	$17,84($sp)
	sw	$16,80($sp)
	.fmask	0x00000fff,0
	s.d	$f30,72($sp)
	s.d	$f28,64($sp)
	s.d	$f26,56($sp)
	s.d	$f24,48($sp)
	s.d	$f22,40($sp)
	s.d	$f20,32($sp)
	# 20 and 24($sp) hold return data structure from C hooks
        # 16 is reserved for 4th argument to hooks, if used.
        # 4, 8, and 12($sp) are space for 1st - 3rd argument.
        # 0($sp) is space for holding return pointer
#ifdef DEBUG_INTERFACE
	la	$registers,Debug_Buffer
	.set	at
	sw	$registers,Debug_Buffer_Pointer
	.set	noat
#endif
	la	$registers,Registers
	lw	$heap_bits,Free
	lui	$addr_mask,0xfc00
	and	$heap_bits,$heap_bits,$addr_mask
	nor	$addr_mask,$0,$addr_mask
	la	$closure_reg,closure_hook
	lw	$closure_free,36($registers)
 # ... fall through ...
 # Argument (in $C_arg1) is a compiled Scheme entry point.  Reload
 # the Scheme registers and go to work...any registers not reloaded
 # here must be callee saves by C.
	.globl	interface_to_scheme
interface_to_scheme:
	lw	$value,8($registers)
	lw	$memtop,0($registers)
	lw	$stack,sp_register
	lw	$free,Free
	and	$dynlink,$addr_mask,$value
	or	$dynlink,$heap_bits,$dynlink
#ifdef DEBUG_INTERFACE
	andi	$at,$free,3
	bne	$at,0,Bad_Free_Pointer
	nop
Continue_Past_Free_Problem:
#endif
	jal	$31,$C_arg1		# Off to compiled code ...
        addi	$s_to_i,$31,100		# Set up scheme_to_interface

	.globl	hook_jump_table
hook_jump_table:
 # This sequence of NOPs is provided to allow for modification of
 # the sequence that appears above without having to recompile the
 # world.  The compiler "knows" the distance between
 # scheme_to_interface_ble and hook_jump_table (100 bytes)
 #
 # $tramp_index has the offset into the table that is desired.
	.globl	link_to_interface
link_to_interface: 	# ...scheme_to_interface-100
	addi	$31,$31,4		# Skip over format word ...

	.globl	trampoline_to_interface
trampoline_to_interface:		# ...scheme_to_interface-96
	j	scheme_to_interface
	add	$C_arg2,$0,$31		# Arg2 <- trampoline data area
	
	break	1			# ...-88 Used to be generate_closure
	nop				# ...-84

	break	2			# ...-80 Used to be push_closure_entry
	nop				# ...-76

	j	cons_closure		# -72
	lw	$7,40($registers)	# closure limit -68

	j	cons_multi		# -64
	lw	$7,40($registers)	# closure limit -60

	j	shortcircuit_apply	# ...-56
	lw	$C_arg2,0($stack)	# procedure -52

	j	set_interrupt_enables	# ...-48
	lw	$value,4($registers)	# ...-44

	nop				# ...-40
	nop				# ...-36
	nop				# ...-32
	nop				# ...-28
	nop				# ...-24
	nop				# ...-20
	nop				# ...-16
	nop				# ...-12
	nop				# ...-8
	nop				# ...-4

 # DO NOT MOVE the following label, it is used above ...
 #  Argument (in $tramp_index) is index into utility_table for the
 # interface procedure to be called.  The Scheme compiler has saved
 # any registers that it may need.  Registers 5 through 7 are loaded
 # with arguments for the C procedure that is being invoked.  The
 # fourth argument (if used) is stored at 16($sp).

	.globl 	scheme_to_interface
scheme_to_interface:
	sw	$value,8($registers)
	sw	$closure_free,36($registers)
#ifdef DEBUG_INTERFACE
	lw	$value,Stack_Bottom
	addi	$0,$0,0			# Load delay
	sltu	$at,$stack,$value
	bne	$at,$0,Stack_Overflow_Detected
	addi	$0,$0,0
	lw	$value,Debug_Buffer_Pointer
	addi	$0,$0,0
	sw	$stack,0($value)        # Stack pointer
        sw	$25,4($value)           # Index
        sw	$C_arg2,8($value)       # 1st arg.
        sw	$C_arg3,12($value)      # 2nd arg.
        sw	$C_arg4,16($value)      # 3rd arg.
	addi	$value,$value,20
	la	$12,Debug_Buffer_End
	bne	$12,$value,Store_Pointer_Back
	nop
	la	$12,Debug_Buffer
	add	$value,$0,$12
Store_Pointer_Back:
	.set	at
	sw	$value,Debug_Buffer_Pointer
	lw	$value,Debug_Call_Count
	lw	$12,Debug_Call_Max
	addi	$value,$value,1
	sw	$value,Debug_Call_Count
	beq	$value,$12,Debug_Tight_Loop
	nop
	.set	noat
#endif
after_overflow:
	la	$24,utility_table	# Find table
	add	$25,$24,$25		# Address of entry
	lw	$25,0($25)		# gr25 <- Entry
	la	$24,sp_register
	sw	$stack,0($24)		# Save Scheme stack pointer
	la	$24,Free
	sw	$free,0($24)		# Save Free
	jal	$31,$25			# Off to interface code
	addi	$C_arg1,$sp,20		# Return value on C stack
	lw	$25,20($sp)		# Get dispatch address
	lw	$C_arg1,24($sp)		# Arg1 <- value component
	jal	$31,$25			# Redispatch ...
	addi	$0,$0,0			# Branch delay...

	.globl	closure_hook
closure_hook:
	# On arrival:
	# GR31 has address of JAL instruction we were supposed to have
	# executed.  This code emulates the JAL.
	# (except that R31 is already set).
	lw	$at,0($31)		# Load JAL instruction
	nop				# Load delay slot
	and	$at,$at,$addr_mask	# clear JAL opcode
	sll	$at,$at,2		# obtain destination address
	or	$at,$at,$heap_bits	# insert top bits into destination
	j	$at			# invoke
	nop				# jump delay slot

	.globl	cons_closure
cons_closure:
	# On arriveal:
	# - GR31 has the address of the manifest closure header,
	#   followed by the closure descriptor (2 words),
	#   followed by the instructions we need to continue with.
	#   The closure descriptor consists of the format+gc-offset word
	#   followed by a PC-relative JAL instruction.
	# - GR4 has the address past the first word on this closure
	#   (assuming the entry point is at closure-free).
	# - GR5 has the increment for closure-free.
	# On return:
	# - GR4 has the address of the closure
	# This code assumes that it can clobber registers 7 and at freely.
 #	lw	$7,40($registers)	# closure limit
	lw	$at,0($31)		# closure header word
	subu	$7,$7,$4		# check if it fits
	bgez	$7,cons_closure_continue
	or	$4,$closure_free,$0	# setup result
	or	$7,$31,$0		# Preserve original return address
	bgezal	$0,invoke_allocate_closure
	addi	$at,$at,2		# Total size = datum(header) + 2

cons_closure_continue:
	add	$closure_free,$closure_free,$5	# allocate
	lw	$5,4($31)		# format+gc-offset word
	lw	$7,8($31)		# JAL instruction
	sw	$0,-12($4)		# Make heap parseable
	sw	$5,-4($4)		# Store format+gc-offset
	srl	$5,$31,2		# return address -> JAL destination
	sw	$at,-8($4)		# Store closure header
	and	$5,$5,$addr_mask	# clear top bits
	addi	$31,$31,12		# Bump past structure
	addu	$5,$5,$7		# JAL instruction
	j	$31			# Return.
	sw	$5,0($4)		# Store the JAL instruction

	.globl	cons_multi
cons_multi:
	# On arriveal:
	# - GR31 has the address of the manifest closure header,
	#   followed by n closure descriptors (2 words each),
	#   followed by the instructions we need to continue with.
	#   Each closure descriptor consists of the format+gc-offset
	#   word followed by a PC-relative JAL instruction.
	# - GR4 has the address past the first word on this closure
	#   (assuming the entry point is at closure-free).
	# - GR5 has the increment for closure-free.
	# - GR6 has the number of entries (>= 1)
	# On return:
	# - GR4 has the address of the closure
	# This code assumes that it can clobber registers 7 and at freely.
 #	lw	$7,40($registers)	# closure limit
	lw	$at,0($31)		# closure header word
	subu	$7,$7,$4		# check if it fits
	bgez	$7,cons_multi_continue
	or	$4,$closure_free,$0	# setup result
	or	$7,$31,$0		# Preserve original return address
	bgezal	$0,invoke_allocate_closure
	addi	$at,$at,1		# Total size = datum(header) + 1

cons_multi_continue:
	add	$closure_free,$closure_free,$5	# allocate
	sw	$at,-12($4)		# Store closure header
	sh	$6,-8($4)		# Store number of entries
	sh	$0,-6($4)		# Tag as multi-closure
	addi	$7,$4,-4		# Pointer to closure entries
	srl	$5,$31,2		# return-address -> JAL destination
	and	$5,$5,$addr_mask	# clear top bits
	addi	$31,$31,4		# bump to first descriptor

store_loop:
	lw	$at,0($31)		# format+gc-offset word
	addi	$6,$6,-1		# decrement count
	addi	$31,$31,8		# bump pointer to block
	sw	$at,0($7)		# store into closure
	lw	$at,-4($31)		# PC-relative JAL
	addi	$7,$7,12		# bump pointer to closure
	add	$at,$at,$5		# absolute JAL instruction
	bgtz	$6,store_loop
	sw	$at,-8($7)		# store JAL instruction
	
	j	$31			# return
	nop				# delay slot

invoke_allocate_closure:
 # $at contains in its datum the minimum size to allocate.
 # $7  contains the "return address" for cons_closure or cons_multi.
 # $31 contains the return address for invoke_allocate_closure.
	addi	$sp,$sp,-80
 # 1 is at, a temp
	sw	$2,80-4($sp)
	sw	$3,80-8($sp)
	and	$4,$at,$addr_mask	# total size (- 1)
	sw	$5,80-12($sp)
	sw	$6,80-16($sp)
	sw	$7,80-20($sp)		# Original value of r31
 #	sw	$8,0($registers)	# memtop is read-only
	la	$7,Free
	sw	$9,0($7)
	sw	$10,80-24($sp)
	sw	$11,80-28($sp)
	sw	$12,80-32($sp)
	sw	$13,80-36($sp)
	sw	$14,80-40($sp)
	sw	$15,80-44($sp)
 # 16-23 are callee-saves registers.
	sw	$24,80-48($sp)
	sw	$25,80-52($sp)
 # 26-29 are taken up by the OS and the C calling convention.
 # 30 is a callee-saves register.
	sw	$31,80-60($sp)		# return address
	jal	allocate_closure
	sw	$closure_free,36($registers) # uncache

	lw	$closure_free,36($registers)
	lw	$31,80-20($sp)		# original value of r31
	lw	$25,80-52($sp)
	lw	$24,80-48($sp)
	lw	$15,80-44($sp)
	lw	$14,80-40($sp)
	lw	$13,80-36($sp)
	lw	$12,80-32($sp)
	lw	$11,80-28($sp)
	lw	$10,80-24($sp)
	lw	$9,Free
	lw	$8,0($registers)
	lw	$7,80-60($sp)		# return address for invoke...
	lw	$6,80-16($sp)
	lw	$5,80-12($sp)
	lw	$3,80-8($sp)
	lw	$2,80-4($sp)
	lw	$at,0($31)		# manifest closure header
	or	$4,$closure_free,$0	# setup result

	j	$7
	addi	$sp,$sp,80

	.globl	shortcircuit_apply
shortcircuit_apply:
	# $C_arg2 contains the procedure one cycle after this point.
	# $C_arg3 contains the frame size
	addi	$at,$0,TC_CCENTRY	# test for compiled entry
	srl	$C_arg4,$C_arg2,26
	bne	$C_arg4,$at,shortcircuit_apply_1
	and	$C_arg2,$addr_mask,$C_arg2 # procedure -> address
	or	$C_arg2,$heap_bits,$C_arg2
	lhu	$C_arg4,-4($C_arg2)	# lose if wrong arity
	addi	$at,$0,0xff
	and	$C_arg4,$at,$C_arg4
	bne	$C_arg4,$C_arg3,shortcircuit_apply_lose
	nop
	j	$C_arg2			# invoke procedure
	addi	$stack,$stack,4		# pop it too

	.globl	shortcircuit_apply_1
shortcircuit_apply_1:
	addi	$at,$0,TC_ENTITY	# Test for entity
	bne	$C_arg4,$at,shortcircuit_apply_lose
	or	$C_arg2,$heap_bits,$C_arg2 # get entity's procedure
	lw	$C_arg2,0($C_arg2)
	addi	$at,$0,TC_CCENTRY	# test for compiled entry
	srl	$C_arg4,$C_arg2,26
	bne	$C_arg4,$at,shortcircuit_apply_lose
	and	$C_arg2,$addr_mask,$C_arg2 # procedure -> address
	or	$C_arg2,$heap_bits,$C_arg2
	lhu	$C_arg4,-4($C_arg2)	# lose if wrong arity
	addi	$at,$0,0xff
	and	$C_arg4,$at,$C_arg4
	addi	$at,$C_arg3,1		# adjust for entity arg
	bne	$C_arg4,$C_arg3,shortcircuit_apply_lose
	nop
	j	$C_arg2			# invoke procedure
	nop				# don't pop entity arg

	.globl	shortcircuit_apply_lose
shortcircuit_apply_lose:
	lw	$C_arg2,0($stack)	# pop procedure into arg register
	addi	$stack,$stack,4
	la	$at,scheme_to_interface	# invoke the standard apply
	j	$at
	addi	$tramp_index,$0,80

	.globl	set_interrupt_enables
set_interrupt_enables:
	# 0($stack) has the new interrupt mask (a fixnum)
	# 4($stack) has the return address (a compiled entry)
	# $value has been set above to old interrupt mask
	lui	$at,(TC_FIXNUM*0x400)	# slap fixnum type code on value
	or	$value,$value,$at
	lw	$C_arg1,0($stack)	# get new interrupt mask
	lw	$C_arg2,48($registers)	# get interrupt code
	and	$C_arg1,$C_arg1,$addr_mask
	sw	$C_arg1,4($registers)	# store new mask in mask register
	# Now, set up the memtop and stack_guard registers.
	# Memtop is -1 if there are any pending interrupts, else
	# "MemTop" if GC interrupt is enabled, else "Heap_Top".
	and	$C_arg2,$C_arg2,$C_arg1	# get masked interrupts
	bne	$C_arg2,$0,set_interrupt_enables_1
	addi	$memtop,$0,-1
	andi	$C_arg2,$C_arg1,4	# test for GC interrupt
	lw	$memtop,MemTop
	bne	$C_arg2,$0,set_interrupt_enables_1
	nop
	lw	$memtop,Heap_Top
	.globl	set_interrupt_enables_1
set_interrupt_enables_1:
	andi	$C_arg2,$C_arg1,1	# test for stack-overflow interrupt
	sw	$memtop,0($registers)
	# Stack_guard's value depends on whether the stack-overflow
	# interrupt is enabled.
	lw	$C_arg3,Stack_Guard
	bne	$C_arg2,$0,set_interrupt_enables_2
	nop
	lw	$C_arg3,Stack_Bottom
	.globl	set_interrupt_enables_2
set_interrupt_enables_2:
	lw	$C_arg2,4($stack)	# get return address
	sw	$C_arg3,44($registers)	# store stack_guard
	and	$C_arg2,$C_arg2,$addr_mask # return to caller
	or	$C_arg2,$C_arg2,$heap_bits
	j	$C_arg2
	addi	$stack,$stack,8

 # Argument 1 (in $C_arg1) is the returned value
	.globl interface_to_C
interface_to_C:
	l.d	$f20,32($sp)
	l.d	$f22,40($sp)
	l.d	$f24,48($sp)
	l.d	$f26,56($sp)
	l.d	$f28,64($sp)
	l.d	$f30,72($sp)
	lw	$16,80($sp)
	lw	$17,84($sp)
	lw	$18,88($sp)
	lw	$19,92($sp)
	lw	$20,96($sp)
	lw	$21,100($sp)
	lw	$22,104($sp)
	lw	$23,108($sp)
	lw	$30,112($sp)
	lw	$31,116($sp)
	addi	$sp,$sp,120		# Pop stack back
	j	$31			# Return
	add	$2,$0,$C_arg1		# Return value to C
	.end	C_to_interface

#ifdef DEBUG_INTERFACE
	.globl	Stack_Overflow_Detected
Stack_Overflow_Detected:
	j	after_overflow
	nop

	.globl	Bad_Free_Pointer
Bad_Free_Pointer:
	j	Continue_Past_Free_Problem
	nop
#endif

	.globl	interface_initialize
	.ent	interface_initialize
interface_initialize:
	.frame	$sp,0,$31
	cfc1	$25,$31		# read FPU control register
	nop
	ori	$25,$25,0xf00	# enable V, Z, O, U traps
	ctc1	$25,$31		# write FPU control register
	nop
	j	$31		# return
	nop
	.end	interface_initialize

	.globl	Debug_Tight_Loop
	.ent	Debug_Tight_Loop
Debug_Tight_Loop:
	beq	$12,$value,Debug_Tight_Loop
	nop
	j	after_overflow
	.end	Debug_Tight_Loop

#ifdef DEBUG_INTERFACE
	.data
	.globl	Debug_Buffer_Pointer
Debug_Buffer_Pointer:
	.word	0
	.globl	Debug_Buffer
Debug_Buffer:
	.word	0:30
Debug_Buffer_End:
	.word	0
	.globl	Debug_Call_Count
Debug_Call_Count:
	.word	0
	.globl	Debug_Call_Max
Debug_Call_Max:
	.word	0
#endif
