### -*-Midas-*-
###
###	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpauxmd/mc68k.m4,v 1.4 1989/11/21 23:32:09 jinx Exp $
###
###	Copyright (c) 1989 Massachusetts Institute of Technology
###
###	This material was developed by the Scheme project at the
###	Massachusetts Institute of Technology, Department of
###	Electrical Engineering and Computer Science.  Permission to
###	copy this software, to redistribute it, and to use it for any
###	purpose is granted, subject to the following restrictions and
###	understandings.
###
###	1. Any copy made of this software must include this copyright
###	notice in full.
###
###	2. Users of this software agree to make their best efforts (a)
###	to return to the MIT Scheme project any improvements or
###	extensions that they make, so that these may be included in
###	future releases; and (b) to inform MIT of noteworthy uses of
###	this software.
###
###	3. All materials developed as a consequence of the use of this
###	software shall duly acknowledge such use, in accordance with
###	the usual standards of acknowledging credit in academic
###	research.
###
###	4. MIT has made no warrantee or representation that the
###	operation of this software will be error-free, and MIT is
###	under no obligation to provide any services, by way of
###	maintenance, update, or otherwise.
###
###	5. In conjunction with products arising from the use of this
###	material, there shall be no use of the name of the
###	Massachusetts Institute of Technology nor of any adaptation
###	thereof in any advertising, promotional, or sales literature
###	without prior written consent from MIT in each case.
###

#### 68K assembly language (HP/Motorola Syntax) part of the compiled
#### code interface. See cmpint.c, cmpint-mc68k.h,  and cmpgc.h for
#### more documentation.
####
#### NOTE:
####	Assumptions:
####
####	1) The C compiler divides registers into two groups:
####	- super temporaries, which are not preserved accross procedure
####	calls and can always be used. On MC68K: a0, a1, d0, d1
####	- preserved registers which are saved by the callee if they are
####	written. On MC68K: all others except linkage registers (a6, sp)
####
####	2) All registers (except double floating point registers) and/or
####	stack locations hold a C long object.
####
####	3) Arguments, if passed on a stack, are popped by the caller
####	or by the procedure return instruction (as on the VAX).  Thus
####	most "leaf" procedures need not worry about them.
####
####	4) There is a hardware or software maintained stack for
####	control.  The procedure calling sequence may leave return
####	addresses in registers, but they must be saved somewhere for
####	multiple calls or recursive procedures.  On MC68K: saved on
####    the stack.
####
####	5) C procedures return values in a super temporary register.
####    On MC68: d0
####
####    6) The following code is compatible with gcc on the MC68K but
####    not (necessarily) other compilers.  GCC returns structures
####    that fit in 64 bits or less in d0/d1.  Many other compilers
####    return a pointer to the structure instead.

####	Utility macros and definitions

define(KEEP_HISTORY,0)			# Debugging switch

define(reference_external,`')		# Declare desire to use an external
define(extern_c_label,`_$1')		# The actual reference

define(define_c_label,
`	global	extern_c_label($1)
extern_c_label($1):')

define(define_debugging_label,
`	global	$1
$1:')

define(dlink, %a4)			# Dynamic link register (contains a
					# pointer to a return address) 
define(rfree, %a5)			# Free pointer
define(regs, %a6)			# Pointer to Registers[0]
define(rmask, %d7)			# Mask to clear type code

# Implementation constants -- Must match object.h

define(HEX, `0x$1')
define(TC_LENGTH, ifdef(`TYPE_CODE_LENGTH', TYPE_CODE_LENGTH, 8))
define(ADDRESS_MASK, eval(((2 ** (32 - TC_LENGTH)) - 1), 16))
	set	address_mask,HEX(ADDRESS_MASK)

# This must match const.h (* 4)

	set	regblock_val,8

reference_external(Ext_Stack_Pointer)
reference_external(Free)
reference_external(Registers)

define(switch_to_scheme_registers,
	`mov.l	%a6,(%sp)
	mov.l	%sp,c_save_stack
	mov.l	extern_c_label(Ext_Stack_Pointer),%sp
	mov.l	extern_c_label(Free),rfree
	lea	extern_c_label(Registers),regs
	mov.l	&address_mask,rmask')

define(switch_to_C_registers,
	`mov.l	rfree,extern_c_label(Free)
	mov.l	%sp,extern_c_label(Ext_Stack_Pointer)
	mov.l	c_save_stack,%sp
	mov.l	(%sp),%a6')

###
### Global data
###

	data

define_debugging_label(c_save_stack)
	space	4
ifelse(KEEP_HISTORY, 1,
`define_debugging_label(ring_pointer)
	long	ring_block_1
define_debugging_label(ring_block_1)
	long	ring_block_2
	space	28
define_debugging_label(ring_block_2)
	long	ring_block_3
	space	28
define_debugging_label(ring_block_3)
	long	ring_block_4
	space	28
define_debugging_label(ring_block_4)
	long	ring_block_5
	space	28
define_debugging_label(ring_block_5)
	long	ring_block_1
	space	28')
	text

### Callable by C conventions.  Swaps to Scheme register set and jumps
### to the entry point specified by its only argument.

define_c_label(C_to_interface)
	link.l	%a6,&-44
	movm.l	%d2-%d7/%a2-%a5,4(%sp)
	mov.l	8(%a6),%a0		# Argument: entry point
	bra.b	interface_to_scheme_internal

### Called by Scheme through a jump instruction in the register block.
### It is a special version of scheme_to_interface below, used when
### a return address is stored in the Scheme stack.

define_debugging_label(comentry_scheme_to_interface_jsr)
define_debugging_label(scheme_to_interface_jsr)
	mov.l	(%sp)+,%d1              # Return addr -> d1
	addq.l	&4,%d1			# Skip format info.
	bra.b	scheme_to_interface

### Called by linker-generated trampolines to invoke the appropriate
### C-written handler.  The return address on the stack is the address
### of the trampoline storage area, passed to the C handler as the
### first argument.

define_debugging_label(comentry_trampoline_to_interface)
define_debugging_label(trampoline_to_interface)
	mov.l	(%sp)+,%d1
###	bra	scheme_to_interface	# Fall through

### Called by Scheme through a jump instruction in the register block.
### It expects an index in %d0, and 4 longword arguments in %d1-%d4

reference_external(utility_table)

define_debugging_label(comentry_scheme_to_interface)
define_debugging_label(scheme_to_interface)
	ifelse(KEEP_HISTORY, 1,
	`lea	ring_pointer,%a1
	mov.l	(%a1),%a0
	mov.l	(%a0),(%a1)
	mov.l	%sp,4(%a0)
	mov.l	%a5,8(%a0)
	mov.l	%d0,12(%a0)
	mov.l	%d1,16(%a0)
	mov.l	%d2,20(%a0)
	mov.l	%d3,24(%a0)
	mov.l	%d4,28(%a0)
	cmp.l	%sp,%a5
	bgt.b	scheme_to_interface_proceed
	nop
define_debugging_label(scheme_to_interface_proceed)')
	switch_to_C_registers()
	mov.l	%d4,-(%sp)		# Push arguments to scheme utility
	mov.l	%d3,-(%sp)
	mov.l	%d2,-(%sp)
	mov.l	%d1,-(%sp)
	lea	extern_c_label(utility_table),%a0
	mov.l	(0,%a0,%d0.w*4),%a0	# C-written Scheme utility
	jsr	(%a0)
	lea	16(%sp),%sp		# Pop arguments to scheme utility

### On return, %d0 contains the address of interface_to_scheme or
### interface_to_C.  %d1 contains the appropriate data for them.

	mov.l	%d0,%a0
	jmp	(%a0)

### The data in %d1 is the address of an entry point to invoke.

define_c_label(interface_to_scheme)
	mov.l	%d1,%a0
###
###	Enter the scheme compiled world.
###	The value register is copied to %d0 because some utilities are
###	expected to return their value there (this should be fixed),
###	and it is stripped and placed in the dlink register since
###	we may be returning after interrupting a procedure which
###	needs this register.  This should also be separated or handled
###	inline.
###
define_debugging_label(interface_to_scheme_internal)
	switch_to_scheme_registers()
	mov.l	regblock_val(regs),%d0
	mov.l	%d0,%d1
	and.l	rmask,%d1
	mov.l	%d1,dlink	
	jmp	(%a0)

### The data in %d1 is a return code (integer) to the interpreter.

define_c_label(interface_to_C)
	mov.l	%d1,%d0			# C return value location
	movm.l	4(%sp),%d2-%d7/%a2-%a5
	unlk	%a6
	rts	

#### TEMPORARY stuff to get it up compatibly with current 68K compiler

### MANY FIELDS ARE NO LONGER NEEDED.  But we can't change the shape
### until the compiler's map of the area is updated and everything is
### recompiled.

# UNUSED
	set	regblock_old_temporaries,40
	set	regblock_n_old_temps,50
	set	regblock_hooks,(regblock_old_temporaries + (regblock_n_old_temps * 4))
	set	regblock_nhooks,10

# USED
# 50 6-byte entry points called as utilities by compiled code.
	set	regblock_entries,(regblock_hooks + (regblock_nhooks * 6))
	set	regblock_nentries,50

# UNUSED
	set	regblock_hooks2,(regblock_entries + (regblock_nentries * 6))
	set	regblock_nhooks2,20

# 900 words for compiled code temporaries (including floating point
# temporaries). Each temporary is allocated three words, regardless of
# whether it is a floating-point temporary.
	set	regblock_temporaries,(regblock_hooks2 + (regblock_nhooks2 * 6))
	set	regblock_ntemps,300

	set	regblock_length,(regblock_temporaries + (regblock_ntemps * 12))

	data

define_c_label(Registers) # Move to C world & reformat later
	space	regblock_length
define_debugging_label(data_patch_area)
	space	64

# This is in the data segment so that it can be modified!
define_debugging_label(code_patch_area)
	space	256	

# Compatibility handlers for existing compiled code.  They just load
# the appropriate registers, plus an index, and jump off into
# scheme_to_interface, above.

	text

	set offset_primitive_apply,HEX(12)
	set offset_primitive_lexpr_apply,HEX(13)
	set offset_apply,HEX(14)
	set offset_error,HEX(15)
	set offset_lexpr_apply,HEX(16)
	set offset_link,HEX(17)
	set offset_interrupt_closure,HEX(18)
	set offset_interrupt_dlink,HEX(19)
	set offset_interrupt_procedure,HEX(1a)
	set offset_interrupt_continuation,HEX(1b)
	set offset_interrupt_ic_procedure,HEX(1c)
	set offset_assignment_trap,HEX(1d)
	set offset_cache_lookup_apply,HEX(1e)
	set offset_lookup_trap,HEX(1f)
	set offset_safe_lookup_trap,HEX(20)
	set offset_unassigned_p_trap,HEX(21)
	set offset_decrement,HEX(22)
	set offset_divide,HEX(23)
	set offset_equal,HEX(24)
	set offset_greater,HEX(25)
	set offset_increment,HEX(26)
	set offset_less,HEX(27)
	set offset_subtract,HEX(28)
	set offset_multiply,HEX(29)
	set offset_negative,HEX(2a)
	set offset_add,HEX(2b)
	set offset_positive,HEX(2c)
	set offset_zero,HEX(2d)
	set offset_access,HEX(2e)
	set offset_reference,HEX(2f)
	set offset_safe_reference,HEX(30)
	set offset_unassigned_p,HEX(31)
	set offset_unbound_p,HEX(32)
	set offset_assignment,HEX(33)
	set offset_definition,HEX(34)
	set offset_lookup_apply,HEX(35)

define(call_utility,
	`movq	&offset_$1,%d0
	bra	scheme_to_interface')

### comentry_error
### comentry_apply
###
### comentry_error is used by compiled code to signal an error.  It
### expects the arguments to be pushed on the stack, the co
### (arguments + 1) to be in d0.w (no type code needed).
###
### comentry_apply is used by compiled code when calling unknown
### procedures. It expects the procedure and arguments to be pushed on
### the stack, and the count to be in d0.w (no type code needed).

define_debugging_label(comentry_error)
	mov.w	%d0,%d1
	ext.l	%d1
	call_utility(error)

define_debugging_label(comentry_apply)
	mov.w	%d0,%d2
	ext.l	%d2
	mov.l	(%sp)+,%d1
	call_utility(apply)

### comentry_lexpr_apply
###
### This entry point is invoked when compiled code calls a known
### lexpr, and the frame must be reformatted.  a0 contains the label
### to invoke, and d0.w contains the number of actual arguments passed
### (not including the procedure).
### Important: This assumes that it is always invoked with a valid
### number of arguments (the compiler checked it), and will not check.

define_debugging_label(comentry_lexpr_apply)
	mov.l	%a0,%d1
	mov.w	%d0,%d2
	ext.l	%d2
	call_utility(lexpr_apply)

### comentry_primitive_apply
### comentry_primitive_lexpr_apply
###
### Both expect the primitive object to be in d6.
###
### comentry_primitive_lexpr_apply is used for "lexpr" primitives
### (those whose arity is not fixed).  In addition, it expects
### regblock_lexpr_actuals to contain the actual number of arguments
### passed.

define_debugging_label(comentry_primitive_apply)
	mov.l	%d6,%d1
	call_utility(primitive_apply)

define_debugging_label(comentry_primitive_lexpr_apply)
	mov.l	%d6,%d1
	call_utility(primitive_lexpr_apply)

define(load_return_address,
	`mov.l	(%sp)+,%d1
	addq.l	&4,%d1')

### comentry_link
###
### Initialize all the variable cache slots for a compiled code block.
### It is called at load time, by the compiled code itself.
### It expects a block address in a0, the address of the constant section
### in a1, and a count of special blocks in d0.w.  The return address
### is on the top of the stack.

define_debugging_label(comentry_link)
	mov.w	%d0,%d4
	ext.l	%d4
	mov.l	%a0,%d2
	mov.l	%a1,%d3
	load_return_address()
	call_utility(link)

### comentry_interrupt_closure
###
### In all of the following it is assumed that regblock_memtop = 0,
### and that the compiler makes use of this when generating an addressing
### mode.
###
### We are expecting the compiler to generate the following code at
### a closure entry point:
###
###	label1:
###		jmp	regblock_comentry_interrupt_closure(regs)
###		dc.w	<format word>
###		dc.w	<offset to block start for gc of tc_compiled_entry>
###	entry_label:
###		add.l	&magic_constant,(%sp)
###		cmp.l	rfree,regblock_memtop(regs)
###		bge.b	label1
###
### comentry_interrupt_procedure
### comentry_interrupt_continuation
### comentry_interrupt_ic_procedure
###
### We are expecting the compiler to generate the following code at
### a procedure or continuation entry point:
###
###	label1:
###		jsr	regblock_comentry_interrupt_procedure(regs)
###		dc.w	<format word>
###		dc.w	<offset to block start for gc of tc_compiled_entry>
###	entry_label:
###		cmp.l	rfree,regblock_memtop(regs)
###		bge.b	label1

define_debugging_label(comentry_interrupt_closure)
	call_utility(interrupt_closure)

### Procedures require the dynamic link, if there is one.  This piece
### of code does not know whether the interrupted procedure uses a
### dynamic link or no, so it tries to guess that information.  It
### will call comutil_interrupt_procedure if there is no dynamic link,
### or comutil_interrupt_dlink if there is one.  The restart code does
### the right thing anyway.  This code assumes that the dynamic link
### is always in the dlink register.
###
### Continuations require VAL, so it is saved.
### Note that continuations never need the dynamic link, since it was
### saved by the caller.

### The heuristic used to determine whether dlink contains a dynamic
### link is as follows:
### - If the contents of dlink have a type code, there is no dynamic
###   link.
### - If the contents of dlink do not have the same (longword)
###   alignment as the Stack Pointer, there is no dynamic link.
### - If the contents of dlink point outside the interval
###   (Stack_Pointer, Stack_Top), there is no dynamic link.
###
### This should be fixed in the future by having a separate entry
### point for procedures with and without a dynamic link.

define_debugging_label(comentry_interrupt_procedure)
	load_return_address()		# Into %d1
	cmp.l	dlink,%sp
	bls.b	interrupt_no_dlink
	cmp.l	dlink,_Stack_Top
	bhs.b	interrupt_no_dlink
	mov.l	dlink,%d2
	sub.l	%sp,%d2
	and.l	&HEX(00000003),%d2
	bne.b	interrupt_no_dlink
	mov.l	dlink,%d2		# Load dlink
	call_utility(interrupt_dlink)
	
interrupt_no_dlink:
	call_utility(interrupt_procedure)

define_debugging_label(comentry_interrupt_continuation)
	load_return_address()
	call_utility(interrupt_continuation)

define_debugging_label(comentry_interrupt_ic_procedure)
	load_return_address()
	call_utility(interrupt_ic_procedure)

### comentry_assignment_trap
###
### Expects a cached-variable extension object in a0, and the assignment
### value in a1.

define_debugging_label(comentry_assignment_trap)
	load_return_address()
	mov.l	%a0,%d2
	mov.l	%a1,%d3
	call_utility(assignment_trap)

### comentry_cache_lookup_apply
###
### Expects the arguments on the stack, frame count in d0.w,
### a cached-variable extension object in a3, and the address of the
### compiled-code block in a1.

define_debugging_label(comentry_cache_lookup_apply)
	mov.w	%d0,%d3
	ext.l	%d3
	mov.l	%a3,%d1
	mov.l	%a1,%d2
	call_utility(cache_lookup_apply)

### comentry_lookup_trap
### comentry_safe_lookup_trap
### comentry_unassigned_p_trap
###
### Expects a cached-variable extension object in a0 (this is what is
### left in the constant area slot by comentry_cache_mumble).
### Returns the value of the variable in d0.

define(define_comp_ref_trap,
`define_debugging_label(comentry_$1)
	mov.l	%a0,%d2
	load_return_address()
	call_utility($1)')

define_comp_ref_trap(lookup_trap)
define_comp_ref_trap(safe_lookup_trap)
define_comp_ref_trap(unassigned_p_trap)

### comentry_reference
### comentry_safe_reference
### comentry_access
### comentry_unassigned_p
### comentry_unbound_p
###
### Expects an environment in a0, and a name in a1.
### Returns the value in d0.

define(define_comp_ref,
`define_debugging_label(comentry_$1)
	load_return_address()
	mov.l	%a0,%d2
	mov.l	%a1,%d3
	call_utility($1)')

define_comp_ref(access)
define_comp_ref(reference)
define_comp_ref(safe_reference)
define_comp_ref(unassigned_p)
define_comp_ref(unbound_p)

### comentry_assignment
### comentry_definition
###
### Expects an environment in a0, a name in a1, and a value in a2.
### Returns the old value in d0.

define(define_comp_assignment,
`define_debugging_label(comentry_$1)
	load_return_address()
	mov.l	%a0,%d2
	mov.l	%a1,%d3
	mov.l	%a2,%d4
	call_utility($1)')

define_comp_assignment(assignment)
define_comp_assignment(definition)

### comentry_lookup_apply
###
### Expects the arguments to be pushed on the stack, the environment
### in d4, the variable in d5, and the frame count in d0.w.

define_debugging_label(comentry_lookup_apply)
	mov.l	%d4,%d1
	mov.l	%d5,%d2
	mov.w	%d0,%d3
	ext.l	%d3
	call_utility(lookup_apply)

# Arithmetic is easy....

define(define_arithmetic_hook,
`define_debugging_label(comentry_$1)
	call_utility($1)')

define_arithmetic_hook(decrement)
define_arithmetic_hook(divide)
define_arithmetic_hook(equal)
define_arithmetic_hook(greater)
define_arithmetic_hook(increment)
define_arithmetic_hook(less)
define_arithmetic_hook(subtract)
define_arithmetic_hook(multiply)
define_arithmetic_hook(negative)
define_arithmetic_hook(add)
define_arithmetic_hook(positive)
define_arithmetic_hook(zero)

# Setup code to install these hooks into the register block.

define(setup_register,
	`mov.w	&HEX(4ef9),(%a0)+			# jmp &...
	mov.l	&comentry_$1,(%a0)+')

define_c_label(asm_reset_hook)
	link.l	%a6,&0
	lea	_Registers,%a1
#
#	setup_register(<name>)				# index	offset
#
	lea	regblock_entries(%a1),%a0

	setup_register(link)				#  0	12c
	setup_register(error)				#  1	132
	setup_register(apply)				#  2	138
	setup_register(lexpr_apply)			#  3	13e
	setup_register(primitive_apply)			#  4	144
	setup_register(primitive_lexpr_apply)		#  5	14a
	setup_register(cache_lookup_apply)		#  6	150
	setup_register(lookup_apply)			#  7	156
	setup_register(interrupt_continuation)		#  8	15c
	setup_register(interrupt_ic_procedure)		#  9	162
	setup_register(interrupt_procedure)		#  a	168
	setup_register(interrupt_closure)		#  b	16e
	setup_register(reference)			#  c	174
	setup_register(safe_reference)			#  d	17a
	setup_register(assignment)			#  e	180
	setup_register(access)				#  f	186
	setup_register(unassigned_p)			# 10	18c
	setup_register(unbound_p)			# 11	192
	setup_register(definition)			# 12	198
	setup_register(lookup_trap)			# 13	19e
	setup_register(safe_lookup_trap)		# 14	1a4
	setup_register(assignment_trap)			# 15	1aa
	setup_register(unassigned_p_trap)		# 16	1b0
	setup_register(add)				# 17	1b6
	setup_register(subtract)			# 18	1bc
	setup_register(multiply)			# 19	1c2
	setup_register(divide)				# 1a	1c8
	setup_register(equal)				# 1b	1ce
	setup_register(less)				# 1c	1d4
	setup_register(greater)				# 1d	1da
	setup_register(increment)			# 1e	1e0
	setup_register(decrement)			# 1f	1e6
	setup_register(zero)				# 20	1ec
	setup_register(positive)			# 21	1f2
	setup_register(negative)			# 22	1f8
	setup_register(scheme_to_interface)		# 23	1fe
	setup_register(trampoline_to_interface)		# 24	204
	setup_register(scheme_to_interface_jsr)		# 25	20a
							# free to 31 incl.

	unlk	%a6
	rts

