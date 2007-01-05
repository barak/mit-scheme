### -*-Midas-*-
###
### $Id: mc68k.m4,v 1.32 2007/01/05 21:19:26 cph Exp $
###
### Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993,
###     1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
###     2004, 2005, 2006, 2007 Massachusetts Institute of Technology
###
### This file is part of MIT/GNU Scheme.
###
### MIT/GNU Scheme is free software; you can redistribute it and/or
### modify it under the terms of the GNU General Public License as
### published by the Free Software Foundation; either version 2 of the
### License, or (at your option) any later version.
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

#### 68K assembly language (HP/Motorola Syntax) part of the compiled
#### code interface.  See cmpint.txt, cmpint.c, cmpint-mc68k.h, and
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
####	references.  On MC68K: a6, sp.
####	- super temporaries, not preserved accross procedure calls and
####	always usable. On MC68K: a0, a1, d0, d1
####	- preserved registers saved by the callee if they are written.
####	On MC68K: all others.
####
####	3) Arguments, if passed on a stack, are popped by the caller
####	or by the procedure return instruction (as on the VAX).  Thus
####	most "leaf" procedures need not worry about them.
####
####	4) There is a hardware or software maintained stack for
####	control.  The procedure calling sequence may leave return
####	addresses in registers, but they must be saved somewhere for
####	nested calls and recursive procedures.  On MC68K: saved on
####    the stack.
####
####	5) C procedures return long values in a super temporary
####    register.  Two word structures are returned in super temporary
####    registers as well.  On MC68K: d0 is used for long returns.
####    Since there are two methods for returning structures on MC68K,
####    there is a flag to choose a mechanism:
####    o  GCC returns two word structures in d0/d1 (set flag GCC in
####       M4_MACHINE_SWITCHES in m.h)
####    o  Other compilers return the address of the structure in d0
####	o  The HP compiler requires that the address of this structure
####	   be in a1 before the procedure is called (set flag HP in
####	   M4_MACHINE_SWITCHES in m.h)
####
####	6) Floating point registers are not preserved by this
####	interface.  The interface is only called from the Scheme
####	interpreter, which does not use floating point data.  Thus
####	although the calling convention would require us to preserve
####	them, they contain garbage.
####
#### Compiled Scheme code uses the following register convention:
####	- a7 (sp) contains the Scheme stack pointer, not the C stack
####	pointer.
####	- a6 (fp) contains a pointer to the Scheme interpreter's
####	"register" block.  This block contains the compiler's copy of
####	MemTop, the interpreter's registers (val, env, exp, etc),
####	temporary locations for compiled code, and the mechanism used
####	to invoke the hooks in this file.
####	- a5 contains the Scheme free pointer.
####	- a4 contains the dynamic link when needed.
####	- d7 contains the Scheme datum mask.
####	- d6 is where Scheme compiled code returns values.
####
####	All other registers are available to the compiler.  A
####	caller-saves convention is used, so the registers need not be
####	preserved by subprocedures.

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

# Call a SCHEME_UTILITY (see cmpint.c) and then dispatch to the
# interface procedure requested with the data to be passed to the
# procedure in d1.
#
# NOTE: Read introductory note about GCC and HP switches

define(allocate_utility_result,
	`ifdef(`HP',
	      `subq.l	&8,%sp
	       mov.l	%sp,%a1',
	      `')')


define(utility_call,
       `jsr	(%a0)		# call C procedure
	ifdef(`HP',
	      `lea	eval(($1+2)*4)(%sp),%sp',
              `lea	eval($1*4)(%sp),%sp')
	mov.l	%d0,%a0
	ifdef(`GCC',
              `',
	      `mov.l	4(%a0),%d1
	       mov.l	0(%a0),%a0')
	jmp	(%a0)')

# Scheme object representation.  Must match object.h

define(HEX, `0x$1')
define(TC_LENGTH, ifdef(`TYPE_CODE_LENGTH', TYPE_CODE_LENGTH, 6))
define(ADDRESS_MASK, eval(((2 ** (32 - TC_LENGTH)) - 1), 16))
define(TYPE_CODE_FACTOR, eval(2 ** (8 - TC_LENGTH)))
define(TYPE_CODE_MASK, eval((256 - TYPE_CODE_FACTOR), 16))
define(CLEAR_TYPE_MASK, eval((TYPE_CODE_FACTOR - 1), 16))

define(TYPE_CODE_TO_BYTE, `$1*TYPE_CODE_FACTOR')
define(TYPE_CODE_TO_OBJECT, `TYPE_CODE_TO_BYTE($1)*0x1000000')

define(EXTRACT_TYPE_CODE,
	`ifelse(TC_LENGTH, 8,
	`mov.b	$1,$2',
	`mov.b	$1,$2
	and.b	&HEX(TYPE_CODE_MASK), $2')')

define(COMPARE_TYPE_CODE,
	`cmp.b	$1,&TYPE_CODE_TO_BYTE($2)')

### External conventions

	set	regblock_memtop,0	# from const.h (* 4)
	set	regblock_int_mask,4
	set	regblock_val,8
	set	regblock_stack_guard,44
	set	regblock_int_code,48
	set	address_mask,HEX(ADDRESS_MASK)

# This must match the compiler (machin.scm)

define(dlink, %a4)			# Dynamic link register (contains a
					# pointer to a return address) 
define(rfree, %a5)			# Free pointer
define(regs, %a6)			# Pointer to Registers[0]
define(rmask, %d7)			# Mask to clear type code
define(rval,%d6)			# Procedure value

reference_external(sp_register)
reference_external(Free)
reference_external(Registers)

# These must match the C compiler

define(switch_to_scheme_registers,
	`mov.l	%a6,(%sp)
	mov.l	%sp,c_save_stack
	mov.l	extern_c_label(sp_register),%sp
	mov.l	extern_c_label(Free),rfree
	lea	extern_c_label(Registers),regs
	mov.l	&address_mask,rmask')

define(switch_to_C_registers,
	`mov.l	rfree,extern_c_label(Free)
	mov.l	%sp,extern_c_label(sp_register)
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

### Initialize the 68881 if present.

define_c_label(interface_initialize)
	link	%a6,&0
	ifdef(`MC68881', `fmov.l	&0x3480,%fpcr')
	unlk	%a6
	rts

### Callable by C conventions.  Swaps to Scheme register set and jumps
### to the entry point specified by its only argument.

define_c_label(C_to_interface)
	link	%a6,&-44
	movm.l	%d2-%d7/%a2-%a5,4(%sp)
	mov.l	8(%a6),%a0		# Argument: entry point
	bra.b	interface_to_scheme_internal

### Called by Scheme through a jump instruction in the register block.
### It expects an index in %d0, and 4 longword arguments in %d1-%d4

reference_external(utility_table)

define_c_label(asm_scheme_to_interface)
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
	mov.l	rval,regblock_val(regs)
	switch_to_C_registers()
	allocate_utility_result()
	mov.l	%d4,-(%sp)		# Push arguments to scheme utility
	mov.l	%d3,-(%sp)
	mov.l	%d2,-(%sp)
	mov.l	%d1,-(%sp)
	lea	extern_c_label(utility_table),%a0
	mov.l	(0,%a0,%d0.w*4),%a0	# C-written Scheme utility
	utility_call(4)                 # 4 arguments

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
	mov.l	regblock_val(regs),rval
	mov.l	rval,%d0
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

#### Optimized entry points

### Additional entry points that take care of common cases and are used to
### shorten code sequences.
### These are not strictly necessary, since the code sequences emitted by
### the compiler could use scheme_to_interface instead, but a few instructions
### are saved this way.

### Called by linker-generated trampolines to invoke the appropriate
### C-written handler.  The return address on the stack is the address
### of the trampoline storage area, passed to the C handler as the
### first argument.

### IMPORTANT:
### All the asm_* routines are declared in cmpint-mc68k.h.
### New ones need to be declared there as well!

define_c_label(asm_trampoline_to_interface)
define_debugging_label(trampoline_to_interface)
	mov.l	(%sp)+,%d1
	bra	scheme_to_interface

### Called by Scheme through a jump instruction in the register block.
### It is a special version of scheme_to_interface below, used when
### a return address is stored in the Scheme stack.

define_c_label(asm_scheme_to_interface_jsr)
define_debugging_label(scheme_to_interface_jsr)
	mov.l	(%sp)+,%d1              # Return addr -> d1
	addq.l	&4,%d1			# Skip format info.
	bra	scheme_to_interface

define(define_interface_indirection,
`define_c_label(asm_$1)
	movq	&HEX($2),%d0
	bra	scheme_to_interface')

define(define_interface_jsr_indirection,
`define_c_label(asm_$1)
	movq	&HEX($2),%d0
	bra	scheme_to_interface_jsr')

define_interface_indirection(primitive_lexpr_apply,13)
define_interface_indirection(error,15)
define_interface_jsr_indirection(link,17)
define_interface_indirection(interrupt_closure,18)
define_interface_jsr_indirection(interrupt_procedure,1a)
define_interface_jsr_indirection(interrupt_continuation,1b)
define_interface_jsr_indirection(assignment_trap,1d)
define_interface_jsr_indirection(reference_trap,1f)
define_interface_jsr_indirection(safe_reference_trap,20)
###
### These are handled directly below.
###
### define_interface_indirection(generic_decrement,22)
### define_interface_indirection(generic_divide,23)
### define_interface_indirection(generic_equal,24)
### define_interface_indirection(generic_greater,25)
### define_interface_indirection(generic_increment,26)
### define_interface_indirection(generic_less,27)
### define_interface_indirection(generic_subtract,28)
### define_interface_indirection(generic_multiply,29)
### define_interface_indirection(generic_negative,2a)
### define_interface_indirection(generic_add,2b)
### define_interface_indirection(generic_positive,2c)
### define_interface_indirection(generic_zero,2d)
###
define_interface_jsr_indirection(primitive_error,36)
define_interface_indirection(generic_quotient,37)
define_interface_indirection(generic_remainder,38)
define_interface_indirection(generic_modulo,39)

# Save an additional instruction here to load the dynamic link.
define_c_label(asm_interrupt_dlink)
	mov.l	dlink,%d2		# Dynamic link -> d2
	movq	&HEX(19),%d0
	bra	scheme_to_interface_jsr

# Bum this one for speed.
define_c_label(asm_primitive_apply)
	switch_to_C_registers()
	allocate_utility_result()
	mov.l	%d1,-(%sp)		# only one argument
	ifdef(`SUNASM',
	`lea	extern_c_label(utility_table),%a0
	mov.l	HEX(12)*4(%a0),%a0',
	`mov.l	extern_c_label(utility_table)+HEX(12)*4,%a0')
	utility_call(1)                 # one argument

	set	tc_compiled_entry,HEX(28)
	set	tc_flonum,HEX(06)
	set	tc_fixnum,HEX(1A)
	set	tc_manifest_nmv,HEX(27)
	set	tc_false,HEX(0)
	set	tc_true,HEX(8)
	set	offset_apply,HEX(14)

define(call_utility,
	`movq	&offset_$1,%d0
	bra	scheme_to_interface')

### Called by Scheme when invoking an unknown procedure.
### Having this short sequence in assembly language avoids the C call
### in the common case where the procedure is compiled and the number
### of arguments is correct.
### The number of actual arguments is in d2, the procedure on top
### of the stack.

define_c_label(asm_shortcircuit_apply)
define_debugging_label(shortcircuit_apply)
	EXTRACT_TYPE_CODE((%sp),%d0)	# Get procedure type
	mov.l	(%sp)+,%d1		# Get procedure
	COMPARE_TYPE_CODE(%d0,tc_compiled_entry)
	bne.b	shortcircuit_apply_1
	mov.l	%d1,%d3			# Extract entry point
	and.l	rmask,%d3
	mov.l	%d3,%a0
	mov.b	-3(%a0),%d3		# Extract the frame size
	ext.w	%d3
	cmp.w	%d2,%d3			# Is the frame size right?
	bne.b	shortcircuit_apply_1
	jmp	(%a0)			# Invoke

define_debugging_label(shortcircuit_apply_1)
	call_utility(apply)

### Optimized versions of shortcircuit_apply for 0-7 arguments.

define(define_apply_size_n,
`define_c_label(asm_shortcircuit_apply_size_$1)
define_debugging_label(shortcircuit_apply_size_$1)
	EXTRACT_TYPE_CODE((%sp),%d0)	# Get procedure type
	mov.l	(%sp)+,%d1		# Get procedure
	COMPARE_TYPE_CODE(%d0,tc_compiled_entry)
	bne.b	shortcircuit_apply_size_$1_1
	mov.l	%d1,%d3			# Extract entry point
	and.l	rmask,%d3
	mov.l	%d3,%a0
	cmp.b	-3(%a0),&$1		# Is the frame size right?
	bne.b	shortcircuit_apply_size_$1_1
	jmp	(%a0)			# Invoke

define_debugging_label(shortcircuit_apply_size_$1_1)
	movq	&$1,%d2			# initialize frame size
	call_utility(apply)')

define_apply_size_n(1)
define_apply_size_n(2)
define_apply_size_n(3)
define_apply_size_n(4)
define_apply_size_n(5)
define_apply_size_n(6)
define_apply_size_n(7)
define_apply_size_n(8)

###	This utility depends on the C compiler preserving d2-d7 and a2-a7.
###	It takes its parameters in d0 and d1, and returns its value in a0.

define_c_label(asm_allocate_closure)
	switch_to_C_registers()
	mov.l	%a1,-(%sp)		# Preserve reg.
	mov.l	%d1,-(%sp)		# Preserve reg.
	mov.l	%d0,-(%sp)		# Push arg.
	jsr	extern_c_label(allocate_closure)
	addq.l	&4,%sp			# Pop arg.
	mov.l	%d0,%a0			# Return value
	mov.l	(%sp)+,%d1		# Restore reg.
	mov.l	(%sp)+,%a1		# Restore reg.
	switch_to_scheme_registers()
	rts

###	These utilities improve the performance of floating point code
###	significantly.
###	Arguments on top of the stack followed by the return address.

define_debugging_label(asm_generic_flonum_result)
	mov.l	rfree,rval
	mov.l	&TYPE_CODE_TO_OBJECT(tc_manifest_nmv)+2,(rfree)+
	fmove.d	%fp0,(rfree)+
	or.l	&TYPE_CODE_TO_OBJECT(tc_flonum),rval
	and.b	&TYPE_CODE_TO_BYTE(1)-1,(%sp)
	rts

define_debugging_label(asm_true_result)
	mov.l	&TYPE_CODE_TO_OBJECT(tc_true),rval
	and.b	&TYPE_CODE_TO_BYTE(1)-1,(%sp)
	rts

define_debugging_label(asm_false_result)
	mov.l	&TYPE_CODE_TO_OBJECT(tc_false),rval
	and.b	&TYPE_CODE_TO_BYTE(1)-1,(%sp)
	rts

define(define_generic_unary,
`define_c_label(asm_generic_$1)
	EXTRACT_TYPE_CODE((%sp),%d0)	# Get arg1s type
 	COMPARE_TYPE_CODE(%d0,tc_flonum)
	bne.b	asm_generic_$1_hook
	mov.l	(%sp)+,%d0		# arg1
	and.l	rmask,%d0
	mov.l	%d0,%a0
	fmove.d	4(%a0),%fp0
	$3.b	&1,%fp0
	bra	asm_generic_flonum_result

asm_generic_$1_hook:
	movq	&HEX($2),%d0
	bra	scheme_to_interface')

define(define_generic_unary_predicate,
`define_c_label(asm_generic_$1)
	EXTRACT_TYPE_CODE((%sp),%d0)	# Get arg1s type
 	COMPARE_TYPE_CODE(%d0,tc_flonum)
	bne.b	asm_generic_$1_hook
	mov.l	(%sp)+,%d0		# arg1
	and.l	rmask,%d0
	mov.l	%d0,%a0
	fmove.d	4(%a0),%fp0
	fb$3	asm_true_result
	bra	asm_false_result

asm_generic_$1_hook:
	movq	&HEX($2),%d0
	bra	scheme_to_interface')

define(define_generic_binary,
`define_c_label(asm_generic_$1)
	EXTRACT_TYPE_CODE((%sp),%d0)	# Get arg1s type
	EXTRACT_TYPE_CODE(4(%sp),%d1)	# Get arg2s type
	mov.l	(%sp),%d2		# arg1
	mov.l	4(%sp),%d3		# arg2
	and.l	rmask,%d2
	and.l	rmask,%d3
	mov.l	%d2,%a0
	mov.l	%d3,%a1
 	COMPARE_TYPE_CODE(%d0,tc_flonum)
	bne.b	asm_generic_$1_fix_flo
	COMPARE_TYPE_CODE(%d1,tc_flonum)
	bne.b	asm_generic_$1_flo_fix
	fmove.d	4(%a0),%fp0
	$3.d	4(%a1),%fp0
	addq.l	&8,%sp
	bra	asm_generic_flonum_result

asm_generic_$1_fix_flo:
	COMPARE_TYPE_CODE(%d0,tc_fixnum)
	bne.b	asm_generic_$1_hook
	COMPARE_TYPE_CODE(%d1,tc_flonum)
	bne.b	asm_generic_$1_hook
	lsl.l	&TC_LENGTH,%d2
	asr.l	&TC_LENGTH,%d2
	fmove.l	%d2,%fp0
	$3.d	4(%a1),%fp0
	addq.l	&8,%sp
	bra	asm_generic_flonum_result

asm_generic_$1_flo_fix:
	COMPARE_TYPE_CODE(%d1,tc_fixnum)
	bne.b	asm_generic_$1_hook
	lsl.l	&TC_LENGTH,%d3
	asr.l	&TC_LENGTH,%d3
	fmove.d	4(%a0),%fp0
	$3.l	%d3,%fp0
	addq.l	&8,%sp
	bra	asm_generic_flonum_result

asm_generic_$1_hook:
	movq	&HEX($2),%d0
	bra	scheme_to_interface')

define(define_generic_binary_predicate,
`define_c_label(asm_generic_$1)
	EXTRACT_TYPE_CODE((%sp),%d0)	# Get arg1s type
	EXTRACT_TYPE_CODE(4(%sp),%d1)	# Get arg2s type
	mov.l	(%sp),%d2		# arg1
	mov.l	4(%sp),%d3		# arg2
	and.l	rmask,%d2
	and.l	rmask,%d3
	mov.l	%d2,%a0
	mov.l	%d3,%a1
 	COMPARE_TYPE_CODE(%d0,tc_flonum)
	bne.b	asm_generic_$1_fix_flo
	COMPARE_TYPE_CODE(%d1,tc_flonum)
	bne.b	asm_generic_$1_flo_fix
	addq.l	&8,%sp
	fmove.d	4(%a0),%fp0
	fcmp.d	%fp0,4(%a1)
	fb$3	asm_true_result
	bra	asm_false_result

asm_generic_$1_fix_flo:
	COMPARE_TYPE_CODE(%d0,tc_fixnum)
	bne.b	asm_generic_$1_hook
	COMPARE_TYPE_CODE(%d1,tc_flonum)
	bne.b	asm_generic_$1_hook
	addq.l	&8,%sp
	lsl.l	&TC_LENGTH,%d2
	asr.l	&TC_LENGTH,%d2
	fmove.l	%d2,%fp0
	fcmp.d	%fp0,4(%a1)
	fb$3	asm_true_result
	bra	asm_false_result

asm_generic_$1_flo_fix:
	COMPARE_TYPE_CODE(%d1,tc_fixnum)
	bne.b	asm_generic_$1_hook
	addq.l	&8,%sp
	lsl.l	&TC_LENGTH,%d3
	asr.l	&TC_LENGTH,%d3
	fmove.d	4(%a0),%fp0
	fcmp.l	%fp0,%d3
	fb$3	asm_true_result
	bra	asm_false_result

asm_generic_$1_hook:
	movq	&HEX($2),%d0
	bra	scheme_to_interface')

define_generic_unary(decrement,22,fsub)
define_generic_binary(divide,23,fdiv)
define_generic_binary_predicate(equal,24,eq)
define_generic_binary_predicate(greater,25,gt)
define_generic_unary(increment,26,fadd)
define_generic_binary_predicate(less,27,lt)
define_generic_binary(subtract,28,fsub)
define_generic_binary(multiply,29,fmul)
define_generic_unary_predicate(negative,2a,lt)
define_generic_binary(add,2b,fadd)
define_generic_unary_predicate(positive,2c,gt)
define_generic_unary_predicate(zero,2d,eq)

### Close-coded stack and interrupt check for use when stack checking
### is enabled.

define_c_label(asm_stack_and_interrupt_check_12)
	mov.l	&-12,-(%sp)
	bra.b	stack_and_interrupt_check

define_c_label(asm_stack_and_interrupt_check_14)
	mov.l	&-14,-(%sp)
	bra.b	stack_and_interrupt_check

define_c_label(asm_stack_and_interrupt_check_18)
	mov.l	&-18,-(%sp)
	bra.b	stack_and_interrupt_check

define_c_label(asm_stack_and_interrupt_check_22)
	mov.l	&-22,-(%sp)
	bra.b	stack_and_interrupt_check

define_c_label(asm_stack_and_interrupt_check_24)
	mov.l	&-24,-(%sp)
#	bra.b	stack_and_interrupt_check

### On entry, 4(%sp) contains the resumption address, and 0(%sp) is
### the offset between the resumption address and the GC label
### address.
define_debugging_label(stack_and_interrupt_check)

### If the Scheme stack pointer is <= Stack_Guard, then the stack has
### overflowed -- in which case we must signal a stack-overflow interrupt.
	cmp.l	%sp,regblock_stack_guard(regs)
	bgt.b	stack_and_interrupt_check_1

### Set the stack-overflow interrupt bit. If the stack-overflow
### interrupt is disabled, skip forward to gc test.  Otherwise, set
### MemTop to -1 and signal the interrupt.
	bset	&0,regblock_int_code+3(regs)
	btst	&0,regblock_int_mask+3(regs)
	beq.b	stack_and_interrupt_check_1
	mov.l	&-1,regblock_memtop(regs)
	bra.b	stack_and_interrupt_check_2

### If (Free >= MemTop), signal an interrupt.
stack_and_interrupt_check_1:
	cmp.l	rfree,regblock_memtop(regs)
	bge.b	stack_and_interrupt_check_2

### No action necessary -- return to resumption address.
	addq.l	&4,%sp
	rts

### Must signal the interrupt -- return to GC label instead.
stack_and_interrupt_check_2:
	mov.l	%d0,-(%sp)
	mov.l	4(%sp),%d0
	add.l	%d0,8(%sp)
	mov.l	(%sp),%d0
	addq.l	&8,%sp
	rts

### Assembly-language implementation of SET-INTERRUPT-ENABLES!
### primitive.  Argument appears at top of stack, return address below
### that.

define_c_label(asm_set_interrupt_enables)
define_debugging_label(set_interrupt_enables)
	# Return value is previous contents of mask register.
	mov.l	regblock_int_mask(regs),rval
	or.l	&TYPE_CODE_TO_OBJECT(tc_fixnum),rval
	mov.l	(%sp)+,%d0		# get new interrupt mask
	and.l	rmask,%d0		# strip fixnum type
	mov.l	%d0,regblock_int_mask(regs) # store it in mask register
	# Setup compiled memtop register: -1 if pending interrupt,
	# Memtop if GC enabled, else Heap_Top.
	movq	&-1,%d1
	mov.l	regblock_int_code(regs),%d2
	and.l	%d0,%d2
	bne.b	set_interrupt_enables_1
	mov.l	extern_c_label(MemTop),%d1
	btst	&2,%d0
	bne.b	set_interrupt_enables_1
	mov.l	extern_c_label(Heap_Top),%d1
set_interrupt_enables_1:
	mov.l	%d1,regblock_memtop(regs)
	# Setup compiled stack_guard register: Stack_Guard if
	# stack-overflow enabled, else Stack_Bottom
	mov.l	extern_c_label(Stack_Guard),%d1
	btst	&0,%d0
	bne.b	set_interrupt_enables_2
	mov.l	extern_c_label(Stack_Bottom),%d1
set_interrupt_enables_2:
	mov.l	%d1,regblock_stack_guard(regs)
	mov.l	(%sp)+,%d0
	and.l	rmask,%d0
	mov.l	%d0,%a0
	jmp	(%a0)
