### -*-Midas-*-
###
###	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpauxmd/mc68k.m4,v 1.8 1989/12/10 00:49:54 cph Exp $
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
####    registers as well.  On MC68K: d0 is used for long returns.  GCC
####    returns two word structures in d0/d1, but many other compilers
####    return the address of the structure in a0.  The code below
####    must be changed if structures are not returned in d0/d1.
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

# Scheme object representation.  Must match object.h

define(HEX, `0x$1')
define(TC_LENGTH, ifdef(`TYPE_CODE_LENGTH', TYPE_CODE_LENGTH, 8))
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

	set	regblock_val,8		# from const.h (* 4)
	set	address_mask,HEX(ADDRESS_MASK)

# This must match the compiler (machin.scm)

define(dlink, %a4)			# Dynamic link register (contains a
					# pointer to a return address) 
define(rfree, %a5)			# Free pointer
define(regs, %a6)			# Pointer to Registers[0]
define(rmask, %d7)			# Mask to clear type code

reference_external(Ext_Stack_Pointer)
reference_external(Free)
reference_external(Registers)

# This must match the C compiler

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

### Initialize the 68881 if present.

define_c_label(interface_initialize)
	link.l	%a6,&0
	ifdef(`MC68881', `fmov.l	&0x7480,%fpcr')
	unlk	%a6
	rts

### Callable by C conventions.  Swaps to Scheme register set and jumps
### to the entry point specified by its only argument.

define_c_label(C_to_interface)
	link.l	%a6,&-44
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
define_interface_indirection(generic_decrement,22)
define_interface_indirection(generic_divide,23)
define_interface_indirection(generic_equal,24)
define_interface_indirection(generic_greater,25)
define_interface_indirection(generic_increment,26)
define_interface_indirection(generic_less,27)
define_interface_indirection(generic_subtract,28)
define_interface_indirection(generic_multiply,29)
define_interface_indirection(generic_negative,2a)
define_interface_indirection(generic_add,2b)
define_interface_indirection(generic_positive,2c)
define_interface_indirection(generic_zero,2d)

# Save an additional instruction here to load the dynamic link.
define_c_label(asm_interrupt_dlink)
	mov.l	%a4,%d2			# Dynamic link -> d2
	movq	&HEX(19),%d0
	bra	scheme_to_interface_jsr

# Bum this one for speed.
define_c_label(asm_primitive_apply)
	switch_to_C_registers()
	mov.l	%d1,-(%sp)		# only one argument
	mov.l	extern_c_label(utility_table)+HEX(12)*4,%a0
	jsr	(%a0)
	addq.l	&4,%sp			# pop the argument

### On return, %d0 contains the address of interface_to_scheme or
### interface_to_C.  %d1 contains the appropriate data for them.

	mov.l	%d0,%a0
	jmp	(%a0)

	set	tc_compiled_entry,HEX(28)
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
	bne.b	shortcircuit_apply_2
	and.l	rmask,%d1		# Extract entry point
	mov.l	%d1,%a0
	mov.b	-3(%a0),%d1		# Extract the frame size
	ext.w	%d1
	cmp.w	%d2,%d1			# Is the frame size right?
	bne.b	shortcircuit_apply_1
	jmp	(%a0)			# Invoke

define_debugging_label(shortcircuit_apply_1)
	mov.l	-4(%sp),%d1		# Recover the type code
					# Fall through
define_debugging_label(shortcircuit_apply_2)
	call_utility(apply)

### Optimized versions of shortcircuit_apply for 0-7 arguments.

define(define_apply_size_n,
`define_c_label(asm_shortcircuit_apply_size_$1)
define_debugging_label(shortcircuit_apply_size_$1)
	EXTRACT_TYPE_CODE((%sp),%d0)	# Get procedure type
	mov.l	(%sp)+,%d1		# Get procedure
	COMPARE_TYPE_CODE(%d0,tc_compiled_entry)
	bne.b	shortcircuit_apply_size_$1_2
	and.l	rmask,%d1		# Extract entry point
	mov.l	%d1,%a0
	cmp.b	-3(%a0),&$1		# Is the frame size right?
	bne.b	shortcircuit_apply_size_$1_1
	jmp	(%a0)			# Invoke

define_debugging_label(shortcircuit_apply_size_$1_1)
	mov.l	-4(%sp),%d1		# Recover the type code
					# Fall through
define_debugging_label(shortcircuit_apply_size_$1_2)
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
