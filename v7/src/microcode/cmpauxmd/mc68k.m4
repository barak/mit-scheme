### -*-Midas-*-
###
###	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpauxmd/mc68k.m4,v 1.1 1989/11/01 18:46:41 jinx Exp $
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

define(reference_external,
	`')		# Declare desire to reference an external
define(extern_c_label,`_$1')
			# The actual reference

define(define_c_label,
`	global	extern_c_label($1)
extern_c_label($1):')

define(define_debugging_label,
`	global	$1
$1:')

define(dlink, %a4)	# Dynamic link register (contains a pointer to
			# a return address) 
define(rfree, %a5)	# Free pointer
define(regs, %a6)	# Pointer to Registers[0]
define(rmask, %d7)	# Mask to clear type code

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

	text

### Callable by C conventions.  Swaps to Scheme register set and jumps
### to the entry point specified by its only argument.

define_c_label(C_to_interface)
	link.l	%a6,-44
	movm.l	%d2-%d7/%a2-%a5,4(%sp)
	mov.l	8(%a6),%a0		# Argument: entry point
	bra.b	interface_to_scheme_internal

### Called by linker-generated trampolines to invoke the appropriate
### C-written handler.  The return address on the stack is the address
### of the trampoline storage area, passed to the C handler as the
### first argument.

define_debugging_label(trampoline_to_interface)
	mov.l	(%sp)+,%d1
###	bra	scheme_to_interface	# Fall through

### Called by Scheme through a jump instruction in the register block.
### It expects an index in %d0, and 4 longword arguments in %d1-%d4

reference_external(utility_table)

define_debugging_label(scheme_to_interface)
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

define_c_label(Registers) # Move to C world & reformat latter
	space	regblock_length
define_debugging_label(data_patch_area)
	space	64

# This is in the data segment so that it can be modified!
define_debugging_label(code_patch_area)
	space	256	
