### -*-Midas-*-
###
###	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpauxmd/i386.m4,v 1.3 1992/02/14 21:08:48 jinx Exp $
###
###	Copyright (c) 1992 Massachusetts Institute of Technology
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

#### Intel i386 assembly language (gas syntax) part of the compiled
#### code interface.  See cmpint.txt, cmpint.c, cmpint-mc68k.h, and
#### cmpgc.h for more documentation.
####
#### NOTE:
####	Assumptions:
####
####	0) Segment registers and paging are set up for 32-bit "flat"
####	operation.
####
####	1) All registers (except double floating point registers) and
####	stack locations hold a C long object.
####
####	2) The C compiler divides registers into three groups:
####	- Linkage registers, used for procedure calls and global
####	references.  On i386 (gcc): %ebp, %esp.
####	- super temporaries, not preserved accross procedure calls and
####	always usable. On i386 (gcc): %eax, %edx, %ecx.
####	- preserved registers saved by the callee if they are written.
####	On i386 (gcc): all others (%ebx, %esi, %edi).
####
####	3) Arguments, if passed on a stack, are popped by the caller
####	or by the procedure return instruction (as on the VAX).  Thus
####	most "leaf" procedures need not worry about them.  On i386,
####	arguments are passed on the stack.
####
####	4) There is a hardware or software maintained stack for
####	control.  The procedure calling sequence may leave return
####	addresses in registers, but they must be saved somewhere for
####	nested calls and recursive procedures.  On i386: saved on
####    the stack by the CALL instruction.
####
####	5) C procedures return long values in a super temporary
####    register.  Two word structures are returned in super temporary
####    registers as well.  On i386, gcc uses %eax for long returns,
####	and returns two word structures in {%eax, %edx}.
####
####	6) Floating point registers are not preserved by this
####	interface.  The interface is only called from the Scheme
####	interpreter, which does not use floating point data.  Thus
####	although the calling convention would require us to preserve
####	them, they contain garbage.
####
#### Compiled Scheme code uses the following register convention:
####	- %esp containts the Scheme stack pointer, not the C stack
####	pointer.
####	- %esi contains a pointer to the Scheme interpreter's "register"
####	block.  This block contains the compiler's copy of MemTop,
####	the interpreter's registers (val, env, exp, etc.),
####	temporary locations for compiled code, and the addresses
####	of various hooks defined in this file.
####	- %edi contains the Scheme free pointer.
####	- %ebp contains the Scheme datum mask.
####	The dynamic link (when needed) is in Registers[REGBLOCK_COMPILER_TEMP]
####	Values are returned in Registers[REGBLOCK_VAL]
####
####	All other registers (%eax, %edx, %ecx, %ebx) are available to
####	the compiler.  A caller-saves convention is used, so the
####	registers need not be preserved by subprocedures.

####	Utility macros and definitions

ifdef(GCC,`errprint("cmpaux-i386.m4 only works with GCC!")')

define(use_external,`')			# Declare desire to use an external
define(external_reference,`_$1')	# The actual reference

define(define_c_label,
`.globl external_reference($1)
external_reference($1):')

define(define_debugging_label,
`.globl $1
$1:')

define(HEX, `0x$1')
define(TC_LENGTH, ifdef(`TYPE_CODE_LENGTH', TYPE_CODE_LENGTH, 8))
define(ADDRESS_MASK, eval(((2 ** (32 - TC_LENGTH)) - 1)))
define(IMMEDIATE, `$$1')

define(REGBLOCK_VAL,8)
define(REGBLOCK_COMPILER_TEMP,16)
define(REGBLOCK_LEXPR_ACTUALS,28)
define(REGBLOCK_PRIMITIVE,32)
define(REGBLOCK_CLOSURE_FREE,36)

define(REGBLOCK_DLINK,REGBLOCK_COMPILER_TEMP)
define(REGBLOCK_UTILITY_ARG4,REGBLOCK_CLOSURE_FREE)

define(regs,%esi)
define(rfree,%edi)
define(rmask,%ebp)

use_external(Free)
use_external(Registers)
use_external(Ext_Stack_Pointer)

	.file	"cmpaux-i386.m4"

.globl C_Stack_Pointer
.comm C_Stack_Pointer,4

.globl C_Frame_Pointer
.comm C_Frame_Pointer,4

.text
	.align 2
define_c_label(interface_initialize)
#	This needs to set the floating point mode.
	ret

define_c_label(C_to_interface)
	pushl	%ebp					# Link according
	movl	%esp,%ebp				#  to C's conventions
	pushl	%edi					# Save callee-saves
	pushl	%esi					#  registers
	pushl	%ebx
	movl	8(%ebp),%edx				# Entry point
	movl	%ebp,C_Frame_Pointer			# Preserve frame ptr
	movl	%esp,C_Stack_Pointer			# Preserve stack ptr
							# Register block = %esi
	movl	IMMEDIATE(external_reference(Registers)),regs
	jmp	external_reference(interface_to_scheme)

define_c_label(asm_trampoline_to_interface)
define_debugging_label(trampoline_to_interface)
	popl	%ecx					# trampoline storage
	jmp	scheme_to_interface

define_c_label(asm_scheme_to_interface_call)
define_debugging_label(scheme_to_interface_call)
	popl	%ecx					# arg1 = ret. add
	addl	IMMEDIATE(4),%ecx			# Skip format info
#	jmp	scheme_to_interface

define_c_label(asm_scheme_to_interface)
define_debugging_label(scheme_to_interface)
	movl	%esp,external_reference(Ext_Stack_Pointer)
	movl	rfree,external_reference(Free)
	movl	C_Stack_Pointer,%esp
	movl	C_Frame_Pointer,%ebp
	pushl	REGBLOCK_UTILITY_ARG4()(regs)		# Utility args
	pushl	%ebx
	pushl	%edx
	pushl	%ecx
	movl	external_reference(utility_table)(,%eax,4),%eax
	call	*%eax

define_debugging_label(scheme_to_interface_return)
	addl	IMMEDIATE(16),%esp			# Pop utility args
	jmp	*%eax					# Invoke handler

define_c_label(interface_to_scheme)
	movl	REGBLOCK_VAL()(regs),%eax		# Value/dynamic link
	movl	IMMEDIATE(ADDRESS_MASK),rmask 		# = %ebp
	movl	external_reference(Free),rfree		# Free pointer = %edi
	movl	external_reference(Ext_Stack_Pointer),%esp
	movl	%eax,%ecx				# Copy if used
	andl	rmask,%ecx				# Set up dynamic link
	movl	%ecx,REGBLOCK_DLINK()(regs)
	jmp	*%edx					# invoke entry point

define_c_label(interface_to_C)
	movl	%edx,%eax				# Set up result
	popl	%ebx					# Restore callee-saves
	popl	%esi					#  registers
	popl	%edi
	leave
	ret

define(define_jump_indirection,
`define_c_label(asm_$1)
	movl	IMMEDIATE(HEX($2)),%eax
	jmp	scheme_to_interface')
	
define(define_call_indirection,
`define_c_label(asm_$1)
	movl	IMMEDIATE(HEX($2)),%eax
	jmp	scheme_to_interface_call')
	
define_call_indirection(interrupt_procedure,1a)
define_call_indirection(interrupt_continuation,1b)
define_jump_indirection(interrupt_closure,18)

define_c_label(interrupt_dlink)
	movl	REGBLOCK_DLINK()(regs),%edx
	movl	IMMEDIATE(HEX(19)),%eax
	jmp	scheme_to_interface_call
