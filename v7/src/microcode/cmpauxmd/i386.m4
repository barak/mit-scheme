### -*-Midas-*-
###
###	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpauxmd/i386.m4,v 1.1 1992/02/14 03:45:31 jinx Exp $
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

define(reference_external,`')		# Declare desire to use an external
define(extern_c_label,`_$1')		# The actual reference

define(define_c_label,
`.globl extern_c_label($1)
extern_c_label($1):')

define(define_debugging_label,
`.globl $1
$1:')

define(HEX, `0x$1')
define(TC_LENGTH, ifdef(`TYPE_CODE_LENGTH', TYPE_CODE_LENGTH, 8))
define(ADDRESS_MASK, eval(((2 ** (32 - TC_LENGTH)) - 1), 16))

define(REGBLOCK_VAL,8)
define(REGBLOCK_COMPILER_TEMP,16)
define(REGBLOCK_LEXPR_ACTUALS,28)
define(REGBLOCK_PRIMITIVE,32)
define(REGBLOCK_CLOSURE_FREE,36)
define(REGBLOCK_CLOSURE_SPACE,40)

define(REGBLOCK_DLINK,REGBLOCK_COMPILER_TEMP)
define(REGBLOCK_UTILITY_ARG4,REGBLOCK_CLOSURE_FREE)

define(regs,%esi)
define(rfree,%edi)
define(rmask,%ebp)

reference_external(Free)
reference_external(Registers)
reference_external(Ext_Stack_Pointer)

define(switch_to_scheme_registers,
	`movl	rmask,??(%ebp)
	movl	$HEX(ADDRESS_MASK),rmask
	')