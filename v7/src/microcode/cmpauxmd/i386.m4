### -*-Midas-*-
###
###	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpauxmd/i386.m4,v 1.11 1992/02/28 20:19:58 jinx Exp $
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

define(use_external,`')			# Declare desire to use an external
define(external_reference,`_$1')	# The actual reference

define(define_c_label,
`.globl external_reference($1)
external_reference($1):')

define(define_debugging_label,
`.globl $1
$1:')

define(HEX, `0x$1')
define(IMMEDIATE, `$$1')

define(TC_LENGTH, ifdef(`TYPE_CODE_LENGTH', TYPE_CODE_LENGTH, 8))
define(DATUM_LENGTH, eval(32 - TC_LENGTH))
define(DATUM_SHIFT, eval((2 ** DATUM_LENGTH)))
define(ADDRESS_MASK, eval((DATUM_SHIFT - 1)))
define(TAG, ($2 + ($1 * DATUM_SHIFT)))

define(TC_FALSE,0)
define(TC_FLONUM,6)
define(TC_TRUE,8)
define(TC_FIXNUM,26)
define(TC_MANIFEST_NM_VECTOR,39)
define(TC_COMPILED_ENTRY,40)

define(REGBLOCK_VAL,8)
define(REGBLOCK_COMPILER_TEMP,16)
define(REGBLOCK_LEXPR_ACTUALS,28)
define(REGBLOCK_PRIMITIVE,32)
define(REGBLOCK_CLOSURE_FREE,36)

define(REGBLOCK_DLINK,REGBLOCK_COMPILER_TEMP)
define(REGBLOCK_UTILITY_ARG4,REGBLOCK_CLOSURE_FREE)

define(COMPILER_REGBLOCK_N_FIXED,16)
define(COMPILER_REGBLOCK_N_HOOKS,80)
define(COMPILER_REGBLOCK_N_TEMPS,256)
define(COMPILER_FIXED_SIZE,1)
define(COMPILER_HOOK_SIZE,1)
define(COMPILER_TEMP_SIZE,3)
define(REGBLOCK_SIZE_IN_OBJECTS,
       eval((COMPILER_REGBLOCK_N_FIXED*COMPILER_FIXED_SIZE)
	    +(COMPILER_REGBLOCK_N_HOOKS*COMPILER_HOOK_SIZE)
	    +(COMPILER_REGBLOCK_N_TEMPS*COMPILER_TEMP_SIZE)))

define(regs,%esi)
define(rfree,%edi)
define(rmask,%ebp)

use_external(Free)
use_external(Registers)
use_external(Ext_Stack_Pointer)

	.file	"cmpaux-i386.s"

.data
	.align 2

.globl C_Stack_Pointer
.comm C_Stack_Pointer,4

.globl C_Frame_Pointer
.comm C_Frame_Pointer,4

define_debugging_label(Regstart)
	.space	128
define_c_label(Registers)
	.space	eval(REGBLOCK_SIZE_IN_OBJECTS*4)

.text
	.align 2

define_c_label(interface_initialize)
	pushl	%ebp
	movl	%esp,%ebp
	subl	IMMEDIATE(4),%esp
	fstcw	-2(%ebp)
	# Set rounding mode to round-to-even, precision control to double,
	# mask the inexact result exception, and unmask the other exceptions.
	andl	IMMEDIATE(0x0000f0e0),-4(%ebp)
	orl	IMMEDIATE(0x00000220),-4(%ebp)
	fldcw	-2(%ebp)
	movw	%cs,%ax					# Obtain code segment
	leave
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
	xorl	%ecx,%ecx
	movb	%eax,%ecx
	movl	external_reference(utility_table)(,%ecx,4),%eax
	call	*%eax

define_debugging_label(scheme_to_interface_return)
	addl	IMMEDIATE(16),%esp			# Pop utility args
	jmp	*%eax					# Invoke handler

define_c_label(interface_to_scheme)
	movl	external_reference(Free),rfree		# Free pointer = %edi
	movl	REGBLOCK_VAL()(regs),%eax		# Value/dynamic link
	movl	IMMEDIATE(ADDRESS_MASK),rmask 		# = %ebp
	movl	external_reference(Ext_Stack_Pointer),%esp
	movl	%eax,%ecx				# Preserve if used
	andl	rmask,%ecx				# Restore potential
	movl	%ecx,REGBLOCK_DLINK()(regs)		#  dynamic link
	jmp	*%edx

define_c_label(interface_to_C)
	ffree	%st (0)					# Free floating "regs"
	ffree	%st (1)
	ffree	%st (2)
	ffree	%st (3)
	ffree	%st (4)
	ffree	%st (5)
	ffree	%st (6)
	movl	%edx,%eax				# Set up result
	popl	%ebx					# Restore callee-saves
	popl	%esi					#  registers
	popl	%edi
	leave
	ret

###	Assembly language hooks used to reduce code size.
###	There is no time advantage to using these over using
###	scheme_to_interface (or scheme_to_interface_call), but the
###	code generated by the compiler can be somewhat smaller.

define(define_jump_indirection,
`define_c_label(asm_$1)
	movb	IMMEDIATE(HEX($2)),%al
	jmp	scheme_to_interface')
	
define(define_call_indirection,
`define_c_label(asm_$1)
	movb	IMMEDIATE(HEX($2)),%al
	jmp	scheme_to_interface_call')
	
define_call_indirection(interrupt_procedure,1a)
define_call_indirection(interrupt_continuation,1b)
define_jump_indirection(interrupt_closure,18)

define_c_label(asm_interrupt_dlink)
	movl	REGBLOCK_DLINK()(regs),%edx
	movb	IMMEDIATE(HEX(19)),%al
	jmp	scheme_to_interface_call

###
###	This sames even more instructions than primitive_apply
###	When the PC is not available.  Instead of jumping here,
###	a call instruction is used, and the longword offset to
###	the primitive object follows the call instruction.
###	This code loads the primitive object and merges with
###	apply_primitive
###

	.align	2
define_c_label(asm_short_primitive_apply)
	popl	%edx					# offset pointer
	movl	(%edx),%ecx				# offset
	movl	(%edx,%ecx),%ecx			# Primitive object
	jmp	external_reference(asm_primitive_apply)	# Merge

	.align	2
define_jump_indirection(primitive_apply,12)

define_jump_indirection(primitive_lexpr_apply,13)
define_jump_indirection(error,15)
define_call_indirection(link,17)
define_call_indirection(assignment_trap,1d)
define_call_indirection(reference_trap,1f)
define_call_indirection(safe_reference_trap,20)
define_call_indirection(primitive_error,36)

###	Assembly language hooks used to increase speed.

# define_jump_indirection(shortcircuit_apply,14)
# 
# define(define_apply_fixed_size,
# `define_c_label(asm_shortcircuit_apply_size_$1)
# 	movl	IMMEDIATE($1),%edx
# 	movb	IMMEDIATE(HEX(14)),%eax
# 	jmp	scheme_to_interface')

	.align	2
define_c_label(asm_shortcircuit_apply)
	movl	%ecx,%eax				# Copy for type code
	movl	%ecx,%ebx				# Copy for address
	shrl	IMMEDIATE(DATUM_LENGTH),%eax		# Select type code
	andl	rmask,%ebx				# Select datum
	cmpb	IMMEDIATE(TC_COMPILED_ENTRY),%al
	jne	asm_shortcircuit_apply_generic
	movsbl	-4(%ebx),%eax				# Extract frame size
	cmpl	%eax,%edx				# Compare to nargs+1
	jne	asm_shortcircuit_apply_generic
	jmp	*%ebx					# Invoke

define_debugging_label(asm_shortcircuit_apply_generic)
	movl	IMMEDIATE(HEX(14)),%eax
	jmp	scheme_to_interface	

define(define_apply_fixed_size,
`	.align	2
define_c_label(asm_shortcircuit_apply_size_$1)
	movl	%ecx,%eax				# Copy for type code
	movl	%ecx,%ebx				# Copy for address
	shrl	IMMEDIATE(DATUM_LENGTH),%eax		# Select type code
	andl	rmask,%ebx				# Select datum
	cmpb	IMMEDIATE(TC_COMPILED_ENTRY),%al
	jne	asm_shortcircuit_apply_generic_$1
	cmpb	IMMEDIATE($1),-4(%ebx)			# Compare frame size
	jne	asm_shortcircuit_apply_generic_$1	# to nargs+1
	jmp	*%ebx

asm_shortcircuit_apply_generic_$1:
	movl	IMMEDIATE($1),%edx
	movb	IMMEDIATE(HEX(14)),%eax
	jmp	scheme_to_interface')

define_apply_fixed_size(1)
define_apply_fixed_size(2)
define_apply_fixed_size(3)
define_apply_fixed_size(4)
define_apply_fixed_size(5)
define_apply_fixed_size(6)
define_apply_fixed_size(7)
define_apply_fixed_size(8)

###	The following code is used by generic arithmetic
###	whether the fixnum case is open-coded in line or not.
###	This takes care of fixnums and flonums so that the common
###	numeric types are much faster than the rare ones
###	(bignums, ratnums, recnums)

	.align	2
asm_generic_flonum_result:
	movl	IMMEDIATE(eval(TAG(TC_MANIFEST_NM_VECTOR,2))),(rfree)
	movl	rfree,%eax
	fstpl	4(rfree)				# fstpd
	orl	IMMEDIATE(eval(TAG(TC_FLONUM,0))),%eax
	andl	rmask,(%esp)
	addl	IMMEDIATE(12),rfree
	movl	%eax,REGBLOCK_VAL()(regs)
	ret

	.align	2
asm_generic_fixnum_result:
	andl	rmask,(%esp)
	orb	IMMEDIATE(TC_FIXNUM),%al
	rorl	IMMEDIATE(TC_LENGTH),%eax
	movl	%eax,REGBLOCK_VAL()(regs)
	ret

	.align	2
asm_generic_return_sharp_t:
	andl	rmask,(%esp)
	movl	IMMEDIATE(eval(TAG(TC_TRUE,0))),REGBLOCK_VAL()(regs)
	ret

	.align	2
asm_generic_return_sharp_f:
	andl	rmask,(%esp)
	movl	IMMEDIATE(eval(TAG(TC_FALSE,0))),REGBLOCK_VAL()(regs)
	ret

define(define_unary_operation,
`	.align	2
define_c_label(asm_generic_$1)
	popl	%edx
	movl	%edx,%eax
	shrl	IMMEDIATE(DATUM_LENGTH),%eax
	cmpb	IMMEDIATE(TC_FIXNUM),%al
	je	asm_generic_$1_fix
	cmpb	IMMEDIATE(TC_FLONUM),%al
	jne	asm_generic_$1_fail
	andl	rmask,%edx
	fld1
	$4	4(%edx)
	jmp	asm_generic_flonum_result

asm_generic_$1_fix:
	movl	%edx,%eax
	shll	IMMEDIATE(TC_LENGTH),%eax
	$3	IMMEDIATE(eval(2 ** TC_LENGTH)),%eax
	jno	asm_generic_fixnum_result

asm_generic_$1_fail:
	pushl	%edx
	movb	IMMEDIATE(HEX($2)),%al
	jmp	scheme_to_interface')

define(define_unary_predicate,
`	.align	2
define_c_label(asm_generic_$1)
	popl	%edx
	movl	%edx,%eax
	shrl	IMMEDIATE(DATUM_LENGTH),%eax
	cmpb	IMMEDIATE(TC_FIXNUM),%al
	je	asm_generic_$1_fix
	cmpb	IMMEDIATE(TC_FLONUM),%al
	jne	asm_generic_$1_fail
	andl	rmask,%edx
	fldl	4(%edx)
	ftst
	fstsw	%ax
	fstp	%st (0)
	sahf
	$4	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fix:
	movl	%edx,%eax
	shll	IMMEDIATE(TC_LENGTH),%eax
	cmpl	IMMEDIATE(0),%eax
	$3	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fail:
	pushl	%edx
	movb	IMMEDIATE(HEX($2)),%al
	jmp	scheme_to_interface')

define(define_binary_operation,
`	.align	2
define_c_label(asm_generic_$1)
	popl	%edx
	popl	%ebx
	movl	%edx,%eax
	movl	%ebx,%ecx
	shrl	IMMEDIATE(DATUM_LENGTH),%eax
	shrl	IMMEDIATE(DATUM_LENGTH),%ecx
	cmpb	IMMEDIATE(TC_FIXNUM),%al
	je	asm_generic_$1_fix
	cmpb	IMMEDIATE(TC_FLONUM),%al
	jne	asm_generic_$1_fail
	cmpb	IMMEDIATE(TC_FLONUM),%cl
	je	asm_generic_$1_flo_flo
	cmpb	IMMEDIATE(TC_FIXNUM),%cl
	jne	asm_generic_$1_fail
	shll	IMMEDIATE(TC_LENGTH),%ebx
	andl	rmask,%edx
	sarl	IMMEDIATE(TC_LENGTH),%ebx
	fldl	4(%edx)					# fldd
	movl	%ebx,(rfree)
	$5	(rfree)					# fisubl
	jmp	asm_generic_flonum_result

asm_generic_$1_fix:
	cmpb	IMMEDIATE(TC_FLONUM),%cl
	je	asm_generic_$1_fix_flo
	cmpb	IMMEDIATE(TC_FIXNUM),%cl
	jne	asm_generic_$1_fail
	movl	%edx,%eax
	movl	%ebx,%ecx
	shll	IMMEDIATE(TC_LENGTH),%eax
	shll	IMMEDIATE(TC_LENGTH),%ecx
	$3	%ecx,%eax				# subl
	jno	asm_generic_fixnum_result

asm_generic_$1_fail:
	pushl	%ebx
	pushl	%edx
	movb	IMMEDIATE(HEX($2)),%al
	jmp	scheme_to_interface

asm_generic_$1_flo_flo:
	andl	rmask,%edx
	andl	rmask,%ebx
	fldl	4(%edx)					# fldd
	$6	4(%ebx)					# fsubl
	jmp	asm_generic_flonum_result	

asm_generic_$1_fix_flo:
	shll	IMMEDIATE(TC_LENGTH),%edx
	andl	rmask,%ebx
	sarl	IMMEDIATE(TC_LENGTH),%edx
	fldl	4(%ebx)					# fldd
	movl	%edx,(rfree)
	$4	(rfree)					# fisubrl
	jmp	asm_generic_flonum_result')

	.align	2
define_c_label(asm_generic_divide)
	popl	%edx
	popl	%ebx
	movl	%edx,%eax
	movl	%ebx,%ecx
	shrl	IMMEDIATE(DATUM_LENGTH),%eax
	shrl	IMMEDIATE(DATUM_LENGTH),%ecx
	cmpb	IMMEDIATE(TC_FIXNUM),%al
	je	asm_generic_divide_fix
	cmpb	IMMEDIATE(TC_FLONUM),%al
	jne	asm_generic_divide_fail
	cmpb	IMMEDIATE(TC_FLONUM),%cl
	je	asm_generic_divide_flo_flo
	cmpb	IMMEDIATE(TC_FIXNUM),%cl
	jne	asm_generic_divide_fail
	movl	%ebx,%ecx
	shll	IMMEDIATE(TC_LENGTH),%ecx
	je	asm_generic_divide_fail
	andl	rmask,%edx
	sarl	IMMEDIATE(TC_LENGTH),%ecx
	fldl	4(%edx)					# fldd
	movl	%ecx,(rfree)
	fidivl	(rfree)
	jmp	asm_generic_flonum_result

asm_generic_divide_fix:
	cmpb	IMMEDIATE(TC_FLONUM),%cl
	jne	asm_generic_divide_fail
	movl	%edx,%ecx
	shll	IMMEDIATE(TC_LENGTH),%ecx
	je	asm_generic_divide_fail
	andl	rmask,%ebx
	sarl	IMMEDIATE(TC_LENGTH),%ecx
	fldl	4(%ebx)					# fldd
	movl	%ecx,(rfree)
	fidivrl	(rfree)
	jmp	asm_generic_flonum_result

asm_generic_divide_flo_flo:
	movl	%ebx,%ecx
	andl	rmask,%ecx
	fldl	4(%ecx)					# fldd
	ftst
	fstsw	%ax
	sahf
	je	asm_generic_divide_by_zero
	andl	rmask,%edx
	fdivrl	4(%edx)
	jmp	asm_generic_flonum_result	

asm_generic_divide_by_zero:
	fstp	%st (0)					# Pop second arg

asm_generic_divide_fail:
	pushl	%ebx
	pushl	%edx
	movb	IMMEDIATE(HEX(23)),%al
	jmp	scheme_to_interface

define(define_binary_predicate,
`	.align	2
define_c_label(asm_generic_$1)
	popl	%edx
	popl	%ebx
	movl	%edx,%eax
	movl	%ebx,%ecx
	shrl	IMMEDIATE(DATUM_LENGTH),%eax
	shrl	IMMEDIATE(DATUM_LENGTH),%ecx
	cmpb	IMMEDIATE(TC_FIXNUM),%al
	je	asm_generic_$1_fix
	cmpb	IMMEDIATE(TC_FLONUM),%al
	jne	asm_generic_$1_fail
	cmpb	IMMEDIATE(TC_FLONUM),%cl
	je	asm_generic_$1_flo_flo
	cmpb	IMMEDIATE(TC_FIXNUM),%cl
	jne	asm_generic_$1_fail
	shll	IMMEDIATE(TC_LENGTH),%ebx
	andl	rmask,%edx
	sarl	IMMEDIATE(TC_LENGTH),%ebx
	fldl	4(%edx)					# fldd
	movl	%ebx,(rfree)
	ficompl	(rfree)
	fstsw	%ax
	sahf
	$5	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fix:
	cmpb	IMMEDIATE(TC_FLONUM),%cl
	je	asm_generic_$1_fix_flo
	cmpb	IMMEDIATE(TC_FIXNUM),%cl
	jne	asm_generic_$1_fail
	shll	IMMEDIATE(TC_LENGTH),%edx
	shll	IMMEDIATE(TC_LENGTH),%ebx
	cmpl	%ebx,%edx
	$3	asm_generic_return_sharp_t	
	jmp	asm_generic_return_sharp_f

asm_generic_$1_flo_flo:
	andl	rmask,%edx
	andl	rmask,%ebx
	fldl	4(%edx)					# fldd
	fcompl	4(%ebx)
	fstsw	%ax
	sahf
	$6	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fix_flo:
	shll	IMMEDIATE(TC_LENGTH),%edx
	andl	rmask,%ebx
	sarl	IMMEDIATE(TC_LENGTH),%edx
	movl	%edx,(rfree)
	fildl	(rfree)
	fcompl	4(%ebx)
	fstsw	%ax
	sahf
	$4	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fail:
	pushl	%ebx
	pushl	%edx
	movb	IMMEDIATE(HEX($2)),%al
	jmp	scheme_to_interface')

# define_jump_indirection(generic_decrement,22)
# define_jump_indirection(generic_divide,23)
# define_jump_indirection(generic_equal,24)
# define_jump_indirection(generic_greater,25)
# define_jump_indirection(generic_increment,26)
# define_jump_indirection(generic_less,27)
# define_jump_indirection(generic_subtract,28)
# define_jump_indirection(generic_multiply,29)
# define_jump_indirection(generic_negative,2a)
# define_jump_indirection(generic_add,2b)
# define_jump_indirection(generic_positive,2c)
# define_jump_indirection(generic_zero,2d)
define_jump_indirection(generic_quotient,37)
define_jump_indirection(generic_remainder,38)
define_jump_indirection(generic_modulo,39)

define_unary_operation(decrement,22,subl,fsubrl)
define_unary_operation(increment,26,addl,faddl)

define_unary_predicate(negative,2a,jl,jb)
define_unary_predicate(positive,2c,jg,ja)
define_unary_predicate(zero,2d,je,je)

# define_binary_operation(name,index,fix*fix,fix*flo,flo*fix,flo*flo)
# define_binary_operation(  $1,   $2,     $3,     $4,     $5,     $6)
define_binary_operation(add,2b,addl,fiaddl,fiaddl,faddl)
define_binary_operation(subtract,28,subl,fisubrl,fisubl,fsubl)
define_binary_operation(multiply,29,imull,fimull,fimull,fmull)
# Divide needs to check for 0, so we can't really use the following
# define_binary_operation(divide,23,NONE,fidivrl,fidivl,fdivl)

# define_binary_predicate(name,index,fix*fix,fix*flo,flo*fix,flo*flo)
define_binary_predicate(equal,24,je,je,je,je)
define_binary_predicate(greater,25,jg,ja,ja,ja)
define_binary_predicate(less,27,jl,jb,jb,jb)
