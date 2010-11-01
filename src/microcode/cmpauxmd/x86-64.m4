### -*-Midas-*-
###
### Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993,
###     1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
###     2004, 2005, 2006, 2007, 2008, 2009, 2010 Massachusetts
###     Institute of Technology
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

### AMD x86-64 assembly language part of the compiled code interface.
### See cmpint.txt, cmpint.c, cmpint-mc68k.h, and cmpgc.h for more
### documentation.
###
### This m4 source expands into either Unix (gas) source or PC
### (masm/wasm) source.
###
### NOTE:
###	Assumptions:
###
###	0) Segment registers and paging are set up for 64-bit "flat"
###	operation.
###
###	1) All registers and stack locations hold a C long object.
###
###	2) The C compiler divides registers into three groups:
###	- Linkage registers, used for procedure calls and global
###	references.  On AMD64 Unix ABI: %rbp, %rsp.
###	- super temporaries, not preserved accross procedure calls and
###	always usable. On AMD64 Unix ABI: everything but what is
###	listed below.
###	- preserved registers saved by the callee if they are written.
###	On AMD64 Unix ABI: %rbp, %rbx, %r12-%r15, MXCSR, x87 control
###	word.
###
###	3) Arguments, if passed on a stack, are popped by the caller
###	or by the procedure return instruction (as on the VAX).  Thus
###	most "leaf" procedures need not worry about them.  On x86-64,
###	arguments beyond the sixth are passed on the stack; the first
###	through sixth are passed in %rdi, %rsi, %rdx, %rcx, %r8, or
###	%r9.  (Non-integer arguments are passed in other ways.)
###
###	4) There is a hardware or software maintained stack for
###	control.  The procedure calling sequence may leave return
###	addresses in registers, but they must be saved somewhere for
###	nested calls and recursive procedures.  On x86-64: saved on
###	the stack by the CALL instruction.
###
###	5) C procedures return long values in a super temporary
###	register.  Two word structures are returned in super temporary
###	registers as well in the AMD64 Unix ABI: %rax and %rdi.
###
###	6) Floating point registers are not preserved by this
###	interface.  The interface is only called from the Scheme
###	interpreter, which does not use floating point data.  Thus
###	although the calling convention would require us to preserve
###	them, they contain garbage.
###
### Compiled Scheme code uses the following register convention:
###	- %rsp contains the Scheme stack pointer, not the C stack
###	pointer.
###	- %rsi contains a pointer to the Scheme interpreter's "register"
###	block.  This block contains the compiler's copy of MemTop,
###	the interpreter's registers (val, env, exp, etc.),
###	temporary locations for compiled code, and the addresses
###	of various hooks defined in this file.
###	- %rdi contains the Scheme free pointer.
###	- %rbp contains the Scheme datum mask.
###	The dynamic link (when needed) is in Registers[REGBLOCK_COMPILER_TEMP]
###	Values are returned in Registers[REGBLOCK_VAL]
###	[TRC 20091025: Later, we ought to use machine registers for
###	these.]
###
###	All other registers are available to the compiler.  A
###	caller-saves convention is used, so the registers need not be
###	preserved by subprocedures.

### The following m4 macros can be defined to change how this file is
### expanded.
###
### DASM
###	If defined, expand to Intel assembly-language syntax, used by
###	Microsoft assembler (MASM) and Watcom assembler (WASM).
###	Otherwise, expand to AT&T syntax, used by GAS.  [TRC 20091025:
###	The Intel syntax probably won't work here.]
###
### WIN32
###	If defined, expand to run under Win32; implies DASM.
###
### SUPPRESS_LEADING_UNDERSCORE
###	If defined, external symbol names are generated as written;
###	otherwise, they have an underscore prepended to them.
### WCC386
###	Should be defined when using Watcom assembler.
### WCC386R
###	Should be defined when using Watcom assembler and generating
###	code to use the Watcom register-based argument conventions.
### TYPE_CODE_LENGTH
###	Normally defined to be 6.  Don't change this unless you know
###	what you're doing.
### VALGRIND_MODE
###	If defined, modify code to make it work with valgrind.

####	Utility macros and definitions

ifdef(`WIN32',
      `define(IF_WIN32,`$1')',
      `define(IF_WIN32,`')')

IF_WIN32(`define(DASM,1)')
ifdef(`WCC386R',`define(WCC386,1)')

ifdef(`DASM',
      `define(IFDASM,`$1')',
      `define(IFDASM,`')')

ifdef(`DASM',
      `define(IFNDASM,`')',
      `define(IFNDASM,`$1')')

ifdef(`DASM',
      `define(use_external_data,`	extrn $1':dword)',
      `define(use_external_data,`')')

ifdef(`DASM',
       `define(use_external_code,`	extrn $1':near)',
       `define(use_external_code,`')')

ifdef(`DASM',
      `define(export_label,`	public $1')',
      `define(export_label,`	.globl $1')')

IFNDASM(`	.file	"cmpaux-x86-64.s"')

# GAS doesn't implement these, for no obvious reason.
IFNDASM(`define(pushad,`pusha')')
IFNDASM(`define(popad,`popa')')
IFNDASM(`define(pushfd,`pushf')')
IFNDASM(`define(popfd,`popf')')

ifdef(`SUPPRESS_LEADING_UNDERSCORE',
       `define(EVR,`$1')',
       `define(EVR,`_$1')')

# When using the Watcom C compiler with register-based calling
# conventions, source-code function names normally expand to `FOO_',
# but functions that are compiled with prefix keywords such as
# `__cdecl' or `__syscall' expand differently.  References to the
# former type of name are marked with `EFR', while references to the
# latter are marked with `EPFR'.

ifdef(`SUPPRESS_LEADING_UNDERSCORE',
      `define(EPFR,`$1')',
      `define(EPFR,`_$1')')

ifdef(`WCC386R',
      `define(EFR,`$1_')',
      `define(EFR,`EPFR($1)')')

define(hook_reference,`EFR(asm_$1)')

define(define_data,`export_label(EVR($1))')

define(define_code_label,`
export_label($1)
$1:')

define(define_c_label,`define_code_label(EPFR($1))')
define(define_debugging_label,`define_code_label($1)')
define(define_hook_label,`define_code_label(hook_reference($1))')

ifdef(`DASM',
      `define(DECLARE_DATA_SEGMENT,`	.data')',
      `define(DECLARE_DATA_SEGMENT,`	.data')')

ifdef(`DASM',
      `define(DECLARE_CODE_SEGMENT,`	.code')',
      `define(DECLARE_CODE_SEGMENT,`	.text')')

ifdef(`DASM',
      `define(declare_alignment,`	align $1')',
      `define(declare_alignment,`	.align $1')')

ifdef(`DASM',
      `define(allocate_word,`EVR($1) dw 0')',
      `define(allocate_word,`	.comm EVR($1),2')')

ifdef(`DASM',
      `define(allocate_longword,`EVR($1) dd 0')',
      `define(allocate_longword,`	.comm EVR($1),4')')

ifdef(`DASM',
      `define(allocate_quadword,`EVR($1) dq 0')',
      `define(allocate_quadword,`	.comm EVR($1),8')')

ifdef(`DASM',
      `define(allocate_space,`EVR($1) db $2 dup (0)')',
      `define(allocate_space,`EVR($1):
	.space $2')')

ifdef(`DASM',
      `define(define_double,`EVR($1) dq $2')',
      `define(define_double,`EVR($1):	.double $2')')

ifdef(`DASM',
      `define(HEX, `0$1H')',
      `define(HEX, `0x$1')')

ifdef(`DASM',
      `define(OP,`$1$3')',
      `define(OP,`$1$2')')

ifdef(`DASM',
      `define(TW,`$2,$1')',
      `define(TW,`$1,$2')')

ifdef(`DASM',
      `define(ABS, `dword ptr $1')',
      `define(ABS, `$1(%rip)')')

ifdef(`DASM',
      `define(IMM, `$1')',
      `define(IMM, `$$1')')

ifdef(`DASM',
      `define(REG,`$1')',
      `define(REG,`%$1')')

ifdef(`DASM',
      `define(ST,`st($1)')',
      `define(ST,`%st ($1)')')

ifdef(`DASM',
      `define(IND,`dword ptr [$1]')',
      `define(IND,`($1)')')

ifdef(`DASM',
      `define(BOF,`byte ptr $1[$2]')',
      `define(BOF,`$1($2)')')

ifdef(`DASM',
      `define(WOF,`word ptr $1[$2]')',
      `define(WOF,`$1($2)')')

ifdef(`DASM',
      `define(LOF,`dword ptr $1[$2]')',
      `define(LOF,`$1($2)')')

ifdef(`DASM',
      `define(QOF,`qword ptr $1[$2]')',
      `define(QOF,`$1($2)')')

ifdef(`DASM',
      `define(IDX,`dword ptr [$1] [$2]')',
      `define(IDX,`($1,$2)')')

ifdef(`DASM',
      `define(SDX,`dword ptr $1[$2+$3*$4]')',
      `define(SDX,`$1($2,$3,$4)')')

ifdef(`DASM',
      `define(IJMP,`$1')',
      `define(IJMP,`*$1')')

define(TC_LENGTH, ifdef(`TYPE_CODE_LENGTH', TYPE_CODE_LENGTH, 6))
define(DATUM_LENGTH, eval(64 - TC_LENGTH))
define(DATUM_SHIFT, eval(1 << DATUM_LENGTH))
# This doesn't work because m4 is !@#&$*%^!#!$(%!&*@#^(.
#define(ADDRESS_MASK, eval(DATUM_SHIFT - 1))
define(ADDRESS_MASK, HEX(3ffffffffffffff))
# TAG doesn't work for the same reason.
#define(TAG, ($2 + ($1 * DATUM_SHIFT)))

define(TC_FALSE,0)
define(TC_FLONUM,6)
define(TC_TRUE,8)
define(TC_FIXNUM,26)
define(TC_MANIFEST_NM_VECTOR,39)
define(TC_COMPILED_ENTRY,40)

# TAG doesn't work due to m4 stupidity, so define these magic
# constants here.  These are computed in terms of the parameters
# above, and ordered lexicographically.

define(IMM_DETAGGED_FIXNUM_MINUS_ONE, `IMM(HEX(ffffffffffffffc0))')
define(IMM_FALSE, `IMM(HEX(0000000000000000))')
define(IMM_FIXNUM_0, `IMM(HEX(6800000000000000))')
define(IMM_FLONUM_0, `IMM(HEX(1800000000000000))')
define(IMM_MANIFEST_NM_VECTOR_1, `IMM(HEX(9c00000000000001))')
define(IMM_TRUE, `IMM(HEX(2000000000000000))')

# Flonums are represented by tagged pointers to the first of two
# quadwords (sixteen bytes) in memory, the first of which is a
# non-marked vector manifest of length 1, so that the GC will not
# trace the other one, which is an IEEE 754 double-precision format
# value.
define(FLONUM_DATA_OFFSET,8)
define(FLONUM_STORAGE_SIZE,16)

define(REGBLOCK_VAL,16)
define(REGBLOCK_COMPILER_TEMP,32)
define(REGBLOCK_DLINK,REGBLOCK_COMPILER_TEMP)

# Define the floating-point processor control word.  Always set
# round-to-even and double precision.  Under Win32, mask all
# exceptions.  Under unix and OS/2, mask only the inexact result
# exception.
ifdef(`WIN32',
      `define(FP_CONTROL_WORD,HEX(023f))',
      `define(FP_CONTROL_WORD,HEX(0220))')

define(regs,REG(rsi))
define(rfree,REG(rdi))
define(rmask,REG(rbp))

IFDASM(`.586p
.model flat')

DECLARE_DATA_SEGMENT()
declare_alignment(2)

use_external_data(EVR(Free))
use_external_data(EVR(stack_pointer))
use_external_data(EVR(utility_table))

ifdef(`WIN32',`
use_external_data(EVR(RegistersPtr))
',`
use_external_data(EVR(Registers))
')

define_data(C_Stack_Pointer)
allocate_quadword(C_Stack_Pointer)

define_data(C_Frame_Pointer)
allocate_quadword(C_Frame_Pointer)

declare_alignment(8)
define_double(flonum_zero,0.0)
define_double(flonum_one,1.0)

DECLARE_CODE_SEGMENT()
declare_alignment(2)

define_c_label(x86_64_fpe_reset_traps)
	OP(push,q)	REG(rbp)
	OP(mov,q)	TW(REG(rsp),REG(rbp))
	OP(sub,q)	TW(IMM(8),REG(rsp))
	stmxcsr		IND(REG(rsp))
	# Clear 7 (invalid operation mask)
	#       8 (denormalized operand mask)
	#       9 (zero-divide exception mask)
	#       10 (overflow exception mask)
	#       11 (underflow exception mask)
	#       15 (flush-to-zero (if set, gives non-IEEE semantics))
	OP(and,l)	TW(IMM(HEX(ffff707f)),IND(REG(rsp)))
	# Set   12 (precision exception mask)
	OP(or,l)	TW(IMM(HEX(1000)),IND(REG(rsp)))
	ldmxcsr		IND(REG(rsp))
	leave
	ret

# Call a function (rdi) with an argument (rsi) and a stack pointer and
# frame pointer from inside C.  When it returns, restore the original
# stack pointer.  This kludge is necessary for operating system
# libraries (notably NetBSD's libpthread) that store important
# information in the stack pointer, and get confused when they are
# called in a signal handler for a signal delivered while Scheme has
# set esp to something they consider funny.

define_c_label(within_c_stack)
	OP(mov,q)	TW(ABS(EVR(C_Stack_Pointer)),REG(rax))
	# Are we currently in C, signalled by having no saved C stack pointer?
	OP(cmp,q)	TW(IMM(0),REG(rax))
	# Yes: just call the function without messing with rsp.
	je		within_c_stack_from_c
	# No: we have to switch rsp to point into the C stack.
	OP(push,q)	REG(rbp)			# Save frame pointer
	OP(mov,q)	TW(REG(rsp),REG(rbp))
	OP(mov,q)	TW(REG(rax),REG(rsp))		# Switch to C stack
	OP(push,q)	REG(rbp)			# Save stack pointer
	OP(mov,q)	TW(REG(rdi),REG(rax))		# arg1 (fn) -> rax
	OP(mov,q)	TW(REG(rsi),REG(rdi))		# arg2 (arg) -> arg1
	call		IJMP(REG(rax))			# call fn(arg)

define_debugging_label(within_c_stack_restore)
	OP(pop,q)	REG(rsp)			# Restore stack pointer
							#   and switch back to
							#   Scheme stack
	OP(pop,q)	REG(rbp)			# Restore frame pointer
	ret

define_debugging_label(within_c_stack_from_c)
	OP(mov,q)	TW(REG(rdi),REG(rax))		# arg1 (fn) -> rax
	OP(mov,q)	TW(REG(rsi),REG(rdi))		# arg2 (arg) -> arg1
	jmp		IJMP(REG(rax))			# tail-call fn(arg)

# C_to_interface passes control from C into Scheme.  To C it is a
# unary procedure; its one argument is passed in rdi.  It saves the
# state of the C world (the C frame pointer and stack pointer) and
# then passes control to interface_to_scheme to set up the state of
# the Scheme world.
#
# Note:  The AMD64 ABI mandates that on entry to a function, RSP - 8
# must be a multiple of 0x10; that is, the stack must be 128-bit
# aligned.  We push six quadwords onto the stack, but there is already
# a return address on the stack, for a total of seven quadwords, which
# is misaligned.  Hence we push an extra dummy zero onto the stack,
# which we must pop off in interface_to_C.

define_c_label(C_to_interface)
	OP(push,q)	REG(rbp)			# Link according
	OP(mov,q)	TW(REG(rsp),REG(rbp))		#  to C's conventions
	OP(push,q)	REG(rbx)			# Save callee-saves
	OP(push,q)	REG(r12)			#  registers
	OP(push,q)	REG(r13)
	OP(push,q)	REG(r14)
	OP(push,q)	REG(r15)
	OP(push,q)	IMM(0)				# Align stack
	OP(mov,q)	TW(REG(rdi),REG(rdx))		# Entry point
							# Preserve frame ptr
	OP(mov,q)	TW(REG(rbp),ABS(EVR(C_Frame_Pointer)))
							# Preserve stack ptr
	OP(mov,q)	TW(REG(rsp),ABS(EVR(C_Stack_Pointer)))
	jmp	EPFR(interface_to_scheme)

define_hook_label(trampoline_to_interface)
define_debugging_label(trampoline_to_interface)
	OP(pop,q)	REG(rbx)			# trampoline storage
	jmp	scheme_to_interface

define_hook_label(scheme_to_interface_call)
define_debugging_label(scheme_to_interface_call)
	OP(pop,q)	REG(rbx)			# arg1 = ret. add
	OP(add,q)	TW(IMM(4),REG(rbx))		# Skip format info
#	jmp	scheme_to_interface

# scheme_to_interface passes control from compiled Scheme code to a
# microcode utility.  The arguments for the utility go respectively in
# rbx, rdx, rcx, and r8.  This loosely matches the AMD64 calling
# convention, where arguments go respectively in rdi, rsi, rdx, rcx,
# and r8.  The differences are that scheme_to_interface uses rdi as an
# implicit first argument to the utility, and rsi is used in compiled
# code for the registers block, since the compiler can't allocate it
# as a general-purpose register because it doesn't admit byte-size
# operations.  Moreover, Scheme uses rdi as the free pointer register,
# which we have to save here in a location unknown to Scheme (the C
# `Free' variable), so it can't be set by compiled code.

define_hook_label(scheme_to_interface)
define_debugging_label(scheme_to_interface)
	OP(mov,q)	TW(REG(rsp),ABS(EVR(stack_pointer)))
	OP(mov,q)	TW(rfree,ABS(EVR(Free)))
	OP(mov,q)	TW(ABS(EVR(C_Stack_Pointer)),REG(rsp))
	OP(mov,q)	TW(ABS(EVR(C_Frame_Pointer)),REG(rbp))

	# Signal to within_c_stack that we are now in C land.
	OP(mov,q)	TW(IMM(0),ABS(EVR(C_Stack_Pointer)))

	OP(sub,q)	TW(IMM(16),REG(rsp))	# alloc struct return
	OP(mov,q)	TW(REG(rsp),REG(rdi))	# Structure is first argument.
	OP(mov,q)	TW(REG(rbx),REG(rsi))	# rbx -> second argument.

	# Find the utility.  rbx is now free as a temporary register
	# to hold the utility table.  rax initially stores the utility
	# number in its low eight bits and possibly garbage in the
	# rest; mask it off and then use it as an index into the
	# utility table, scaled by 8 (bytes per pointer).
	OP(lea,q)	TW(ABS(EVR(utility_table)),REG(rbx))
	OP(and,q)	TW(IMM(HEX(ff)),REG(rax))
	OP(mov,q)	TW(SDX(,REG(rbx),REG(rax),8),REG(rax))

	call		IJMP(REG(rax))

define_debugging_label(scheme_to_interface_return)
	OP(pop,q)	REG(rax)		# pop struct return
	OP(pop,q)	REG(rdx)
	jmp		IJMP(REG(rax))		# Invoke handler

define_c_label(interface_to_scheme)
ifdef(`WIN32',						# Register block = %rsi
`	OP(mov,q)	TW(ABS(EVR(RegistersPtr)),regs)',
`	OP(lea,q)	TW(ABS(EVR(Registers)),regs)')
	OP(mov,q)	TW(ABS(EVR(Free)),rfree)	# Free pointer = %rdi
	OP(mov,q)	TW(QOF(REGBLOCK_VAL(),regs),REG(rax)) # Value/dynamic link
	OP(mov,q)	TW(IMM(ADDRESS_MASK),rmask)	# = %rbp
	# Restore the C stack pointer, which we zeroed back in
	# scheme_to_interface, for within_c_stack.
	OP(mov,q)	TW(REG(rsp),ABS(EVR(C_Stack_Pointer)))
	OP(mov,q)	TW(ABS(EVR(stack_pointer)),REG(rsp))
	OP(mov,q)	TW(REG(rax),REG(rcx))		# Preserve if used
	OP(and,q)	TW(rmask,REG(rcx))		# Restore potential dynamic link
	OP(mov,q)	TW(REG(rcx),QOF(REGBLOCK_DLINK(),regs))
	jmp		IJMP(REG(rdx))

IF_WIN32(`
use_external_code(EFR(WinntExceptionTransferHook))
define_code_label(EFR(callWinntExceptionTransferHook))
	call	EFR(WinntExceptionTransferHook)
	mov	rdx,rax
')

define_c_label(interface_to_C)
	OP(mov,q)	TW(REG(rdx),REG(rax))		# Set up result
	# We need a dummy register for the POP (which is three bytes
	# shorter than ADD $8,RSP); since we're about to pop into r15
	# anyway, we may as well use that.
	OP(pop,q)	REG(r15)			# Undo stack alignment
	OP(pop,q)	REG(r15)			# Restore callee-saves
	OP(pop,q)	REG(r14)			#  registers
	OP(pop,q)	REG(r13)
	OP(pop,q)	REG(r12)
	OP(pop,q)	REG(rbx)
	leave
	ret

###	Assembly language hooks used to reduce code size.
###	There is no time advantage to using these over using
###	scheme_to_interface (or scheme_to_interface_call), but the
###	code generated by the compiler can be somewhat smaller.

define(define_jump_indirection,
`define_hook_label($1)
	OP(mov,b)	TW(IMM(HEX($2)),REG(al))
	jmp	scheme_to_interface')

define(define_call_indirection,
`define_hook_label($1)
	OP(mov,b)	TW(IMM(HEX($2)),REG(al))
	jmp	scheme_to_interface_call')

define_call_indirection(interrupt_procedure,1a)
define_call_indirection(interrupt_continuation,1b)
define_jump_indirection(interrupt_closure,18)
define_jump_indirection(interrupt_continuation_2,3b)

define_hook_label(interrupt_dlink)
	OP(mov,q)	TW(QOF(REGBLOCK_DLINK(),regs),REG(rdx))
	OP(mov,b)	TW(IMM(HEX(19)),REG(al))
	jmp	scheme_to_interface_call

declare_alignment(2)
define_jump_indirection(primitive_apply,12)

define_jump_indirection(primitive_lexpr_apply,13)
define_jump_indirection(error,15)
define_call_indirection(link,17)
define_call_indirection(assignment_trap,1d)
define_call_indirection(reference_trap,1f)
define_call_indirection(safe_reference_trap,20)
define_call_indirection(primitive_error,36)

###	Assembly language hooks used to increase speed.

# define_jump_indirection(sc_apply,14)
# 
# define(define_apply_fixed_size,
# `define_hook_label(sc_apply_size_$1)
# 	OP(mov,q)	TW(IMM($1),REG(rdx))
# 	OP(mov,b)	TW(IMM(HEX(14)),REG(al))
# 	jmp	scheme_to_interface')

declare_alignment(2)
define_hook_label(sc_apply)
	OP(mov,q)	TW(REG(rbx),REG(rax))		# Copy for type code
	OP(mov,q)	TW(REG(rbx),REG(rcx))		# Copy for address
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rax))	# Select type code
	OP(and,q)	TW(rmask,REG(rcx))		# Select datum
	OP(cmp,b)	TW(IMM(TC_COMPILED_ENTRY),REG(al))
	jne	asm_sc_apply_generic
	OP(movs,bq,x)	TW(BOF(-4,REG(rcx)),REG(rax))	# Extract frame size
	OP(cmp,q)	TW(REG(rax),REG(rdx))		# Compare to nargs+1
	jne	asm_sc_apply_generic
	jmp	IJMP(REG(rcx))				# Invoke

define_debugging_label(asm_sc_apply_generic)
	OP(mov,q)	TW(IMM(HEX(14)),REG(rax))
	jmp	scheme_to_interface	

define(define_apply_fixed_size,
`declare_alignment(2)
define_hook_label(sc_apply_size_$1)
	OP(mov,q)	TW(REG(rbx),REG(rax))		# Copy for type code
	OP(mov,q)	TW(REG(rbx),REG(rcx))		# Copy for address
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rax))	# Select type code
	OP(and,q)	TW(rmask,REG(rcx))		# Select datum
	OP(cmp,b)	TW(IMM(TC_COMPILED_ENTRY),REG(al))
	jne	asm_sc_apply_generic_$1
	OP(cmp,b)	TW(IMM($1),BOF(-4,REG(rcx)))	# Compare frame size
	jne	asm_sc_apply_generic_$1			# to nargs+1
	jmp	IJMP(REG(rcx))

asm_sc_apply_generic_$1:
	OP(mov,q)	TW(IMM($1),REG(rdx))
	OP(mov,b)	TW(IMM(HEX(14)),REG(al))
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

declare_alignment(2)
asm_generic_fixnum_result:
	OP(and,q)	TW(rmask,IND(REG(rsp)))
	OP(or,b)	TW(IMM(TC_FIXNUM),REG(al))
	OP(ror,q)	TW(IMM(TC_LENGTH),REG(rax))
	OP(mov,q)	TW(REG(rax),QOF(REGBLOCK_VAL(),regs))
	ret

declare_alignment(2)
asm_generic_flonum_result:
	OP(and,q)	TW(rmask,IND(REG(rsp)))
	OP(mov,q)	TW(IMM_MANIFEST_NM_VECTOR_1,REG(rcx))
	OP(mov,q)	TW(REG(rcx),IND(rfree))
	movsd		TW(REG(xmm0),QOF(FLONUM_DATA_OFFSET,rfree))
	OP(mov,q)	TW(IMM_FLONUM_0,REG(rax))
	OP(or,q)	TW(rfree,REG(rax))
	OP(lea,q)	TW(QOF(FLONUM_STORAGE_SIZE,rfree),rfree)
	OP(mov,q)	TW(REG(rax),QOF(REGBLOCK_VAL(),regs))
	ret

declare_alignment(2)
asm_generic_return_sharp_t:
	OP(and,q)	TW(rmask,IND(REG(rsp)))
	OP(mov,q)	TW(IMM_TRUE,REG(rax))
	OP(mov,q)	TW(REG(rax),QOF(REGBLOCK_VAL(),regs))
	ret

declare_alignment(2)
asm_generic_return_sharp_f:
	OP(and,q)	TW(rmask,IND(REG(rsp)))
	OP(mov,q)	TW(IMM_FALSE,REG(rax))
	OP(mov,q)	TW(REG(rax),QOF(REGBLOCK_VAL(),regs))
	ret

define(define_unary_operation,
`declare_alignment(2)
define_hook_label(generic_$1)
	OP(pop,q)	REG(rdx)
	OP(mov,q)	TW(REG(rdx),REG(rax))
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rax))
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(al))
	je	asm_generic_$1_fix
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(al))
	jne	asm_generic_$1_fail

asm_generic_$1_flo:
	OP(and,q)	TW(rmask,REG(rdx))
	movsd		TW(QOF(FLONUM_DATA_OFFSET,REG(rdx)),REG(xmm0))
	$4		TW(ABS(EVR(flonum_one)),REG(xmm0))
	jmp	asm_generic_flonum_result

asm_generic_$1_fix:
	OP(mov,q)	TW(REG(rdx),REG(rax))
	OP(shl,q)	TW(IMM(TC_LENGTH),REG(rax))
	OP($3,q)	TW(IMM(eval(1 << TC_LENGTH)),REG(rax))
	jno	asm_generic_fixnum_result

asm_generic_$1_fail:
	OP(push,q)	REG(rdx)
	OP(mov,b)	TW(IMM(HEX($2)),REG(al))
	jmp	scheme_to_interface')

define(define_unary_predicate,
`declare_alignment(2)
define_hook_label(generic_$1)
	OP(pop,q)	REG(rdx)
	OP(mov,q)	TW(REG(rdx),REG(rax))
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rax))
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(al))
	je	asm_generic_$1_fix
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(al))
	jne	asm_generic_$1_fail

asm_generic_$1_flo:
	OP(and,q)	TW(rmask,REG(rdx))
	movsd		TW(QOF(FLONUM_DATA_OFFSET,REG(rdx)),REG(xmm0))
	ucomisd		TW(ABS(EVR(flonum_zero)),REG(xmm0))
	$4	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fix:
	OP(mov,q)	TW(REG(rdx),REG(rax))
	OP(shl,q)	TW(IMM(TC_LENGTH),REG(rax))
	OP(cmp,q)	TW(IMM(0),REG(rax))
	$3	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fail:
	OP(push,q)	REG(rdx)
	OP(mov,b)	TW(IMM(HEX($2)),REG(al))
	jmp	scheme_to_interface')

define(define_binary_operation,
`define_binary_operation_with_setup($1,$2,$3,$4,
	`OP(shl,q)	TW(IMM(TC_LENGTH),REG(rax))')')

define(define_binary_operation_with_setup,
`declare_alignment(2)
define_hook_label(generic_$1)
	OP(pop,q)	REG(rdx)
	OP(pop,q)	REG(rbx)
	OP(mov,q)	TW(REG(rdx),REG(rax))
	OP(mov,q)	TW(REG(rbx),REG(rcx))
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rax))
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rcx))
	OP(cmp,b)	TW(REG(al),REG(cl))
	jne	asm_generic_$1_fail
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(al))
	je	asm_generic_$1_fix
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(al))
	jne	asm_generic_$1_fail

asm_generic_$1_flo:
	OP(and,q)	TW(rmask,REG(rdx))
	OP(and,q)	TW(rmask,REG(rbx))
	movsd		TW(QOF(FLONUM_DATA_OFFSET,REG(rdx)),REG(xmm0))
	$4		TW(QOF(FLONUM_DATA_OFFSET,REG(rbx)),REG(xmm0))
	jmp	asm_generic_flonum_result

asm_generic_$1_fix:
	OP(mov,q)	TW(REG(rdx),REG(rax))
	OP(mov,q)	TW(REG(rbx),REG(rcx))
	$5						# Set up rax.
	OP(shl,q)	TW(IMM(TC_LENGTH),REG(rcx))
	OP($3,q)	TW(REG(rcx),REG(rax))		# subq
	jno	asm_generic_fixnum_result

asm_generic_$1_fail:
	OP(push,q)	REG(rbx)
	OP(push,q)	REG(rdx)
	OP(mov,b)	TW(IMM(HEX($2)),REG(al))
	jmp	scheme_to_interface')

define(define_binary_predicate,
`declare_alignment(2)
define_hook_label(generic_$1)
	OP(pop,q)	REG(rdx)
	OP(pop,q)	REG(rbx)
	OP(mov,q)	TW(REG(rdx),REG(rax))
	OP(mov,q)	TW(REG(rbx),REG(rcx))
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rax))
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rcx))
	OP(cmp,b)	TW(REG(al),REG(cl))
	jne	asm_generic_$1_fail
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(al))
	je	asm_generic_$1_fix
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(al))
	jne	asm_generic_$1_fail

asm_generic_$1_flo:
	OP(and,q)	TW(rmask,REG(rdx))
	OP(and,q)	TW(rmask,REG(rbx))
	movsd		TW(QOF(FLONUM_DATA_OFFSET,REG(rdx)),REG(xmm0))
	ucomisd		TW(QOF(FLONUM_DATA_OFFSET,REG(rbx)),REG(xmm0))
	$4	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fix:
	OP(shl,q)	TW(IMM(TC_LENGTH),REG(rdx))
	OP(shl,q)	TW(IMM(TC_LENGTH),REG(rbx))
	OP(cmp,q)	TW(REG(rbx),REG(rdx))
	$3	asm_generic_return_sharp_t	
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fail:
	OP(push,q)	REG(rbx)
	OP(push,q)	REG(rdx)
	OP(mov,b)	TW(IMM(HEX($2)),REG(al))
	jmp	scheme_to_interface')

# Division is hairy.  I'm not sure whether this will do the right
# thing for infinities and NaNs.

define_hook_label(generic_divide)
	OP(pop,q)	REG(rdx)
	OP(pop,q)	REG(rbx)
	# We want to divide rdx by rbx.  First put the numerator's tag
	# in al and the denominator's tag in cl.
	OP(mov,q)	TW(REG(rdx),REG(rax))
	OP(mov,q)	TW(REG(rbx),REG(rcx))
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rax))
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rcx))
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(al))
	je	asm_generic_divide_fix
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(al))
	jne	asm_generic_divide_fail
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(cl))
	je	asm_generic_divide_flo_by_flo
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(cl))
	jne	asm_generic_divide_fail

asm_generic_divide_flo_by_fix:
	# Numerator (rdx) is a flonum, denominator (rbx) is a fixnum.
	OP(mov,q)	TW(REG(rbx),REG(rcx))
	OP(shl,q)	TW(IMM(TC_LENGTH),REG(rcx))
	# Division by zero -- bail.
	jz	asm_generic_divide_fail
	OP(and,q)	TW(rmask,REG(rdx))
	OP(sar,q)	TW(IMM(TC_LENGTH),REG(rcx))
	movsd		TW(QOF(FLONUM_DATA_OFFSET,REG(rdx)),REG(xmm0))
	OP(cvtsi2sd,q)	TW(REG(rcx),REG(xmm1))
	divsd		TW(REG(xmm1),REG(xmm0))
	jmp	asm_generic_flonum_result

asm_generic_divide_fix:
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(cl))
	jne asm_generic_divide_fail

asm_generic_divide_fix_by_flo:
	# Numerator (rdx) is a fixnum, denominator (rbx) is a flonum.
	OP(mov,q)	TW(REG(rbx),REG(rax))
	OP(and,q)	TW(rmask,REG(rax))
	OP(mov,q)	TW(REG(rdx),REG(rcx))
	movsd		TW(QOF(FLONUM_DATA_OFFSET,REG(rax)),REG(xmm1))
	OP(shl,q)	TW(IMM(TC_LENGTH),REG(rcx))
	jz	asm_generic_divide_zero_by_flo
	OP(sar,q)	TW(IMM(TC_LENGTH),REG(rcx))
	OP(cvtsi2sd,q)	TW(REG(rcx),REG(xmm0))
	divsd		TW(REG(xmm1),REG(xmm0))
	jmp	asm_generic_flonum_result

asm_generic_divide_zero_by_flo:
	# rcx contains zero, representing a numerator exactly zero.
	# Defer division of 0 by 0.0; otherwise, yield exactly zero.
	OP(cvtsi2sd,q)	TW(REG(rcx),REG(xmm0))
	ucomisd		TW(REG(xmm1),REG(xmm0))
	je	asm_generic_divide_fail
	OP(and,q)	TW(rmask,IND(REG(rsp)))
	OP(mov,q)	TW(IMM_FIXNUM_0,REG(rax))
	OP(mov,q)	TW(REG(rax),QOF(REGBLOCK_VAL(),regs))
	ret

asm_generic_divide_flo_by_flo:
	# Numerator (rdx) and denominator (rbx) are both flonums.
	OP(mov,q)	TW(REG(rdx),REG(rax))
	OP(mov,q)	TW(REG(rbx),REG(rcx))
	OP(and,q)	TW(rmask,REG(rax))
	OP(and,q)	TW(rmask,REG(rcx))
	movsd		TW(QOF(FLONUM_DATA_OFFSET,REG(rax)),REG(xmm0))
	movsd		TW(QOF(FLONUM_DATA_OFFSET,REG(rcx)),REG(xmm1))
	ucomisd		TW(ABS(EVR(flonum_zero)),REG(xmm1))
	je	asm_generic_divide_fail
	divsd		TW(REG(xmm1),REG(xmm0))
	jmp	asm_generic_flonum_result

asm_generic_divide_fail:
	OP(push,q)	REG(rbx)
	OP(push,q)	REG(rdx)
	OP(mov,b)	TW(IMM(HEX(23)),REG(al))
	jmp	scheme_to_interface

define_unary_operation(decrement,22,sub,subsd)
define_unary_operation(increment,26,add,addsd)

# define_unary_predicate(name,index,fxjcc,fljcc)
# define_unary_predicate(  $1,   $2,    $3,  $4)
define_unary_predicate(negative,2a,jl,jb)
define_unary_predicate(positive,2c,jg,ja)
define_unary_predicate(zero,2d,je,je)

# define_binary_operation(name,index,fxop,flop)
# define_binary_operation(  $1,   $2,  $3,  $4)
define_binary_operation(add,2b,add,addsd)
define_binary_operation(subtract,28,sub,subsd)

# To set up rax, kill its tag, but leave it unshifted; the other
# operand will be shifted already, so that it will already include the
# factor of 2^6 desired in the product.
define_binary_operation_with_setup(multiply,29,imul,mulsd,
	`OP(and,q)	TW(rmask,REG(rax))')

# define_binary_predicate(name,index,fixjcc,flojcc)
# define_binary_predicate(  $1,   $2, $3)
define_binary_predicate(equal,24,je,je)
define_binary_predicate(greater,25,jg,ja)
define_binary_predicate(less,27,jl,jb)

#define_jump_indirection(generic_decrement,22)
#define_jump_indirection(generic_divide,23)
#define_jump_indirection(generic_equal,24)
#define_jump_indirection(generic_greater,25)
#define_jump_indirection(generic_increment,26)
#define_jump_indirection(generic_less,27)
#define_jump_indirection(generic_subtract,28)
#define_jump_indirection(generic_multiply,29)
#define_jump_indirection(generic_negative,2a)
#define_jump_indirection(generic_add,2b)
#define_jump_indirection(generic_positive,2c)
#define_jump_indirection(generic_zero,2d)
define_jump_indirection(generic_quotient,37)
define_jump_indirection(generic_remainder,38)
define_jump_indirection(generic_modulo,39)

# Input and output in rax, shift count in rcx, all detagged fixnums.
# Return address is at the top of the stack, untagged.  This hook must
# not use any registers other than rax and rcx; if it does, the code
# to generate calls to it, in compiler/machines/x86-64/rulfix.scm,
# must clear the register map first.

define_hook_label(fixnum_shift)
	OP(sar,q)	TW(IMM(TC_LENGTH),REG(rcx))
	js	asm_fixnum_shift_negative

asm_fixnum_lsh:
	OP(cmp,q)	TW(IMM(DATUM_LENGTH),REG(rcx))
	jge	asm_fixnum_lsh_overflow
	OP(shl,q)	TW(REG(cl),REG(rax))
	ret

asm_fixnum_lsh_overflow:
	OP(xor,q)	TW(REG(rax),REG(rax))
	ret

asm_fixnum_shift_negative:
	OP(neg,q)	REG(rcx)

asm_fixnum_rsh:
	OP(cmp,q)	TW(IMM(DATUM_LENGTH),REG(rcx))
	jge	asm_fixnum_rsh_overflow
	OP(sar,q)	TW(REG(cl),REG(rax))

	# Turn rax back into a detagged fixnum by masking off the low
	# six bits.  -1 has all bits set, but its detagged format has
	# the low six bits clear.  Use rcx as a temporary register
	# because AND can't take a 64-bit immediate operand; only MOV
	# can.
	OP(mov,q)	TW(IMM_DETAGGED_FIXNUM_MINUS_ONE,REG(rcx))
	OP(and,q)	TW(REG(rcx),REG(rax))
	ret

asm_fixnum_rsh_overflow:
	OP(cmp,q)	TW(IMM(0),REG(rax))
	js	asm_fixnum_rsh_overflow_negative

asm_fixnum_rsh_overflow_nonnegative:
	OP(xor,q)	TW(REG(rax),REG(rax))
	ret

asm_fixnum_rsh_overflow_negative:
	OP(mov,q)	TW(IMM_DETAGGED_FIXNUM_MINUS_ONE,REG(rax))
	ret

define_c_label(sse_read_mxcsr)
	enter		IMM(8),IMM(0)
	stmxcsr		IND(REG(rsp))
	OP(mov,l)	TW(IND(REG(rsp)),REG(eax))
	leave
	ret

define_c_label(sse_write_mxcsr)
	enter		IMM(8),IMM(0)
	OP(mov,l)	TW(REG(edi),IND(REG(rsp)))
	ldmxcsr		IND(REG(rsp))
	leave
	ret

define_c_label(x87_clear_exceptions)
	fnclex
	ret

define_c_label(x87_trap_exceptions)
	fwait
	ret

define_c_label(x87_read_control_word)
	enter		IMM(4),IMM(0)
	fnstcw		IND(REG(esp))
	OP(mov,w)	TW(IND(REG(esp)),REG(ax))
	leave
	ret

define_c_label(x87_write_control_word)
	enter		IMM(4),IMM(0)
	OP(mov,w)	TW(REG(di),IND(REG(rsp)))
	fldcw		IND(REG(esp))
	leave
	ret

define_c_label(x87_read_status_word)
	enter		IMM(4),IMM(0)
	fnstsw		IND(REG(esp))
	OP(mov,w)	TW(IND(REG(esp)),REG(ax))
	leave
	ret

define_c_label(x87_read_environment)
	fnstenv		IND(REG(rdi))
	# fnstenv masks all exceptions (go figure), so we must load
	# the control word back in order to undo that.
	fldcw		IND(REG(eax))
	ret

define_c_label(x87_write_environment)
	fldenv		IND(REG(rdi))
	ret

IFDASM(`end')

# Mark the C stack nonexecutable.

ifdef(`__linux__', `ifdef(`__ELF__', `.section .note.GNU-stack,"",%progbits')')

### Edwin Variables:
### comment-column: 56
### comment-start: "#"
### End:
