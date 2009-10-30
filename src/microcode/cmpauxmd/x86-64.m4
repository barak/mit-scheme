### -*-Midas-*-
###
### Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993,
###     1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
###     2004, 2005, 2006, 2007, 2008, 2009 Massachusetts Institute of
###     Technology
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
###	- %rsp containts the Scheme stack pointer, not the C stack
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
### DISABLE_387
###	If defined, do not generate 387 floating-point instructions.
### VALGRIND_MODE
###	If defined, modify code to make it work with valgrind.

####	Utility macros and definitions

ifdef(`WIN32',
      `define(IF_WIN32,`$1')',
      `define(IF_WIN32,`')')

ifdef(`DISABLE_387',
      `define(IF387,`')',
      `define(IF387,`$1')')

ifdef(`DISABLE_387',
      `define(IFN387,`$1')',
      `define(IFN387,`')')

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
      `define(DOF,`qword ptr $1[$2]')',
      `define(DOF,`$1($2)')')

ifdef(`DASM',
      `define(IDX,`dword ptr [$1] [$2]')',
      `define(IDX,`($1,$2)')')

ifdef(`DASM',
      `define(SDX,`dword ptr $1[$2*$3]')',
      `define(SDX,`$1(,$2,$3)')')

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
# above.

define(IMM_MANIFEST_NM_VECTOR_1, `IMM(HEX(9c00000000000001))')
define(IMM_TRUE, `IMM(HEX(2000000000000000))')
define(IMM_FALSE, `IMM(HEX(0000000000000000))')

define(REGBLOCK_VAL,16)
define(REGBLOCK_COMPILER_TEMP,32)
define(REGBLOCK_LEXPR_ACTUALS,56)
define(REGBLOCK_PRIMITIVE,64)
define(REGBLOCK_CLOSURE_FREE,72)

define(REGBLOCK_DLINK,REGBLOCK_COMPILER_TEMP)
define(REGBLOCK_UTILITY_ARG4,REGBLOCK_CLOSURE_FREE)

define(COMPILER_REGBLOCK_N_FIXED,16)
define(COMPILER_REGBLOCK_N_HOOKS,80)
define(COMPILER_REGBLOCK_N_TEMPS,256)
define(COMPILER_FIXED_SIZE,1)
define(COMPILER_HOOK_SIZE,1)
define(COMPILER_TEMP_SIZE,2)
define(REGBLOCK_SIZE_IN_OBJECTS,
       eval((COMPILER_REGBLOCK_N_FIXED*COMPILER_FIXED_SIZE)
	    +(COMPILER_REGBLOCK_N_HOOKS*COMPILER_HOOK_SIZE)
	    +(COMPILER_REGBLOCK_N_TEMPS*COMPILER_TEMP_SIZE)))

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
define_data(Regstart)
allocate_space(Regstart,256)

define_data(Registers)
allocate_space(Registers,eval(REGBLOCK_SIZE_IN_OBJECTS*8))
')

define_data(i387_presence)
allocate_quadword(i387_presence)

define_data(C_Stack_Pointer)
allocate_quadword(C_Stack_Pointer)

define_data(C_Frame_Pointer)
allocate_quadword(C_Frame_Pointer)

# [TRC 20091025: CPUID is always supported.]
# define_data(x86_64_cpuid_supported)
# allocate_quadword(x86_64_cpuid_supported)

# [TRC 20091025: The cache synchronization bug does not occur in any
# x86-64 machines of which I am aware.]
# define_data(x86_64_cpuid_needed)
# allocate_quadword(x86_64_cpuid_needed)

DECLARE_CODE_SEGMENT()
declare_alignment(2)

# [TRC 20091025: We need to check for MMX/SSEn instructions too.]

define_c_label(x86_64_interface_initialize)
	OP(push,q)	REG(rbp)
	OP(mov,q)	TW(REG(rsp),REG(rbp))
	OP(xor,q)	TW(REG(rax),REG(rax))		# No 387 available

# [TRC 20091025: The AMD64 reference manual suggests using the CPUID
# instruction to detect instruction subsets instead.]

# Unfortunately, the `movl cr0,ecx' instruction is privileged.
# Use the deprecated `smsw cx' instruction instead.

IF387(`
#	OP(mov,q)	TW(REG(cr0),REG(rcx))		# Test for 387 presence
ifdef(`VALGRIND_MODE',`',`
	smsw		REG(cx)
	OP(mov,q)	TW(IMM(HEX(12)),REG(rdx))
	OP(and,q)	TW(REG(rdx),REG(rcx))
	OP(cmp,q)	TW(REG(rdx),REG(rcx))
	jne	x86_64_initialize_no_fp
')
	OP(inc,q)	REG(rax)			# 387 available
	OP(sub,q)	TW(IMM(8),REG(rsp))
	fclex
	fnstcw		WOF(-2,REG(rbp))
	OP(and,w)	TW(IMM(HEX(f0e0)),WOF(-2,REG(rbp)))
	OP(or,w)	TW(IMM(FP_CONTROL_WORD),WOF(-2,REG(rbp)))
	fldcw		WOF(-2,REG(rbp))
x86_64_initialize_no_fp:
')
	OP(mov,q)	TW(REG(rax),ABS(EVR(i387_presence)))

# [TRC 20091025: CPUID is always supported.]

# Do a bunch of hair to determine if we need to do cache synchronization.
# See if the CPUID instruction is supported.

#	OP(xor,q)	TW(REG(rax),REG(rax))
#	OP(mov,q)	TW(REG(rax),ABS(EVR(x86_64_cpuid_supported)))
#	OP(mov,q)	TW(REG(rax),ABS(EVR(x86_64_cpuid_needed)))

# First test: can we toggle the AC bit?

#	pushfd
#	OP(pop,l)	REG(eax)
#	OP(mov,l)	TW(REG(eax),REG(ecx))
#	OP(xor,l)	TW(IMM(HEX(00040000)),REG(eax))
#	OP(push,l)	REG(eax)
#	popfd
#	pushfd
#	OP(pop,l)	REG(eax)

# if AC bit can't be toggled, this is a 386 (and doesn't support CPUID).

#	OP(xor,l)	TW(REG(ecx),REG(eax))
#	jz		no_cpuid_instr
#	OP(push,l)	REG(ecx)			# restore EFLAGS
#	popfd

# Now test to see if the ID bit can be toggled.

#	OP(mov,l)	TW(REG(ecx),REG(eax))
#	OP(xor,l)	TW(IMM(HEX(00200000)),REG(eax))
#	OP(push,l)	REG(eax)
#	popfd
#	pushfd
#	OP(pop,l)	REG(eax)

# if ID bit can't be toggled, this is a 486 that doesn't support CPUID.

#	OP(xor,l)	TW(REG(ecx),REG(eax))
#	jz		no_cpuid_instr
#	OP(push,l)	REG(ecx)			# restore EFLAGS
#	popfd

# Now we know that cpuid is supported.

#	OP(mov,q)	TW(IMM(HEX(00000001)),ABS(EVR(x86_64_cpuid_supported)))

# Next, use the CPUID instruction to determine the processor type.

#	OP(push,l)	REG(ebx)
#	OP(xor,l)	TW(REG(eax),REG(eax))
#	cpuid

# Check that CPUID accepts argument 1.

#	OP(cmp,l)	TW(IMM(HEX(00000001)),REG(eax))
#	jl		done_setting_up_cpuid

# Detect "GenuineIntel".

#	OP(cmp,l)	TW(IMM(HEX(756e6547)),REG(ebx))
#	jne		not_intel_cpu
#	OP(cmp,l)	TW(IMM(HEX(49656e69)),REG(edx))
#	jne		not_intel_cpu
#	OP(cmp,l)	TW(IMM(HEX(6c65746e)),REG(ecx))
#	jne		not_intel_cpu

# For CPU families 4 (486), 5 (Pentium), or 6 (Pentium Pro, Pentium
# II, Pentium III), don't use CPUID synchronization.

#	OP(mov,l)	TW(IMM(HEX(01)),REG(eax))
#	cpuid
#	OP(shr,l)	TW(IMM(HEX(08)),REG(eax))
#	OP(and,l)	TW(IMM(HEX(0000000F)),REG(eax))
#	OP(cmp,l)	TW(IMM(HEX(4)),REG(eax))
#	jl		done_setting_up_cpuid
#	OP(cmp,l)	TW(IMM(HEX(6)),REG(eax))
#	jg		done_setting_up_cpuid
#
#	jmp		cpuid_not_needed
#
#not_intel_cpu:

# Detect "AuthenticAMD".

#	OP(cmp,l)	TW(IMM(HEX(68747541)),REG(ebx))
#	jne		not_amd_cpu
#	OP(cmp,l)	TW(IMM(HEX(69746e65)),REG(edx))
#	jne		not_amd_cpu
#	OP(cmp,l)	TW(IMM(HEX(444d4163)),REG(ecx))
#	jne		not_amd_cpu

# Problem appears to exist only on Athlon models 1, 3, and 4.

#	OP(mov,l)	TW(IMM(HEX(01)),REG(eax))
#	cpuid

#	OP(mov,l)	TW(REG(eax),REG(ecx))
#	OP(shr,l)	TW(IMM(HEX(08)),REG(eax))
#	OP(and,l)	TW(IMM(HEX(0000000F)),REG(eax))
#	OP(cmp,l)	TW(IMM(HEX(6)),REG(eax))	# family 6 = Athlon
#	jne		done_setting_up_cpuid

#	OP(mov,l)	TW(REG(ecx),REG(eax))
#	OP(shr,l)	TW(IMM(HEX(04)),REG(eax))
#	OP(and,l)	TW(IMM(HEX(0000000F)),REG(eax))
#	OP(cmp,l)	TW(IMM(HEX(6)),REG(eax))	# model 6 and up OK
#	jge		done_setting_up_cpuid
#	OP(cmp,l)	TW(IMM(HEX(2)),REG(eax))	# model 2 OK
#	je		done_setting_up_cpuid

#	OP(mov,l)	TW(IMM(HEX(00000001)),ABS(EVR(x86_64_cpuid_needed)))

#not_amd_cpu:
#done_setting_up_cpuid:
#	OP(pop,l)	REG(ebx)
#no_cpuid_instr:
	leave
	ret

define_c_label(C_to_interface)
	OP(push,q)	REG(rbp)			# Link according
	OP(mov,q)	TW(REG(rsp),REG(rbp))		#  to C's conventions
	OP(push,q)	REG(rbx)			# Save callee-saves
	OP(push,q)	REG(r12)			#  registers
	OP(push,q)	REG(r13)
	OP(push,q)	REG(r14)
	OP(push,q)	REG(r15)
	OP(mov,q)	TW(REG(rdi),REG(rdx))		# Entry point
							# Preserve frame ptr
	OP(mov,q)	TW(REG(rbp),ABS(EVR(C_Frame_Pointer)))
							# Preserve stack ptr
	OP(mov,q)	TW(REG(rsp),ABS(EVR(C_Stack_Pointer)))
	jmp	EPFR(interface_to_scheme)

define_hook_label(trampoline_to_interface)
define_debugging_label(trampoline_to_interface)
	OP(pop,q)	REG(rcx)			# trampoline storage
	jmp	scheme_to_interface

define_hook_label(scheme_to_interface_call)
define_debugging_label(scheme_to_interface_call)
	OP(pop,q)	REG(rcx)			# arg1 = ret. add
	OP(add,q)	TW(IMM(4),REG(rcx))		# Skip format info
#	jmp	scheme_to_interface

define_hook_label(scheme_to_interface)
define_debugging_label(scheme_to_interface)

# These two moves must happen _before_ the ffree instructions below.
# Otherwise recovery from SIGFPE there will fail.
	OP(mov,q)	TW(REG(rsp),ABS(EVR(stack_pointer)))
	OP(mov,q)	TW(rfree,ABS(EVR(Free)))

# [TRC 20091025: I think this should be excised.]

IF387(`
	OP(cmp,q)	TW(IMM(0),ABS(EVR(i387_presence)))
	je	scheme_to_interface_proceed
	ffree	ST(0)					# Free floating "regs"
	ffree	ST(1)
	ffree	ST(2)
	ffree	ST(3)
	ffree	ST(4)
	ffree	ST(5)
	ffree	ST(6)
	ffree	ST(7)
scheme_to_interface_proceed:
')

	OP(mov,q)	TW(ABS(EVR(C_Stack_Pointer)),REG(rsp))
	OP(mov,q)	TW(ABS(EVR(C_Frame_Pointer)),REG(rbp))

	OP(sub,q)	TW(IMM(16),REG(rsp))	# alloc struct return

	# Shuffle Scheme -> AMD64 calling conventions:
	#   struct pointer -> rdi
	#   rcx -> rsi
	#   rdx -> rdx
	#   rbx -> rcx
	#   arg4 -> r8
	# Parallel assignment problems:
	#   arg4 depends on rsi: do arg4->r8 first
	#   target depends on rcx (why?): use r11 as a temporary
	# [TRC 20091025: Perhaps we can rearrange LIAR to generate
	# arguments in the registers we want, to avoid this
	# shuffling.]

	OP(mov,q)	TW(REG(rcx),REG(r11))

	OP(xor,q)	TW(REG(rcx),REG(rcx))
	OP(mov,b)	TW(REG(al),REG(cl))
	OP(mov,q)	TW(SDX(EVR(utility_table),REG(rcx),8),REG(rax))

	OP(mov,q)	TW(REG(rsp),REG(rdi))
	OP(mov,q)	TW(DOF(REGBLOCK_UTILITY_ARG4(),regs),REG(r8))
	OP(mov,q)	TW(REG(r11),REG(rsi))
	OP(mov,q)	TW(REG(rbx),REG(rcx))

	call		IJMP(REG(rax))

define_debugging_label(scheme_to_interface_return)
	OP(pop,q)	REG(rax)		# pop struct return
	OP(pop,q)	REG(rdx)
	jmp		IJMP(REG(rax))		# Invoke handler

define_c_label(interface_to_scheme)
IF387(`
	OP(cmp,q)	TW(IMM(0),ABS(EVR(i387_presence)))
	je	interface_to_scheme_proceed
	ffree	ST(0)					# Free floating "regs"
	ffree	ST(1)
	ffree	ST(2)
	ffree	ST(3)
	ffree	ST(4)
	ffree	ST(5)
	ffree	ST(6)
	ffree	ST(7)
interface_to_scheme_proceed:
')
							# Register block = %rsi
							# Scheme offset in NT
ifdef(`WIN32',
`	OP(mov,q)	TW(ABS(EVR(RegistersPtr)),regs)',
`	OP(lea,q)	TW(ABS(EVR(Registers)),regs)')

	OP(mov,q)	TW(ABS(EVR(Free)),rfree)	# Free pointer = %rdi
	OP(mov,q)	TW(DOF(REGBLOCK_VAL(),regs),REG(rax)) # Value/dynamic link
	OP(mov,q)	TW(IMM(ADDRESS_MASK),rmask)	# = %rbp

	OP(mov,q)	TW(ABS(EVR(stack_pointer)),REG(rsp))
	OP(mov,q)	TW(REG(rax),REG(rcx))		# Preserve if used
	OP(and,q)	TW(rmask,REG(rcx))		# Restore potential dynamic link
	OP(mov,q)	TW(REG(rcx),DOF(REGBLOCK_DLINK(),regs))
	jmp		IJMP(REG(rdx))

IF_WIN32(`
use_external_code(EFR(WinntExceptionTransferHook))
define_code_label(EFR(callWinntExceptionTransferHook))
	call	EFR(WinntExceptionTransferHook)
	mov	rdx,rax
')

define_c_label(interface_to_C)
IF387(`
	OP(cmp,q)	TW(IMM(0),ABS(EVR(i387_presence)))
	je	interface_to_C_proceed
	ffree	ST(0)					# Free floating "regs"
	ffree	ST(1)
	ffree	ST(2)
	ffree	ST(3)
	ffree	ST(4)
	ffree	ST(5)
	ffree	ST(6)
	ffree	ST(7)
interface_to_C_proceed:')

	OP(mov,q)	TW(REG(rdx),REG(rax))		# Set up result
	OP(pop,q)	REG(r15)			# Restore callee-saves
	OP(pop,q)	REG(r14)			#  registers
	OP(pop,q)	REG(r13)
	OP(pop,q)	REG(r12)
	OP(pop,q)	REG(rbx)
	leave
	ret

# [TRC 20091025: The cache synchronization bug does not occur in any
# x86-64 machines of which I am aware.]

#define_code_label(EFR(x86_64_cache_synchronize))
#	OP(push,q)	REG(rbp)
#	OP(mov,q)	TW(REG(rsp),REG(rbp))
#	OP(push,q)	REG(rbx)
#	OP(xor,q)	TW(REG(rax),REG(rax))
#	cpuid
#	OP(pop,q)	REG(rbx)
#	leave
#	ret

### Run the CPUID instruction for serialization.

#define_hook_label(serialize_cache)
#	pushad
#	OP(xor,q)	TW(REG(rax),REG(rax))
#	cpuid
#	popad
#	ret

### Stub to be used in place of above on machines that don't need it.

#define_hook_label(dont_serialize_cache)
#	ret

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
	OP(mov,q)	TW(DOF(REGBLOCK_DLINK(),regs),REG(rdx))
	OP(mov,b)	TW(IMM(HEX(19)),REG(al))
	jmp	scheme_to_interface_call

###
###	This saves even more instructions than primitive_apply
###	When the PC is not available.  Instead of jumping here,
###	a call instruction is used, and the longword offset to
###	the primitive object follows the call instruction.
###	This code loads the primitive object and merges with
###	apply_primitive
###
###     [TRC 20091025: But on the x86-64, we have RIP-relative
###     addressing, so we don't need this.]
###

#declare_alignment(2)
#define_hook_label(short_primitive_apply)
#	OP(pop,l)	REG(edx)			# offset pointer
#	OP(mov,l)	TW(IND(REG(edx)),REG(ecx))	# offset
#							# Primitive object
#	OP(mov,l)	TW(IDX(REG(edx),REG(ecx)),REG(ecx))
#							# Merge
#	jmp	hook_reference(primitive_apply)

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
	OP(mov,q)	TW(REG(rcx),REG(rax))		# Copy for type code
	OP(mov,q)	TW(REG(rcx),REG(rbx))		# Copy for address
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rax))	# Select type code
	OP(and,q)	TW(rmask,REG(rbx))		# Select datum
	OP(cmp,b)	TW(IMM(TC_COMPILED_ENTRY),REG(al))
	jne	asm_sc_apply_generic
	# [TRC 20091025: How big are the frame sizes?]
	OP(movs,bq,x)	TW(BOF(-4,REG(rbx)),REG(rax))	# Extract frame size
	OP(cmp,q)	TW(REG(rax),REG(rdx))		# Compare to nargs+1
	jne	asm_sc_apply_generic
	jmp	IJMP(REG(rbx))				# Invoke

define_debugging_label(asm_sc_apply_generic)
	OP(mov,q)	TW(IMM(HEX(14)),REG(rax))
	jmp	scheme_to_interface	

define(define_apply_fixed_size,
`declare_alignment(2)
define_hook_label(sc_apply_size_$1)
	OP(mov,q)	TW(REG(rcx),REG(rax))		# Copy for type code
	OP(mov,q)	TW(REG(rcx),REG(rbx))		# Copy for address
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rax))	# Select type code
	OP(and,q)	TW(rmask,REG(rbx))		# Select datum
	OP(cmp,b)	TW(IMM(TC_COMPILED_ENTRY),REG(al))
	jne	asm_sc_apply_generic_$1
	# [TRC 20091025: How big are the frame sizes?]
	OP(cmp,b)	TW(IMM($1),BOF(-4,REG(rbx)))	# Compare frame size
	jne	asm_sc_apply_generic_$1	# to nargs+1
	jmp	IJMP(REG(rbx))

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

IF387(`declare_alignment(2)
asm_generic_flonum_result:
	# The MOV instruction can take a 64-bit immediate operand only
	# if the target is a register, so we store the manifest in rax
	# before moving it to memory.
	OP(mov,q)	TW(IMM_MANIFEST_NM_VECTOR_1,REG(rax))
	OP(mov,q)	TW(REG(rax), IND(rfree))
	# The OR instruction cannot take a 64-bit immediate either, so
	# we need to store the tag in rax first, shift it up, and then
	# OR the datum into it.
	OP(mov,q)	TW(IMM(TC_FLONUM),REG(rax))
	OP(shl,q)	TW(IMM(DATUM_LENGTH),REG(rax))
	OP(or,q)	TW(rfree,REG(rax))
	OP(fstp,l)	DOF(8,rfree)			# fstpd
	OP(and,q)	TW(rmask,IND(REG(rsp)))
	OP(add,q)	TW(IMM(16),rfree)
	OP(mov,q)	TW(REG(rax),DOF(REGBLOCK_VAL(),regs))
	ret

declare_alignment(2)
asm_generic_fixnum_result:
	OP(and,q)	TW(rmask,IND(REG(rsp)))
	OP(or,b)	TW(IMM(TC_FIXNUM),REG(al))
	OP(ror,q)	TW(IMM(TC_LENGTH),REG(rax))
	OP(mov,q)	TW(REG(rax),LOF(REGBLOCK_VAL(),regs))
	ret

declare_alignment(2)
asm_generic_return_sharp_t:
	OP(and,q)	TW(rmask,IND(REG(rsp)))
	OP(mov,q)	TW(IMM_TRUE,REG(rax))
	OP(mov,q)	TW(REG(rax),LOF(REGBLOCK_VAL(),regs))
	ret

declare_alignment(2)
asm_generic_return_sharp_f:
	OP(and,q)	TW(rmask,IND(REG(rsp)))
	OP(mov,q)	TW(IMM_FALSE,REG(rax))
	OP(mov,q)	TW(REG(rax),LOF(REGBLOCK_VAL(),regs))
	ret')

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
	OP(and,q)	TW(rmask,REG(rdx))
	fld1
	OP($4,l)	DOF(8,REG(rdx))
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
	OP(and,q)	TW(rmask,REG(rdx))
	OP(fld,l)	DOF(8,REG(rdx))
	ftst
	fstsw	REG(ax)
	fstp	ST(0)
	sahf
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
	je	asm_generic_$1_flo

asm_generic_$1_fail:
	OP(push,q)	REG(rbx)
	OP(push,q)	REG(rdx)
	OP(mov,b)	TW(IMM(HEX($2)),REG(al))
	jmp	scheme_to_interface

asm_generic_$1_fix:
	OP(mov,q)	TW(REG(rdx),REG(rax))
	OP(mov,q)	TW(REG(rbx),REG(rcx))
	OP(shl,q)	TW(IMM(TC_LENGTH),REG(rax))
	OP(shl,q)	TW(IMM(TC_LENGTH),REG(rcx))
	$5
	OP($3,q)	TW(REG(rcx),REG(rax))		# subq
	jo	asm_generic_$1_fail
	jmp	asm_generic_fixnum_result

asm_generic_$1_flo:
	OP(and,q)	TW(rmask,REG(rdx))
	OP(and,q)	TW(rmask,REG(rbx))
	OP(fld,l)	DOF(8,REG(rdx))			# fldd
	OP($4,l)	DOF(8,REG(rbx))			# fsubl
	jmp	asm_generic_flonum_result')

IF387(`declare_alignment(2)
define_hook_label(generic_divide)
	OP(pop,q)	REG(rdx)
	OP(pop,q)	REG(rbx)
	OP(mov,q)	TW(REG(rdx),REG(rax))
	OP(mov,q)	TW(REG(rbx),REG(rcx))
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rax))
	OP(shr,q)	TW(IMM(DATUM_LENGTH),REG(rcx))
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(al))
	je	asm_generic_divide_fix
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(al))
	jne	asm_generic_divide_fail
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(cl))
	je	asm_generic_divide_flo_flo
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(cl))
	jne	asm_generic_divide_fail
	OP(mov,q)	TW(REG(rbx),REG(rcx))
	OP(shl,q)	TW(IMM(TC_LENGTH),REG(rcx))
	je	asm_generic_divide_fail
	OP(and,q)	TW(rmask,REG(rdx))
	OP(sar,q)	TW(IMM(TC_LENGTH),REG(rcx))
	OP(fld,l)	DOF(8,REG(rdx))			# fldd
	OP(mov,q)	TW(REG(rcx),IND(rfree))
	OP(fidiv,l)	IND(rfree)
	jmp	asm_generic_flonum_result

asm_generic_divide_fix:
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(cl))
	jne	asm_generic_divide_fail
	OP(mov,q)	TW(REG(rdx),REG(rcx))
	OP(shl,q)	TW(IMM(TC_LENGTH),REG(rcx))
	je	asm_generic_divide_fail
	OP(and,q)	TW(rmask,REG(rbx))
	OP(sar,q)	TW(IMM(TC_LENGTH),REG(rcx))
	OP(fld,l)	DOF(8,REG(rbx))			# fldd
	OP(mov,q)	TW(REG(rcx),IND(rfree))
	OP(fidivr,l)	IND(rfree)
	jmp	asm_generic_flonum_result

asm_generic_divide_flo_flo:
	OP(mov,q)	TW(REG(rbx),REG(rcx))
	OP(and,q)	TW(rmask,REG(rcx))
	OP(fld,l)	DOF(8,REG(rcx))			# fldd
	ftst
	fstsw	REG(ax)
	sahf
	je	asm_generic_divide_by_zero
	OP(and,q)	TW(rmask,REG(rdx))
	OP(fdivr,l)	DOF(8,REG(rdx))
	jmp	asm_generic_flonum_result	

asm_generic_divide_by_zero:
	fstp	ST(0)					# Pop second arg

asm_generic_divide_fail:
	OP(push,q)	REG(rbx)
	OP(push,q)	REG(rdx)
	OP(mov,b)	TW(IMM(HEX(23)),REG(al))
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
	jne	asm_generic_$1_fail

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

IF387(`define_unary_operation(decrement,22,sub,fsubr)
define_unary_operation(increment,26,add,fadd)

define_unary_predicate(negative,2a,jl,jb)
define_unary_predicate(positive,2c,jg,ja)
define_unary_predicate(zero,2d,je,je)

# define_binary_operation(name,index,fix*fix,flo*flo, fixup)
# define_binary_operation(  $1,   $2,     $3,     $4, $5)
# The fixup is optional; only multiplication needs it to shift the
# result back down by six bits.
define_binary_operation(add,2b,add,fadd)
define_binary_operation(subtract,28,sub,fsub)
define_binary_operation(multiply,29,imul,fmul,
	`OP(shr,q)	TW(IMM(6),REG(rax))')
# Divide needs to check for 0, so we cant really use the following
# define_binary_operation(divide,23,NONE,fdiv)

# define_binary_predicate(name,index,fix*fix,flo*flo)
define_binary_predicate(equal,24,je,je)
define_binary_predicate(greater,25,jg,ja)
define_binary_predicate(less,27,jl,jb)')

IFN387(`define_jump_indirection(generic_decrement,22)
define_jump_indirection(generic_divide,23)
define_jump_indirection(generic_equal,24)
define_jump_indirection(generic_greater,25)
define_jump_indirection(generic_increment,26)
define_jump_indirection(generic_less,27)
define_jump_indirection(generic_subtract,28)
define_jump_indirection(generic_multiply,29)
define_jump_indirection(generic_negative,2a)
define_jump_indirection(generic_add,2b)
define_jump_indirection(generic_positive,2c)
define_jump_indirection(generic_zero,2d)')

# These don't currently differ according to whether there
# is a 387 or not.

define_jump_indirection(generic_quotient,37)
define_jump_indirection(generic_remainder,38)
define_jump_indirection(generic_modulo,39)

define_jump_indirection(nofp_decrement,22)
define_jump_indirection(nofp_divide,23)
define_jump_indirection(nofp_equal,24)
define_jump_indirection(nofp_greater,25)
define_jump_indirection(nofp_increment,26)
define_jump_indirection(nofp_less,27)
define_jump_indirection(nofp_subtract,28)
define_jump_indirection(nofp_multiply,29)
define_jump_indirection(nofp_negative,2a)
define_jump_indirection(nofp_add,2b)
define_jump_indirection(nofp_positive,2c)
define_jump_indirection(nofp_zero,2d)
define_jump_indirection(nofp_quotient,37)
define_jump_indirection(nofp_remainder,38)
define_jump_indirection(nofp_modulo,39)

IFDASM(`end')

### Edwin Variables:
### comment-column: 56
### comment-start: "#"
### End:
