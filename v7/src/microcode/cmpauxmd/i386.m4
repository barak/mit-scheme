### -*-Midas-*-
###
###	$Id: i386.m4,v 1.41 1995/10/24 06:28:41 cph Exp $
###
###	Copyright (c) 1992-95 Massachusetts Institute of Technology
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

### Intel i386 assembly language part of the compiled code interface.
### See cmpint.txt, cmpint.c, cmpint-mc68k.h, and cmpgc.h for more
### documentation.
###
### This m4 source expands into either Unix (gas) source or DOS
### (masm) source.
###
### NOTE:
###	Assumptions:
###
###	0) Segment registers and paging are set up for 32-bit "flat"
###	operation.
###
###	1) All registers (except double floating point registers) and
###	stack locations hold a C long object.
###
###	2) The C compiler divides registers into three groups:
###	- Linkage registers, used for procedure calls and global
###	references.  On i386 (gcc and Zortech C): %ebp, %esp.
###	- super temporaries, not preserved accross procedure calls and
###	always usable. On i386 (gcc and Zortech C): %eax, %edx, %ecx.
###	- preserved registers saved by the callee if they are written.
###	On i386 (gcc and Zortech C): all others (%ebx, %esi, %edi).
###
###	3) Arguments, if passed on a stack, are popped by the caller
###	or by the procedure return instruction (as on the VAX).  Thus
###	most "leaf" procedures need not worry about them.  On i386,
###	arguments are passed on the stack.
###
###	4) There is a hardware or software maintained stack for
###	control.  The procedure calling sequence may leave return
###	addresses in registers, but they must be saved somewhere for
###	nested calls and recursive procedures.  On i386: saved on
###	the stack by the CALL instruction.
###
###	5) C procedures return long values in a super temporary
###	register.  Two word structures are returned differently,
###	depending on the C compiler used.  When using GCC, two-word
###	structures are returned in {%eax, %edx}.  When using a
###	compiler compatible with MicroSoft's C compiler (e.g. Zortech
###	C), two word structures are returned by returning in %eax the
###	address of a structure allocated statically.  If the Scheme
###	system ever becomes reentrant, this will have to change.
###	Note the assumption below is that all DOS compilers are
###	compatible with MicroSoft C.
###
###	6) Floating point registers are not preserved by this
###	interface.  The interface is only called from the Scheme
###	interpreter, which does not use floating point data.  Thus
###	although the calling convention would require us to preserve
###	them, they contain garbage.
###
### Compiled Scheme code uses the following register convention:
###	- %esp containts the Scheme stack pointer, not the C stack
###	pointer.
###	- %esi contains a pointer to the Scheme interpreter's "register"
###	block.  This block contains the compiler's copy of MemTop,
###	the interpreter's registers (val, env, exp, etc.),
###	temporary locations for compiled code, and the addresses
###	of various hooks defined in this file.
###	- %edi contains the Scheme free pointer.
###	- %ebp contains the Scheme datum mask.
###	The dynamic link (when needed) is in Registers[REGBLOCK_COMPILER_TEMP]
###	Values are returned in Registers[REGBLOCK_VAL]
###
###	All other registers (%eax, %edx, %ecx, %ebx) are available to
###	the compiler.  A caller-saves convention is used, so the
###	registers need not be preserved by subprocedures.

### The following m4 macros can be defined to change how this file is
### expanded.
###
### DASM
###	If defined, expand to Intel assembly-language syntax, used by
###	Microsoft assembler (MASM) and Watcom assembler (WASM).
###	Otherwise, expand to AT&T syntax, used by GAS.
###
### DOS
###	If defined, expand to run under DOS; implies DASM.
### WIN32
###	If defined, expand to run under Win32; implies DASM.
###	Previously, this was defined in conjunction with DOS, but now
###	DOS must not be defined!
### OS2
###	If defined, expand to run under OS/2.  This macro does nothing
###	more than define SUPPRESS_LEADING_UNDERSCORE and
###	CALLER_ALLOCS_STRUCT_RETURN, which are the conventions used to
###	call OS/2 API procedures; note that EMX/GCC doesn't define
###	these symbols because it thinks it's running under unix.
###
### If none of { DOS, WIN32, OS2 } is defined, expansion is for unix.
###
### SUPPRESS_LEADING_UNDERSCORE
###	If defined, external symbol names are generated as written;
###	otherwise, they have an underscore prepended to them.
### CALLER_ALLOCS_STRUCT_RETURN
###	Controls the conventions used to return 8-byte structs from C
###	procedures.  If defined, the caller allocates space on the
###	stack and passes a pointer to that space on the top of the
###	stack.  Otherwise, the callee returns the struct in EAX/EDX.
### WCC386R
###	Should be defined when using Watcom assembler and generating
###	code to use the Watcom register-based argument conventions.
### HACK_SEGMENT_REGS
###	If defined, two code/data segments are maintained, one for C
###	code and one for Scheme code, and the segment registers are
###	switched between the two automatically.  Currently works only
###	for Win32s.
### LINUX_ELF
###	If defined, expand to run under Linux ELF.  This is the
###	remains of a failed experiment; do NOT define this symbol.
### TYPE_CODE_LENGTH
###	Normally defined to be 6.  Don't change this unless you know
###	what you're doing.
### DISABLE_387
###	If defined, do not generate 387 floating-point instructions.

####	Utility macros and definitions

ifdef(`DOS',
      `define(IFDOS,`$1')',
      `define(IFDOS,`')')

ifdef(`DOS',
      `define(IFNDOS,`')',
      `define(IFNDOS,`$1')')

ifdef(`WIN32',
      `define(IF_WIN32,`$1')',
      `define(IF_WIN32,`')')

ifdef(`OS2',
      `define(IFOS2,`$1')',
      `define(IFOS2,`')')

ifdef(`LINUX_ELF',
      `define(IF_LINUX_ELF,`$1')',
      `define(IF_LINUX_ELF,`')')

ifdef(`DISABLE_387',
      `define(IF387,`')',
      `define(IF387,`$1')')

ifdef(`DISABLE_387',
      `define(IFN387,`$1')',
      `define(IFN387,`')')

IFDOS(`define(DASM,1)')
IF_WIN32(`define(DASM,1)')

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

IFNDASM(`	.file	"cmpaux-i386.s"')

IFOS2(`define(`SUPPRESS_LEADING_UNDERSCORE',1)')
IF_LINUX_ELF(`define(`SUPPRESS_LEADING_UNDERSCORE',1)')

ifdef(`WCC386R',
      `define(EVR,`_$1')',
      `ifdef(`SUPPRESS_LEADING_UNDERSCORE',
	     `define(EVR,`$1')',
	     `define(EVR,`_$1')')')

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

ifdef(`DOS',
      `define(declare_alignment,`')',
`ifdef(`DASM',
      `define(declare_alignment,`	align $1')',
      `define(declare_alignment,`	.align $1')')')

ifdef(`DASM',
      `define(allocate_word,`EVR($1) dw 0')',
      `define(allocate_word,`	.comm EVR($1),2')')

ifdef(`DASM',
      `define(allocate_longword,`EVR($1) dd 0')',
      `define(allocate_longword,`	.comm EVR($1),4')')

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
      `define(ABS, `$1')')

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

# Define the floating-point processor control word.  Always set
# round-to-even and double precision.  Under DOS and Win32, mask all
# exceptions.  Under unix and OS/2, mask only the inexact result
# exception.
ifdef(`DOS',
      `define(FP_CONTROL_WORD,HEX(023f))',
      `ifdef(`WIN32',
             `define(FP_CONTROL_WORD,HEX(023f))',
             `define(FP_CONTROL_WORD,HEX(0220))')')

define(regs,REG(esi))
define(rfree,REG(edi))
define(rmask,REG(ebp))

IFDOS(`.386
.model tiny')

DECLARE_DATA_SEGMENT()
declare_alignment(2)

use_external_data(EVR(Free))
use_external_data(EVR(Ext_Stack_Pointer))
use_external_data(EVR(utility_table))

ifdef(`WIN32',`
use_external_data(EVR(RegistersPtr))
',`ifdef(`DOS',`
use_external_data(EVR(Registers))
',`
define_data(Regstart)
allocate_space(Regstart,128)

define_data(Registers)
allocate_space(Registers,eval(REGBLOCK_SIZE_IN_OBJECTS*4))
')
')

IF_WIN32(`use_external_data(EVR(winnt_address_delta))')

define_data(i387_presence)
allocate_longword(i387_presence)

define_data(C_Stack_Pointer)
allocate_longword(C_Stack_Pointer)

define_data(C_Frame_Pointer)
allocate_longword(C_Frame_Pointer)

IF_WIN32(`define(HACK_SEGMENT_REGS,1)')
IF_LINUX_ELF(`define(HACK_SEGMENT_REGS,1)')

ifdef(`HACK_SEGMENT_REGS',`

define_data(Scheme_Transfer_Address)
allocate_longword(Scheme_Transfer_Address)

define_data(Scheme_Code_Segment_Selector)
allocate_word(Scheme_Code_Segment_Selector)

define_data(Scheme_Data_Segment_Selector)
allocate_word(Scheme_Data_Segment_Selector)

define_data(Scheme_Stack_Segment_Selector)
allocate_word(Scheme_Stack_Segment_Selector)

define_data(C_Code_Segment_Selector)
allocate_word(C_Code_Segment_Selector)

define_data(C_Data_Segment_Selector)
allocate_word(C_Data_Segment_Selector)

define_data(C_Extra_Segment_Selector)
allocate_word(C_Extra_Segment_Selector)

define_data(C_Stack_Segment_Selector)
allocate_word(C_Stack_Segment_Selector)

IF_WIN32(`define(LRET,`db	0cbh')')
IF_LINUX_ELF(`define(LRET,`lret')')

IF_WIN32(`define(SEGMENT_DELTA,`EVR(winnt_address_delta)')')
IF_LINUX_ELF(`define(SEGMENT_DELTA,`IMM(0x08000000)')')

',`IFDOS(`

define_data(C_Stack_Segment_Selector)
allocate_word(C_Stack_Segment_Selector)

define_data(Scheme_Stack_Segment_Selector)
allocate_word(Scheme_Stack_Segment_Selector)

')')

IFOS2(`define(CALLER_ALLOCS_STRUCT_RETURN,1)')
IF_LINUX_ELF(`define(CALLER_ALLOCS_STRUCT_RETURN,1)')

DECLARE_CODE_SEGMENT()
declare_alignment(2)

define_c_label(i386_interface_initialize)
	OP(push,l)	REG(ebp)
	OP(mov,l)	TW(REG(esp),REG(ebp))

							# Initialize selectors
ifdef(`HACK_SEGMENT_REGS',`
	OP(lea,l)	TW(ABS(cross_segment_transfer_point),REG(eax))
	OP(mov,l)	TW(REG(eax),EVR(Scheme_Transfer_Address))
	OP(mov,w)	TW(REG(es),EVR(C_Extra_Segment_Selector)) # This assumes it is constant

	OP(mov,w)	TW(REG(cs),EVR(C_Code_Segment_Selector))
	OP(mov,w)	TW(EVR(Scheme_Code_Segment_Selector),REG(ax))
	OP(cmp,w)	TW(IMM(0),REG(ax))
	jne		skip_code_assignment
	OP(mov,w)	TW(REG(cs),EVR(Scheme_Code_Segment_Selector))
skip_code_assignment:

	OP(mov,w)	TW(REG(ds),EVR(C_Data_Segment_Selector))
	OP(mov,w)	TW(EVR(Scheme_Data_Segment_Selector),REG(ax))
	OP(cmp,w)	TW(IMM(0),REG(ax))
	jne		skip_data_assignment
	OP(mov,w)	TW(REG(ds),EVR(Scheme_Data_Segment_Selector))
skip_data_assignment:

	OP(mov,w)	TW(REG(ss),EVR(C_Stack_Segment_Selector))
	OP(mov,w)	TW(EVR(Scheme_Stack_Segment_Selector),REG(ax))
	OP(cmp,w)	TW(IMM(0),REG(ax))
	jne		skip_stack_assignment
	OP(mov,w)	TW(REG(ds),EVR(Scheme_Stack_Segment_Selector))
skip_stack_assignment:
',`IFDOS(`
	OP(mov,w)	TW(REG(ss),EVR(C_Stack_Segment_Selector))
	OP(mov,w)	TW(EVR(Scheme_Stack_Segment_Selector),REG(ax))
	OP(cmp,w)	TW(IMM(0),REG(ax))
	jne		skip_stack_assignment
	OP(mov,w)	TW(REG(ds),EVR(Scheme_Stack_Segment_Selector))
skip_stack_assignment:
')
')
	OP(xor,l)	TW(REG(eax),REG(eax))		# No 387 available

# Unfortunately, the `movl cr0,ecx' instruction is privileged.
# Use the deprecated `smsw cx' instruction instead.

IF387(`
#	OP(mov,l)	TW(REG(cr0),REG(ecx))		# Test for 387 presence
	smsw		REG(cx)
	OP(mov,l)	TW(IMM(HEX(12)),REG(edx))
	OP(and,l)	TW(REG(edx),REG(ecx))
	OP(cmp,l)	TW(REG(edx),REG(ecx))
	jne	i386_initialize_no_fp
	OP(inc,l)	REG(eax)			# 387 available
	OP(sub,l)	TW(IMM(4),REG(esp))
	fclex
	fnstcw		WOF(-2,REG(ebp))
	OP(and,w)	TW(IMM(HEX(f0e0)),WOF(-2,REG(ebp)))
	OP(or,w)	TW(IMM(FP_CONTROL_WORD),WOF(-2,REG(ebp)))
	fldcw		WOF(-2,REG(ebp))
i386_initialize_no_fp:
')
	OP(mov,l)	TW(REG(eax),ABS(EVR(i387_presence)))
	leave
	ret

define_c_label(C_to_interface)
	OP(push,l)	REG(ebp)			# Link according
	OP(mov,l)	TW(REG(esp),REG(ebp))		#  to C's conventions
	OP(push,l)	REG(edi)			# Save callee-saves
	OP(push,l)	REG(esi)			#  registers
	OP(push,l)	REG(ebx)
	OP(mov,l)	TW(LOF(8,REG(ebp)),REG(edx))	# Entry point
							# Preserve frame ptr
	OP(mov,l)	TW(REG(ebp),EVR(C_Frame_Pointer))
							# Preserve stack ptr
	OP(mov,l)	TW(REG(esp),EVR(C_Stack_Pointer))
							# Register block = %esi
							# Scheme offset in NT

ifdef(`WIN32',
`	OP(mov,l)	TW(ABS(EVR(RegistersPtr)),regs)',
`	OP(lea,l)	TW(ABS(EVR(Registers)),regs)')
ifdef(`HACK_SEGMENT_REGS',
`	OP(sub,l)	TW(SEGMENT_DELTA,regs)')
	jmp	EPFR(interface_to_scheme)

define_hook_label(trampoline_to_interface)
define_debugging_label(trampoline_to_interface)
	OP(pop,l)	REG(ecx)			# trampoline storage
	jmp	scheme_to_interface

define_hook_label(scheme_to_interface_call)
define_debugging_label(scheme_to_interface_call)
	OP(pop,l)	REG(ecx)			# arg1 = ret. add
	OP(add,l)	TW(IMM(4),REG(ecx))		# Skip format info
#	jmp	scheme_to_interface

define_hook_label(scheme_to_interface)
define_debugging_label(scheme_to_interface)
ifdef(`HACK_SEGMENT_REGS',
`	OP(push,l)	LOF(36,regs)			# 4th utility arg
	OP(push,l)	REG(eax)			# Save utility index

	OP(mov,w)	TW(REG(es),REG(ax))		# C ds
	OP(mov,w)	TW(REG(ax),REG(ds))

	OP(mov,w)	TW(EVR(C_Extra_Segment_Selector),REG(ax)) # C es
	OP(mov,w)	TW(REG(ax),REG(es))

# Map Free to C data space
	OP(add,l)	TW(SEGMENT_DELTA,rfree)
	OP(mov,l)	TW(rfree,EVR(Free))

# Map SP to C data space
	OP(mov,l)	TW(REG(esp),REG(eax))
	OP(add,l)	TW(SEGMENT_DELTA,REG(eax))
	OP(mov,l)	TW(REG(eax),EVR(Ext_Stack_Pointer))

# Switch stack segment
	OP(mov,w)	TW(EVR(C_Stack_Segment_Selector),REG(ss))
	OP(mov,l)	TW(EVR(C_Stack_Pointer),REG(esp))
	OP(mov,l)	TW(EVR(C_Frame_Pointer),REG(ebp))

	OP(xor,l)	TW(REG(eax),REG(eax))
	OP(mov,w)	TW(EVR(C_Code_Segment_Selector),REG(ax))
	OP(push,l)	REG(eax)
	OP(push,l)	EVR(Scheme_Transfer_Address)
	LRET

cross_segment_transfer_point:
ifdef(`CALLER_ALLOCS_STRUCT_RETURN',`
	OP(sub,l)	TW(IMM(8),REG(esp))	# alloc space for struct return
')
	OP(mov,l)	TW(EVR(Ext_Stack_Pointer),REG(eax))
	OP(push,l)	LOF(4,REG(eax))			# 4th utility arg
	OP(add,l)	TW(IMM(8),EVR(Ext_Stack_Pointer))
	OP(mov,l)	TW(IND(REG(eax)),REG(eax))	# utility index
',
`	OP(mov,l)	TW(REG(esp),EVR(Ext_Stack_Pointer))
	OP(mov,l)	TW(rfree,EVR(Free))

IFDOS(`	OP(mov,w)	TW(EVR(C_Stack_Segment_Selector),REG(ss))')	# Swap stack segments

	OP(mov,l)	TW(EVR(C_Stack_Pointer),REG(esp))
	OP(mov,l)	TW(EVR(C_Frame_Pointer),REG(ebp))

ifdef(`CALLER_ALLOCS_STRUCT_RETURN',`
	OP(sub,l)	TW(IMM(8),REG(esp))	# alloc space for struct return
')
	OP(push,l)	LOF(REGBLOCK_UTILITY_ARG4(),regs) # Utility args
')
	OP(push,l)	REG(ebx)
	OP(push,l)	REG(edx)
	OP(push,l)	REG(ecx)

ifdef(`CALLER_ALLOCS_STRUCT_RETURN',`
	OP(mov,l)	TW(REG(esp),REG(ecx))	# push pointer to struct return
	OP(add,l)	TW(IMM(16),REG(ecx))
	OP(push,l)	REG(ecx)
')

	OP(xor,l)	TW(REG(ecx),REG(ecx))
	OP(mov,b)	TW(REG(al),REG(cl))
	OP(mov,l)	TW(SDX(EVR(utility_table),REG(ecx),4),REG(eax))
	call		IJMP(REG(eax))

define_debugging_label(scheme_to_interface_return)
ifdef(`CALLER_ALLOCS_STRUCT_RETURN',`
	OP(add,l)	TW(IMM(4),REG(esp))	# pop pointer to struct return
')
	OP(add,l)	TW(IMM(16),REG(esp))		# Pop utility args

IFDOS(`
	OP(mov,l)	TW(LOF(4,REG(eax)),REG(edx))
	OP(mov,l)	TW(IND(REG(eax)),REG(eax))
')

ifdef(`CALLER_ALLOCS_STRUCT_RETURN',`
	OP(pop,l)	REG(eax)	# Pop struct return into registers
	OP(pop,l)	REG(edx)
')
	jmp		IJMP(REG(eax))			# Invoke handler

define_c_label(interface_to_scheme)
IF387(`
	OP(cmp,l)	TW(IMM(0),ABS(EVR(i387_presence)))
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
ifdef(`HACK_SEGMENT_REGS',
`	OP(mov,l)	TW(EVR(Free),rfree)
	OP(sub,l)	TW(SEGMENT_DELTA,rfree)
	OP(mov,l)	TW(IMM(ADDRESS_MASK),rmask)

	OP(mov,l)	TW(EVR(Ext_Stack_Pointer),REG(eax)) # Switch stacks
	OP(sub,l)	TW(SEGMENT_DELTA,REG(eax))
	OP(mov,w)	TW(EVR(Scheme_Stack_Segment_Selector),REG(ss))
	OP(mov,l)	TW(REG(eax),REG(esp))

	OP(sub,l)	TW(SEGMENT_DELTA,REG(edx))	# Entry point to new space
	OP(xor,l)	TW(REG(ecx),REG(ecx))		# Setup cross-segment jump
	OP(mov,w)	TW(EVR(Scheme_Code_Segment_Selector),REG(cx))

	OP(mov,w)	TW(REG(ds),REG(ax))		# Store C ds in es,
	OP(mov,w)	TW(REG(ax),REG(es))		#  unused by Scheme.
	OP(mov,w)	TW(EVR(Scheme_Data_Segment_Selector),REG(ax)) # Switch data segments
	OP(mov,w)	TW(REG(ax),REG(ds))

	OP(push,l)	REG(ecx)
	OP(push,l)	REG(edx)

	OP(mov,l)	TW(LOF(REGBLOCK_VAL(),regs),REG(eax)) # Value/dynamic link
	OP(mov,l)	TW(REG(eax),REG(ecx))		# Preserve if used
	OP(and,l)	TW(rmask,REG(ecx))		# Restore potential dynamic link
	OP(mov,l)	TW(REG(ecx),LOF(REGBLOCK_DLINK(),regs))
	LRET						# Perform cross-segment jump
',
`	OP(mov,l)	TW(EVR(Free),rfree)		# Free pointer = %edi
	OP(mov,l)	TW(LOF(REGBLOCK_VAL(),regs),REG(eax)) # Value/dynamic link
	OP(mov,l)	TW(IMM(ADDRESS_MASK),rmask)	# = %ebp

IFDOS(`	OP(mov,w)	TW(EVR(Scheme_Stack_Segment_Selector),REG(ss))') # Swap stack segments

	OP(mov,l)	TW(EVR(Ext_Stack_Pointer),REG(esp))
	OP(mov,l)	TW(REG(eax),REG(ecx))		# Preserve if used
	OP(and,l)	TW(rmask,REG(ecx))		# Restore potential dynamic link
	OP(mov,l)	TW(REG(ecx),LOF(REGBLOCK_DLINK(),regs))
	jmp		IJMP(REG(edx))')

IF_WIN32(`
use_external_code(EFR(WinntExceptionTransferHook))
define_code_label(EFR(callWinntExceptionTransferHook))
	call	EFR(WinntExceptionTransferHook)
	mov	edx,eax
')

define_c_label(interface_to_C)
IF387(`
	OP(cmp,l)	TW(IMM(0),ABS(EVR(i387_presence)))
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

	OP(mov,l)	TW(REG(edx),REG(eax))		# Set up result
	OP(pop,l)	REG(ebx)			# Restore callee-saves
	OP(pop,l)	REG(esi)			#  registers
	OP(pop,l)	REG(edi)
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
	OP(mov,l)	TW(LOF(REGBLOCK_DLINK(),regs),REG(edx))
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

declare_alignment(2)
define_hook_label(short_primitive_apply)
	OP(pop,l)	REG(edx)			# offset pointer
	OP(mov,l)	TW(IND(REG(edx)),REG(ecx))	# offset
							# Primitive object
	OP(mov,l)	TW(IDX(REG(edx),REG(ecx)),REG(ecx))
							# Merge
	jmp	hook_reference(primitive_apply)

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
# 	OP(mov,l)	TW(IMM($1),REG(edx))
# 	OP(mov,b)	TW(IMM(HEX(14)),REG(al))
# 	jmp	scheme_to_interface')

declare_alignment(2)
define_hook_label(sc_apply)
	OP(mov,l)	TW(REG(ecx),REG(eax))		# Copy for type code
	OP(mov,l)	TW(REG(ecx),REG(ebx))		# Copy for address
	OP(shr,l)	TW(IMM(DATUM_LENGTH),REG(eax))	# Select type code
	OP(and,l)	TW(rmask,REG(ebx))		# Select datum
	OP(cmp,b)	TW(IMM(TC_COMPILED_ENTRY),REG(al))
	jne	asm_sc_apply_generic
	OP(movs,bl,x)	TW(BOF(-4,REG(ebx)),REG(eax))	# Extract frame size
	OP(cmp,l)	TW(REG(eax),REG(edx))		# Compare to nargs+1
	jne	asm_sc_apply_generic
	jmp	IJMP(REG(ebx))				# Invoke

define_debugging_label(asm_sc_apply_generic)
	OP(mov,l)	TW(IMM(HEX(14)),REG(eax))
	jmp	scheme_to_interface	

define(define_apply_fixed_size,
`declare_alignment(2)
define_hook_label(sc_apply_size_$1)
	OP(mov,l)	TW(REG(ecx),REG(eax))		# Copy for type code
	OP(mov,l)	TW(REG(ecx),REG(ebx))		# Copy for address
	OP(shr,l)	TW(IMM(DATUM_LENGTH),REG(eax))	# Select type code
	OP(and,l)	TW(rmask,REG(ebx))		# Select datum
	OP(cmp,b)	TW(IMM(TC_COMPILED_ENTRY),REG(al))
	jne	asm_sc_apply_generic_$1
	OP(cmp,b)	TW(IMM($1),BOF(-4,REG(ebx)))	# Compare frame size
	jne	asm_sc_apply_generic_$1	# to nargs+1
	jmp	IJMP(REG(ebx))

asm_sc_apply_generic_$1:
	OP(mov,l)	TW(IMM($1),REG(edx))
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
	OP(mov,l)	TW(IMM(eval(TAG(TC_MANIFEST_NM_VECTOR,2))),IND(rfree))
	OP(mov,l)	TW(rfree,REG(eax))
	OP(fstp,l)	DOF(4,rfree)			# fstpd
	OP(or,l)	TW(IMM(eval(TAG(TC_FLONUM,0))),REG(eax))
	OP(and,l)	TW(rmask,IND(REG(esp)))
	OP(add,l)	TW(IMM(12),rfree)
	OP(mov,l)	TW(REG(eax),LOF(REGBLOCK_VAL(),regs))
	ret

declare_alignment(2)
asm_generic_fixnum_result:
	OP(and,l)	TW(rmask,IND(REG(esp)))
	OP(or,b)	TW(IMM(TC_FIXNUM),REG(al))
	OP(ror,l)	TW(IMM(TC_LENGTH),REG(eax))
	OP(mov,l)	TW(REG(eax),LOF(REGBLOCK_VAL(),regs))
	ret

declare_alignment(2)
asm_generic_return_sharp_t:
	OP(and,l)	TW(rmask,IND(REG(esp)))
	OP(mov,l)	TW(IMM(eval(TAG(TC_TRUE,0))),LOF(REGBLOCK_VAL(),regs))
	ret

declare_alignment(2)
asm_generic_return_sharp_f:
	OP(and,l)	TW(rmask,IND(REG(esp)))
	OP(mov,l)	TW(IMM(eval(TAG(TC_FALSE,0))),LOF(REGBLOCK_VAL(),regs))
	ret')

define(define_unary_operation,
`declare_alignment(2)
define_hook_label(generic_$1)
	OP(pop,l)	REG(edx)
	OP(mov,l)	TW(REG(edx),REG(eax))
	OP(shr,l)	TW(IMM(DATUM_LENGTH),REG(eax))
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(al))
	je	asm_generic_$1_fix
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(al))
	jne	asm_generic_$1_fail
	OP(and,l)	TW(rmask,REG(edx))
	fld1
	OP($4,l)	DOF(4,REG(edx))
	jmp	asm_generic_flonum_result

asm_generic_$1_fix:
	OP(mov,l)	TW(REG(edx),REG(eax))
	OP(shl,l)	TW(IMM(TC_LENGTH),REG(eax))
	OP($3,l)	TW(IMM(eval(2 ** TC_LENGTH)),REG(eax))
	jno	asm_generic_fixnum_result

asm_generic_$1_fail:
	OP(push,l)	REG(edx)
	OP(mov,b)	TW(IMM(HEX($2)),REG(al))
	jmp	scheme_to_interface')

define(define_unary_predicate,
`declare_alignment(2)
define_hook_label(generic_$1)
	OP(pop,l)	REG(edx)
	OP(mov,l)	TW(REG(edx),REG(eax))
	OP(shr,l)	TW(IMM(DATUM_LENGTH),REG(eax))
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(al))
	je	asm_generic_$1_fix
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(al))
	jne	asm_generic_$1_fail
	OP(and,l)	TW(rmask,REG(edx))
	OP(fld,l)	DOF(4,REG(edx))
	ftst
	fstsw	REG(ax)
	fstp	ST(0)
	sahf
	$4	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fix:
	OP(mov,l)	TW(REG(edx),REG(eax))
	OP(shl,l)	TW(IMM(TC_LENGTH),REG(eax))
	OP(cmp,l)	TW(IMM(0),REG(eax))
	$3	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fail:
	OP(push,l)	REG(edx)
	OP(mov,b)	TW(IMM(HEX($2)),REG(al))
	jmp	scheme_to_interface')

define(define_binary_operation,
`declare_alignment(2)
define_hook_label(generic_$1)
	OP(pop,l)	REG(edx)
	OP(pop,l)	REG(ebx)
	OP(mov,l)	TW(REG(edx),REG(eax))
	OP(mov,l)	TW(REG(ebx),REG(ecx))
	OP(shr,l)	TW(IMM(DATUM_LENGTH),REG(eax))
	OP(shr,l)	TW(IMM(DATUM_LENGTH),REG(ecx))
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(al))
	je	asm_generic_$1_fix
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(al))
	jne	asm_generic_$1_fail
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(cl))
	je	asm_generic_$1_flo_flo
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(cl))
	jne	asm_generic_$1_fail
	OP(shl,l)	TW(IMM(TC_LENGTH),REG(ebx))
	OP(and,l)	TW(rmask,REG(edx))
	OP(sar,l)	TW(IMM(TC_LENGTH),REG(ebx))
	OP(fld,l)	DOF(4,REG(edx))			# fldd
	OP(mov,l)	TW(REG(ebx),IND(rfree))
	OP($5,l)	IND(rfree)				# fisubl
	jmp	asm_generic_flonum_result

asm_generic_$1_fix:
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(cl))
	je	asm_generic_$1_fix_flo
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(cl))
	jne	asm_generic_$1_fail
	OP(mov,l)	TW(REG(edx),REG(eax))
	OP(mov,l)	TW(REG(ebx),REG(ecx))
	OP(shl,l)	TW(IMM(TC_LENGTH),REG(eax))
	OP(shl,l)	TW(IMM(TC_LENGTH),REG(ecx))
	OP($3,l)	TW(REG(ecx),REG(eax))		# subl
	jno	asm_generic_fixnum_result

asm_generic_$1_fail:
	OP(push,l)	REG(ebx)
	OP(push,l)	REG(edx)
	OP(mov,b)	TW(IMM(HEX($2)),REG(al))
	jmp	scheme_to_interface

asm_generic_$1_flo_flo:
	OP(and,l)	TW(rmask,REG(edx))
	OP(and,l)	TW(rmask,REG(ebx))
	OP(fld,l)	DOF(4,REG(edx))			# fldd
	OP($6,l)	DOF(4,REG(ebx))			# fsubl
	jmp	asm_generic_flonum_result	

asm_generic_$1_fix_flo:
	OP(shl,l)	TW(IMM(TC_LENGTH),REG(edx))
	OP(and,l)	TW(rmask,REG(ebx))
	OP(sar,l)	TW(IMM(TC_LENGTH),REG(edx))
	OP(fld,l)	DOF(4,REG(ebx))			# fldd
	OP(mov,l)	TW(REG(edx),IND(rfree))
	OP($4,l)	IND(rfree)			# fisubrl
	jmp	asm_generic_flonum_result')

IF387(`declare_alignment(2)
define_hook_label(generic_divide)
	OP(pop,l)	REG(edx)
	OP(pop,l)	REG(ebx)
	OP(mov,l)	TW(REG(edx),REG(eax))
	OP(mov,l)	TW(REG(ebx),REG(ecx))
	OP(shr,l)	TW(IMM(DATUM_LENGTH),REG(eax))
	OP(shr,l)	TW(IMM(DATUM_LENGTH),REG(ecx))
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(al))
	je	asm_generic_divide_fix
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(al))
	jne	asm_generic_divide_fail
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(cl))
	je	asm_generic_divide_flo_flo
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(cl))
	jne	asm_generic_divide_fail
	OP(mov,l)	TW(REG(ebx),REG(ecx))
	OP(shl,l)	TW(IMM(TC_LENGTH),REG(ecx))
	je	asm_generic_divide_fail
	OP(and,l)	TW(rmask,REG(edx))
	OP(sar,l)	TW(IMM(TC_LENGTH),REG(ecx))
	OP(fld,l)	DOF(4,REG(edx))			# fldd
	OP(mov,l)	TW(REG(ecx),IND(rfree))
	OP(fidiv,l)	IND(rfree)
	jmp	asm_generic_flonum_result

asm_generic_divide_fix:
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(cl))
	jne	asm_generic_divide_fail
	OP(mov,l)	TW(REG(edx),REG(ecx))
	OP(shl,l)	TW(IMM(TC_LENGTH),REG(ecx))
	je	asm_generic_divide_fail
	OP(and,l)	TW(rmask,REG(ebx))
	OP(sar,l)	TW(IMM(TC_LENGTH),REG(ecx))
	OP(fld,l)	DOF(4,REG(ebx))			# fldd
	OP(mov,l)	TW(REG(ecx),IND(rfree))
	OP(fidivr,l)	IND(rfree)
	jmp	asm_generic_flonum_result

asm_generic_divide_flo_flo:
	OP(mov,l)	TW(REG(ebx),REG(ecx))
	OP(and,l)	TW(rmask,REG(ecx))
	OP(fld,l)	DOF(4,REG(ecx))			# fldd
	ftst
	fstsw	REG(ax)
	sahf
	je	asm_generic_divide_by_zero
	OP(and,l)	TW(rmask,REG(edx))
	OP(fdivr,l)	DOF(4,REG(edx))
	jmp	asm_generic_flonum_result	

asm_generic_divide_by_zero:
	fstp	ST(0)					# Pop second arg

asm_generic_divide_fail:
	OP(push,l)	REG(ebx)
	OP(push,l)	REG(edx)
	OP(mov,b)	TW(IMM(HEX(23)),REG(al))
	jmp	scheme_to_interface')

define(define_binary_predicate,
`declare_alignment(2)
define_hook_label(generic_$1)
	OP(pop,l)	REG(edx)
	OP(pop,l)	REG(ebx)
	OP(mov,l)	TW(REG(edx),REG(eax))
	OP(mov,l)	TW(REG(ebx),REG(ecx))
	OP(shr,l)	TW(IMM(DATUM_LENGTH),REG(eax))
	OP(shr,l)	TW(IMM(DATUM_LENGTH),REG(ecx))
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(al))
	je	asm_generic_$1_fix
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(al))
	jne	asm_generic_$1_fail
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(cl))
	je	asm_generic_$1_flo_flo
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(cl))
	jne	asm_generic_$1_fail
	OP(shl,l)	TW(IMM(TC_LENGTH),REG(ebx))
	OP(and,l)	TW(rmask,REG(edx))
	OP(sar,l)	TW(IMM(TC_LENGTH),REG(ebx))
	OP(fld,l)	DOF(4,REG(edx))			# fldd
	OP(mov,l)	TW(REG(ebx),IND(rfree))
	OP(ficomp,l)	IND(rfree)
	fstsw	REG(ax)
	sahf
	$5	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fix:
	OP(cmp,b)	TW(IMM(TC_FLONUM),REG(cl))
	je	asm_generic_$1_fix_flo
	OP(cmp,b)	TW(IMM(TC_FIXNUM),REG(cl))
	jne	asm_generic_$1_fail
	OP(shl,l)	TW(IMM(TC_LENGTH),REG(edx))
	OP(shl,l)	TW(IMM(TC_LENGTH),REG(ebx))
	OP(cmp,l)	TW(REG(ebx),REG(edx))
	$3	asm_generic_return_sharp_t	
	jmp	asm_generic_return_sharp_f

asm_generic_$1_flo_flo:
	OP(and,l)	TW(rmask,REG(edx))
	OP(and,l)	TW(rmask,REG(ebx))
	OP(fld,l)	DOF(4,REG(edx))			# fldd
	OP(fcomp,l)	DOF(4,REG(ebx))
	fstsw	REG(ax)
	sahf
	$6	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fix_flo:
	OP(shl,l)	TW(IMM(TC_LENGTH),REG(edx))
	OP(and,l)	TW(rmask,REG(ebx))
	OP(sar,l)	TW(IMM(TC_LENGTH),REG(edx))
	OP(mov,l)	TW(REG(edx),IND(rfree))
	OP(fild,l)	IND(rfree)
	OP(fcomp,l)	DOF(4,REG(ebx))
	fstsw	REG(ax)
	sahf
	$4	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f

asm_generic_$1_fail:
	OP(push,l)	REG(ebx)
	OP(push,l)	REG(edx)
	OP(mov,b)	TW(IMM(HEX($2)),REG(al))
	jmp	scheme_to_interface')

IF387(`define_unary_operation(decrement,22,sub,fsubr)
define_unary_operation(increment,26,add,fadd)

define_unary_predicate(negative,2a,jl,jb)
define_unary_predicate(positive,2c,jg,ja)
define_unary_predicate(zero,2d,je,je)

# define_binary_operation(name,index,fix*fix,fix*flo,flo*fix,flo*flo)
# define_binary_operation(  $1,   $2,     $3,     $4,     $5,     $6)
define_binary_operation(add,2b,add,fiadd,fiadd,fadd)
define_binary_operation(subtract,28,sub,fisubr,fisub,fsub)
define_binary_operation(multiply,29,imul,fimul,fimul,fmul)
# Divide needs to check for 0, so we cant really use the following
# define_binary_operation(divide,23,NONE,fidivr,fidiv,fdiv)

# define_binary_predicate(name,index,fix*fix,fix*flo,flo*fix,flo*flo)
define_binary_predicate(equal,24,je,je,je,je)
define_binary_predicate(greater,25,jg,ja,ja,ja)
define_binary_predicate(less,27,jl,jb,jb,jb)')

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
### End:
