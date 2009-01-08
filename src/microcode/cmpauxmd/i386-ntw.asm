;;; -*-Midas-*-
;;;
;;; $Id: i386.m4,v 1.70 2008/04/25 01:19:04 cph Exp $
;;;
;;; Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993,
;;;     1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
;;;     2004, 2005, 2006, 2007, 2008 Massachusetts Institute of
;;;     Technology
;;;
;;; This file is part of MIT/GNU Scheme.
;;;
;;; MIT/GNU Scheme is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; MIT/GNU Scheme is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with MIT/GNU Scheme; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;;; 02110-1301, USA.
;;; Intel IA-32 assembly language part of the compiled code interface.
;;; See cmpint.txt, cmpint.c, cmpint-mc68k.h, and cmpgc.h for more
;;; documentation.
;;;
;;; This m4 source expands into either Unix (gas) source or PC
;;; (masm/wasm) source.
;;;
;;; NOTE:
;;;	Assumptions:
;;;
;;;	0) Segment registers and paging are set up for 32-bit "flat"
;;;	operation.
;;;
;;;	1) All registers (except double floating point registers) and
;;;	stack locations hold a C long object.
;;;
;;;	2) The C compiler divides registers into three groups:
;;;	- Linkage registers, used for procedure calls and global
;;;	references.  On i386 (gcc and Zortech C): %ebp, %esp.
;;;	- super temporaries, not preserved accross procedure calls and
;;;	always usable. On i386 (gcc and Zortech C): %eax, %edx, %ecx.
;;;	- preserved registers saved by the callee if they are written.
;;;	On i386 (gcc and Zortech C): all others (%ebx, %esi, %edi).
;;;
;;;	3) Arguments, if passed on a stack, are popped by the caller
;;;	or by the procedure return instruction (as on the VAX).  Thus
;;;	most "leaf" procedures need not worry about them.  On i386,
;;;	arguments are passed on the stack.
;;;
;;;	4) There is a hardware or software maintained stack for
;;;	control.  The procedure calling sequence may leave return
;;;	addresses in registers, but they must be saved somewhere for
;;;	nested calls and recursive procedures.  On i386: saved on
;;;	the stack by the CALL instruction.
;;;
;;;	5) C procedures return long values in a super temporary
;;;	register.  Two word structures are returned differently,
;;;	depending on the C compiler used.  When using GCC, two-word
;;;	structures are returned in {%eax, %edx}.  When using a
;;;	compiler compatible with MicroSoft's C compiler (e.g. Zortech
;;;	C), two word structures are returned by returning in %eax the
;;;	address of a structure allocated statically.  If the Scheme
;;;	system ever becomes reentrant, this will have to change.
;;;
;;;	6) Floating point registers are not preserved by this
;;;	interface.  The interface is only called from the Scheme
;;;	interpreter, which does not use floating point data.  Thus
;;;	although the calling convention would require us to preserve
;;;	them, they contain garbage.
;;;
;;; Compiled Scheme code uses the following register convention:
;;;	- %esp containts the Scheme stack pointer, not the C stack
;;;	pointer.
;;;	- %esi contains a pointer to the Scheme interpreter's "register"
;;;	block.  This block contains the compiler's copy of MemTop,
;;;	the interpreter's registers (val, env, exp, etc.),
;;;	temporary locations for compiled code, and the addresses
;;;	of various hooks defined in this file.
;;;	- %edi contains the Scheme free pointer.
;;;	- %ebp contains the Scheme datum mask.
;;;	The dynamic link (when needed) is in Registers[REGBLOCK_COMPILER_TEMP]
;;;	Values are returned in Registers[REGBLOCK_VAL]
;;;
;;;	All other registers (%eax, %edx, %ecx, %ebx) are available to
;;;	the compiler.  A caller-saves convention is used, so the
;;;	registers need not be preserved by subprocedures.
;;; The following m4 macros can be defined to change how this file is
;;; expanded.
;;;
;;; DASM
;;;	If defined, expand to Intel assembly-language syntax, used by
;;;	Microsoft assembler (MASM) and Watcom assembler (WASM).
;;;	Otherwise, expand to AT&T syntax, used by GAS.
;;;
;;; WIN32
;;;	If defined, expand to run under Win32; implies DASM.
;;;
;;; SUPPRESS_LEADING_UNDERSCORE
;;;	If defined, external symbol names are generated as written;
;;;	otherwise, they have an underscore prepended to them.
;;; WCC386
;;;	Should be defined when using Watcom assembler.
;;; WCC386R
;;;	Should be defined when using Watcom assembler and generating
;;;	code to use the Watcom register-based argument conventions.
;;; TYPE_CODE_LENGTH
;;;	Normally defined to be 6.  Don't change this unless you know
;;;	what you're doing.
;;; DISABLE_387
;;;	If defined, do not generate 387 floating-point instructions.
;;; VALGRIND_MODE
;;;	If defined, modify code to make it work with valgrind.
;;;;	Utility macros and definitions
; GAS doesn't implement these, for no obvious reason.
; When using the Watcom C compiler with register-based calling
; conventions, source-code function names normally expand to `FOO_',
; but functions that are compiled with prefix keywords such as
; `__cdecl' or `__syscall' expand differently.  References to the
; former type of name are marked with `EFR', while references to the
; latter are marked with `EPFR'.
; Define the floating-point processor control word.  Always set
; round-to-even and double precision.  Under Win32, mask all
; exceptions.  Under unix and OS/2, mask only the inexact result
; exception.
.586p
.model flat
	.data
	align 2
	extrn _Free:dword
	extrn _stack_pointer:dword
	extrn _utility_table:dword
	extrn _RegistersPtr:dword
	public _i387_presence
_i387_presence dd 0
	public _C_Stack_Pointer
_C_Stack_Pointer dd 0
	public _C_Frame_Pointer
_C_Frame_Pointer dd 0
	public _ia32_cpuid_supported
_ia32_cpuid_supported dd 0
	public _ia32_cpuid_needed
_ia32_cpuid_needed dd 0
	.code
	align 2
	public _i386_interface_initialize
_i386_interface_initialize:
	push	ebp
	mov	ebp,esp
	xor	eax,eax		; No 387 available
; Unfortunately, the `movl cr0,ecx' instruction is privileged.
; Use the deprecated `smsw cx' instruction instead.
;	OP(mov,l)	TW(REG(cr0),REG(ecx))		; Test for 387 presence
	smsw		cx
	mov	edx,012H
	and	ecx,edx
	cmp	ecx,edx
	jne	i386_initialize_no_fp
	inc	eax			; 387 available
	sub	esp,4
	fclex
	fnstcw		word ptr -2[ebp]
	and	word ptr -2[ebp],0f0e0H
	or	word ptr -2[ebp],0023fH
	fldcw		word ptr -2[ebp]
i386_initialize_no_fp:
	mov	dword ptr _i387_presence,eax
; Do a bunch of hair to determine if we need to do cache synchronization.
; See if the CPUID instruction is supported.
	xor	eax,eax
	mov	dword ptr _ia32_cpuid_supported,eax
	mov	dword ptr _ia32_cpuid_needed,eax
; First test: can we toggle the AC bit?
	pushfd
	pop	eax
	mov	ecx,eax
	xor	eax,000040000H
	push	eax
	popfd
	pushfd
	pop	eax
; if AC bit can't be toggled, this is a 386 (and doesn't support CPUID).
	xor	eax,ecx
	jz		no_cpuid_instr
	push	ecx			; restore EFLAGS
	popfd
; Now test to see if the ID bit can be toggled.
	mov	eax,ecx
	xor	eax,000200000H
	push	eax
	popfd
	pushfd
	pop	eax
; if ID bit can't be toggled, this is a 486 that doesn't support CPUID.
	xor	eax,ecx
	jz		no_cpuid_instr
	push	ecx			; restore EFLAGS
	popfd
; Now we know that cpuid is supported.
	mov	dword ptr _ia32_cpuid_supported,000000001H
; Next, use the CPUID instruction to determine the processor type.
	push	ebx
	xor	eax,eax
	cpuid
; Check that CPUID accepts argument 1.
	cmp	eax,000000001H
	jl		done_setting_up_cpuid
; Detect "GenuineIntel".
;	OP(cmp,l)	TW(IMM(HEX(756e6547)),REG(ebx))
;	jne		not_intel_cpu
;	OP(cmp,l)	TW(IMM(HEX(49656e69)),REG(edx))
;	jne		not_intel_cpu
;	OP(cmp,l)	TW(IMM(HEX(6c65746e)),REG(ecx))
;	jne		not_intel_cpu
; For CPU families 4 (486), 5 (Pentium), or 6 (Pentium Pro, Pentium
; II, Pentium III), don't use CPUID synchronization.
;	OP(mov,l)	TW(IMM(HEX(01)),REG(eax))
;	cpuid
;	OP(shr,l)	TW(IMM(HEX(08)),REG(eax))
;	OP(and,l)	TW(IMM(HEX(0000000F)),REG(eax))
;	OP(cmp,l)	TW(IMM(HEX(4)),REG(eax))
;	jl		done_setting_up_cpuid
;	OP(cmp,l)	TW(IMM(HEX(6)),REG(eax))
;	jg		done_setting_up_cpuid
;
;	jmp		cpuid_not_needed
;
;not_intel_cpu:
; Detect "AuthenticAMD".
	cmp	ebx,068747541H
	jne		not_amd_cpu
	cmp	edx,069746e65H
	jne		not_amd_cpu
	cmp	ecx,0444d4163H
	jne		not_amd_cpu
; Problem appears to exist only on Athlon models 1, 3, and 4.
	mov	eax,001H
	cpuid
	mov	ecx,eax
	shr	eax,008H
	and	eax,00000000FH
	cmp	eax,06H	; family 6 = Athlon
	jne		done_setting_up_cpuid
	mov	eax,ecx
	shr	eax,004H
	and	eax,00000000FH
	cmp	eax,06H	; model 6 and up OK
	jge		done_setting_up_cpuid
	cmp	eax,02H	; model 2 OK
	je		done_setting_up_cpuid
	mov	dword ptr _ia32_cpuid_needed,000000001H
not_amd_cpu:
done_setting_up_cpuid:
	pop	ebx
no_cpuid_instr:
	leave
	ret
	public _C_to_interface
_C_to_interface:
	push	ebp			; Link according
	mov	ebp,esp		;  to C's conventions
	push	edi			; Save callee-saves
	push	esi			;  registers
	push	ebx
	mov	edx,dword ptr 8[ebp]	; Entry point
							; Preserve frame ptr
	mov	_C_Frame_Pointer,ebp
							; Preserve stack ptr
	mov	_C_Stack_Pointer,esp
							; Register block = %esi
							; Scheme offset in NT
	mov	esi,dword ptr _RegistersPtr
	jmp	_interface_to_scheme
	public asm_trampoline_to_interface_
asm_trampoline_to_interface_:
	public trampoline_to_interface
trampoline_to_interface:
	pop	ecx			; trampoline storage
	jmp	scheme_to_interface
	public asm_scheme_to_interface_call_
asm_scheme_to_interface_call_:
	public scheme_to_interface_call
scheme_to_interface_call:
	pop	ecx			; arg1 = ret. add
	add	ecx,4		; Skip format info
;	jmp	scheme_to_interface
	public asm_scheme_to_interface_
asm_scheme_to_interface_:
	public scheme_to_interface
scheme_to_interface:
; These two moves must happen _before_ the ffree instructions below.
; Otherwise recovery from SIGFPE there will fail.
	mov	_stack_pointer,esp
	mov	_Free,edi
	cmp	dword ptr _i387_presence,0
	je	scheme_to_interface_proceed
	ffree	st(0)					; Free floating "regs"
	ffree	st(1)
	ffree	st(2)
	ffree	st(3)
	ffree	st(4)
	ffree	st(5)
	ffree	st(6)
	ffree	st(7)
scheme_to_interface_proceed:
	mov	esp,_C_Stack_Pointer
	mov	ebp,_C_Frame_Pointer
	sub	esp,8	; alloc struct return
	push	dword ptr 36[esi] ; push utility args
	push	ebx
	push	edx
	push	ecx
	mov	ecx,esp	; push ptr to struct return
	add	ecx,16
	push	ecx
	xor	ecx,ecx
	mov	cl,al
	mov	eax,dword ptr _utility_table[ecx*4]
	call		eax
	public scheme_to_interface_return
scheme_to_interface_return:
	add	esp,20	; pop utility args
	pop	eax		; pop struct return
	pop	edx
	jmp		eax		; Invoke handler
	public _interface_to_scheme
_interface_to_scheme:
	cmp	dword ptr _i387_presence,0
	je	interface_to_scheme_proceed
	ffree	st(0)					; Free floating "regs"
	ffree	st(1)
	ffree	st(2)
	ffree	st(3)
	ffree	st(4)
	ffree	st(5)
	ffree	st(6)
	ffree	st(7)
interface_to_scheme_proceed:
	mov	edi,_Free		; Free pointer = %edi
	mov	eax,dword ptr 8[esi] ; Value/dynamic link
	mov	ebp,67108863	; = %ebp
	mov	esp,_stack_pointer
	mov	ecx,eax		; Preserve if used
	and	ecx,ebp		; Restore potential dynamic link
	mov	dword ptr 16[esi],ecx
	jmp		edx
	extrn WinntExceptionTransferHook_:near
	public callWinntExceptionTransferHook_
callWinntExceptionTransferHook_:
	call	WinntExceptionTransferHook_
	mov	edx,eax
	public _interface_to_C
_interface_to_C:
	cmp	dword ptr _i387_presence,0
	je	interface_to_C_proceed
	ffree	st(0)					; Free floating "regs"
	ffree	st(1)
	ffree	st(2)
	ffree	st(3)
	ffree	st(4)
	ffree	st(5)
	ffree	st(6)
	ffree	st(7)
interface_to_C_proceed:
	mov	eax,edx		; Set up result
	pop	ebx			; Restore callee-saves
	pop	esi			;  registers
	pop	edi
	leave
	ret
	public ia32_cache_synchronize_
ia32_cache_synchronize_:
	push	ebp
	mov	ebp,esp
	push	ebx
	xor	eax,eax
	cpuid
	pop	ebx
	leave
	ret
;;; Run the CPUID instruction for serialization.
	public asm_serialize_cache_
asm_serialize_cache_:
	pushad
	xor	eax,eax
	cpuid
	popad
	ret
;;; Stub to be used in place of above on machines that don't need it.
	public asm_dont_serialize_cache_
asm_dont_serialize_cache_:
	ret
;;;	Assembly language hooks used to reduce code size.
;;;	There is no time advantage to using these over using
;;;	scheme_to_interface (or scheme_to_interface_call), but the
;;;	code generated by the compiler can be somewhat smaller.
	public asm_interrupt_procedure_
asm_interrupt_procedure_:
	mov	al,01aH
	jmp	scheme_to_interface_call
	public asm_interrupt_continuation_
asm_interrupt_continuation_:
	mov	al,01bH
	jmp	scheme_to_interface_call
	public asm_interrupt_closure_
asm_interrupt_closure_:
	mov	al,018H
	jmp	scheme_to_interface
	public asm_interrupt_continuation_2_
asm_interrupt_continuation_2_:
	mov	al,03bH
	jmp	scheme_to_interface
	public asm_interrupt_dlink_
asm_interrupt_dlink_:
	mov	edx,dword ptr 16[esi]
	mov	al,019H
	jmp	scheme_to_interface_call
;;;
;;;	This saves even more instructions than primitive_apply
;;;	When the PC is not available.  Instead of jumping here,
;;;	a call instruction is used, and the longword offset to
;;;	the primitive object follows the call instruction.
;;;	This code loads the primitive object and merges with
;;;	apply_primitive
;;;
	align 2
	public asm_short_primitive_apply_
asm_short_primitive_apply_:
	pop	edx			; offset pointer
	mov	ecx,dword ptr [edx]	; offset
							; Primitive object
	mov	ecx,dword ptr [edx] [ecx]
							; Merge
	jmp	asm_primitive_apply_
	align 2
	public asm_primitive_apply_
asm_primitive_apply_:
	mov	al,012H
	jmp	scheme_to_interface
	public asm_primitive_lexpr_apply_
asm_primitive_lexpr_apply_:
	mov	al,013H
	jmp	scheme_to_interface
	public asm_error_
asm_error_:
	mov	al,015H
	jmp	scheme_to_interface
	public asm_link_
asm_link_:
	mov	al,017H
	jmp	scheme_to_interface_call
	public asm_assignment_trap_
asm_assignment_trap_:
	mov	al,01dH
	jmp	scheme_to_interface_call
	public asm_reference_trap_
asm_reference_trap_:
	mov	al,01fH
	jmp	scheme_to_interface_call
	public asm_safe_reference_trap_
asm_safe_reference_trap_:
	mov	al,020H
	jmp	scheme_to_interface_call
	public asm_primitive_error_
asm_primitive_error_:
	mov	al,036H
	jmp	scheme_to_interface_call
;;;	Assembly language hooks used to increase speed.
; define_jump_indirection(sc_apply,14)
; 
; define(define_apply_fixed_size,
; `define_hook_label(sc_apply_size_$1)
; 	OP(mov,l)	TW(IMM($1),REG(edx))
; 	OP(mov,b)	TW(IMM(HEX(14)),REG(al))
; 	jmp	scheme_to_interface')
	align 2
	public asm_sc_apply_
asm_sc_apply_:
	mov	eax,ecx		; Copy for type code
	mov	ebx,ecx		; Copy for address
	shr	eax,26	; Select type code
	and	ebx,ebp		; Select datum
	cmp	al,40
	jne	asm_sc_apply_generic
	movsx	eax,byte ptr -4[ebx]	; Extract frame size
	cmp	edx,eax		; Compare to nargs+1
	jne	asm_sc_apply_generic
	jmp	ebx				; Invoke
	public asm_sc_apply_generic
asm_sc_apply_generic:
	mov	eax,014H
	jmp	scheme_to_interface	
	align 2
	public asm_sc_apply_size_1_
asm_sc_apply_size_1_:
	mov	eax,ecx		; Copy for type code
	mov	ebx,ecx		; Copy for address
	shr	eax,26	; Select type code
	and	ebx,ebp		; Select datum
	cmp	al,40
	jne	asm_sc_apply_generic_1
	cmp	byte ptr -4[ebx],1	; Compare frame size
	jne	asm_sc_apply_generic_1	; to nargs+1
	jmp	ebx
asm_sc_apply_generic_1:
	mov	edx,1
	mov	al,014H
	jmp	scheme_to_interface
	align 2
	public asm_sc_apply_size_2_
asm_sc_apply_size_2_:
	mov	eax,ecx		; Copy for type code
	mov	ebx,ecx		; Copy for address
	shr	eax,26	; Select type code
	and	ebx,ebp		; Select datum
	cmp	al,40
	jne	asm_sc_apply_generic_2
	cmp	byte ptr -4[ebx],2	; Compare frame size
	jne	asm_sc_apply_generic_2	; to nargs+1
	jmp	ebx
asm_sc_apply_generic_2:
	mov	edx,2
	mov	al,014H
	jmp	scheme_to_interface
	align 2
	public asm_sc_apply_size_3_
asm_sc_apply_size_3_:
	mov	eax,ecx		; Copy for type code
	mov	ebx,ecx		; Copy for address
	shr	eax,26	; Select type code
	and	ebx,ebp		; Select datum
	cmp	al,40
	jne	asm_sc_apply_generic_3
	cmp	byte ptr -4[ebx],3	; Compare frame size
	jne	asm_sc_apply_generic_3	; to nargs+1
	jmp	ebx
asm_sc_apply_generic_3:
	mov	edx,3
	mov	al,014H
	jmp	scheme_to_interface
	align 2
	public asm_sc_apply_size_4_
asm_sc_apply_size_4_:
	mov	eax,ecx		; Copy for type code
	mov	ebx,ecx		; Copy for address
	shr	eax,26	; Select type code
	and	ebx,ebp		; Select datum
	cmp	al,40
	jne	asm_sc_apply_generic_4
	cmp	byte ptr -4[ebx],4	; Compare frame size
	jne	asm_sc_apply_generic_4	; to nargs+1
	jmp	ebx
asm_sc_apply_generic_4:
	mov	edx,4
	mov	al,014H
	jmp	scheme_to_interface
	align 2
	public asm_sc_apply_size_5_
asm_sc_apply_size_5_:
	mov	eax,ecx		; Copy for type code
	mov	ebx,ecx		; Copy for address
	shr	eax,26	; Select type code
	and	ebx,ebp		; Select datum
	cmp	al,40
	jne	asm_sc_apply_generic_5
	cmp	byte ptr -4[ebx],5	; Compare frame size
	jne	asm_sc_apply_generic_5	; to nargs+1
	jmp	ebx
asm_sc_apply_generic_5:
	mov	edx,5
	mov	al,014H
	jmp	scheme_to_interface
	align 2
	public asm_sc_apply_size_6_
asm_sc_apply_size_6_:
	mov	eax,ecx		; Copy for type code
	mov	ebx,ecx		; Copy for address
	shr	eax,26	; Select type code
	and	ebx,ebp		; Select datum
	cmp	al,40
	jne	asm_sc_apply_generic_6
	cmp	byte ptr -4[ebx],6	; Compare frame size
	jne	asm_sc_apply_generic_6	; to nargs+1
	jmp	ebx
asm_sc_apply_generic_6:
	mov	edx,6
	mov	al,014H
	jmp	scheme_to_interface
	align 2
	public asm_sc_apply_size_7_
asm_sc_apply_size_7_:
	mov	eax,ecx		; Copy for type code
	mov	ebx,ecx		; Copy for address
	shr	eax,26	; Select type code
	and	ebx,ebp		; Select datum
	cmp	al,40
	jne	asm_sc_apply_generic_7
	cmp	byte ptr -4[ebx],7	; Compare frame size
	jne	asm_sc_apply_generic_7	; to nargs+1
	jmp	ebx
asm_sc_apply_generic_7:
	mov	edx,7
	mov	al,014H
	jmp	scheme_to_interface
	align 2
	public asm_sc_apply_size_8_
asm_sc_apply_size_8_:
	mov	eax,ecx		; Copy for type code
	mov	ebx,ecx		; Copy for address
	shr	eax,26	; Select type code
	and	ebx,ebp		; Select datum
	cmp	al,40
	jne	asm_sc_apply_generic_8
	cmp	byte ptr -4[ebx],8	; Compare frame size
	jne	asm_sc_apply_generic_8	; to nargs+1
	jmp	ebx
asm_sc_apply_generic_8:
	mov	edx,8
	mov	al,014H
	jmp	scheme_to_interface
;;;	The following code is used by generic arithmetic
;;;	whether the fixnum case is open-coded in line or not.
;;;	This takes care of fixnums and flonums so that the common
;;;	numeric types are much faster than the rare ones
;;;	(bignums, ratnums, recnums)
	align 2
asm_generic_flonum_result:
	mov	dword ptr [edi],-1677721598
	mov	eax,edi
	fstp	qword ptr 4[edi]			; fstpd
	or	eax,402653184
	and	dword ptr [esp],ebp
	add	edi,12
	mov	dword ptr 8[esi],eax
	ret
	align 2
asm_generic_fixnum_result:
	and	dword ptr [esp],ebp
	or	al,26
	ror	eax,6
	mov	dword ptr 8[esi],eax
	ret
	align 2
asm_generic_return_sharp_t:
	and	dword ptr [esp],ebp
	mov	dword ptr 8[esi],536870912
	ret
	align 2
asm_generic_return_sharp_f:
	and	dword ptr [esp],ebp
	mov	dword ptr 8[esi],0
	ret
	align 2
	public asm_generic_divide_
asm_generic_divide_:
	pop	edx
	pop	ebx
	mov	eax,edx
	mov	ecx,ebx
	shr	eax,26
	shr	ecx,26
	cmp	al,26
	je	asm_generic_divide_fix
	cmp	al,6
	jne	asm_generic_divide_fail
	cmp	cl,6
	je	asm_generic_divide_flo_flo
	cmp	cl,26
	jne	asm_generic_divide_fail
	mov	ecx,ebx
	shl	ecx,6
	je	asm_generic_divide_fail
	and	edx,ebp
	sar	ecx,6
	fld	qword ptr 4[edx]			; fldd
	mov	dword ptr [edi],ecx
	fidiv	dword ptr [edi]
	jmp	asm_generic_flonum_result
asm_generic_divide_fix:
	cmp	cl,6
	jne	asm_generic_divide_fail
	mov	ecx,edx
	shl	ecx,6
	je	asm_generic_divide_fail
	and	ebx,ebp
	sar	ecx,6
	fld	qword ptr 4[ebx]			; fldd
	mov	dword ptr [edi],ecx
	fidivr	dword ptr [edi]
	jmp	asm_generic_flonum_result
asm_generic_divide_flo_flo:
	mov	ecx,ebx
	and	ecx,ebp
	fld	qword ptr 4[ecx]			; fldd
	ftst
	fstsw	ax
	sahf
	je	asm_generic_divide_by_zero
	and	edx,ebp
	fdivr	qword ptr 4[edx]
	jmp	asm_generic_flonum_result	
asm_generic_divide_by_zero:
	fstp	st(0)					; Pop second arg
asm_generic_divide_fail:
	push	ebx
	push	edx
	mov	al,023H
	jmp	scheme_to_interface
	align 2
	public asm_generic_decrement_
asm_generic_decrement_:
	pop	edx
	mov	eax,edx
	shr	eax,26
	cmp	al,26
	je	asm_generic_decrement_fix
	cmp	al,6
	jne	asm_generic_decrement_fail
	and	edx,ebp
	fld1
	fsubr	qword ptr 4[edx]
	jmp	asm_generic_flonum_result
asm_generic_decrement_fix:
	mov	eax,edx
	shl	eax,6
	sub	eax,64
	jno	asm_generic_fixnum_result
asm_generic_decrement_fail:
	push	edx
	mov	al,022H
	jmp	scheme_to_interface
	align 2
	public asm_generic_increment_
asm_generic_increment_:
	pop	edx
	mov	eax,edx
	shr	eax,26
	cmp	al,26
	je	asm_generic_increment_fix
	cmp	al,6
	jne	asm_generic_increment_fail
	and	edx,ebp
	fld1
	fadd	qword ptr 4[edx]
	jmp	asm_generic_flonum_result
asm_generic_increment_fix:
	mov	eax,edx
	shl	eax,6
	add	eax,64
	jno	asm_generic_fixnum_result
asm_generic_increment_fail:
	push	edx
	mov	al,026H
	jmp	scheme_to_interface
	align 2
	public asm_generic_negative_
asm_generic_negative_:
	pop	edx
	mov	eax,edx
	shr	eax,26
	cmp	al,26
	je	asm_generic_negative_fix
	cmp	al,6
	jne	asm_generic_negative_fail
	and	edx,ebp
	fld	qword ptr 4[edx]
	ftst
	fstsw	ax
	fstp	st(0)
	sahf
	jb	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_negative_fix:
	mov	eax,edx
	shl	eax,6
	cmp	eax,0
	jl	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_negative_fail:
	push	edx
	mov	al,02aH
	jmp	scheme_to_interface
	align 2
	public asm_generic_positive_
asm_generic_positive_:
	pop	edx
	mov	eax,edx
	shr	eax,26
	cmp	al,26
	je	asm_generic_positive_fix
	cmp	al,6
	jne	asm_generic_positive_fail
	and	edx,ebp
	fld	qword ptr 4[edx]
	ftst
	fstsw	ax
	fstp	st(0)
	sahf
	ja	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_positive_fix:
	mov	eax,edx
	shl	eax,6
	cmp	eax,0
	jg	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_positive_fail:
	push	edx
	mov	al,02cH
	jmp	scheme_to_interface
	align 2
	public asm_generic_zero_
asm_generic_zero_:
	pop	edx
	mov	eax,edx
	shr	eax,26
	cmp	al,26
	je	asm_generic_zero_fix
	cmp	al,6
	jne	asm_generic_zero_fail
	and	edx,ebp
	fld	qword ptr 4[edx]
	ftst
	fstsw	ax
	fstp	st(0)
	sahf
	je	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_zero_fix:
	mov	eax,edx
	shl	eax,6
	cmp	eax,0
	je	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_zero_fail:
	push	edx
	mov	al,02dH
	jmp	scheme_to_interface
; define_binary_operation(name,index,fix*fix,flo*flo)
; define_binary_operation(  $1,   $2,     $3,     $4)
	align 2
	public asm_generic_add_
asm_generic_add_:
	pop	edx
	pop	ebx
	mov	eax,edx
	mov	ecx,ebx
	shr	eax,26
	shr	ecx,26
	cmp	cl,al
	jne	asm_generic_add_fail
	cmp	al,26
	je	asm_generic_add_fix
	cmp	al,6
	je	asm_generic_add_flo
asm_generic_add_fail:
	push	ebx
	push	edx
	mov	al,02bH
	jmp	scheme_to_interface
asm_generic_add_fix:
	mov	eax,edx
	mov	ecx,ebx
	shl	eax,6
	shl	ecx,6
	add	eax,ecx		; subl
	jo	asm_generic_add_fail
	jmp	asm_generic_fixnum_result
asm_generic_add_flo:
	and	edx,ebp
	and	ebx,ebp
	fld	qword ptr 4[edx]			; fldd
	fadd	qword ptr 4[ebx]			; fsubl
	jmp	asm_generic_flonum_result
	align 2
	public asm_generic_subtract_
asm_generic_subtract_:
	pop	edx
	pop	ebx
	mov	eax,edx
	mov	ecx,ebx
	shr	eax,26
	shr	ecx,26
	cmp	cl,al
	jne	asm_generic_subtract_fail
	cmp	al,26
	je	asm_generic_subtract_fix
	cmp	al,6
	je	asm_generic_subtract_flo
asm_generic_subtract_fail:
	push	ebx
	push	edx
	mov	al,028H
	jmp	scheme_to_interface
asm_generic_subtract_fix:
	mov	eax,edx
	mov	ecx,ebx
	shl	eax,6
	shl	ecx,6
	sub	eax,ecx		; subl
	jo	asm_generic_subtract_fail
	jmp	asm_generic_fixnum_result
asm_generic_subtract_flo:
	and	edx,ebp
	and	ebx,ebp
	fld	qword ptr 4[edx]			; fldd
	fsub	qword ptr 4[ebx]			; fsubl
	jmp	asm_generic_flonum_result
	align 2
	public asm_generic_multiply_
asm_generic_multiply_:
	pop	edx
	pop	ebx
	mov	eax,edx
	mov	ecx,ebx
	shr	eax,26
	shr	ecx,26
	cmp	cl,al
	jne	asm_generic_multiply_fail
	cmp	al,26
	je	asm_generic_multiply_fix
	cmp	al,6
	je	asm_generic_multiply_flo
asm_generic_multiply_fail:
	push	ebx
	push	edx
	mov	al,029H
	jmp	scheme_to_interface
asm_generic_multiply_fix:
	mov	eax,edx
	mov	ecx,ebx
	shl	eax,6
	shl	ecx,6
	imul	eax,ecx		; subl
	jo	asm_generic_multiply_fail
	jmp	asm_generic_fixnum_result
asm_generic_multiply_flo:
	and	edx,ebp
	and	ebx,ebp
	fld	qword ptr 4[edx]			; fldd
	fmul	qword ptr 4[ebx]			; fsubl
	jmp	asm_generic_flonum_result
; Divide needs to check for 0, so we cant really use the following
; define_binary_operation(divide,23,NONE,fdiv)
; define_binary_predicate(name,index,fix*fix,flo*flo)
	align 2
	public asm_generic_equal_
asm_generic_equal_:
	pop	edx
	pop	ebx
	mov	eax,edx
	mov	ecx,ebx
	shr	eax,26
	shr	ecx,26
	cmp	cl,al
	jne	asm_generic_equal_fail
	cmp	al,26
	jne	asm_generic_equal_fail
	shl	edx,6
	shl	ebx,6
	cmp	edx,ebx
	je	asm_generic_return_sharp_t	
	jmp	asm_generic_return_sharp_f
asm_generic_equal_fail:
	push	ebx
	push	edx
	mov	al,024H
	jmp	scheme_to_interface
	align 2
	public asm_generic_greater_
asm_generic_greater_:
	pop	edx
	pop	ebx
	mov	eax,edx
	mov	ecx,ebx
	shr	eax,26
	shr	ecx,26
	cmp	cl,al
	jne	asm_generic_greater_fail
	cmp	al,26
	jne	asm_generic_greater_fail
	shl	edx,6
	shl	ebx,6
	cmp	edx,ebx
	jg	asm_generic_return_sharp_t	
	jmp	asm_generic_return_sharp_f
asm_generic_greater_fail:
	push	ebx
	push	edx
	mov	al,025H
	jmp	scheme_to_interface
	align 2
	public asm_generic_less_
asm_generic_less_:
	pop	edx
	pop	ebx
	mov	eax,edx
	mov	ecx,ebx
	shr	eax,26
	shr	ecx,26
	cmp	cl,al
	jne	asm_generic_less_fail
	cmp	al,26
	jne	asm_generic_less_fail
	shl	edx,6
	shl	ebx,6
	cmp	edx,ebx
	jl	asm_generic_return_sharp_t	
	jmp	asm_generic_return_sharp_f
asm_generic_less_fail:
	push	ebx
	push	edx
	mov	al,027H
	jmp	scheme_to_interface
; These don't currently differ according to whether there
; is a 387 or not.
	public asm_generic_quotient_
asm_generic_quotient_:
	mov	al,037H
	jmp	scheme_to_interface
	public asm_generic_remainder_
asm_generic_remainder_:
	mov	al,038H
	jmp	scheme_to_interface
	public asm_generic_modulo_
asm_generic_modulo_:
	mov	al,039H
	jmp	scheme_to_interface
	public asm_nofp_decrement_
asm_nofp_decrement_:
	mov	al,022H
	jmp	scheme_to_interface
	public asm_nofp_divide_
asm_nofp_divide_:
	mov	al,023H
	jmp	scheme_to_interface
	public asm_nofp_equal_
asm_nofp_equal_:
	mov	al,024H
	jmp	scheme_to_interface
	public asm_nofp_greater_
asm_nofp_greater_:
	mov	al,025H
	jmp	scheme_to_interface
	public asm_nofp_increment_
asm_nofp_increment_:
	mov	al,026H
	jmp	scheme_to_interface
	public asm_nofp_less_
asm_nofp_less_:
	mov	al,027H
	jmp	scheme_to_interface
	public asm_nofp_subtract_
asm_nofp_subtract_:
	mov	al,028H
	jmp	scheme_to_interface
	public asm_nofp_multiply_
asm_nofp_multiply_:
	mov	al,029H
	jmp	scheme_to_interface
	public asm_nofp_negative_
asm_nofp_negative_:
	mov	al,02aH
	jmp	scheme_to_interface
	public asm_nofp_add_
asm_nofp_add_:
	mov	al,02bH
	jmp	scheme_to_interface
	public asm_nofp_positive_
asm_nofp_positive_:
	mov	al,02cH
	jmp	scheme_to_interface
	public asm_nofp_zero_
asm_nofp_zero_:
	mov	al,02dH
	jmp	scheme_to_interface
	public asm_nofp_quotient_
asm_nofp_quotient_:
	mov	al,037H
	jmp	scheme_to_interface
	public asm_nofp_remainder_
asm_nofp_remainder_:
	mov	al,038H
	jmp	scheme_to_interface
	public asm_nofp_modulo_
asm_nofp_modulo_:
	mov	al,039H
	jmp	scheme_to_interface
end
;;; Edwin Variables:
;;; comment-column: 56
;;; End:
