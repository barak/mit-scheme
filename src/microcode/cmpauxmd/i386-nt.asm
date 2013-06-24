;;; -*-Midas-*-
;;;
;;; $Id: i386.m4,v 1.57 2002/03/11 21:39:18 cph Exp $
;;;
;;; Copyright (c) 1992-2002 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.
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
;;; OS2
;;;	If defined, expand to run under OS/2.  This macro does nothing
;;;	more than define SUPPRESS_LEADING_UNDERSCORE and
;;;	CALLER_ALLOCS_STRUCT_RETURN, which are the conventions used to
;;;	call OS/2 API procedures; note that EMX/GCC doesn't define
;;;	these symbols because it thinks it's running under unix.
;;;
;;; If none of { WIN32, OS2 } is defined, expansion is for unix.
;;;
;;; SUPPRESS_LEADING_UNDERSCORE
;;;	If defined, external symbol names are generated as written;
;;;	otherwise, they have an underscore prepended to them.
;;; CALLER_ALLOCS_STRUCT_RETURN
;;; STATIC_STRUCT_RETURN
;;;	Controls the conventions used to return 8-byte structs from C
;;;	procedures.  If CALLER_ALLOCS_STRUCT_RETURN is defined, the
;;;	caller allocates space on the stack and passes a pointer to
;;;	that space on the top of the stack.  If STATIC_STRUCT_RETURN
;;;	is defined, the callee returns a pointer to a static struct in
;;;	EAX.  Otherwise, the callee returns the struct in EAX/EDX.
;;; CALLEE_POPS_STRUCT_RETURN
;;;	Modifies the CALLER_ALLOCS_STRUCT_RETURN calling convention.
;;;	Under the modified convention, the callee pops the pointer to
;;;	the allocated space, so the caller doesn't have to.  This
;;;	convention is used by GCC 2.9.x.
;;; WCC386
;;;	Should be defined when using Watcom assembler.
;;; WCC386R
;;;	Should be defined when using Watcom assembler and generating
;;;	code to use the Watcom register-based argument conventions.
;;; LINUX_ELF
;;;	If defined, expand to run under Linux ELF.
;;; TYPE_CODE_LENGTH
;;;	Normally defined to be 6.  Don't change this unless you know
;;;	what you're doing.
;;; DISABLE_387
;;;	If defined, do not generate 387 floating-point instructions.
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
	extrn _Ext_Stack_Pointer:dword
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
; First, test to see if the CPUID instruction is supported.
	xor	eax,eax
	mov	dword ptr _ia32_cpuid_supported,eax
	mov	dword ptr _ia32_cpuid_needed,eax
	pushfd
	pop	eax
	mov	ecx,eax
	xor	eax,000040000H
	push	eax
	popfd
	pushfd
	pop	eax
	xor	eax,ecx
	jz		no_cpuid_instr
; Restore original EFLAGS.
	push	ecx
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
	public _asm_trampoline_to_interface
_asm_trampoline_to_interface:
	public trampoline_to_interface
trampoline_to_interface:
	pop	ecx			; trampoline storage
	jmp	scheme_to_interface
	public _asm_scheme_to_interface_call
_asm_scheme_to_interface_call:
	public scheme_to_interface_call
scheme_to_interface_call:
	pop	ecx			; arg1 = ret. add
	add	ecx,4		; Skip format info
;	jmp	scheme_to_interface
	public _asm_scheme_to_interface
_asm_scheme_to_interface:
	public scheme_to_interface
scheme_to_interface:
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
	mov	_Ext_Stack_Pointer,esp
	mov	_Free,edi
	mov	esp,_C_Stack_Pointer
	mov	ebp,_C_Frame_Pointer
	push	dword ptr 36[esi] ; Utility args
	push	ebx
	push	edx
	push	ecx
	xor	ecx,ecx
	mov	cl,al
	mov	eax,dword ptr _utility_table[ecx*4]
	call		eax
	public scheme_to_interface_return
scheme_to_interface_return:
	add	esp,16		; Pop utility args
	jmp		eax			; Invoke handler
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
	mov	esp,_Ext_Stack_Pointer
	mov	ecx,eax		; Preserve if used
	and	ecx,ebp		; Restore potential dynamic link
	mov	dword ptr 16[esi],ecx
	jmp		edx
	extrn _WinntExceptionTransferHook:near
	public _callWinntExceptionTransferHook
_callWinntExceptionTransferHook:
	call	_WinntExceptionTransferHook
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
	public _ia32_cache_synchronize
_ia32_cache_synchronize:
	push	ebp
	mov	ebp,esp
	push	ebx
	xor	eax,eax
	cpuid
	pop	ebx
	leave
	ret
;;; Run the CPUID instruction for serialization.
	public _asm_serialize_cache
_asm_serialize_cache:
	pushad
	xor	eax,eax
	cpuid
	popad
	ret
;;; Stub to be used in place of above on machines that don't need it.
	public _asm_dont_serialize_cache
_asm_dont_serialize_cache:
	ret
;;;	Assembly language hooks used to reduce code size.
;;;	There is no time advantage to using these over using
;;;	scheme_to_interface (or scheme_to_interface_call), but the
;;;	code generated by the compiler can be somewhat smaller.
	public _asm_interrupt_procedure
_asm_interrupt_procedure:
	mov	al,01aH
	jmp	scheme_to_interface_call
	public _asm_interrupt_continuation
_asm_interrupt_continuation:
	mov	al,01bH
	jmp	scheme_to_interface_call
	public _asm_interrupt_closure
_asm_interrupt_closure:
	mov	al,018H
	jmp	scheme_to_interface
	public _asm_interrupt_continuation_2
_asm_interrupt_continuation_2:
	mov	al,03bH
	jmp	scheme_to_interface
	public _asm_interrupt_dlink
_asm_interrupt_dlink:
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
	public _asm_short_primitive_apply
_asm_short_primitive_apply:
	pop	edx			; offset pointer
	mov	ecx,dword ptr [edx]	; offset
							; Primitive object
	mov	ecx,dword ptr [edx] [ecx]
							; Merge
	jmp	_asm_primitive_apply
	align 2
	public _asm_primitive_apply
_asm_primitive_apply:
	mov	al,012H
	jmp	scheme_to_interface
	public _asm_primitive_lexpr_apply
_asm_primitive_lexpr_apply:
	mov	al,013H
	jmp	scheme_to_interface
	public _asm_error
_asm_error:
	mov	al,015H
	jmp	scheme_to_interface
	public _asm_link
_asm_link:
	mov	al,017H
	jmp	scheme_to_interface_call
	public _asm_assignment_trap
_asm_assignment_trap:
	mov	al,01dH
	jmp	scheme_to_interface_call
	public _asm_reference_trap
_asm_reference_trap:
	mov	al,01fH
	jmp	scheme_to_interface_call
	public _asm_safe_reference_trap
_asm_safe_reference_trap:
	mov	al,020H
	jmp	scheme_to_interface_call
	public _asm_primitive_error
_asm_primitive_error:
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
	public _asm_sc_apply
_asm_sc_apply:
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
	public _asm_sc_apply_size_1
_asm_sc_apply_size_1:
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
	public _asm_sc_apply_size_2
_asm_sc_apply_size_2:
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
	public _asm_sc_apply_size_3
_asm_sc_apply_size_3:
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
	public _asm_sc_apply_size_4
_asm_sc_apply_size_4:
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
	public _asm_sc_apply_size_5
_asm_sc_apply_size_5:
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
	public _asm_sc_apply_size_6
_asm_sc_apply_size_6:
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
	public _asm_sc_apply_size_7
_asm_sc_apply_size_7:
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
	public _asm_sc_apply_size_8
_asm_sc_apply_size_8:
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
	public _asm_generic_divide
_asm_generic_divide:
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
	public _asm_generic_decrement
_asm_generic_decrement:
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
	public _asm_generic_increment
_asm_generic_increment:
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
	public _asm_generic_negative
_asm_generic_negative:
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
	public _asm_generic_positive
_asm_generic_positive:
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
	public _asm_generic_zero
_asm_generic_zero:
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
; define_binary_operation(name,index,fix*fix,fix*flo,flo*fix,flo*flo)
; define_binary_operation(  $1,   $2,     $3,     $4,     $5,     $6)
	align 2
	public _asm_generic_add
_asm_generic_add:
	pop	edx
	pop	ebx
	mov	eax,edx
	mov	ecx,ebx
	shr	eax,26
	shr	ecx,26
	cmp	al,26
	je	asm_generic_add_fix
	cmp	al,6
	jne	asm_generic_add_fail
	cmp	cl,6
	je	asm_generic_add_flo_flo
	cmp	cl,26
	jne	asm_generic_add_fail
	shl	ebx,6
	and	edx,ebp
	sar	ebx,6
	fld	qword ptr 4[edx]			; fldd
	mov	dword ptr [edi],ebx
	fiadd	dword ptr [edi]				; fisubl
	jmp	asm_generic_flonum_result
asm_generic_add_fix:
	cmp	cl,6
	je	asm_generic_add_fix_flo
	cmp	cl,26
	jne	asm_generic_add_fail
	mov	eax,edx
	mov	ecx,ebx
	shl	eax,6
	shl	ecx,6
	add	eax,ecx		; subl
	jno	asm_generic_fixnum_result
asm_generic_add_fail:
	push	ebx
	push	edx
	mov	al,02bH
	jmp	scheme_to_interface
asm_generic_add_flo_flo:
	and	edx,ebp
	and	ebx,ebp
	fld	qword ptr 4[edx]			; fldd
	fadd	qword ptr 4[ebx]			; fsubl
	jmp	asm_generic_flonum_result	
asm_generic_add_fix_flo:
	shl	edx,6
	and	ebx,ebp
	sar	edx,6
	fld	qword ptr 4[ebx]			; fldd
	mov	dword ptr [edi],edx
	fiadd	dword ptr [edi]			; fisubrl
	jmp	asm_generic_flonum_result
	align 2
	public _asm_generic_subtract
_asm_generic_subtract:
	pop	edx
	pop	ebx
	mov	eax,edx
	mov	ecx,ebx
	shr	eax,26
	shr	ecx,26
	cmp	al,26
	je	asm_generic_subtract_fix
	cmp	al,6
	jne	asm_generic_subtract_fail
	cmp	cl,6
	je	asm_generic_subtract_flo_flo
	cmp	cl,26
	jne	asm_generic_subtract_fail
	shl	ebx,6
	and	edx,ebp
	sar	ebx,6
	fld	qword ptr 4[edx]			; fldd
	mov	dword ptr [edi],ebx
	fisub	dword ptr [edi]				; fisubl
	jmp	asm_generic_flonum_result
asm_generic_subtract_fix:
	cmp	cl,6
	je	asm_generic_subtract_fix_flo
	cmp	cl,26
	jne	asm_generic_subtract_fail
	mov	eax,edx
	mov	ecx,ebx
	shl	eax,6
	shl	ecx,6
	sub	eax,ecx		; subl
	jno	asm_generic_fixnum_result
asm_generic_subtract_fail:
	push	ebx
	push	edx
	mov	al,028H
	jmp	scheme_to_interface
asm_generic_subtract_flo_flo:
	and	edx,ebp
	and	ebx,ebp
	fld	qword ptr 4[edx]			; fldd
	fsub	qword ptr 4[ebx]			; fsubl
	jmp	asm_generic_flonum_result	
asm_generic_subtract_fix_flo:
	shl	edx,6
	and	ebx,ebp
	sar	edx,6
	fld	qword ptr 4[ebx]			; fldd
	mov	dword ptr [edi],edx
	fisubr	dword ptr [edi]			; fisubrl
	jmp	asm_generic_flonum_result
	align 2
	public _asm_generic_multiply
_asm_generic_multiply:
	pop	edx
	pop	ebx
	mov	eax,edx
	mov	ecx,ebx
	shr	eax,26
	shr	ecx,26
	cmp	al,26
	je	asm_generic_multiply_fix
	cmp	al,6
	jne	asm_generic_multiply_fail
	cmp	cl,6
	je	asm_generic_multiply_flo_flo
	cmp	cl,26
	jne	asm_generic_multiply_fail
	shl	ebx,6
	and	edx,ebp
	sar	ebx,6
	fld	qword ptr 4[edx]			; fldd
	mov	dword ptr [edi],ebx
	fimul	dword ptr [edi]				; fisubl
	jmp	asm_generic_flonum_result
asm_generic_multiply_fix:
	cmp	cl,6
	je	asm_generic_multiply_fix_flo
	cmp	cl,26
	jne	asm_generic_multiply_fail
	mov	eax,edx
	mov	ecx,ebx
	shl	eax,6
	shl	ecx,6
	imul	eax,ecx		; subl
	jno	asm_generic_fixnum_result
asm_generic_multiply_fail:
	push	ebx
	push	edx
	mov	al,029H
	jmp	scheme_to_interface
asm_generic_multiply_flo_flo:
	and	edx,ebp
	and	ebx,ebp
	fld	qword ptr 4[edx]			; fldd
	fmul	qword ptr 4[ebx]			; fsubl
	jmp	asm_generic_flonum_result	
asm_generic_multiply_fix_flo:
	shl	edx,6
	and	ebx,ebp
	sar	edx,6
	fld	qword ptr 4[ebx]			; fldd
	mov	dword ptr [edi],edx
	fimul	dword ptr [edi]			; fisubrl
	jmp	asm_generic_flonum_result
; Divide needs to check for 0, so we cant really use the following
; define_binary_operation(divide,23,NONE,fidivr,fidiv,fdiv)
; define_binary_predicate(name,index,fix*fix,fix*flo,flo*fix,flo*flo)
	align 2
	public _asm_generic_equal
_asm_generic_equal:
	pop	edx
	pop	ebx
	mov	eax,edx
	mov	ecx,ebx
	shr	eax,26
	shr	ecx,26
	cmp	al,26
	je	asm_generic_equal_fix
	cmp	al,6
	jne	asm_generic_equal_fail
	cmp	cl,6
	je	asm_generic_equal_flo_flo
	cmp	cl,26
	jne	asm_generic_equal_fail
	shl	ebx,6
	and	edx,ebp
	sar	ebx,6
	fld	qword ptr 4[edx]			; fldd
	mov	dword ptr [edi],ebx
	ficomp	dword ptr [edi]
	fstsw	ax
	sahf
	je	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_equal_fix:
	cmp	cl,6
	je	asm_generic_equal_fix_flo
	cmp	cl,26
	jne	asm_generic_equal_fail
	shl	edx,6
	shl	ebx,6
	cmp	edx,ebx
	je	asm_generic_return_sharp_t	
	jmp	asm_generic_return_sharp_f
asm_generic_equal_flo_flo:
	and	edx,ebp
	and	ebx,ebp
	fld	qword ptr 4[edx]			; fldd
	fcomp	qword ptr 4[ebx]
	fstsw	ax
	sahf
	je	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_equal_fix_flo:
	shl	edx,6
	and	ebx,ebp
	sar	edx,6
	mov	dword ptr [edi],edx
	fild	dword ptr [edi]
	fcomp	qword ptr 4[ebx]
	fstsw	ax
	sahf
	je	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_equal_fail:
	push	ebx
	push	edx
	mov	al,024H
	jmp	scheme_to_interface
	align 2
	public _asm_generic_greater
_asm_generic_greater:
	pop	edx
	pop	ebx
	mov	eax,edx
	mov	ecx,ebx
	shr	eax,26
	shr	ecx,26
	cmp	al,26
	je	asm_generic_greater_fix
	cmp	al,6
	jne	asm_generic_greater_fail
	cmp	cl,6
	je	asm_generic_greater_flo_flo
	cmp	cl,26
	jne	asm_generic_greater_fail
	shl	ebx,6
	and	edx,ebp
	sar	ebx,6
	fld	qword ptr 4[edx]			; fldd
	mov	dword ptr [edi],ebx
	ficomp	dword ptr [edi]
	fstsw	ax
	sahf
	ja	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_greater_fix:
	cmp	cl,6
	je	asm_generic_greater_fix_flo
	cmp	cl,26
	jne	asm_generic_greater_fail
	shl	edx,6
	shl	ebx,6
	cmp	edx,ebx
	jg	asm_generic_return_sharp_t	
	jmp	asm_generic_return_sharp_f
asm_generic_greater_flo_flo:
	and	edx,ebp
	and	ebx,ebp
	fld	qword ptr 4[edx]			; fldd
	fcomp	qword ptr 4[ebx]
	fstsw	ax
	sahf
	ja	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_greater_fix_flo:
	shl	edx,6
	and	ebx,ebp
	sar	edx,6
	mov	dword ptr [edi],edx
	fild	dword ptr [edi]
	fcomp	qword ptr 4[ebx]
	fstsw	ax
	sahf
	ja	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_greater_fail:
	push	ebx
	push	edx
	mov	al,025H
	jmp	scheme_to_interface
	align 2
	public _asm_generic_less
_asm_generic_less:
	pop	edx
	pop	ebx
	mov	eax,edx
	mov	ecx,ebx
	shr	eax,26
	shr	ecx,26
	cmp	al,26
	je	asm_generic_less_fix
	cmp	al,6
	jne	asm_generic_less_fail
	cmp	cl,6
	je	asm_generic_less_flo_flo
	cmp	cl,26
	jne	asm_generic_less_fail
	shl	ebx,6
	and	edx,ebp
	sar	ebx,6
	fld	qword ptr 4[edx]			; fldd
	mov	dword ptr [edi],ebx
	ficomp	dword ptr [edi]
	fstsw	ax
	sahf
	jb	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_less_fix:
	cmp	cl,6
	je	asm_generic_less_fix_flo
	cmp	cl,26
	jne	asm_generic_less_fail
	shl	edx,6
	shl	ebx,6
	cmp	edx,ebx
	jl	asm_generic_return_sharp_t	
	jmp	asm_generic_return_sharp_f
asm_generic_less_flo_flo:
	and	edx,ebp
	and	ebx,ebp
	fld	qword ptr 4[edx]			; fldd
	fcomp	qword ptr 4[ebx]
	fstsw	ax
	sahf
	jb	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_less_fix_flo:
	shl	edx,6
	and	ebx,ebp
	sar	edx,6
	mov	dword ptr [edi],edx
	fild	dword ptr [edi]
	fcomp	qword ptr 4[ebx]
	fstsw	ax
	sahf
	jb	asm_generic_return_sharp_t
	jmp	asm_generic_return_sharp_f
asm_generic_less_fail:
	push	ebx
	push	edx
	mov	al,027H
	jmp	scheme_to_interface
; These don't currently differ according to whether there
; is a 387 or not.
	public _asm_generic_quotient
_asm_generic_quotient:
	mov	al,037H
	jmp	scheme_to_interface
	public _asm_generic_remainder
_asm_generic_remainder:
	mov	al,038H
	jmp	scheme_to_interface
	public _asm_generic_modulo
_asm_generic_modulo:
	mov	al,039H
	jmp	scheme_to_interface
	public _asm_nofp_decrement
_asm_nofp_decrement:
	mov	al,022H
	jmp	scheme_to_interface
	public _asm_nofp_divide
_asm_nofp_divide:
	mov	al,023H
	jmp	scheme_to_interface
	public _asm_nofp_equal
_asm_nofp_equal:
	mov	al,024H
	jmp	scheme_to_interface
	public _asm_nofp_greater
_asm_nofp_greater:
	mov	al,025H
	jmp	scheme_to_interface
	public _asm_nofp_increment
_asm_nofp_increment:
	mov	al,026H
	jmp	scheme_to_interface
	public _asm_nofp_less
_asm_nofp_less:
	mov	al,027H
	jmp	scheme_to_interface
	public _asm_nofp_subtract
_asm_nofp_subtract:
	mov	al,028H
	jmp	scheme_to_interface
	public _asm_nofp_multiply
_asm_nofp_multiply:
	mov	al,029H
	jmp	scheme_to_interface
	public _asm_nofp_negative
_asm_nofp_negative:
	mov	al,02aH
	jmp	scheme_to_interface
	public _asm_nofp_add
_asm_nofp_add:
	mov	al,02bH
	jmp	scheme_to_interface
	public _asm_nofp_positive
_asm_nofp_positive:
	mov	al,02cH
	jmp	scheme_to_interface
	public _asm_nofp_zero
_asm_nofp_zero:
	mov	al,02dH
	jmp	scheme_to_interface
	public _asm_nofp_quotient
_asm_nofp_quotient:
	mov	al,037H
	jmp	scheme_to_interface
	public _asm_nofp_remainder
_asm_nofp_remainder:
	mov	al,038H
	jmp	scheme_to_interface
	public _asm_nofp_modulo
_asm_nofp_modulo:
	mov	al,039H
	jmp	scheme_to_interface
end
;;; Edwin Variables:
;;; comment-column: 56
;;; End:
