;;; -*-Midas-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dosxcutl.asm,v 1.1.1.1 1992/07/28 14:28:15 jinx Exp $
;;;
;;;	Copyright (c) 1992 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

.386
.model tiny
	.code

	public _DPMI_GP_exception_method
_DPMI_GP_exception_method:
	cmp	40[esp],080000000h
	je	DPMI_exception_method_merge
	lea	esp,32[esp]		; pop args
;	jmpf	-32[esp]		; invoke previous handler
	db	0ffh
	db	06ch
	db	024h
	db	0e0h

DPMI_exception_method_merge:
	lea	esp,8[esp]		; pop previous handler
	;; fall through

;;	frame on entry to DPMI_exception_method
;;	
;;28	trapped SS
;;24	trapped	ESP
;;20	trapped EFLAGS
;;16	trapped CS
;;12	trapped EIP
;;8	TRAP error code
;;4	DPMI return hook CS
;;0	DPMI return hook EIP
;; <Above this is a standard DPMI exception frame>
;;20	TRAP number
;;16	C handler DS
;;12	C handler CS	
;;8	C handler EIP
;;4	trap handling SS
;;0	trap handling ESP
;; <old ebp goes here>
;;
;; This code assumes that the trapped ESP is valid.
;; It will push from it.
;; Thus this code cannot be used for a stack fault exception.

	public _DPMI_exception_method
_DPMI_exception_method:
	push	ebp
	mov	ebp,esp
	push	es
	push	eax
	push	ecx
	push	edx
	push	ebx

	mov	eax,4[ebp+4]		; trap frame SS
	mov	ecx,0[ebp+4]		; trap frame ESP
	xor	ebx,ebx
	mov	ebx,28[ebp+28]		; trapped SS
	mov	edx,24[ebp+28]		; trapped ESP
	cmp	ecx,0
	jne	DPMI_set_up_trap_frame
	mov	ecx,edx			; Use the trapped stack
	mov	eax,ebx			; to build the trap frame

DPMI_set_up_trap_frame:
	push	eax
	pop	es

	sub	ecx,4			; push trapped SS
	mov	es:[ecx],ebx
	
	sub	ecx,4			; push trapped ESP
	mov	es:[ecx],edx
	
	sub	ecx,4			; push trapped EFLAGS
	mov	eax,20[ebp+28]
	mov	es:[ecx],eax

	sub	ecx,4			; push trapped CS
	xor	eax,eax
	mov	ax,16[ebp+28]
	mov	es:[ecx],eax

	sub	ecx,4			; push trapped EIP
	mov	eax,12[ebp+28]
	mov	es:[ecx],eax

	sub	ecx,4			; push trap code
	mov	eax,8[ebp+28]
	mov	es:[ecx],eax

	sub	ecx,4			; push trap number
	mov	eax,20[ebp+4]
	mov	es:[ecx],eax

	sub	ecx,4			; push funcptr DS
	mov	eax,16[ebp+4]
	mov	es:[ecx],eax

	sub	ecx,4			; push funcptr CS
	mov	eax,12[ebp+4]
	mov	es:[ecx],eax

	sub	ecx,4			; push funcptr EIP
	mov	eax,8[ebp+4]
	mov	es:[ecx],eax

	mov	28[ebp+28],es		; store hook SS
	mov	24[ebp+28],ecx		; store hook ESP

	mov	16[ebp+28],cs		; replace trapped CS
	jmp	DPMI_obtain_hook_pc

DPMI_after_obtain_hook_pc:
	pop	eax			; PC of obtain_pc
	mov	12[ebp+28],eax		; replace trapped EIP

	pop	ebx
	pop	edx
	pop	ecx
	pop	eax
	pop	es
	pop	ebp
        lea     esp,24[esp]		; pop args

;       The assembler does not assemble the following instruction correctly.
;	ret	far			; resume thread
        db      0cbh

;;	Kludge to obtain the offset of DPMI_exception_method_hook
	
DPMI_obtain_hook_pc:
	call	DPMI_after_obtain_hook_pc

;;	Intercepted trap frame:
;;	
;;36	trapped SS
;;32	trapped ESP	<Typically a pointer to offset 40>
;;28	trapped EFLAGS
;;24	trapped CS
;;20	trapped EIP
;;16	trap code
;;12	trap number
;;8	C function DS
;;4	C function CS
;;0	C function EIP

	public DPMI_exception_method_hook
DPMI_exception_method_hook:
	push	ebp			; preserve trapped ebp
	mov	ebp,esp
	push	gs			; -4
	push	fs			; -8
	push	es			; -12
	push	ds			; -16

;; The following code is shared by the exception handlers under DPMI
;; and X32.

	public	common_exception_method_merge
common_exception_method_merge:
	push	36[ebp+4]		; -20 trapped ss
	push	24[ebp+4]		; -24 trapped cs
	push	28[ebp+4]		; -28 trapped eflags
	push	20[ebp+4]		; -32 trapped eip
	push	edi			; -36
	push	esi			; -40
	push	[ebp]			; -44 trapped ebp
	push	32[ebp+4]		; -48 trapped esp
	push	ebx			; -52
	push	edx			; -56
	push	ecx			; -60
	push	eax			; -64
	push	esp			; sigcontext ptr
	push	16[ebp+4]		; trap code
	push	12[ebp+4]		; trap number

	mov	ds,8[ebp+4]		; DS of handler
	mov	edx,4[ebp+4]		; CS of handler
	mov	eax,0[ebp+4]		; EIP of handler
	cmp	edx,0			; test CS of handler
	jne	common_use_far_call
	call	eax			; Invoke handler
	jmp	common_continue_after_exception

common_after_continuation_setup:
;;	Build far RET frame on stack

	push	edx			; CS of handler
	push	eax			; EIP of handler

;	ret	far			; Invoke handler
        db      0cbh

common_use_far_call:
	push	cs			; Simulate a far call
	call	common_after_continuation_setup

common_continue_after_exception:
;;
;;	If the handler returns, update machine state and `return' to
;;	the trapped code.
;;
	add	esp,12			; pop args to C handler

	mov	eax,-48[ebp]		; update esp
	mov	32[ebp+4],eax
	mov	eax,-44[ebp]		; update ebp
	mov	[ebp],eax
	mov	eax,-32[ebp]		; update eip
	mov	20[ebp+4],eax
	mov	eax,-28[ebp]		; update eflags
	mov	28[ebp+4],eax
	mov	eax,-24[ebp]		; update cs
	mov	24[ebp+4],eax
	mov	eax,-20[ebp]		; update ss
	mov	36[ebp+4],eax

	pop	eax
	pop	ecx
	pop	edx
	pop	ebx
	add	esp,8			; ignore esp and ebp
	pop	esi
	pop	edi
	add	esp,16			; ignore eip, eflags, cs, ss
	pop	ds
	pop	es
	pop	fs
	pop	gs

;;	If this were part of the OS, the following instructions would
;;	do what we want, assuming that we were running at a higher
;;	privilege level than the interrupted task.  We need a
;;	`return-to-outer-level' IRETD that restores ESP and SS in
;;	addition to EIP, CS, and EFLAGS.
;;
;;	However, the architecture does not allow us to specify that
;;	explicitly, and in all likelihood an IRETD will be taken to
;;	mean a `return-to-samel-level' IRETD, which will not pop and
;;	update SS and ESP!
;;
;;	pop	ebp
;;	lea	esp,20[esp]		; bump past trap info
;;	iretd				; I wish
;;	
;;	The only way to correctly emulate it is to construct a piece
;;	of code that contains an explicit far jump to the return
;;	CS:EIP after loading EFLAGS, SS, and ESP from the stack.
;;	Unfortunately we can't conveniently create such a thunk here,
;;	since we don't have a pair of selectors representing a code
;;	segment and a writable data segment with the same base and
;;	limit.
;;
;;	Instead what this code will do is check whether the stack would
;;	not change (same SS and offset to immediately above the frame).
;;	If so, after moving the data around, we'll just do a far return.
;;
;;	Otherwise, we will build a far return frame on the target stack,
;;	switch stacks, and do a far return.
;;
;;	This will only work if the target stack is reasonable (and is
;;	big enough for a few words).  This is particularly not true in
;;	the case of a stack fault, but we would expect the returning
;;	handler to have changed the stack to a valid one in that case
;;	-- not a valid assumption.
;;
;;	In addition, the stack comparison assumes that different selectors
;;	mean different stacks, which is also not a valid assumption.
;;	particularly since 32-bit programs often have different SS and DS
;;	selectors mapping over the same linear range.
;;	The code also assumes that even if the selectors are the same,
;;	the target range is either identical to the default,
;;	or non-overlapping.

	push	eax			; -4
	mov	ax,ss
	cmp	ax,36[ebp+4]
	jne	common_different_stacks
	lea	eax,40[ebp+4]
	cmp	eax,32[ebp+4]
	jne	common_different_stacks

;; 	Easy case:  The target stack is what we would return to trivially.
;;	Overwrite SS and ESP with CS and EIP, restore flags, and do a far
;;	return.

	mov	eax,24[ebp+4]		; Move CS
	mov	36[ebp+4],eax
	mov	eax,20[ebp+4]		; Move EIP
	mov	32[ebp+4],eax
	pop	eax
	pop	ebp
	lea	esp,28[ebp]		; Pop trap info and old CS and EIP
	popfd				; Restore eflags
;
;       The assembler does not assemble the following instruction correctly.
;	ret	far			; resume thread
        db      0cbh
	
common_different_stacks:
	push	edx			; -8  Scratch regs
	push	ds			; -12 These two must be contiguous
	push	ecx			; -16  for LDS instruction below!
	mov	ds,36[ebp+4]		; target stack SS
	mov	ecx,32[ebp+4]		; target stack ESP

	sub	ecx,4			; push target CS
	mov	eax,24[ebp+4]
	mov	[ecx],eax

	sub	ecx,4			; push target EIP
	mov	eax,20[ebp+4]
	mov	[ecx],eax

	sub	ecx,4			; push target EFLAGS
	mov	eax,28[ebp+4]
	mov	[ecx],eax
;;
;;	Switch stacks
;;
	mov	dx,ss			; Preserve current stack
	mov	ax,ds
	mov	ss,ax			; This instruction locks
	mov	esp,ecx			;  interrupts around this one!
	mov	ds,dx
	mov	ecx,ebp

	mov	ebp,[ecx]		; Restore regs
	mov	eax,-4[ecx]
	mov	edx,-8[ecx]
	lds	ecx,-16[ecx]
	popfd

;	ret	far			; resume thread
        db      0cbh

;;	Locked data for X32.
;;	It includes all the data and code accessed during a hardware
;;	interrupt or an exception before X32 is reset, i.e. while
;;	it cannot process a page fault.

	.data
	public _X32_locked_data_start
_X32_locked_data_start dd 0

X32_excp_buffer			db 64 dup(0)
	public	_X32_excp_handlers
_X32_excp_handlers 		db 32*20 dup (0)

	public _X32_ds_val
_X32_ds_val	dd 06765h
	public _X32_timer_interrupt_previous
_X32_timer_interrupt_previous	dd 0
				dd 0
				dd 0

	public _scm_itimer_counter
_scm_itimer_counter dd 0
	public _scm_itimer_reload
_scm_itimer_reload dd 0

	public _IntCode
_IntCode dd 0
	public _IntEnb
_IntEnb dd 0
	public _MemTop
_MemTop dd 0

	public _Regstart
_Regstart db 128 dup (0)
	public _Registers
_Registers dd 0

	public _X32_locked_data_end
_X32_locked_data_end db 3452 dup (0)
	.code

;;	Exception handlers for X32 and X32V.
;;	This code is not reentrant.
;;	The same exception within this code will really confused the world.
;;	It should be rewritten in the future to be reentrant.

;;	frame on entry to _X32_exception_method (sp points to 0)
;;
;;  12	pointer to interrupt structure
;;   8	eflags at interrupt
;;   4	CS for IRETD
;;   0	EIP for IRETD
;;  -4	old DS
;;  -8	old EAX	
;; -12	old ECX
;; -16	old EDX

X32FRAME equ 16

;; The pointer to the interrupt structure points to offset 0 of
;; a block on SS whose layout is

;;  32	dword SS
;;  28	dword ESP
;;  24	dword EFLAGS
;;  20	dword CS
;;  16	dword EIP
;;  14	mode 0 for int. in real mode, 1 for int. in prot. mode, 2 for excp.
;;  12	INT# 0 - 256
;;  10	word GS
;;   8	word FS
;;   6	word ES
;;   4	word DS
;;   0	dword EAX
;;  -4	trap error code

	public	_X32_locked_code_start
_X32_locked_code_start:

	public	_X32_exception_method
_X32_exception_method:
	push	ds			; Preserve registers
	push	eax
	push	ecx
	push	edx

	mov	ecx,12[esp+X32FRAME]	; Pointer to structure
	mov	ds,_X32_ds_val		; Temporary buffer

	mov	ax,2501h		; Restore X32's internal state
	int	21h
        mov     ax,4c00h                        ;terminate program
        int     21h

	lea	edx,X32_excp_buffer

	xor	eax,eax
	mov	ax,ss:32[ecx]		; SS at time of trap
	mov	0[edx],eax

	mov	eax,ss:28[ecx]		; ESP at time of trap
	mov	4[edx],eax

	mov	eax,ss:24[ecx]		; EFLAGS at time of trap
	mov	8[edx],eax

	xor	eax,eax
	mov	ax,ss:20[ecx]		; CS at time of trap
	mov	12[edx],eax

	mov	eax,ss:16[ecx]		; EIP at time of trap
	mov	16[edx],eax

	mov	eax,ss:-4[ecx]		; Trap code
	mov	20[edx],eax

	xor	eax,eax			; Trapped gs
	mov	ax,ss:10[ecx]
	mov	24[edx],eax
	
	xor	eax,eax			; Trapped fs
	mov	ax,ss:8[ecx]
	mov	28[edx],eax

	xor	eax,eax			; Trapped es
	mov	ax,ss:6[ecx]
	mov	32[edx],eax

	xor	eax,eax			; Trapped ds
	mov	ax,ss:4[ecx]
	mov	36[edx],eax

	mov	eax,ss:[ecx]		; Trapped eax
	mov	40[edx],eax

	mov	eax,[esp]		; Trapped edx
	mov	44[edx],eax

	mov	eax,4[esp]		; Trapped ecx
	mov	48[edx],eax

	xor	eax,eax
	mov	ax,ss:12[ecx]		; Trap number
	mov	52[edx],eax

	mov	ecx,eax			; Multiply by 20
	shl	eax,2
	add	eax,ecx
	shl	eax,2

	lea	ecx,dword ptr _X32_excp_handlers
	add	ecx,eax			; handler info for this excp.

	mov	eax,cs:[ecx]		; handler ESP
	cmp	eax,0			; Use trapped stack?
	jne	X32_set_up_trap_sp

	lss	esp,fword ptr [edx]	; Restore trapped stack
	jmp	X32_set_up_trap_sp_merge

X32_set_up_trap_sp:
	lss	esp,fword ptr cs:[ecx]	; Use stack specified by handler

X32_set_up_trap_sp_merge:	

	mov	ax,2501h		; Restore X32's internal state
	int	21h
	jmp	X32_set_up_trap_stack

;;	Note: X32_set_up_stack does not need to be locked in memory
;;	because we get to it after resetting X32 (i.e. scheme is
;;	executing as a normal program again), so it should be able to
;;	page it in if necessary.

;;	X32 timer interrupt.
;;	Must be locked in memory (and all the data it accesses.

INT_Timer equ 64
REGBLOCK_MEMTOP equ 0

	public	_X32_timer_interrupt
_X32_timer_interrupt:
	push	ds
	mov	ds,cs:_X32_ds_val
	cmp	dword ptr _scm_itimer_reload,0
	je	x32_timer_return
	dec	dword ptr _scm_itimer_counter
	cmp	dword ptr _scm_itimer_counter,0
	jne	x32_timer_return
	push	eax
	mov	eax,dword ptr _scm_itimer_reload
	mov	dword ptr _scm_itimer_counter,eax
	or	_IntCode,INT_Timer
	mov	eax,_IntCode
	and	eax,_IntEnb
	cmp	eax,0
	je	x32_timer_continue
	mov	dword ptr _Registers[REGBLOCK_MEMTOP],-1

x32_timer_continue:
	pop	eax

x32_timer_return:
	pop	ds
	jmp	fword ptr cs:_X32_timer_interrupt_previous

	public	_X32_locked_code_end
_X32_locked_code_end:


X32_set_up_trap_stack:
	push	0[edx]			; Trapped SS
	push	4[edx]			; Trapped ESP
	push	8[edx]			; Trapped EFLAGS
	push	12[edx]			; Trapped CS
	push	16[edx]			; Trapped EIP
	push	20[edx]			; Trap code
	push	52[edx]			; Trap number
	push	16[ecx]			; C handler DS
	push	12[ecx]			; C handler CS
	push	8[ecx]			; C handler EIP
	push	ebp			; Trapped EBP
	mov	ebp,esp
	push	24[edx]			; Trapped GS
	push	28[edx]			; Trapped FS
	push	32[edx]			; Trapped ES
	push	36[edx]			; Trapped DS
	mov	eax,40[edx]		; Restore trapped EAX
	mov	ecx,48[edx]		; Restore trapped ECX
	mov	edx,44[edx]		; Restore trapped EDX
	jmp	common_exception_method_merge

	public _X32_asm_initialize
_X32_asm_initialize:
	mov	_X32_ds_val,ds
	ret

end
