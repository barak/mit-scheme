;;; -*-Midas-*-
;;;
;;;	$Id: doskbutl.asm,v 1.4 1992/09/03 07:30:20 jinx Exp $
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

;;	Stack on entry to _DOSX_scheme_system_isr
;;
;;32	IRETD EFLAGS
;;28	IRETD CS
;;24	IRETD EIP
;;20	CS for next handler in chain
;;16	EIP for next handler in chain
;;12    address of modifier mask
;;8	offset for unshifted table
;;4	offset for shifted table
;;0	DS for scan_code to ascii tables

	extrn	scheme_system_isr:near
	public	_DOSX_scheme_system_isr
	public	_DPMI_PM_scheme_system_isr

_DOSX_scheme_system_isr:
_DPMI_PM_scheme_system_isr:
	pushfd
	call	scheme_system_isr
	jnc	DOSX_scheme_system_dismiss

;; Chain to next handler (flags unmodified)
	popfd
	lea	esp,16[esp]
;	ret	far
	db	0cbh

;; Dismiss/finish interrupt (update flags)

DOSX_scheme_system_dismiss:
	push	eax
	mov	eax,4[esp]		;updated flags
	mov	40[esp],eax		;flags to restore
	pop	eax
	popfd
	lea	esp,24[esp]
	iretd		

;;	Stack on entry to _DPMI_scheme_system_isr
;;
;;20	CS for next (real mode) handler in chain
;;16	IP for next (real mode) handler in chain
;;12    address of modifier mask
;;8	offset for unshifted table
;;4	offset for shifted table
;;0	DS for scan_code to ascii tables

	public _DPMI_RM_scheme_system_isr
_DPMI_RM_scheme_system_isr:
	mov	eax,es:28[edi]		; real mode eax
	xor	edx,edx
	mov	dx,es:32[edi]		; real mode flags
	push	edx
	call	scheme_system_isr
	jnc	DPMI_scheme_system_dismiss

;; Chain to next real mode handler (flags unmodified)
	lea	esp,4[esp]		; drop flags
	mov	eax,16[esp]		; real mode IP (padded to dword)
	mov	es:42[edi],ax
	mov	eax,20[esp]		; real mode CS (padded to dword)
	mov	es:44[edi],ax
	lea	esp,24[esp]		; pop args
	iret				; tell DPMI we're done

;; Dismiss/finish interrupt in real mode (update flags, simulate RM iret)

DPMI_scheme_system_dismiss:
	pop	eax			; updated flags
	mov	es:32[edi],ax
	mov	ax,ds:[esi]		; real mode IRET ip
	mov	es:42[edi],ax
	mov	ax,ds:2[esi]		; real mode IRET cs
	mov	es:44[edi],ax
	add	word ptr es:46[edi],6	; bump real mode sp
	lea	esp,24[esp]		; pop args
	iret				; tell DPMI we're done	

;; These macros taken from x32's mac32.asm

;Macro for start of a real mode code segment
start16code     macro
__X386_CODESEG_16       segment para use16 public 'CODE'
assume cs:__X386_CODESEG_16,ds:nothing,es:nothing,fs:nothing,gs:nothing,ss:nothing
endm

;Macro for end of real mode code segment
end16code       macro
__X386_CODESEG_16       ends
endm

start16code
	public	_RM_keyboard_pattern_start
_RM_keyboard_pattern_start:

modifier_mask:
	db 2 dup (0)
shifted_table_offset:
	db 2 dup (0)
unshifted_table_offset:
	db 2 dup (0)

chain:
	popf
	db	0eah			; jmpf	next_in_chain
	db 4 dup (0)
			
kbd_isr:
	pushf
	cmp	ah,4fh
	jne	chain
	cmp	al,39h
	ja	chain
	push	bx			; Preserve bx
	push	ax			; Preserve scan code
	mov	ah,2h
	int	16h			; Get modifier bits
	and	al,byte ptr cs:modifier_mask
	pop	bx			; Get scan code
	push	bx
	and	bx,3fh			; Drop fncn
	cmp	al,8h			; Only meta bit set?
	je	do_unshifted
	cmp	al,0			; No modifier bits set?
	je	do_unshifted
do_shifted:
	push	si
	mov	si,word ptr cs:shifted_table_offset
	mov	bl,byte ptr cs:[bx+si]
	pop	si
	jmp	merge
do_unshifted:
	push	si
	mov	si,word ptr cs:unshifted_table_offset
	mov	bl,byte ptr cs:[bx+si]
	pop	si
merge:
	cmp	bl,0			; No translation?
	je	abort_translation
	; bt	al,2h			; Control set?
	db	0fh,0bah,0e0h,2		; bt	al,2h
	jnc	after_ctrl
	and	bl,09fh 		; controlify
after_ctrl:
	; bt	al,3h			; Alt set?
	db	0fh,0bah,0e0h,3
	jnc	after_meta
	or	bl,080h 		; metify
after_meta:
	cmp	bl,0f0h 		; M-p ?
	je	abort_translation
	pop	ax
	push	cx			; Preserve cx
	push	ax
	mov	ch,al			; Scan code
	cmp	bl,0			; C-Space?
	jne	after_ctrl_space
	mov	ch,3			; Fudge scan code
after_ctrl_space:
	mov	cl,bl			; ASCII value
	mov	ah,05h			; fcn. number
	int	16h			; Record keystroke
	pop	ax			; Restore registers
	pop	cx
	pop	bx
	push	bp
	mov	bp,sp
	and	8[bp],0feh  		; clc iret's flags
	pop	bp
	popf
	clc
	iret

abort_translation:
	pop	ax
	pop	bx
	jmp	chain
	
	public	_RM_keyboard_pattern_end
_RM_keyboard_pattern_end:
	nop

end16code

end
