;;; -*-Midas-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/doskbutl.asm,v 1.2 1992/05/13 16:49:53 jinx Exp $
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
.model small
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

	public _DOSX_scheme_system_isr
	public _DPMI_PM_scheme_system_isr

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

;;	Stack on entry to scheme_system_isr
;;
;;24    address of modifier mask
;;20	offset for unshifted table
;;16	offset for shifted table
;;12	DS for scan_code to ascii tables
;;8	Flags to restore/modify
;;4	EIP for low-level hook (DPMI or DOSX)
;;0	Old ebp [pushed on entry]
;;
;;	Arguments:
;; AL = scan code
;; AH = 4fh
;; CF set
;;
;;	Return:
;; AL = scan code
;; CF clear if scan code should be ignored (interrupt dismissed).

chain_to_next_handler:
	stc				; set the carry flag
	ret

scheme_system_isr:
	cmp	ah,4fh
	jne	chain_to_next_handler
	cmp	al,39h
        ja      chain_to_next_handler

;; process a keystroke

	push	ebp
	mov	ebp,esp
        push    eax             ; Preserve accross interrupt

        mov     ah,2h           ; Get shift bits
        int     16h             ; Return in AL
        
        push    ecx
        push    edx             ; Preserve regs
        push    es

        mov     edx,12[ebp]      ; Segment selector
        push    edx
        pop     es
	
        mov     edx,24[ebp]     ; Modifier mask address
        and     al,es:[edx]     ; Ignore modifiers
        push    eax             ; Save result
        
        mov     ecx,-4[ebp]     ; Scan code + function number
        and     ecx,3fh         ; Only scan code
        mov     edx,20[ebp]     ; Unshifted table offset
        and     eax,47h         ; Shift, ctrl, and CAPS-LOCK mask
        cmp     al,0
        je      index_into_table
        mov     edx,16[ebp]      ; Shifted table offset

index_into_table:
        mov     al,es:[edx] [ecx]  ; Get ASCII value
        pop     edx             ; Masked modifier bits
        cmp     al,0            ; Null entries mean chain
        je      abort_translation

        bt      edx,2           ; Control set?
        jnc     after_control
        and     al,09fh         ; Clear bits 6 and 5

after_control:
        bt      edx,3           ; Alt set?
        jnc     after_meta
        or      al,080h         ; Set bit 8

after_meta:
	cmp	al,0f0h		; M-p ?
	je	abort_translation
        mov     ecx,-4[ebp]     ; Get scan code

	cmp	al,0		; C-Space ?
	jne	after_ctrl_space
	mov	cl,3		; Fudge scan code

after_ctrl_space:
	mov	ch,cl
        mov     cl,al           ; Transfer ASCII value
        
        mov     ah,5h           ; Insert keystroke
        int     16h             ; CH = scan code, CL = ASCII
                                ; returns AL = 0h if win, 1h if buffer full

	and	byte ptr 8[ebp],0feh	; clear interrupt carry flag
        pop     es
        pop     edx
        pop     ecx
	pop	eax
	pop	ebp
	clc				; clear our carry flag
	ret

abort_translation:
        pop     es
        pop     edx
        pop     ecx
	pop	eax
	pop	ebp
	stc				; set carry flag
	ret
end
