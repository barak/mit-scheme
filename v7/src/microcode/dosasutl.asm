;;; -*-Midas-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dosasutl.asm,v 1.2 1992/07/28 18:03:24 jinx Exp $
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

	public	_getCS
_getCS:
	xor	eax,eax			; clear eax
	mov	ax,cs			; copy code segment descriptor
	ret

	public	_getSS
_getSS:
	xor	eax,eax			; clear eax
	mov	ax,ss			; copy code segment descriptor
	ret

;;	Frame on entry to farcpy

;;24	size
;;20	src_sel
;;16	src_off
;;12	dst_sel
;;8	dst_off
;;4	ret add
;;0	previous ebp

	public	_farcpy
_farcpy:
	push	ebp
	mov	ebp,esp
	push	ebx
	push	ds
	push	es

	mov	eax,12[ebp]
	mov	ds,ax			; dst sel
	mov	eax,20[ebp]
	mov	es,ax			; src sel
	mov	edx,8[ebp]		; dst off
	mov	ecx,16[ebp]		; src off
	mov	eax,24[ebp]		; count
	jmp	enter_loop

farcpy_loop:
	mov	bl,es:[ecx]
	mov	ds:[edx],bl
	inc	ecx
	inc	edx

enter_loop:
	dec	eax
	jge	farcpy_loop

	pop	es
	pop	ds
	pop	ebx
	pop	ebp
	ret
end
