;;; -*-Midas-*-
;;;
;;; $Id: dosasutl.asm,v 1.4 1999/01/02 06:11:34 cph Exp $
;;;
;;; Copyright (c) 1992, 1999 Massachusetts Institute of Technology
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
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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
