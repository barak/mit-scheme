;;; -*-Midas-*-
;;;
;;; $Id: ntasutl.asm,v 1.6 1999/01/02 06:11:34 cph Exp $
;;;
;;; Copyright (c) 1992-1999 Massachusetts Institute of Technology
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
.model flat
	.code

	public	_getCS
_getCS:
	xor	eax,eax			; clear eax
	mov	ax,cs			; copy code segment descriptor
	ret

	public	_getDS
_getDS:
	xor	eax,eax			; clear eax
	mov	ax,ds			; copy code segment descriptor
	ret

	public	_getSS
_getSS:
	xor	eax,eax			; clear eax
	mov	ax,ss			; copy code segment descriptor
	ret
end
