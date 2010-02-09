#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; VAX utility procedures
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Effective Addressing

;;; *** NOTE: If this format changes, inerly.scm must also be changed! ***

(define ea-tag
  "Effective-Address")

(define (make-effective-address keyword categories value)
  (vector ea-tag keyword categories value))

(define (effective-address? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? (vector-ref object 0) ea-tag)))

(define-integrable (ea-keyword ea)
  (vector-ref ea 1))

(define-integrable (ea-categories ea)
  (vector-ref ea 2))

(define-integrable (ea-value ea)
  (vector-ref ea 3))

;; For completeness

(define (ea-keyword-early ea)
  (vector-ref ea 1))

(define (ea-categories-early ea)
  (vector-ref ea 2))

(define (ea-value-early ea)
  (vector-ref ea 3))

;;;; Addressing modes

(define-ea-database
  ((S (? value))
   (R)
   (BYTE (6 value)
	 (2 0)))

  ((X (? n) (? base ea-i-?))
   (R M W V)
   (BYTE (4 n)
	 (4 4))
   (OPERAND ? base))

  ((R (? n))
   (R M W V)
   (BYTE (4 n)
	 (4 5)))

  ((@R (? n))
   (R M W A V I)
   (BYTE (4 n)
	 (4 6)))

  ((@-R (? n))
   (R M W A V I)
   (BYTE (4 n)
	 (4 7)))

  ((@R+ (? n))
   (R M W A V I)
   (BYTE (4 n)
	 (4 8)))

  ((@@R+ (? n))
   (R M W A V I)
   (BYTE (4 n)
	 (4 9)))

  ((@RO B (? n) (? off))
   (R M W A V I)
   (BYTE (4 n)
	 (4 10))
   (BYTE (8 off SIGNED)))

  ((@@RO B (? n) (? off))
   (R M W A V I)
   (BYTE (4 n)
	 (4 11))
   (BYTE (8 off SIGNED)))

  ((@RO W (? n) (? off))
   (R M W A V I)
   (BYTE (4 n)
	 (4 12))
   (BYTE (16 off SIGNED)))

  ((@@RO W (? n) (? off))
   (R M W A V I)
   (BYTE (4 n)
	 (4 13))
   (BYTE (16 off SIGNED)))

  ((@RO L (? n) (? off))
   (R M W A V I)
   (BYTE (4 n)
	 (4 14))
   (BYTE (32 off SIGNED)))

  ((@RO UL (? n) (? off))		; Kludge
   (R M W A V I)
   (BYTE (4 n)
	 (4 14))
   (BYTE (32 off UNSIGNED)))

  ((@@RO L (? n) (? off))
   (R M W A V I)
   (BYTE (4 n)
	 (4 15))
   (BYTE (32 off SIGNED)))

  ((& (? value))
   (R M W A V I)
   (BYTE (4 15)
	 (4 8))
   (IMMEDIATE value SIGNED))

  ((&U (? value))			; Kludge
   (R M W A V I)
   (BYTE (4 15)
	 (4 8))
   (IMMEDIATE value UNSIGNED))

  ((@& (? value))			; Absolute
   (R M W A V I)
   (BYTE (4 15)
	 (4 9))
   (BYTE (32 value)))

  ((@PCO B (? off))
   (R M W A V I)
   (BYTE (4 15)
	 (4 10))
   (BYTE (8 off SIGNED)))

  ((@@PCO B (? off))
   (R M W A V I)
   (BYTE (4 15)
	 (4 11))
   (BYTE (8 off SIGNED)))

  ((@PCO W (? off))
   (R M W A V I)
   (BYTE (4 15)
	 (4 12))
   (BYTE (16 off SIGNED)))

  ((@@PCO W (? off))
   (R M W A V I)
   (BYTE (4 15)
	 (4 13))
   (BYTE (16 off SIGNED)))

  ((@PCO L (? off))
   (R M W A V I)
   (BYTE (4 15)
	 (4 14))
   (BYTE (32 off SIGNED)))

  ((@@PCO L (? off))
   (R M W A V I)
   (BYTE (4 15)
	 (4 15))
   (BYTE (32 off SIGNED)))

  ;; Self adjusting modes
  ;; The ranges seem wrong, but are correct given that disp
  ;; must be adjusted for the longer modes.  

  ((@PCR (? label))
   (R M W A V I)
   (VARIABLE-WIDTH
    (disp `(- ,label (+ *PC* 2)))
    ((-128 127)				; (@PCO B label)
     (BYTE (4 15)
	   (4 10))
     (BYTE (8 disp SIGNED)))
    ((-32767 32768)			; (@PCO W label)
     (BYTE (4 15)
	   (4 12))
     (BYTE (16 (- disp 1) SIGNED)))
    ((() ())				; (@PCO L label)
     (BYTE (4 15)
	   (4 14))
     (BYTE (32 (- disp 3) SIGNED)))))

  ((@@PCR (? label))
   (R M W A V I)
   (VARIABLE-WIDTH
    (disp `(- ,label (+ *PC* 2)))
    ((-128 127)				; (@@PCO B label)
     (BYTE (4 15)
	   (4 11))
     (BYTE (8 disp SIGNED)))
    ((-32767 32768)			; (@@PCO W label)
     (BYTE (4 15)
	   (4 13))
     (BYTE (16 (- disp 1) SIGNED)))
    ((() ())				; (@@PCO L label)
     (BYTE (4 15)
	   (4 15))
     (BYTE (32 (- disp 3) SIGNED)))))

  ((@PCRO (? label) (? offset))	; Kludge
   (R M W A V I)
   (VARIABLE-WIDTH
    (disp `(+ ,offset (- ,label (+ *PC* 2))))
    ((-128 127)				; (@PCO B label)
     (BYTE (4 15)
	   (4 10))
     (BYTE (8 disp UNSIGNED)))
    ((-32767 32768)			; (@PCO W label)
     (BYTE (4 15)
	   (4 12))
     (BYTE (16 (- disp 1) UNSIGNED)))
    ((() ())				; (@PCO L label)
     (BYTE (4 15)
	   (4 14))
     (BYTE (32 (- disp 3) UNSIGNED))))))

;;;; Effective address processing

(define *immediate-type*)

(define (process-ea expression type)
  (fluid-let ((*immediate-type*
	       (if (eq? '? type) *immediate-type* type)))
    (let ((match (pattern-lookup ea-database expression)))
      (cond (match (match))
	    ;; Guarantee idempotency for early instruction processing.
	    ((effective-address? expression) expression)
	    (else #F)))))

(define (coerce-to-type expression type #!optional unsigned?)
  (let ((unsigned? (and (not (default-object? unsigned?))
			unsigned?)))
    (syntax-evaluation
     expression
     (case type
       ((B) (if unsigned? coerce-8-bit-unsigned coerce-8-bit-signed))
       ((W) (if unsigned? coerce-16-bit-unsigned coerce-16-bit-signed))
       ((L) (if unsigned? coerce-32-bit-unsigned coerce-32-bit-signed))
       ((D F G H L O Q)
	(error "coerce-to-type: Unimplemented type" type))
       (else (error "coerce-to-type: Unknown type" type))))))

;;; Transformers

(define-symbol-transformer cc
  (NEQ . #x2) (NEQU . #x2) (EQL . #x3) (EQLU . #x3)
  (GTR . #x4) (LEQ . #x5) (GEQ . #x8) (LSS . #x9) (GTRU . #xA) (LEQU . #xB)
  (VC . #xC) (VS . #xD) (GEQU . #xE) (CC . #xE) (LSSU . #xF) (CS . #xF))

(define-symbol-transformer inverse-cc
  (NEQ . #x3) (NEQU . #x3) (EQL . #x2) (EQLU . #x2)
  (GTR . #x5) (LEQ . #x4) (GEQ . #x9) (LSS . #x8) (GTRU . #xB) (LEQU . #xA)
  (VC . #xD) (VS . #xC) (GEQU . #xF) (CC . #xF) (LSSU . #xE) (CS . #xE))

(define-transformer displacement
  (lambda (expression)
    (and (pair? expression)
	 (or (eq? (car expression) '@PCR)
	     (eq? (car expression) '@PCO))
	 expression)))

;;;; Effective address transformers

(define-ea-transformer ea-a-b a b)
(define-ea-transformer ea-a-d a d)
(define-ea-transformer ea-a-f a f)
(define-ea-transformer ea-a-g a g)
(define-ea-transformer ea-a-h a h)
(define-ea-transformer ea-a-l a l)
(define-ea-transformer ea-a-o a o)
(define-ea-transformer ea-a-q a q)
(define-ea-transformer ea-a-w a w)
(define-ea-transformer ea-m-b m b)
(define-ea-transformer ea-m-d m d)
(define-ea-transformer ea-m-f m f)
(define-ea-transformer ea-m-g m g)
(define-ea-transformer ea-m-h m h)
(define-ea-transformer ea-m-l m l)
(define-ea-transformer ea-m-w m w)
(define-ea-transformer ea-r-b r b)
(define-ea-transformer ea-r-d r d)
(define-ea-transformer ea-r-f r f)
(define-ea-transformer ea-r-g r g)
(define-ea-transformer ea-r-h r h)
(define-ea-transformer ea-r-l r l)
(define-ea-transformer ea-r-o r o)
(define-ea-transformer ea-r-q r q)
(define-ea-transformer ea-r-w r w)
(define-ea-transformer ea-v-b v b)
(define-ea-transformer ea-w-b w b)
(define-ea-transformer ea-w-d w d)
(define-ea-transformer ea-w-f w f)
(define-ea-transformer ea-w-g w g)
(define-ea-transformer ea-w-h w h)
(define-ea-transformer ea-w-l w l)
(define-ea-transformer ea-w-o w o)
(define-ea-transformer ea-w-q w q)
(define-ea-transformer ea-w-w w w)
(define-ea-transformer ea-i-? i ?)