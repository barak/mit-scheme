#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/insutl.scm,v 1.1 1987/08/13 01:13:58 jinx Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; VAX utility procedures

(declare (usual-integrations))

;;;; Effective Addressing

;;; NOTE: If this format changes, inerly.scm may also need to be changed!

(define ea-tag "Effective-Address")

(define (make-effective-address keyword categories value)
  (vector ea-tag keyword categories value))

(define (effective-address? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? (vector-ref object 0) ea-tag))

(define-integrable (ea-keyword ea)
  (vector-ref ea 1))

(define-integrable (ea-categories ea)
  (vector-ref ea 2))

(define-integrable (ea-value ea)
  (vector-ref ea 3))

;;;; Addressing modes

;; Missing: index and immediate modes.

(define ea-database
  (make-ea-database
   ((S (? value))
    (R)
    (BYTE (6 value)
	  (2 0)))

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

   ((@@RO L (? n) (? off))
    (R M W A V I)
    (BYTE (4 n)
	  (4 15))
    (BYTE (32 off SIGNED)))

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
    (BYTE (32 off SIGNED)))))

;;;; Effective address processing

;; Handling of index and immediate modes
;; Index mode:
;;   (X (? n) (? base ea))
;;   base is prefixed by (BYTE (4 n) (4 4)).
;; Immediate mode:
;;   (& (? value))
;;   The operand size dependent value is preceeded by
;;   (BYTE (4 15) (4 8)) 

(define (process-ea expression type)
  (define (wrap keyword cats reg mode value)
    (make-effective-address
     keyword
     cats
     (cons-syntax
      (syntax-evaluation reg coerce-4-bit-unsigned)
      (cons-syntax (syntax-evaluation mode coerce-4-bit-unsigned)
		   value))))

  (define (kernel expression)
    (let ((match (pattern-lookup ea-database expression)))
      (cond (match (match))
	    ((and (pair? expression) (eq? (car expression) '&))
	     (wrap '& '(R A V I)	; M and W unpredictable
		   15 8
		   (coerce-to-type (cadr expression) type)))
	    (else #F))))
	  
  (cond ((not (pair? expression)) #F)
	((eq? (car expression) 'X)
	 (let ((base (kernel (caddr expression))))
	   (and base
		(memq 'I (ea-categories base))
		(wrap 'X '(R M W A V)
		      (cadr expression) 4
		      (ea-value result)))))
	(else (kernel expression))))

(define (coerce-to-type expression type)
  (syntax-evaluation
   expression
   (case type
     ((b) coerce-8-bit-signed)
     ((w) coerce-16-bit-signed)
     ((l) coerce-32-bit-signed)
     ((d f g h l o q)
      (error "coerce-to-type: Unimplemented type" type))
     (else (error "coerce-to-type: Unknown type" type)))))

;;;; Transformers

(define-symbol-transformer cc
  (NEQ . #x2) (NEQU . #x2) (EQL . #x3) (EQLU . #x3)
  (GTR . #x4) (LEQ . #x5) (GEQ . #x8) (LSS . #x9) (GTRU . #xA) (LEQU . #xB)
  (VC . #xC) (VS . #xD) (GEQU . #xE) (CC . #xE) (LSSU . #xF) (CS . #xF))

(define-transformer displacement
  (lambda (expression)
    (and (pair? expression)
	 (or (eq? (car expression) '@PCR)
	     (eq? (car expression) '@PCO))
	 expression)))

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
