;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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

;;;; Spectrum Specific Coercions

(declare (usual-integrations))

(define (parse-word expression tail)
  (expand-descriptors (cdr expression)
    (lambda (instruction size)
      (if (not (zero? (remainder size 32)))
	  (error "PARSE-WORD: Instructions must be 32 bit multiples" size))
      (let ((instruction (apply optimize-group-syntax instruction)))
	(if (null? tail)
	    `(CONS ,instruction '())
	    `(CONS-SYNTAX ,instruction (CONS ,(car tail) '())))))))

(define (expand-descriptors descriptors receiver)
  (if (null? descriptors)
      (receiver '() 0)
      (expand-descriptors (cdr descriptors)
	(lambda (instruction* size*)
	  (expand-descriptor (car descriptors)
	    (lambda (instruction size)
	      (receiver (append! instruction instruction*)
			(+ size size*))))))))

(define (expand-descriptor descriptor receiver)
  (let ((size (car descriptor)))
    (receiver `(,(integer-syntaxer (cadr descriptor)
				   (if (null? (cddr descriptor))
				       'UNSIGNED
				       (caddr descriptor))
				   size))
	      size)))

(define (coerce-right-signed nbits)
  (let ((offset (1+ (expt 2 nbits))))
    (lambda (n)
      (unsigned-integer->bit-string nbits
				    (if (negative? n)
					(+ (* n 2) offset)
					(* n 2))))))

(define coerce-assemble3:x
  (standard-coercion
   (lambda (n)
     (+ (* (land n 3) 2) (quotient n 4)))))

(define coerce-assemble12:X
  (standard-coercion
   (lambda (n)
     (let ((qr (integer-divide n 4)))
       (if (not (zero? (integer-divide-remainder qr)))
	   (error "COERCE-ASSEMBLE12:X: offset not multiple of 4" n))
       (let ((n (integer-divide-quotient qr)))
	 (+ (* (land n #x3FF) 2) (quotient (land n #x400) #x400)))))))

(define coerce-assemble12:Y
  (standard-coercion
   (lambda (n)
     (quotient (land (quotient n 4) #x800) #x800))))

(define coerce-assemble17:X
  (standard-coercion
   (lambda (n)
     (let ((qr (integer-divide n 4)))
       (if (not (zero? (integer-divide-remainder qr)))
	   (error "COERCE-ASSEMBLE17:X: offset not multiple of 4" n))
       (quotient (land (integer-divide-quotient qr) #xF800) #x800)))))

(define coerce-assemble17:Y
  (standard-coercion
   (lambda (n)
     (let ((n (quotient n 4)))
       (+ (quotient (land n #x400) #x400) (* (land n #x3FF) 2))))))

(define coerce-assemble17:Z
  (standard-coercion
   (lambda (n)
     (+ (quotient (land (quotient n 4) #x10000) #x10000)))))

(define coerce-assemble21:X
  (standard-coercion
   (lambda (n)
     (+ (* (land n #x7C) #x4000)
	(* (land n #x180) #x80)
	(* (land n #x3) #x1000)
	(quotient (land n #xFFE00) #x100)
	(quotient (land n #x100000) #x100000)))))

(define make-coercion
  (coercion-maker
   `((ASSEMBLE3:X . ,coerce-assemble3:x)
     (ASSEMBLE12:X . ,coerce-assemble12:x)
     (ASSEMBLE12:Y . ,coerce-assemble12:y)
     (ASSEMBLE17:X . ,coerce-assemble17:x)
     (ASSEMBLE17:Y . ,coerce-assemble17:y)
     (ASSEMBLE17:Z . ,coerce-assemble17:z)
     (ASSEMBLE21:X . ,coerce-assemble21:x)
     (RIGHT-SIGNED . ,coerce-right-signed)
     (UNSIGNED . ,coerce-unsigned-integer)
     (SIGNED . ,coerce-signed-integer))))

(define-coercion 'UNSIGNED 1)
(define-coercion 'UNSIGNED 2)
(define-coercion 'UNSIGNED 3)
(define-coercion 'UNSIGNED 4)
(define-coercion 'UNSIGNED 5)
(define-coercion 'UNSIGNED 6)
(define-coercion 'UNSIGNED 7)
(define-coercion 'UNSIGNED 8)
(define-coercion 'UNSIGNED 9)
(define-coercion 'UNSIGNED 10)
(define-coercion 'UNSIGNED 11)
(define-coercion 'UNSIGNED 12)
(define-coercion 'UNSIGNED 13)
(define-coercion 'UNSIGNED 14)
(define-coercion 'UNSIGNED 16)
(define-coercion 'UNSIGNED 32)

(define-coercion 'SIGNED 8)
(define-coercion 'SIGNED 16)
(define-coercion 'SIGNED 32)

(define-coercion 'RIGHT-SIGNED 5)
(define-coercion 'RIGHT-SIGNED 11)
(define-coercion 'RIGHT-SIGNED 14)
(define-coercion 'ASSEMBLE3:X 3)
(define-coercion 'ASSEMBLE12:X 11)
(define-coercion 'ASSEMBLE12:Y 1)
(define-coercion 'ASSEMBLE17:X 5)
(define-coercion 'ASSEMBLE17:Y 11)
(define-coercion 'ASSEMBLE17:Z 1)
(define-coercion 'ASSEMBLE21:X 21)

;;; Edwin Variables:
;;; Scheme Environment: (access lap-syntaxer-package compiler-package)
;;; End:
(define-coercion 'ASSEMBLE21:X 21)