;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/bitstr.scm,v 13.41 1987/01/23 00:09:36 jinx Exp $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Bit String Primitives

(declare (usual-integrations))

(in-package system-global-environment
(let-syntax ()
  (define-macro (define-primitives . names)
    `(BEGIN ,@(map (lambda (name)
		     `(DEFINE ,name
			,(make-primitive-procedure name)))
		   names)))
  (define-primitives
   bit-string-allocate make-bit-string bit-string?
   bit-string-length bit-string-ref bit-string-clear! bit-string-set!
   bit-string-zero? bit-string=?
   bit-string-fill! bit-string-move! bit-string-movec!
   bit-string-or! bit-string-and! bit-string-andc!
   bit-substring-move-right!
   bit-string->unsigned-integer unsigned-integer->bit-string
   read-bits! write-bits!)))

(define (bit-string-append x y)
  (let ((x-length (bit-string-length x))
	(y-length (bit-string-length y)))
    (let ((result (bit-string-allocate (+ x-length y-length))))
      (bit-substring-move-right! x 0 x-length result 0)
      (bit-substring-move-right! y 0 y-length result x-length)
      result)))

(define (bit-substring bit-string start end)
  (let ((result (bit-string-allocate (- end start))))
    (bit-substring-move-right! bit-string start end result 0)
    result))

(define (signed-integer->bit-string nbits number)
  (unsigned-integer->bit-string nbits
				(if (negative? number)
				    (+ number (expt 2 nbits))
				    number)))

(define (bit-string->signed-integer bit-string)
  (let ((unsigned-result (bit-string->unsigned-integer bit-string))
	(nbits (bit-string-length bit-string)))
    (if (bit-string-ref bit-string (-1+ nbits))	;Sign bit.
	(- unsigned-result (expt 2 nbits))
	unsigned-result)))
	unsigned-result)))