#| -*-Scheme-*-

$Id: coerce.scm,v 1.1 1996/07/26 14:25:07 adams Exp $

Copyright (c) 1996 Massachusetts Institute of Technology

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

;;;; Coerce-to-compiled-procedure
;;; package: (runtime coerce-to-compiled-procedure)

;;  This file must be syntaxed with teh 8.0 compiler loaded

(declare (usual-integrations))

;;  COERCE-TO-COMPILED-PROCEDURE
;;
;;  This code is special.  It uses several hooks into the 8.0 compiler in
;;  order to generate efficient code.  Care has to be taken to ensure
;;  that none of the compiled code uses COERCE-TO-COMPILED-PROCEDURE,
;;  otherwise we would have a loop.
;;


(define-macro (special-operator name)
  `(QUOTE ,(environment-lookup (->environment '(COMPILER MIDEND)) name)))

(define-macro (%funcall procedure . arguments)
  `((special-operator %internal-apply-unchecked)
    ,(+ (length arguments) 1)
    ,procedure
    ,@arguments))

;; (%compiled-entry? <object>)
;; (%compiled-entry-maximum-arity? <arity+1> <object>)

(define-integrable %compiled-entry? (special-operator %compiled-entry?))

(define-integrable %compiled-entry-maximum-arity?
  (special-operator %compiled-entry-maximum-arity?))


(define (coerce-to-compiled-procedure/compiled object arity)

  (let ((xx ((lambda (#!optional xx) xx)))
	(+  fix:+)
	(=  fix:=)
	(<  fix:<))

    (define (use-microcode)
      ((ucode-primitive coerce-to-compiled-procedure 2) object arity))

    (define (default)
      (use-microcode))

    (define (mismatch)
      (use-microcode))

    (define (make-trampoline f arity min max)
      (cond
       ((= min max)
	(mismatch))
       ((< max 128)
	(case arity
	  ((1)
	   (case max
	     ((2)  (lambda () (%funcall f xx)))
	     ((3)  (lambda () (%funcall f xx xx)))
	     ((4)  (lambda () (%funcall f xx xx xx)))
	     ((5)  (lambda () (%funcall f xx xx xx xx)))
	     (else (default))))
	  ((2)
	   (case max
	     ((3)  (lambda (a1) (f a1 xx)))
	     ((4)  (lambda (a1) (f a1 xx xx)))
	     ((5)  (lambda (a1) (f a1 xx xx xx)))
	     (else (default))))
	  ((3)
	   (case max
	     ((4)  (lambda (a1 a2) (f a1 a2 xx)))
	     ((5)  (lambda (a1 a2) (f a1 a2 xx xx)))
	     (else (default))))
	  ((4)
	   (case max
	     ((5)  (lambda (a1 a2 a3) (f a1 a2 a3 xx)))
	     (else (default))))
	  (else (default))))
       (else;; max >= 128
	(make-listifying-trampoline f arity min max))))

    (define (make-listifying-trampoline f arity min max)
      (case arity
	((1)
	 (case min
	   ((1)
	    (case max
	      ((254) (lambda () (%funcall f '())))
	      ((253) (lambda () (%funcall f xx '())))
	      ((252) (lambda () (%funcall f xx xx '())))
	      ((251) (lambda () (%funcall f xx xx xx '())))
	      ((250) (lambda () (%funcall f xx xx xx xx '())))
	      (else  (default))))
	   (else (default))))
	((2)
	 (if (< (+ min max) 256)
	     (case max
	       ((254) (lambda (a1) (%funcall f (list a1))))
	       ((253) (lambda (a1) (%funcall f a1 '())))
	       ((252) (lambda (a1) (%funcall f a1 xx '())))
	       ((251) (lambda (a1) (%funcall f a1 xx xx '())))
	       ((250) (lambda (a1) (%funcall f a1 xx xx xx '())))
	       (else (default)))
	     (default)))
	((3)
	 (if (< (+ min max) 256)
	     (case max
	       ((254) (lambda (a1 a2) (%funcall f (list a1 a2))))
	       ((253) (lambda (a1 a2) (%funcall f a1 (list a2))))
	       ((252) (lambda (a1 a2) (%funcall f a1 a2 '())))
	       ((251) (lambda (a1 a2) (%funcall f a1 a2 xx '())))
	       ((250) (lambda (a1 a2) (%funcall f a1 a2 xx xx '())))
	       (else (default)))
	     (default)))
	((4)
	 (if (< (+ min max) 256)
	     (case max
	       ((254) (lambda (a1 a2 a3) (%funcall f (list a1 a2 a3))))
	       ((253) (lambda (a1 a2 a3) (%funcall f a1 (list a2 a3))))
	       ((252) (lambda (a1 a2 a3) (%funcall f a1 a2 (list a3))))
	       ((251) (lambda (a1 a2 a3) (%funcall f a1 a2 a3 '())))
	       ((250) (lambda (a1 a2 a3) (%funcall f a1 a2 a3 xx '())))
	       (else (default)))
	     (default)))
	((5)
	 (if (< (+ min max) 256)
	     (case max
	       ((254) (lambda (a1 a2 a3 a4) (%funcall f (list a1 a2 a3 a4))))
	       ((253) (lambda (a1 a2 a3 a4) (%funcall f a1 (list a2 a3 a4))))
	       ((252) (lambda (a1 a2 a3 a4) (%funcall f a1 a2 (list a3 a4))))
	       ((251) (lambda (a1 a2 a3 a4) (%funcall f a1 a2 a3 (list a4))))
	       ((250) (lambda (a1 a2 a3 a4) (%funcall f a1 a2 a3 a4 '())))
	       (else (default)))
	     (default)))
	((6)
	 (if (< (+ min max) 256)
	     (case max
	       ((254) (lambda (a1 a2 a3 a4 a5)
			(%funcall f (list a1 a2 a3 a4 a5))))
	       ((253) (lambda (a1 a2 a3 a4 a5)
			(%funcall f a1 (list a2 a3 a4 a5))))
	       ((252) (lambda (a1 a2 a3 a4 a5)
			(%funcall f a1 a2 (list a3 a4 a5))))
	       ((251) (lambda (a1 a2 a3 a4 a5)
			(%funcall f a1 a2 a3 (list a4 a5))))
	       ((250) (lambda (a1 a2 a3 a4 a5)
			(%funcall f a1 a2 a3 a4 (list a5))))
	       (else (default)))
	     (default)))
	(else (default))))

    (if (and (%compiled-entry? object)
	     (fixnum? arity))
	(if (and (%compiled-entry-maximum-arity? arity object)
		 (< arity 128))
	    object
	    (let ((info ((ucode-primitive compiled-entry-kind 1) object)))
	      ;; max = (-1)^tail? * (1 + req + opt + tail?)
	      ;; min = (1 + req)
	      (let ((min (system-hunk3-cxr1 info))
		    (max (system-hunk3-cxr2 info)))
		(make-trampoline object arity min max))))
	(use-microcode))))


(define (%compiled-entry-arity p)
  (let ((info ((ucode-primitive compiled-entry-kind 1) p)))
    ;; max = (-1)^tail? * (1 + req + opt + tail?)
    ;; min = (1 + req)
    (let ((min (system-hunk3-cxr1 info))
	  (max (system-hunk3-cxr2 info)))
      (cons min max))))

(define coerce-to-compiled-procedure)

(define (initialize-package!)
  (set! coerce-to-compiled-procedure
	(if (compiled-procedure? coerce-to-compiled-procedure/compiled)
	    coerce-to-compiled-procedure/compiled
	    (ucode-primitive coerce-to-compiled-procedure)))
  unspecific)
