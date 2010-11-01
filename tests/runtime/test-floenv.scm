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

;;;; Tests of the floating-point environment

(declare (usual-integrations))

(define-test 'FLO:DEFAULT-ROUNDING-MODE
  (lambda ()
    (assert-eqv (flo:default-rounding-mode) 'TO-NEAREST)))

(define-test 'FLO:ROUNDING-MODES
  (lambda ()
    (assert-eqv
     (lset-difference eq?
		      (flo:rounding-modes)
		      '(TO-NEAREST TOWARD-ZERO DOWNWARD UPWARD))
     '())))

(define-test 'FLO:ROUNDING-MODE
  (lambda ()
    (assert-memv (flo:rounding-mode) (flo:rounding-modes))))

(define (for-each-rounding-mode receiver)
  (for-each receiver '(TO-NEAREST TOWARD-ZERO DOWNWARD UPWARD)))

(for-each-rounding-mode
 (lambda (mode)
   (define-test (symbol-append 'FLO:SET-ROUNDING-MODE ': mode)
     (lambda ()
       (let ((mode* (flo:rounding-mode)))
	 (dynamic-wind (lambda () (flo:set-rounding-mode! mode))
		       (lambda () (assert-eqv (flo:rounding-mode) mode))
		       (lambda () (flo:set-rounding-mode! mode*)))
	 (assert-eqv (flo:rounding-mode) mode*))))))

(for-each-rounding-mode
 (lambda (mode)
   (define-test (symbol-append 'FLO:WITH-ROUNDING-MODE ': mode)
     (lambda ()
       (let ((mode* (flo:rounding-mode)))
	 (flo:with-rounding-mode mode
	   (lambda ()
	     (assert-eqv (flo:rounding-mode) mode)))
	 (assert-eqv (flo:rounding-mode) mode*))))))

(for-each-rounding-mode
 (lambda (mode)
   (define inputs '(-2.0 -1.5 -1.0 -0.5 -0.0 0.0 0.5 1.0 1.5 2.0))
   (define (define-rounding-test name operator outputs)
     (define-test (symbol-append 'ROUNDING-MODE-INDEPENDENT ': mode '/ name)
       (lambda ()
	 (do ((inputs inputs (cdr inputs))
	      (outputs outputs (cdr outputs))
	      (i 0 (+ i 1)))
	     ((not (and (pair? inputs) (pair? outputs))))
	   (let ((input (car inputs)) (output (car outputs)))
	     (run-sub-test
	      (lambda ()
		(assert-eqv
		 (flo:with-rounding-mode mode (lambda () (operator input)))
		 output))))))))
   (define-rounding-test 'CEILING ceiling
     '(-2.0 -1.0 -1.0 -0.0 -0.0 0.0 1.0 1.0 2.0 2.0))
   (define-rounding-test 'FLOOR floor
     '(-2.0 -2.0 -1.0 -1.0 -0.0 0.0 0.0 1.0 1.0 2.0))
   (define-rounding-test 'ROUND round
     '(-2.0 -2.0 -1.0 -0.0 -0.0 0.0 0.0 1.0 2.0 2.0))
   (define-rounding-test 'TRUNCATE truncate
     '(-2.0 -1.0 -1.0 -0.0 -0.0 0.0 0.0 1.0 1.0 2.0))))

;++ Add tests for rounding-mode-dependent operations...

(define floating-point-exception-descriptors '())

(define (no-op x) x)			;Do not integrate!

(define (define-fpe-descriptor name unmaskable? exception condition-type)
  (let ((descriptor (list name exception condition-type unmaskable? '())))
    (cond ((assq name floating-point-exception-descriptors)
	   => (lambda (descriptor*)
		(set-cdr! descriptor* (cdr descriptor))))
	  (else
	   (set! floating-point-exception-descriptors
		 (cons descriptor floating-point-exception-descriptors))
	   unspecific))))

(define (define-fpe-elicitor name elicitor-name procedure)
  (cond ((assq name floating-point-exception-descriptors)
	 => (lambda (descriptor)
	      (let ((elicitors (list-ref descriptor 4)))
		(cond ((assq elicitor-name elicitors)
		       => (lambda (pair) (set-cdr! pair procedure)))
		      (else
		       (set-car! (list-tail descriptor 4)
				 (cons (cons elicitor-name procedure)
				       elicitors)))))))
	(else
	 (error:bad-range-argument name 'DEFINE-FPE-ELICITOR))))

(define (for-each-exception receiver)
  (for-each (lambda (descriptor)
	      (apply receiver descriptor))
	    floating-point-exception-descriptors))

(define (for-each-unmaskable-exception receiver)
  (for-each-exception
   (lambda (name exception condition-type unmaskable? elicitors)
     (if unmaskable?
	 (receiver name exception condition-type elicitors)))))

(define (for-each-exception-elicitor receiver)
  (for-each-exception
   (lambda (name exception condition-type unmaskable? elicitors)
     (for-each (lambda (name.elicitor)
		 (receiver name exception condition-type unmaskable?
			   (car name.elicitor)
			   (cdr name.elicitor)))
	       elicitors))))

(define (for-each-unmaskable-exception-elicitor receiver)
  (for-each-unmaskable-exception
   (lambda (name exception condition-type elicitors)
     (for-each (lambda (name.elicitor)
		 (receiver name exception condition-type
			   (car name.elicitor)
			   (cdr name.elicitor)))
	       elicitors))))

(define-fpe-descriptor 'DIVIDE-BY-ZERO #t flo:exception:divide-by-zero
  condition-type:floating-point-divide-by-zero)

(define-fpe-elicitor 'DIVIDE-BY-ZERO 'RAISE
  (lambda () (flo:raise-exceptions! (flo:exception:divide-by-zero))))

(define-fpe-elicitor 'DIVIDE-BY-ZERO 'POSITIVE-ONE-OVER-ZERO
  (lambda () (flo:/ (no-op +1.) (no-op 0.))))

(define-fpe-elicitor 'DIVIDE-BY-ZERO 'NEGATIVE-ONE-OVER-ZERO
  (lambda () (flo:/ (no-op -1.) (no-op 0.))))

(define-fpe-elicitor 'DIVIDE-BY-ZERO 'LOG-ZERO
  (lambda () (flo:log (no-op 0.))))

(define-fpe-descriptor 'INEXACT-RESULT #f flo:exception:inexact-result
  condition-type:inexact-floating-point-result)

(define-fpe-elicitor 'INEXACT-RESULT 'RAISE
  (lambda () (flo:raise-exceptions! (flo:exception:inexact-result))))

(define-fpe-elicitor 'INEXACT-RESULT 'ONE-PLUS-EPSILON-OVER-TWO
  (lambda ()
    (flo:+ (no-op 1.) (flo:* (no-op .5) microcode-id/floating-epsilon))))

(define-fpe-descriptor 'INVALID-OPERATION #t flo:exception:invalid-operation
  condition-type:invalid-floating-point-operation)

(define-fpe-elicitor 'INVALID-OPERATION 'RAISE
  (lambda () (flo:raise-exceptions! (flo:exception:invalid-operation))))

(define-fpe-elicitor 'INVALID-OPERATION 'ZERO-OVER-ZERO
  (lambda ()
    (flo:/ (no-op 0.) (no-op 0.))))

(define-fpe-descriptor 'OVERFLOW #t flo:exception:overflow
  condition-type:floating-point-overflow)

(define-fpe-elicitor 'OVERFLOW 'RAISE
  (lambda () (flo:raise-exceptions! (flo:exception:overflow))))

;++ The maximum and minimum exponents should not be hard-coded here.

(define-fpe-elicitor 'OVERFLOW 'MOST-POSITIVE-NUMBER-TIMES-TWO
  (let ((flo:shift (make-primitive-procedure 'FLONUM-DENORMALIZE 2)))
    (lambda ()
      (flo:* (no-op 2.) (flo:shift (no-op 1.) 1023)))))

(define-fpe-descriptor 'UNDERFLOW #t flo:exception:underflow
  condition-type:floating-point-underflow)

(define-fpe-elicitor 'UNDERFLOW 'RAISE
  (lambda () (flo:raise-exceptions! (flo:exception:underflow))))

(define-fpe-elicitor 'UNDERFLOW 'LEAST-POSITIVE-NUMBER-OVER-TWO
  (let ((flo:shift (make-primitive-procedure 'FLONUM-DENORMALIZE 2)))
    (lambda ()
      ;; Problem: On every machine I tested (several different models
      ;; of each of x87, amd64, and sparc), if this doesn't trap, it
      ;; doesn't set the underflow exception flag either.  So all tests
      ;; relying on the exception flag will fail.
      (flo:* (no-op .5) (flo:shift (no-op 1.) -1022)))))

(define (for-each-unmaskable-exception receiver)
  (for-each-exception
   (lambda (name exception condition-type unmaskable? elicitors)
     (if unmaskable? (receiver name exception condition-type elicitors)))))

(for-each-exception
 (lambda (name exception condition-type unmaskable? elicitors)
   condition-type unmaskable? elicitors	;ignore
   (define-test (symbol-append 'FLO:EXCEPTIONS->NAMES ': name)
     (lambda ()
       (assert-equal (flo:exceptions->names (exception)) (list name))))
   (define-test (symbol-append 'FLO:NAMES->EXCEPTIONS ': name)
     (lambda ()
       (assert-equal (flo:names->exceptions (list name)) (exception))))))

(define-test 'FLO:EXCEPTIONS->NAMES
  (lambda ()
    (let ((descriptors floating-point-exception-descriptors))
      (assert-equal
       (lset-difference
	eq?
	(flo:exceptions->names
	 (reduce fix:or 0 (map (lambda (f) (f)) (map cadr descriptors))))
	(map car descriptors))
       '()))))

(define-test 'FLO:NAMES->EXCEPTIONS
  (lambda ()
    (let ((descriptors floating-point-exception-descriptors))
      (assert-eqv
       (flo:names->exceptions (map car descriptors))
       (reduce fix:or 0 (map (lambda (f) (f)) (map cadr descriptors)))))))

(define-test 'FLO:SUPPORTED-EXCEPTIONS
  (lambda ()
    (flo:supported-exceptions)))

(define-test 'FLO:SUPPORTED-EXCEPTION-NAMES
  (lambda ()
    (assert-eqv
     (lset-difference eq?
		      (flo:exceptions->names (flo:supported-exceptions))
		      (map car floating-point-exception-descriptors))
     '())))

(define-test 'FLO:MASKED-EXCEPTIONS
  (lambda ()
    (flo:masked-exceptions)))

(define (define-set-masked-exceptions-test name to-mask)
  (define-test (symbol-append 'FLO:SET-MASKED-EXCEPTIONS! ': name)
    (lambda ()
      (let ((exceptions (fix:andc (flo:supported-exceptions) (to-mask)))
	    (mask (flo:masked-exceptions)))
	(dynamic-wind
	 (lambda () unspecific)
	 (lambda ()
	   (assert-eqv (flo:set-masked-exceptions! exceptions) mask)
	   (assert-eqv (flo:masked-exceptions) exceptions))
	 (lambda () (flo:set-masked-exceptions! mask)))))))

(define (define-with-exception-mask-test name to-mask)
  (define-test (symbol-append 'FLO:WITH-EXCEPTION-MASK ': name)
    (lambda ()
      (let ((exceptions (fix:andc (flo:supported-exceptions) (to-mask))))
	(flo:with-exception-mask exceptions
	  (lambda ()
	    (assert-eqv (flo:masked-exceptions) exceptions)))))))

(define-set-masked-exceptions-test 'ALL (lambda () 0))
(define-set-masked-exceptions-test 'NONE flo:unmaskable-exceptions)

(define-with-exception-mask-test 'ALL (lambda () 0))
(define-with-exception-mask-test 'NONE flo:unmaskable-exceptions)

(for-each-unmaskable-exception
 (lambda (name exception condition-type elicitors)
   elicitors				;ignore
   (define-test (symbol-append 'FLO:WITH-EXCEPTION-MASK ': name)
     (lambda ()
       (let ((mask (fix:andc (flo:supported-exceptions) (exception))))
	 (flo:with-exception-mask mask
	   (lambda ()
	     (assert-eqv (flo:masked-exceptions) mask))))))))

(for-each-unmaskable-exception
 (lambda (name exception condition-type elicitors)
   elicitors				;ignore
   (define-test (symbol-append 'FLO:MASK-EXCEPTIONS! ': name)
     (lambda ()
       (let ((mask
	      (fix:andc (flo:supported-exceptions)
			(flo:unmaskable-exceptions))))
	 (flo:with-exception-mask mask
	   (lambda ()
	     (assert-eqv (flo:mask-exceptions! (exception)) mask)
	     (assert-eqv (flo:masked-exceptions)
			 (fix:or mask (exception))))))))))

(for-each-unmaskable-exception
 (lambda (name exception condition-type elicitors)
   elicitors				;ignore
   (define-test (symbol-append 'FLO:UNMASK-EXCEPTIONS! ': name)
     (lambda ()
       (flo:with-exception-mask (flo:supported-exceptions)
	 (lambda ()
	   (assert-eqv (flo:unmask-exceptions! (exception))
		       (flo:supported-exceptions))
	   (assert-eqv (flo:masked-exceptions)
		       (fix:andc (flo:supported-exceptions) (exception)))))))))

(for-each-unmaskable-exception
 (lambda (name exception condition-type elicitors)
   elicitors				;ignore
   (define-test (symbol-append 'FLO:SET-MASKED-EXCEPTIONS! ': name ': 'ENABLE)
     (lambda ()
       (let ((mask
	      (fix:andc (flo:supported-exceptions)
			(flo:unmaskable-exceptions))))
	 (flo:with-exception-mask (fix:or mask (exception))
	   (lambda ()
	     (assert-eqv (flo:set-masked-exceptions! mask)
			 (fix:or mask (exception)))
	     (assert-eqv (flo:masked-exceptions) mask))))))))

(for-each-unmaskable-exception
 (lambda (name exception condition-type elicitors)
   elicitors				;ignore
   (define-test (symbol-append 'FLO:SET-MASKED-EXCEPTIONS! ': name ': 'DISABLE)
     (lambda ()
       (let ((mask (fix:andc (flo:supported-exceptions) (exception))))
	 (flo:with-exception-mask (flo:supported-exceptions)
	   (lambda ()
	     (assert-eqv (flo:set-masked-exceptions! mask)
			 (flo:supported-exceptions))
	     (assert-eqv (flo:masked-exceptions) mask))))))))

(for-each-unmaskable-exception-elicitor
 (lambda (name exception condition-type elicitor-name elicitor)
   (define-test (symbol-append 'ELICIT ': name ': elicitor-name)
     (lambda ()
       (assert-error (lambda ()
		       (flo:with-exception-mask
			   (fix:andc (flo:supported-exceptions) (exception))
			 elicitor))
		     (list condition-type))))))

(for-each-unmaskable-exception-elicitor
 (lambda (name exception condition-type elicitor-name elicitor)
   (define-test (symbol-append 'ELICIT-DEFERRED ': name ': elicitor-name)
     (lambda ()
       (assert-error
	(lambda ()
	  (flo:with-exception-mask
	      (fix:andc (flo:supported-exceptions) (flo:unmaskable-exceptions))
	    (lambda ()
	      (flo:deferring-exceptions
	       (lambda ()
		 (let ((flag #f))
		   (dynamic-wind (lambda () unspecific)
				 (lambda () (elicitor) (set! flag #t))
				 (lambda () (assert-true flag)))))))))
	(list condition-type))))))

(for-each-exception-elicitor
 (lambda (name exception condition-type unmaskable? elicitor-name elicitor)
   unmaskable?				;ignore
   (define-test (symbol-append 'ELICIT-IGNORED ': name ': elicitor-name)
     (lambda ()
       (flo:ignoring-exceptions elicitor)))))

(for-each-exception-elicitor
 (lambda (name exception condition-type unmaskable? elicitor-name elicitor)
   unmaskable?				;ignore
   (define-test (symbol-append 'ELICIT-AND-TEST ': name ': elicitor-name)
     (lambda ()
       (assert-eqv (flo:ignoring-exceptions
		    (lambda ()
		      (elicitor)
		      (flo:test-exceptions (exception))))
		   (exception))))))

(for-each-exception-elicitor
 (lambda (name exception condition-type unmaskable? elicitor-name elicitor)
   unmaskable?				;ignore
   (define-test (symbol-append 'ELICIT-CLEAR-TEST ': name ': elicitor-name)
     (lambda ()
       (assert-eqv (flo:ignoring-exceptions
		    (lambda ()
		      (elicitor)
		      (flo:clear-exceptions! (exception))
		      (flo:test-exceptions (exception))))
		   0)))))

(define-test 'FLO:ENVIRONMENT
  (lambda ()
    (flo:environment)))

(define-test 'FLO:SET-ENVIRONMENT
  (lambda ()
    (flo:set-environment! (flo:environment))))

(define-test 'FLO:DEFAULT-ENVIRONMENT
  (lambda ()
    (flo:default-environment)))

(define-test 'FLO:SET-DEFAULT-ENVIRONMENT
  (lambda ()
    (let ((environment (flo:environment)))
      (dynamic-wind
       (lambda () unspecific)
       (lambda () (flo:set-environment! (flo:default-environment)))
       (lambda () (flo:set-environment! environment))))))

(define-test 'FLO:WITH-DEFAULT-ENVIRONMENT
  (lambda ()
    (flo:with-default-environment (lambda () 0))))

(define (define-default-environment-test name procedure)
  (define-test (symbol-append 'FLO:DEFAULT-ENVIRONMENT ': name)
    (lambda ()
      (flo:preserving-environment
       (lambda ()
	 ;; Futz with the floating-point environment first,
	 ;; guaranteeing that it is not the default one.
	 (flo:set-rounding-mode!
	  (if (eq? 'UPWARD (flo:default-rounding-mode))
	      'TO-NEAREST
	      'UPWARD))
	 (flo:set-masked-exceptions!
	  (if (= (flo:supported-exceptions) (flo:default-exception-mask))
	      (fix:andc (flo:supported-exceptions)
			(flo:unmaskable-exceptions))
	      (flo:supported-exceptions)))
	 (flo:with-default-environment procedure))))))

(define-default-environment-test 'ROUNDING-MODE
  (lambda ()
    (assert-eqv (flo:rounding-mode) (flo:default-rounding-mode))))

(define-default-environment-test 'MASKED-EXCEPTIONS
  (lambda ()
    (assert-eqv (flo:masked-exceptions) (flo:default-exception-mask))))
