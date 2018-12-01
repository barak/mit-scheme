#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

;;; Many tests fail if there are accrued exceptions when you run them.
;;; This is pretty silly, but you can work around it provisionally by
;;; evaluating (FLO:CLEAR-EXCEPTIONS! (FLO:SUPPORTED-EXCEPTIONS))
;;; before running the tests.

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
   (define-test (symbol 'FLO:SET-ROUNDING-MODE ': mode)
     (lambda ()
       (let ((mode* (flo:rounding-mode)))
         (flo:preserving-environment
          (lambda ()
            (flo:set-rounding-mode! mode)
            (assert-eqv (flo:rounding-mode) mode)))
	 (assert-eqv (flo:rounding-mode) mode*))))))

(for-each-rounding-mode
 (lambda (mode)
   (define-test (symbol 'FLO:WITH-ROUNDING-MODE ': mode)
     (lambda ()
       (let ((mode* (flo:rounding-mode)))
	 (flo:with-rounding-mode mode
	   (lambda ()
	     (assert-eqv (flo:rounding-mode) mode)))
	 (assert-eqv (flo:rounding-mode) mode*))))))

(define-syntax define-rounding-test
  (syntax-rules ()
    ((define-rounding-test name operator mode inputs outputs)
     (define-test (symbol 'ROUNDING-MODE-INDEPENDENT ': mode '/ name)
       (map (lambda (input output)
              (lambda ()
                (with-test-properties
                    (lambda ()
                      (assert-eqv
                       (flo:with-rounding-mode mode
                                               (lambda () (operator input)))
                       output))
                    'EXPRESSION `(,name ,input))))
            inputs outputs)))))

(for-each-rounding-mode
 (lambda (mode)
   (define no-op identity-procedure)
   (define inputs '(-2.0 -1.5 -1.0 -0.5 -0.0 0.0 0.5 1.0 1.5 2.0))
   (define infs '(-inf.0 +inf.0))
   ;; XXX Check NaNs without traps.
   (let ((outputs '(-2.0 -1.0 -1.0 -0.0 -0.0 0.0 1.0 1.0 2.0 2.0)))
     (define-rounding-test 'CEILING/INLINE ceiling mode inputs outputs)
     (define-rounding-test 'CEILING/INLINE ceiling mode infs infs)
     (define-rounding-test 'CEILING (no-op ceiling) mode inputs outputs)
     (define-rounding-test 'CEILING (no-op ceiling) mode infs infs)
     (define-rounding-test 'FLO:CEILING/INLINE flo:ceiling mode inputs outputs)
     (define-rounding-test 'FLO:CEILING/INLINE flo:ceiling mode infs infs)
     (define-rounding-test 'FLO:CEILING (no-op flo:ceiling) mode inputs
       outputs)
     (define-rounding-test 'FLO:CEILING (no-op flo:ceiling) mode infs infs)
     (let ((outputs (map inexact->exact outputs)))
       (define-rounding-test 'CEILING->EXACT/INLINE ceiling->exact mode inputs
         outputs)
       (define-rounding-test 'CEILING->EXACT (no-op ceiling->exact) mode inputs
         outputs)
       (define-rounding-test 'FLO:CEILING->EXACT/INLINE flo:ceiling->exact mode
         inputs outputs)
       (define-rounding-test 'FLO:CEILING->EXACT (no-op flo:ceiling->exact)
         mode inputs outputs)))
   (let ((outputs '(-2.0 -2.0 -1.0 -1.0 -0.0 0.0 0.0 1.0 1.0 2.0)))
     (define-rounding-test 'FLOOR/INLINE floor mode inputs outputs)
     (define-rounding-test 'FLOOR/INLINE floor mode infs infs)
     (define-rounding-test 'FLOOR (no-op floor) mode inputs outputs)
     (define-rounding-test 'FLOOR (no-op floor) mode infs infs)
     (define-rounding-test 'FLO:FLOOR/INLINE flo:floor mode inputs outputs)
     (define-rounding-test 'FLO:FLOOR/INLINE flo:floor mode infs infs)
     (define-rounding-test 'FLO:FLOOR (no-op flo:floor) mode inputs outputs)
     (define-rounding-test 'FLO:FLOOR (no-op flo:floor) mode infs infs)
     (let ((outputs (map inexact->exact outputs)))
       (define-rounding-test 'FLOOR->EXACT/INLINE floor->exact mode inputs
         outputs)
       (define-rounding-test 'FLOOR->EXACT (no-op floor->exact) mode inputs
         outputs)
       (define-rounding-test 'FLO:FLOOR->EXACT/INLINE flo:floor->exact mode
         inputs outputs)
       (define-rounding-test 'FLO:FLOOR->EXACT (no-op flo:floor->exact) mode
         inputs outputs)))
   (let ((outputs'(-2.0 -2.0 -1.0 -0.0 -0.0 0.0 0.0 1.0 2.0 2.0)))
     (define-rounding-test 'ROUND/INLINE round mode inputs outputs)
     (define-rounding-test 'ROUND/INLINE round mode infs infs)
     (define-rounding-test 'ROUND (no-op round) mode inputs outputs)
     (define-rounding-test 'ROUND (no-op round) mode infs infs)
     (define-rounding-test 'FLO:ROUND/INLINE flo:round mode inputs outputs)
     (define-rounding-test 'FLO:ROUND/INLINE flo:round mode infs infs)
     (define-rounding-test 'FLO:ROUND (no-op flo:round) mode inputs outputs)
     (define-rounding-test 'FLO:ROUND (no-op flo:round) mode infs infs)
     (let ((outputs (map inexact->exact outputs)))
       (define-rounding-test 'ROUND->EXACT/INLINE round->exact mode inputs
         outputs)
       (define-rounding-test 'ROUND->EXACT (no-op round->exact) mode inputs
         outputs)
       (define-rounding-test 'FLO:ROUND->EXACT/INLINE flo:round->exact mode
         inputs outputs)
       (define-rounding-test 'FLO:ROUND->EXACT (no-op flo:round->exact) mode
         inputs outputs)))
   (let ((outputs '(-2.0 -1.0 -1.0 -0.0 -0.0 0.0 0.0 1.0 1.0 2.0)))
     (define-rounding-test 'TRUNCATE/INLINE truncate mode inputs outputs)
     (define-rounding-test 'TRUNCATE/INLINE truncate mode infs infs)
     (define-rounding-test 'TRUNCATE (no-op truncate) mode inputs outputs)
     (define-rounding-test 'TRUNCATE (no-op truncate) mode infs infs)
     (define-rounding-test 'FLO:TRUNCATE/INLINE flo:truncate mode inputs
       outputs)
     (define-rounding-test 'FLO:TRUNCATE/INLINE flo:truncate mode infs infs)
     (define-rounding-test 'FLO:TRUNCATE (no-op flo:truncate) mode inputs
       outputs)
     (define-rounding-test 'FLO:TRUNCATE (no-op flo:truncate) mode infs infs)
     (let ((outputs (map inexact->exact outputs)))
       (define-rounding-test 'TRUNCATE->EXACT/INLINE truncate->exact mode
         inputs outputs)
       (define-rounding-test 'TRUNCATE->EXACT (no-op truncate->exact) mode
         inputs outputs)
       (define-rounding-test 'FLO:TRUNCATE->EXACT/INLINE flo:truncate->exact
         mode inputs outputs)
       (define-rounding-test 'FLO:TRUNCATE->EXACT (no-op flo:truncate->exact)
         mode inputs outputs)))))

;++ Add tests for rounding-mode-dependent operations...

(define floating-point-exception-descriptors '())

(define (no-op x) x)			;Do not integrate!

(define (define-fpe-descriptor name trappable? exception condition-type)
  (let ((descriptor (list name exception condition-type trappable? '())))
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

(define (for-each-trappable-exception receiver)
  (for-each-exception
   (lambda (name exception condition-type trappable? elicitors)
     (if (and trappable? (flo:have-trap-enable/disable?))
	 (receiver name exception condition-type elicitors)))))

(define (for-each-exception-elicitor receiver)
  (for-each-exception
   (lambda (name exception condition-type trappable? elicitors)
     (for-each (lambda (name.elicitor)
		 (receiver name exception condition-type trappable?
			   (car name.elicitor)
			   (cdr name.elicitor)))
	       elicitors))))

(define (for-each-trappable-exception-elicitor receiver)
  (for-each-trappable-exception
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
  (lambda () (flo:/ (no-op 0.) (no-op 0.))))

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
      ;; For some reason, on every architecture I tested (x87, amd64,
      ;; sparc), if underflow is not trapped then when the result is
      ;; exact it is not raised either.  Multiplying by .50000001, as
      ;; suggested by Bill Kahan, forces the result to be inexact and
      ;; thereby forces the machine to raise the underflow exception.
      ;; (Note that if underflow is trapped, then the machine traps
      ;; whether or not the result is exact.  Go figure.)
      (flo:* (no-op .5000001) (flo:shift (no-op 1.) -1022)))))

(define (for-each-trappable-exception receiver)
  (for-each-exception
   (lambda (name exception condition-type trappable? elicitors)
     (if (and trappable? (flo:have-trap-enable/disable?))
	 (receiver name exception condition-type elicitors)))))

(for-each-exception
 (lambda (name exception condition-type trappable? elicitors)
   condition-type trappable? elicitors	;ignore
   (define-test (symbol 'FLO:EXCEPTIONS->NAMES ': name)
     (lambda ()
       (assert-equal (flo:exceptions->names (exception)) (list name))))
   (define-test (symbol 'FLO:NAMES->EXCEPTIONS ': name)
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

(define-test 'FLO:TRAPPED-EXCEPTIONS
  (lambda ()
    (flo:trapped-exceptions)))

(define (define-set-trapped-exceptions-test name to-trap)
  (if (flo:have-trap-enable/disable?)
      (define-test (symbol 'FLO:SET-TRAPPED-EXCEPTIONS! ': name)
	(lambda ()
	  (let ((exceptions (to-trap))
		(trapped (flo:trapped-exceptions)))
	    (flo:preserving-environment
	     (lambda ()
	       (assert-eqv (flo:set-trapped-exceptions! exceptions) trapped)
	       (assert-eqv (flo:trapped-exceptions) exceptions))))))))

(define (define-with-trapped-exceptions-test name to-trap)
  (if (flo:have-trap-enable/disable?)
      (define-test (symbol 'FLO:WITH-TRAPPED-EXCEPTIONS ': name)
	(lambda ()
	  (let ((exceptions (to-trap)))
	    (flo:with-trapped-exceptions exceptions
	      (lambda ()
		(assert-eqv (flo:trapped-exceptions) exceptions))))))))

(define-set-trapped-exceptions-test 'ALL (lambda () 0))
(define-set-trapped-exceptions-test 'NONE flo:trappable-exceptions)

(define-with-trapped-exceptions-test 'ALL (lambda () 0))
(define-with-trapped-exceptions-test 'NONE flo:trappable-exceptions)

(for-each-trappable-exception
 (lambda (name exception condition-type elicitors)
   condition-type elicitors		;ignore
   (define-test (symbol 'FLO:WITH-TRAPPED-EXCEPTIONS ': name)
     (lambda ()
       (flo:with-trapped-exceptions (exception)
	 (lambda ()
	   (assert-eqv (flo:trapped-exceptions) (exception))))))))

(for-each-trappable-exception
 (lambda (name exception condition-type elicitors)
   condition-type elicitors		;ignore
   (define-test (symbol 'FLO:TRAP-EXCEPTIONS! ': name)
     (lambda ()
       (flo:with-trapped-exceptions 0
	 (lambda ()
	   (assert-eqv (flo:trap-exceptions! (exception)) 0)
	   (assert-eqv (flo:trapped-exceptions) (exception))))))))

(for-each-trappable-exception
 (lambda (name exception condition-type elicitors)
   condition-type elicitors		;ignore
   (define-test (symbol 'FLO:UNTRAP-EXCEPTIONS! ': name)
     (lambda ()
       (flo:with-trapped-exceptions (flo:trappable-exceptions)
	 (lambda ()
	   (assert-eqv (flo:untrap-exceptions! (exception))
		       (flo:trappable-exceptions))
	   (assert-eqv (flo:trapped-exceptions)
		       (fix:andc (flo:trappable-exceptions) (exception)))))))))

(for-each-trappable-exception
 (lambda (name exception condition-type elicitors)
   condition-type elicitors		;ignore
   (define-test (symbol 'FLO:SET-TRAPPED-EXCEPTIONS! ': name ': 'ENABLE)
     (lambda ()
       (flo:with-trapped-exceptions 0
	 (lambda ()
	   (assert-eqv (flo:set-trapped-exceptions! (exception)) 0)
	   (assert-eqv (flo:trapped-exceptions) (exception))))))))

(for-each-trappable-exception
 (lambda (name exception condition-type elicitors)
   condition-type elicitors		;ignore
   (define-test (symbol 'FLO:SET-TRAPPED-EXCEPTIONS! ': name ': 'DISABLE)
     (lambda ()
       (let ((exceptions (fix:andc (flo:trappable-exceptions) (exception))))
	 (flo:with-trapped-exceptions (flo:trappable-exceptions)
	   (lambda ()
	     (assert-eqv (flo:set-trapped-exceptions! exceptions)
			 (flo:trappable-exceptions))
	     (assert-eqv (flo:trapped-exceptions) exceptions))))))))

(for-each-trappable-exception-elicitor
 (lambda (name exception condition-type elicitor-name elicitor)
   (define-test (symbol 'ELICIT ': name ': elicitor-name)
     (lambda ()
       (assert-error (lambda ()
		       (flo:with-trapped-exceptions (exception) elicitor))
		     (list condition-type))))))

(for-each-trappable-exception-elicitor
 (lambda (name exception condition-type elicitor-name elicitor)
   exception				;ignore
   (define-test (symbol 'ELICIT-DEFERRED ': name ': elicitor-name)
     (lambda ()
       (assert-error
	(lambda ()
	  (flo:with-trapped-exceptions (flo:trappable-exceptions)
	    (lambda ()
	      (flo:deferring-exception-traps
	       (lambda ()
		 (let ((flag #f))
		   (dynamic-wind (lambda () unspecific)
				 (lambda () (elicitor) (set! flag #t))
				 (lambda () (assert-true flag)))))))))
	(list condition-type))))))

(for-each-exception-elicitor
 (lambda (name exception condition-type trappable? elicitor-name elicitor)
   exception condition-type trappable?	;ignore
   (define-test (symbol 'ELICIT-IGNORED ': name ': elicitor-name)
     (lambda ()
       (flo:ignoring-exception-traps elicitor)))))

(for-each-exception-elicitor
 (lambda (name exception condition-type trappable? elicitor-name elicitor)
   condition-type trappable?		;ignore
   (define-test (symbol 'ELICIT-AND-TEST ': name ': elicitor-name)
     (lambda ()
       (assert-eqv (flo:ignoring-exception-traps
		    (lambda ()
		      (elicitor)
		      (flo:test-exceptions (exception))))
		   (exception))))))

(for-each-exception-elicitor
 (lambda (name exception condition-type trappable? elicitor-name elicitor)
   condition-type trappable?		;ignore
   (define-test (symbol 'ELICIT-CLEAR-TEST ': name ': elicitor-name)
     (lambda ()
       (assert-eqv (flo:ignoring-exception-traps
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
    (flo:preserving-environment
     (lambda ()
       (flo:set-environment! (flo:environment))))))

(define-test 'FLO:DEFAULT-ENVIRONMENT
  (lambda ()
    (flo:default-environment)))

(define-test 'FLO:SET-DEFAULT-ENVIRONMENT
  (lambda ()
    (flo:preserving-environment
     (lambda ()
       (flo:set-environment! (flo:default-environment))))))

(define-test 'FLO:WITH-DEFAULT-ENVIRONMENT
  (lambda ()
    (flo:with-default-environment (lambda () 0))))

(define (define-default-environment-test name procedure)
  (define-test (symbol 'FLO:DEFAULT-ENVIRONMENT ': name)
    (lambda ()
      (flo:preserving-environment
       (lambda ()
	 ;; Futz with the floating-point environment first,
	 ;; guaranteeing that it is not the default one.
	 (flo:set-rounding-mode!
	  (if (eq? 'UPWARD (flo:default-rounding-mode))
	      'TO-NEAREST
	      'UPWARD))
	 (flo:set-trapped-exceptions!
	  (if (= (flo:trappable-exceptions) (flo:default-trapped-exceptions))
	      (fix:andc (flo:default-trapped-exceptions)
			(flo:trappable-exceptions))
	      (flo:trappable-exceptions)))
	 (flo:with-default-environment procedure))))))

(define-default-environment-test 'ROUNDING-MODE
  (lambda ()
    (assert-eqv (flo:rounding-mode) (flo:default-rounding-mode))))

(if (flo:have-trap-enable/disable?)
    (define-default-environment-test 'TRAPPED-EXCEPTIONS
      (lambda ()
	(assert-eqv (flo:trapped-exceptions)
		    (flo:default-trapped-exceptions)))))

(define-test 'preserving-environment
  (lambda ()
    ;; Trigger a bunch of floating-point exceptions _before_ we have
    ;; registered interest in the floating-point environment -- but
    ;; block thread-switching so nobody else can get in -- and then
    ;; confirm that within flo:preserving-environment,
    ;; flo:clear-exceptions! actually clears the exceptions.
    (expect-failure
     (lambda ()
       (assert-eqv
        (without-interruption
         (lambda ()
           ;; Trigger as many floating-point exceptions as we can.  If
           ;; they trap, ignore it.  Don't enable floating-point traps
           ;; since we're testing what happens if we haven't done any
           ;; operations that touch the floating-point environment.
           (ignore-errors
            (lambda ()
              (flo:sqrt -1.)
              (flo:/ flo:smallest-positive-normal 2.)
              (flo:sqrt 2.)
              (flo:exp (flo:* 2. flo:greatest-normal-exponent-base-e))))
           (flo:preserving-environment
            (lambda ()
              ;; Clear the exceptions.
              (flo:clear-exceptions! (flo:supported-exceptions))
              ;; Test the exceptions.  They should actually be cleared.
              (flo:test-exceptions (flo:supported-exceptions))))))
        0)))))
