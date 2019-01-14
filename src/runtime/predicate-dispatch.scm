#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Predicates: dispatch
;;; package: (runtime predicate-dispatch)

(declare (usual-integrations))

(define (predicate-dispatcher? object)
  (and (entity? object)
       (metadata? (entity-extra object))))

(add-boot-init!
 (lambda ()
   (register-predicate! predicate-dispatcher? 'predicate-dispatcher
			'<= entity?)))

(define (get-metadata procedure caller)
  (let ((metadata (entity-extra procedure)))
    (if (not (metadata? metadata))
        (error:not-a predicate-dispatcher? procedure caller))
    metadata))

(define (make-predicate-dispatcher name arity make-handler-set)
  (if (not (> (procedure-arity-min arity) 0))
      (error:bad-range-argument arity 'make-predicate-dispatcher))
  (let ((metadata
	 (make-metadata name
			arity
			(make-handler-set arity (make-default-handler name)))))
    (make-entity (make-procedure arity metadata)
		 metadata)))

(define (make-default-handler name)
  (lambda args
    (error "Inapplicable generic procedure:" name args)))

(define (make-procedure arity metadata)
  (let ((get-handler ((metadata-handler-set metadata) 'get-handler)))
    (case (and (eqv? (procedure-arity-min arity) (procedure-arity-max arity))
	       (procedure-arity-min arity))
      ((1)
       (lambda (self arg)
	 (declare (ignore self))
	 ((get-handler arg) arg)))
      ((2)
       (lambda (self arg1 arg2)
	 (declare (ignore self))
	 ((get-handler arg1 arg2) arg1 arg2)))
      ((3)
       (lambda (self arg1 arg2 arg3)
	 (declare (ignore self))
	 ((get-handler arg1 arg2 arg3) arg1 arg2 arg3)))
      ((4)
       (lambda (self arg1 arg2 arg3 arg4)
	 (declare (ignore self))
	 ((get-handler arg1 arg2 arg3 arg4) arg1 arg2 arg3 arg4)))
      (else
       (lambda (self . args)
	 (declare (ignore self))
	 (apply (apply get-handler args) args))))))

(define (simple-predicate-dispatcher name arity)
  (make-predicate-dispatcher name arity simple-handler-set))

(define (standard-predicate-dispatcher name arity)
  (make-predicate-dispatcher name arity most-specific-handler-set))

(define (chaining-predicate-dispatcher name arity)
  (make-predicate-dispatcher name arity chaining-handler-set))

(define (cached-standard-predicate-dispatcher name arity)
  (make-predicate-dispatcher name arity cached-most-specific-handler-set))

(define (cached-chaining-predicate-dispatcher name arity)
  (make-predicate-dispatcher name arity cached-chaining-handler-set))

(define-record-type <metadata>
    (make-metadata name arity handler-set)
    metadata?
  (name metadata-name)
  (arity metadata-arity)
  (handler-set metadata-handler-set))

(define (predicate-dispatcher-name dispatch)
  (metadata-name (get-metadata dispatch 'predicate-dispatcher-name)))

(define (predicate-dispatcher-arity dispatch)
  (metadata-arity (get-metadata dispatch 'predicate-dispatcher-arity)))

(define (predicate-dispatcher-rules dispatch)
  (map list-copy
       (((get-handler-set dispatch 'predicate-dispatcher-rules) 'get-rules))))

(define (get-handler-set dispatch caller)
  (metadata-handler-set (get-metadata dispatch caller)))

(define (define-predicate-dispatch-handler dispatch predicates handler)
  (let ((metadata (get-metadata dispatch 'define-predicate-dispatch-handler)))
    (guarantee-list-of unary-procedure? predicates
		       'define-predicate-dispatch-handler)
    (guarantee-procedure-of-arity handler (metadata-arity metadata)
				  'define-predicate-dispatch-handler)
    (((metadata-handler-set metadata) 'set-handler!) predicates handler)))

(define (define-predicate-dispatch-default-handler dispatch handler)
  (((get-handler-set dispatch 'define-predicate-dispatch-default-handler)
    'set-default-handler!)
   handler))

;;;; Handler set implementations

(define (simple-handler-set arity default-handler)
  (declare (ignore arity))
  (let ((rules '()))

    (define (get-handler . args)
      (let loop ((rules rules))
	(if (pair? rules)
	    (if (predicates-match? (cdar rules) args)
		(caar rules)
		(loop (cdr rules)))
	    default-handler)))

    (define (set-handler! predicates handler)
      (let ((rule
             (find (lambda (rule)
                     (equal? (cdr rule) predicates))
                   rules)))
        (if rule
            (let ((handler* (car rule)))
              (if handler
                  (set-car! rule handler)
                  (set! rules (delq rule rules)))
              handler*)
            (begin
              (if handler
                  (set! rules
                        (cons (cons handler predicates)
                              rules)))
              #f))))

    (define (get-default-handler)
      default-handler)

    (define (set-default-handler! handler)
      (set! default-handler handler)
      unspecific)

    (lambda (operator)
      (case operator
	((get-handler) get-handler)
	((set-handler!) set-handler!)
        ((get-default-handler) get-default-handler)
        ((set-default-handler!) set-default-handler!)
	((get-rules) (lambda () rules))
	(else (error "Unknown operator:" operator))))))

(define (predicates-match? predicates args)
  (let loop ((predicates predicates) (args args))
    (or (not (pair? predicates))
	(and ((car predicates) (car args))
	     (loop (cdr predicates) (cdr args))))))

(define (make-subsetting-handler-set make-effective-handler)
  (lambda (arity default-handler)
    (let* ((delegate (simple-handler-set arity default-handler))
	   (delegate-get-rules (delegate 'get-rules))
	   (delegate-get-default-handler (delegate 'get-default-handler)))

      (define (get-handler . args)
        (let ((matching
	       (let loop ((rules (delegate-get-rules)) (matching '()))
		 (if (pair? rules)
		     (loop (cdr rules)
			   (if (predicates-match? (cdar rules) args)
			       (cons (car rules) matching)
			       matching))
		     matching))))
	  (make-effective-handler (map car (sort matching rule<?))
				  delegate-get-default-handler)))

      (lambda (operator)
        (case operator
          ((get-handler) get-handler)
          (else (delegate operator)))))))

(define (rule<? r1 r2)
  (let loop ((ps1 (cdr r1)) (ps2 (cdr r2)))
    (and (pair? ps1)
	 (cond ((eqv? (car ps1) (car ps2)) (loop (cdr ps1) (cdr ps2)))
	       ((predicate<= (car ps1) (car ps2)) #t)
	       ((predicate<= (car ps2) (car ps1)) #f)
	       (else (loop (cdr ps1) (cdr ps2)))))))

(define most-specific-handler-set
  (make-subsetting-handler-set
   (lambda (handlers get-default-handler)
     (if (pair? handlers)
	 (car handlers)
	 (get-default-handler)))))

(define chaining-handler-set
  (make-subsetting-handler-set
   (lambda (handlers get-default-handler)
     (let loop ((handlers handlers))
       (if (pair? handlers)
           (lambda args
             (apply (car handlers)
                    (loop (cdr handlers))
                    args))
           (get-default-handler))))))

(define (cached-most-specific-handler-set arity default-handler)
  (cached-handler-set arity
		      (most-specific-handler-set arity default-handler)
		      object->dispatch-tag))

(define (cached-chaining-handler-set arity default-handler)
  (cached-handler-set arity
		      (chaining-handler-set arity default-handler)
		      object->dispatch-tag))

(define (cached-handler-set arity delegate get-key)
  (let ((cache (new-cache (procedure-arity-min arity)))
	(nmin (procedure-arity-min arity))
	(delegate-get-handler (delegate 'get-handler))
	(delegate-set-handler! (delegate 'set-handler!))
	(delegate-set-default-handler! (delegate 'set-default-handler!)))

    (define get-handler
      (case (and (eqv? nmin (procedure-arity-max arity)) nmin)
	((1)
	 (lambda (a1)
	   (or (probe-cache-1 cache (get-key a1))
	       (handle-cache-miss (list a1)))))
	((2)
	 (lambda (a1 a2)
	   (or (probe-cache-2 cache (get-key a1) (get-key a2))
	       (handle-cache-miss (list a1 a2)))))
	((3)
	 (lambda (a1 a2 a3)
	   (or (probe-cache-3 cache (get-key a1) (get-key a2) (get-key a3))
	       (handle-cache-miss (list a1 a2 a3)))))
	((4)
	 (lambda (a1 a2 a3 a4)
	   (or (probe-cache-4 cache (get-key a1) (get-key a2) (get-key a3)
			      (get-key a4))
	       (handle-cache-miss (list a1 a2 a3 a4)))))
	(else
	 (lambda args
	   (or (probe-cache cache (compute-tags args))
	       (handle-cache-miss args))))))

    (define (set-handler! predicates handler)
      (clear-cache!)
      (delegate-set-handler! predicates handler))

    (define (set-default-handler! handler)
      (clear-cache!)
      (delegate-set-default-handler! handler))

    (define (handle-cache-miss args)
      (let ((tags (compute-tags args))
	    (handler (apply delegate-get-handler args)))
	(without-interruption
	 (lambda ()
	   (set! cache (fill-cache cache tags handler))
	   unspecific))
	handler))

    (define (compute-tags args)
      (let ((p (list 'tags)))
	(let loop ((n nmin) (args* args) (p p))
	  (if (fix:> n 0)
	      (begin
		(if (not (pair? args*))
		    (error:wrong-number-of-arguments get-handler arity args))
		(let ((p* (list (get-key (car args*)))))
		  (set-cdr! p p*)
		  (loop (fix:- n 1) (cdr args*) p*)))))
	(cdr p)))

    (define (clear-cache!)
      (without-interruption
       (lambda ()
	 (set! cache (new-cache nmin))
	 unspecific)))

    (lambda (operator)
      (case operator
        ((get-handler) get-handler)
	((set-handler!) set-handler!)
	((set-default-handler!) set-default-handler!)
        (else (delegate operator))))))