#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Generic dispatch on argument types
;;; package: (runtime type-dispatch)

(declare (usual-integrations))

(define type-dispatcher?
  (refine-type apply-hook?
    (lambda (e)
      (metadata? (%apply-hook-extra e)))))

(define-record-type <metadata>
    (make-metadata name arity handler-set)
    metadata?
  (name metadata-name)
  (arity metadata-arity)
  (handler-set metadata-handler-set))

(define (type-dispatcher-name dispatch)
  (metadata-name (get-metadata dispatch 'type-dispatcher-name)))

(define (type-dispatcher-arity dispatch)
  (metadata-arity (get-metadata dispatch 'type-dispatcher-arity)))

(define (type-dispatcher-rules dispatch)
  (map list-copy
       (((get-handler-set dispatch 'type-dispatcher-rules) 'get-rules))))

(define (get-handler-set dispatch caller)
  (metadata-handler-set (get-metadata dispatch caller)))

(define (define-type-dispatch-handler dispatcher types handler)
  (let ((metadata (get-metadata dispatcher 'define-type-dispatch-handler)))
    (guarantee-list-of type? types 'define-type-dispatch-handler)
    (guarantee-procedure-of-arity handler
				  (metadata-arity metadata)
				  'define-type-dispatch-handler)
    (((metadata-handler-set metadata) 'set-handler!) types handler)))

(define (get-metadata procedure caller)
  (let ((metadata (apply-hook-extra procedure)))
    (if (not (metadata? metadata))
        (error:not-a type-dispatcher? procedure caller))
    metadata))

(define (standard-type-dispatcher name arity #!optional default-handler)
  (make-type-dispatcher name arity default-handler
    (lambda (arity default-handler)
      (handler-set-cached
       (handler-set-subsetting (simple-handler-set arity default-handler)
	 (lambda (handlers get-default-handler)
	   (if (pair? handlers)
	       (car handlers)
	       (get-default-handler))))))))

(define (chaining-type-dispatcher name arity #!optional default-handler)
  (make-type-dispatcher name arity default-handler
    (lambda (arity default-handler)
      (handler-set-cached
       (handler-set-subsetting (simple-handler-set arity default-handler)
	 (lambda (handlers get-default-handler)
	   (let loop ((handlers handlers))
	     (if (pair? handlers)
		 (lambda args
		   (apply (car handlers)
			  (loop (cdr handlers))
			  args))
		 (get-default-handler)))))))))

(define (make-type-dispatcher name arity default-handler make-handler-set)
  (if (not (> (procedure-arity-min arity) 0))
      (error:bad-range-argument arity 'make-type-dispatcher))
  (let ((metadata
	 (make-metadata name
			arity
			(make-handler-set arity
					  (if (default-object? default-handler)
					      (make-default-handler name)
					      default-handler)))))
    (make-apply-hook (make-procedure arity metadata)
		     metadata)))

(define (make-default-handler name)
  (lambda args
    (error "Inapplicable generic procedure:" name args)))

(define (make-procedure arity metadata)
  (let ((get-handler ((metadata-handler-set metadata) 'get-handler)))
    (case (and (eqv? (procedure-arity-min arity) (procedure-arity-max arity))
	       (procedure-arity-min arity))
      ((1)
       (lambda (arg)
	 ((get-handler arg) arg)))
      ((2)
       (lambda (arg1 arg2)
	 ((get-handler arg1 arg2) arg1 arg2)))
      ((3)
       (lambda (arg1 arg2 arg3)
	 ((get-handler arg1 arg2 arg3) arg1 arg2 arg3)))
      ((4)
       (lambda (arg1 arg2 arg3 arg4)
	 ((get-handler arg1 arg2 arg3 arg4) arg1 arg2 arg3 arg4)))
      (else
       (lambda args
	 (apply (apply get-handler args) args))))))

;;;; Handler set implementations

(define (simple-handler-set arity default-handler)
  (let ((rules '()))

    (define (get-handler . args)
      (let ((rule
	     (find (lambda (rule)
		     (types-match? (%rule-types rule) args))
		   rules)))
	(if rule
	    (%rule-handler rule)
	    default-handler)))

    (define (set-handler! types handler)
      (let ((rule
             (find (lambda (rule)
                     (equal? (%rule-types rule) types))
                   rules)))
        (if rule
            (let ((handler* (%rule-handler rule)))
              (if handler
                  (%set-rule-handler! rule handler)
                  (set! rules (delq rule rules)))
              handler*)
            (begin
              (if handler
                  (set! rules
                        (cons (%make-rule handler types)
                              rules)))
              #f))))

    (lambda (operator)
      (case operator
	((get-handler) get-handler)
	((set-handler!) set-handler!)
        ((get-default-handler) (lambda () default-handler))
	((get-rules) (lambda () rules))
	((get-arity) (lambda () arity))
	(else (error "Unknown operator:" operator))))))

(define-integrable %make-rule cons)
(define-integrable %rule-handler car)
(define-integrable %rule-types cdr)
(define-integrable %set-rule-handler! set-car!)

(define (rule<? r1 r2)
  (let loop ((ps1 (%rule-types r1)) (ps2 (%rule-types r2)))
    (and (pair? ps1)
	 (cond ((eqv? (car ps1) (car ps2)) (loop (cdr ps1) (cdr ps2)))
	       ((type<= (car ps1) (car ps2)) #t)
	       ((type<= (car ps2) (car ps1)) #f)
	       (else (loop (cdr ps1) (cdr ps2)))))))

(define (types-match? types args)
  (let loop ((types types) (args args))
    (or (not (pair? types))
	(and ((car types) (car args))
	     (loop (cdr types) (cdr args))))))

(define (handler-set-subsetting delegate make-effective-handler)
  (let ((get-rules (delegate 'get-rules))
	(get-default-handler (delegate 'get-default-handler)))

    (define (get-handler . args)
      (let ((matching
	     (filter (lambda (rule)
		       (types-match? (%rule-types rule) args))
		     (get-rules))))
	(make-effective-handler (map car (sort matching rule<?))
				get-default-handler)))

    (lambda (operator)
      (case operator
	((get-handler) get-handler)
	(else (delegate operator))))))

(define (handler-set-cached delegate)
  (let ((arity ((delegate 'get-arity)))
	(d:get-handler (delegate 'get-handler))
	(d:set-handler! (delegate 'set-handler!)))
    (let ((nmin (procedure-arity-min arity))
	  (nmax (procedure-arity-max arity)))
      (let ((cache (new-cache nmin)))

	(define get-handler
	  (case (and (eqv? nmin nmax) nmin)
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
	       (or (probe-cache cache (compute-types args))
		   (handle-cache-miss args))))))

	(define (set-handler! types handler)
	  (let ((handler* (d:set-handler! types handler)))
	    (if (not handler*)
		(clear-cache!))
	    handler*))

	(define (handle-cache-miss args)
	  (let ((types (compute-types args))
		(handler (apply d:get-handler args)))
	    (without-interruption
	     (lambda ()
	       (set! cache (fill-cache cache types handler))
	       unspecific))
	    handler))

	(define (compute-types args)
	  (let ((p (list 'types)))
	    (let loop ((n nmin) (args* args) (p p))
	      (if (fix:> n 0)
		  (begin
		    (if (not (pair? args*))
			(error:wrong-number-of-arguments
			 get-handler arity args))
		    (let ((p* (list (get-key (car args*)))))
		      (set-cdr! p p*)
		      (loop (fix:- n 1) (cdr args*) p*)))))
	    (cdr p)))

	(define (get-key object)
	  (%apply-hook-extra (object->type object)))

	(define (clear-cache!)
	  (without-interruption
	   (lambda ()
	     (set! cache (new-cache nmin))
	     unspecific)))

	(lambda (operator)
	  (case operator
            ((get-handler) get-handler)
	    ((set-handler!) set-handler!)
            (else (delegate operator))))))))