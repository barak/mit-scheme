#| -*-Scheme-*-

$Id: 383c30660cb73539873be67c7e50f238df438611 $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Microcode Procedures
;;; package: (runtime procedure)

(declare (usual-integrations))

;;;; Generic Procedures

(define (procedure? object)
  (let ((object (skip-entities object)))
    (or (%compound-procedure? object)
	(%primitive-procedure? object)
	(%compiled-procedure? object))))

(define-guarantee procedure "procedure")
(define-guarantee compound-procedure "compound procedure")
(define-guarantee primitive-procedure "primitive procedure")
(define-guarantee compiled-procedure "compiled procedure")

(define (procedure-lambda procedure)
  (discriminate-procedure procedure
			  (lambda (procedure) procedure #f)
			  %compound-procedure-lambda
			  compiled-procedure/lambda
			  'PROCEDURE-LAMBDA))

(define (procedure-environment procedure)
  (discriminate-procedure procedure
			  (lambda (procedure)
			    (error:bad-range-argument procedure
						      'PROCEDURE-ENVIRONMENT))
			  %compound-procedure-environment
			  compiled-procedure/environment
			  'PROCEDURE-ENVIRONMENT))

(define (procedure-components procedure receiver)
  (discriminate-procedure procedure
			  (lambda (procedure)
			    (error:bad-range-argument procedure
						      'PROCEDURE-COMPONENTS))
			  (lambda (procedure)
			    (receiver
			     (%compound-procedure-lambda procedure)
			     (%compound-procedure-environment procedure)))
			  (lambda (procedure)
			    (receiver
			     (compiled-procedure/lambda procedure)
			     (compiled-procedure/environment procedure)))
			  'PROCEDURE-COMPONENTS))

(define (discriminate-procedure procedure if-primitive if-compound if-compiled
				caller)
  (let ((procedure* (skip-entities procedure)))
    (cond ((%primitive-procedure? procedure*) (if-primitive procedure*))
	  ((%compound-procedure? procedure*) (if-compound procedure*))
	  ((%compiled-procedure? procedure*) (if-compiled procedure*))
	  (else (error:wrong-type-argument procedure "procedure" caller)))))

(define (skip-entities object)
  (if (%entity? object)
      (skip-entities (if (%entity-is-apply-hook? object)
			 (apply-hook-procedure object)
			 (entity-procedure object)))
      object))

(define (procedure-arity procedure)
  (let loop ((p procedure) (e 0))
    (cond ((%primitive-procedure? p)
	   (let ((arity ((ucode-primitive primitive-procedure-arity) p)))
	     (cond ((fix:< arity 0)
		    (cons 0 #f))
		   ((fix:<= e arity)
		    (let ((arity (fix:- arity e)))
		      (cons arity arity)))
		   (else
		    (error "Illegal arity for entity:" procedure)))))
	  ((%compound-procedure? p)
	   (lambda-components (%compound-procedure-lambda p)
	     (lambda (name required optional rest auxiliary decl body)
	       name auxiliary decl body
	       (let ((r (fix:- (length required) e)))
		 (cond (rest
			(cons (fix:max 0 r) #f))
		       ((fix:>= r 0)
			(cons r (fix:+ r (length optional))))
		       (else
			(error "Illegal arity for entity:" procedure)))))))
	  ((%compiled-procedure? p)
	   (let ((info (compiled-entry-kind p))
		 (e+1 (fix:+ e 1)))
	     ;; max = (-1)^tail? * (1 + req + opt + tail?)
	     ;; min = (1 + req)
	     (let ((min (fix:- (system-hunk3-cxr1 info) e+1))
		   (max (system-hunk3-cxr2 info)))
	       (cond ((fix:< max 0)
		      (cons (if (fix:< min 0) 0 min) #f))
		     ((fix:>= min 0)
		      (cons min (fix:- max e+1)))
		     (else
		      (error "Illegal arity for entity:" procedure))))))
	  ((%entity? p)
	   (if (%entity-is-apply-hook? p)
	       (loop (apply-hook-procedure p) e)
	       (loop (entity-procedure p) (fix:+ e 1))))
	  (else
	   (error:wrong-type-argument procedure "procedure"
				      'PROCEDURE-ARITY)))))

(define (procedure-arity-valid? procedure n-arguments)
  (guarantee-index-fixnum n-arguments 'PROCEDURE-ARITY-VALID?)
  (let ((arity (procedure-arity procedure)))
    (and (<= (car arity) n-arguments)
	 (or (not (cdr arity))
	     (<= n-arguments (cdr arity))))))

(define (thunk? object)
  (and (procedure? object)
       (procedure-arity-valid? object 0)))

(define-guarantee thunk "thunk")

(define-integrable (procedure-of-arity? object arity)
  (and (procedure? object)
       (procedure-arity-valid? object arity)))

(define (guarantee-procedure-of-arity object arity caller)
  (guarantee-procedure object caller)
  (if (not (procedure-arity-valid? object arity))
      (error:bad-range-argument object caller)))

(define (make-procedure-arity min #!optional max simple-ok?)
  (guarantee-index-fixnum min 'MAKE-PROCEDURE-ARITY)
  (let ((max
	 (if (default-object? max)
	     min
	     (begin
	       (if max
		   (begin
		     (guarantee-index-fixnum max 'MAKE-PROCEDURE-ARITY)
		     (if (not (fix:>= max min))
			 (error:bad-range-argument max
						   'MAKE-PROCEDURE-ARITY))))
	       max))))
    (if (and (eqv? min max)
	     (if (default-object? simple-ok?) #f simple-ok?))
	min
	(cons min max))))

(define (procedure-arity? object)
  (if (simple-arity? object)
      #t
      (general-arity? object)))

(define-guarantee procedure-arity "procedure arity")

(define (procedure-arity-min arity)
  (cond ((simple-arity? arity) arity)
	((general-arity? arity) (car arity))
	(else (error:not-procedure-arity arity 'PROCEDURE-ARITY-MIN))))

(define (procedure-arity-max arity)
  (cond ((simple-arity? arity) arity)
	((general-arity? arity) (cdr arity))
	(else (error:not-procedure-arity arity 'PROCEDURE-ARITY-MAX))))

(define-integrable (simple-arity? object)
  (index-fixnum? object))

(define-integrable (general-arity? object)
  (and (pair? object)
       (index-fixnum? (car object))
       (if (cdr object)
	   (and (index-fixnum? (cdr object))
		(fix:>= (cdr object) (car object)))
	   #t)))

;;;; Interpreted Procedures

(define-integrable (%primitive-procedure? object)
  (object-type? (ucode-type primitive) object))

(define-integrable (%primitive-procedure-name procedure)
  (intern ((ucode-primitive get-primitive-name) procedure)))

(define-integrable (%primitive-procedure-implemented? procedure)
  ((ucode-primitive get-primitive-address)
   (%primitive-procedure-name procedure)
   #f))

(define (primitive-procedure? object)
  (%primitive-procedure? (skip-entities object)))

(define (make-primitive-procedure name #!optional arity)
  (let ((arity (if (default-object? arity) #f arity)))
    (let ((result ((ucode-primitive get-primitive-address) name arity)))
      (if (not (or (object-type? (ucode-type primitive) result)
		   (eq? arity #t)))
	  (if result
	      (error "MAKE-PRIMITIVE-PROCEDURE: inconsistent arity" name
		     (error-irritant/noise " new:") arity
		     (error-irritant/noise " old:") result)
	      (error "MAKE-PRIMITIVE-PROCEDURE: unknown name" name)))
      result)))

(define (primitive-procedure-name procedure)
  (%primitive-procedure-name
   (%primitive-procedure-arg procedure 'PRIMITIVE-PROCEDURE-NAME)))

(define (implemented-primitive-procedure? procedure)
  (%primitive-procedure-implemented?
   (%primitive-procedure-arg procedure 'IMPLEMENTED-PRIMITIVE-PROCEDURE?)))

(define (%primitive-procedure-arg procedure caller)
  (let ((procedure* (skip-entities procedure)))
    (guarantee-primitive-procedure procedure* caller)
    procedure*))

(declare (integrate-operator %compound-procedure?))
(define (%compound-procedure? object)
  (or (object-type? (ucode-type procedure) object)
      (object-type? (ucode-type extended-procedure) object)))

(define-integrable (%compound-procedure-lambda procedure)
  (system-pair-car procedure))

(define-integrable (%compound-procedure-environment procedure)
  (system-pair-cdr procedure))

(define (compound-procedure? object)
  (%compound-procedure? (skip-entities object)))

;;;; Compiled Procedures

(define-integrable (%compiled-procedure? object)
  (and (object-type? (ucode-type compiled-entry) object)
       (eq? 0 (system-hunk3-cxr0 (compiled-entry-kind object)))))

(define-integrable compiled-entry-kind
  (ucode-primitive compiled-entry-kind 1))

(define (compiled-procedure? object)
  (let ((object (skip-entities object)))
    (%compiled-procedure? object)))

(define (compiled-procedure-frame-size procedure)
  (let loop ((p procedure))
    (cond ((%compiled-procedure? p)
	   (let ((max (system-hunk3-cxr2 (compiled-entry-kind p))))
	     ;; max = (-1)^tail? * (1 + req + opt + tail?)
	     ;; frame = req + opt + tail?
	     (if (< max 0)
		 (- -1 max)
		 (- max 1))))
	  ((%entity? p)
	   (if (%entity-is-apply-hook? p)
	       (loop (apply-hook-procedure p))
	       (+ (loop (entity-procedure p)) 1)))
	  (else
	   (error:wrong-type-argument procedure "compiled procedure"
				      'COMPILED-PROCEDURE-FRAME-SIZE)))))

(define (%compiled-closure? object)
  (and (%compiled-procedure? object)
       (compiled-code-block/manifest-closure?
	(compiled-code-address->block object))))

(define %compiled-closure->entry
  (ucode-primitive compiled-closure->entry 1))

(define (compiled-closure? object)
  (let ((object (skip-entities object)))
    (%compiled-closure? object)))

(define (compiled-closure->entry closure)
  (%compiled-closure->entry
   (let ((closure* (skip-entities closure)))
     (if (not (%compiled-closure? closure*))
	 (error:wrong-type-argument closure "compiled closure"
				    'COMPILED-CLOSURE->ENTRY))
     closure*)))

;; In the following two procedures, offset can be #f to support
;; old-style 68020 closures.  When offset is not #f, it works on all
;; architectures.

(define (compiled-closure/ref closure index offset)
  (if (not offset)
      ((ucode-primitive primitive-object-ref 2) closure (+ 2 index))
      ((ucode-primitive primitive-object-ref 2)
       (if (compiled-closure? closure)
	   ((ucode-primitive compiled-code-address->block 1) closure)
	   ;; Closure may also be a vector in this case.
	   closure)
       (+ index offset))))

(define-integrable (compiled-closure/set! closure index offset value)
  (if (not offset)
      ((ucode-primitive primitive-object-set! 3) closure (+ 2 index) value)
      ((ucode-primitive primitive-object-set! 3)
       ((ucode-primitive compiled-code-address->block 1)
	closure)
       (+ index offset)
       value))
  unspecific)

;;;; Entities and Apply Hooks

(define-integrable (make-entity procedure extra)
  (system-pair-cons (ucode-type entity) procedure extra))

(define-integrable (%entity? object)
  (object-type? (ucode-type entity) object))

(define (entity? object)
  (and (%entity? object)
       (not (%entity-is-apply-hook? object))))

(define-integrable (entity-procedure entity)
  (system-pair-car entity))

(define-integrable (entity-extra entity)
  (system-pair-cdr entity))

(define (set-entity-procedure! entity procedure)
  (if (procedure-chains-to procedure entity)
      (error:bad-range-argument procedure 'SET-ENTITY-PROCEDURE!))
  (system-pair-set-car! entity procedure))

(define-integrable (set-entity-extra! entity extra)
  (system-pair-set-cdr! entity extra))

(define (make-apply-hook procedure extra)
  (make-entity (lambda (entity . args)
		 (apply (apply-hook-procedure entity) args))
	       (hunk3-cons apply-hook-tag procedure extra)))

(define (apply-hook? object)
  (and (%entity? object)
       (%entity-is-apply-hook? object)))

(define-integrable (%entity-is-apply-hook? object)
  (%entity-extra/apply-hook? (entity-extra object)))

(define (%entity-extra/apply-hook? extra)
  ;; The wabbit cares about this one.
  (and (object-type? (ucode-type hunk3) extra)
       (eq? (system-hunk3-cxr0 extra) apply-hook-tag)))

(define apply-hook-tag
  "apply-hook-tag")

(define-integrable (apply-hook-procedure apply-hook)
  (system-hunk3-cxr1 (entity-extra apply-hook)))

(define-integrable (apply-hook-extra apply-hook)
  (system-hunk3-cxr2 (entity-extra apply-hook)))

(define (set-apply-hook-procedure! apply-hook procedure)
  (if (procedure-chains-to procedure apply-hook)
      (error:bad-range-argument procedure 'SET-APPLY-HOOK-PROCEDURE!))
  (system-hunk3-set-cxr1! (entity-extra apply-hook) procedure))

(define-integrable (set-apply-hook-extra! apply-hook procedure)
  (system-hunk3-set-cxr2! (entity-extra apply-hook) procedure))

;;;; Arity dispatched entities

(define (make-arity-dispatched-procedure default . dispatched-cases)
  ;; DISPATCHED-CASES are the procedures to invoke for 0, 1, 2 etc
  ;; arguments, or #F if the DEFAULT is to be used.  The DEFAULT has a
  ;; SELF argument.
  (make-entity default
	       (list->vector
		(cons (fixed-objects-item 'ARITY-DISPATCHER-TAG)
		      dispatched-cases))))

(define (arity-dispatched-procedure? object)
  (and (%entity? object)
       (vector? (entity-extra object))
       (fix:< 0 (vector-length (entity-extra object)))
       (eq? (vector-ref (entity-extra object) 0)
	    (fixed-objects-item 'ARITY-DISPATCHER-TAG))))

(define (procedure-chains-to p1 p2)
  (let loop ((p1 p1))
    (if (eq? p1 p2)
	#t
	(if (%entity? p1)
	    (cond ((%entity-is-apply-hook? p1)
		   (loop (apply-hook-procedure p1)))
		  ((arity-dispatched-procedure? p1)
		   (let ((v (entity-extra p1)))
		     (let ((n (vector-length v)))
		       (let per-arity ((i 1))
			 (if (< i n)
			     (if (let ((p (vector-ref v i)))
				   (and p
					(loop p)))
				 #t
				 (per-arity (fix:+ i 1)))
			     #f)))))
		  (else
		   (loop (entity-procedure p1))))
	    
	    
	    #f))))