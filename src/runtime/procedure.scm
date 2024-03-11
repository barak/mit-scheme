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

;;;; Microcode Procedures
;;; package: (runtime procedure)

(declare (usual-integrations))

;;;; Types

(define compound-procedure?
  (disjoin-types simple-procedure?
		 extended-procedure?))

(define compiled-procedure?
  (restrict-type compiled-entry-address?
    (lambda (entry)
      (eq? 0 (system-triple-first (compiled-entry-kind entry))))))

(define compiled-closure?
  (restrict-type compiled-procedure?
    (lambda (entry)
      (compiled-code-block/manifest-closure?
	(compiled-code-address->block entry)))))

(define procedure? %any-object?)
(define thunk? %any-object?)
(define unary-procedure? %any-object?)
(define binary-procedure?)
(define simple-arity?)
(define general-arity?)
(define procedure-arity?)
(seq:after-record 'add-action!
  (lambda ()
    (set! procedure?
	  (disjoin-types primitive-procedure?
			 compound-procedure?
			 compiled-procedure?
			 apply-hook?
			 entity?
			 applicable-record?))
    (set! thunk?
	  (restrict-type procedure?
	    (lambda (proc)
	      (procedure-arity-valid? proc 0))))
    (set! unary-procedure?
	  (restrict-type procedure?
	    (lambda (proc)
	      (procedure-arity-valid? proc 1))))
    (set! binary-procedure?
	  (restrict-type procedure?
	    (lambda (proc)
	      (procedure-arity-valid? proc 2))))
    (set! simple-arity?
	  non-negative-fixnum?)
    (set! general-arity?
	  (restrict-type pair?
	    (lambda (p)
	      (and (non-negative-fixnum? (car p))
		   (if (cdr p)
		       (and (non-negative-fixnum? (cdr p))
			    (fx>=? (cdr p) (car p)))
		       #t)))))
    (set! procedure-arity?
	  (disjoin-types simple-arity? general-arity?))
    unspecific))

;;;; Generic Procedures

(define (procedure-lambda procedure)
  (discriminate-procedure procedure
			  (lambda (procedure) procedure #f)
			  %compound-procedure-lambda
			  compiled-procedure/lambda
			  'procedure-lambda))

(define (procedure-environment procedure)
  (discriminate-procedure procedure
			  (lambda (procedure)
			    (error:bad-range-argument procedure
						      'procedure-environment))
			  %compound-procedure-environment
			  compiled-procedure/environment
			  'procedure-environment))

(define (procedure-components procedure receiver)
  (discriminate-procedure procedure
			  (lambda (procedure)
			    (error:bad-range-argument procedure
						      'procedure-components))
			  (lambda (procedure)
			    (receiver
			     (%compound-procedure-lambda procedure)
			     (%compound-procedure-environment procedure)))
			  (lambda (procedure)
			    (receiver
			     (compiled-procedure/lambda procedure)
			     (compiled-procedure/environment procedure)))
			  'procedure-components))

(declare (integrate-operator discriminate-procedure))
(define (discriminate-procedure procedure if-primitive if-compound if-compiled
				caller)
  (declare (integrate if-primitive if-compound if-compiled caller))
  (let ((procedure* (skip-entities procedure)))
    (cond ((primitive-procedure? procedure*) (if-primitive procedure*))
	  ((compound-procedure? procedure*) (if-compound procedure*))
	  ((compiled-procedure? procedure*) (if-compiled procedure*))
	  (else (error:wrong-type-argument procedure "procedure" caller)))))

(define (skip-entities object)
  (cond ((%entity? object)
	 (skip-entities (%entity-procedure object)))
	((%apply-hook? object)
	 (skip-entities (%apply-hook-procedure object)))
	((applicable-record? object)
	 (skip-entities (record-applicator object)))
	(else
	 object)))

(define (procedure-arity procedure)
  (define (loop p)
    (cond ((primitive-procedure? p)
	   (let ((arity ((ucode-primitive primitive-procedure-arity) p)))
	     (if (fix:< arity 0)
		 (cons 0 #f)
		 (cons arity arity))))
	  ((compound-procedure? p)
	   (scode-lambda-arity (%compound-procedure-lambda p)))
	  ((compiled-procedure? p)
	   (let ((info (compiled-entry-kind p)))
	     ;; max = (-1)^tail? * (1 + req + opt + tail?)
	     ;; min = (1 + req)
	     (cons (fix:- (system-triple-second info) 1)
		   (let ((max (system-triple-third info)))
		     (and (fix:>= max 0)
			  (fix:- max 1))))))
	  ((%entity? p)
	   (let ((p* (%entity-procedure p)))
	     (or (%arity-1 (loop p*))
		 (error "Illegal arity for entity:" p*))))
	  ((%apply-hook? p)
	   (loop (%apply-hook-procedure p)))
	  ((applicable-record? p)
	   (let ((p* (record-applicator p)))
	     (or (%arity-1 (loop p*))
		 (error "Illegal arity for record applicator:" p*))))
	  (else
	   (error:not-a procedure? procedure 'procedure-arity))))

  (define (%arity-1 arity)
    (let ((min (car arity))
	  (max (cdr arity)))
      (and (or (not max)
	       (fix:> max 0))
	   (cons (if (fix:> min 0) (fix:- min 1) 0)
		 (and max (fix:- max 1))))))

  (loop procedure))

;; Here because it's needed during cold load for interpreted code.
(define (scode-lambda-arity l)
  (cond ((object-type? (ucode-type lambda) l)
	 (let ((min (fix:- (vector-length (system-pair-cdr l)) 1)))
	   (cons min min)))
	((object-type? (ucode-type extended-lambda) l)
	 (let ((arity (object-datum (system-triple-third l))))
	   (let ((n-required (fix:and (fix:lsh arity -8) #xff))
		 (n-optional (fix:and arity #xff)))
	     (cond ((fix:= 1 (fix:lsh arity -16))
		    (cons n-required #f))
		   ((fix:> n-optional 0)
		    (cons n-required (fix:+ n-required n-optional)))
		   (else
		    (cons n-required n-required))))))
	(else
	 (error:not-a scode-lambda? l 'scode-lambda-arity))))

(define (procedure-arity-valid? procedure arity)
  (procedure-arity<= arity (procedure-arity procedure)))

(define-integrable (procedure-of-arity? object arity)
  (and (procedure? object)
       (procedure-arity-valid? object arity)))

(define (guarantee-procedure-of-arity object arity caller)
  (guarantee procedure? object caller)
  (if (not (procedure-arity-valid? object arity))
      (error:bad-range-argument object caller))
  object)

(define (make-procedure-arity min #!optional max simple-ok?)
  (guarantee index-fixnum? min 'make-procedure-arity)
  (let ((max
	 (if (default-object? max)
	     min
	     (begin
	       (if max
		   (begin
		     (guarantee index-fixnum? max 'make-procedure-arity)
		     (if (not (fix:>= max min))
			 (error:bad-range-argument max
						   'make-procedure-arity))))
	       max))))
    (if (and (eqv? min max)
	     (if (default-object? simple-ok?) #f simple-ok?))
	min
	(cons min max))))

(define (procedure-arity-min arity)
  (cond ((simple-arity? arity) arity)
	((general-arity? arity) (car arity))
	(else (error:not-a procedure-arity? arity 'procedure-arity-min))))

(define (procedure-arity-max arity)
  (cond ((simple-arity? arity) arity)
	((general-arity? arity) (cdr arity))
	(else (error:not-a procedure-arity? arity 'procedure-arity-max))))

(define (procedure-arity<= arity1 arity2)
  (and (fix:<= (procedure-arity-min arity2)
	       (procedure-arity-min arity1))
       (or (not (procedure-arity-max arity2))
	   (and (procedure-arity-max arity1)
		(fix:<= (procedure-arity-max arity1)
			(procedure-arity-max arity2))))))

(define (procedure-arity-intersection a1 a2)
  (make-procedure-arity (fix:max (procedure-arity-min a1)
				 (procedure-arity-min a2))
                        (let ((m1 (procedure-arity-max a1))
                              (m2 (procedure-arity-max a2)))
			  (if m1
			      (if m2 (fix:min m1 m2) m1)
			      m2))))

;;;; Interpreted Procedures

(define-integrable (%primitive-procedure-name procedure)
  (intern ((ucode-primitive get-primitive-name) procedure)))

(define-integrable (%primitive-procedure-implemented? procedure)
  ((ucode-primitive get-primitive-address)
   (%primitive-procedure-name procedure)
   #f))

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
  (guarantee primitive-procedure? procedure 'primitive-procedure-name)
  (%primitive-procedure-name procedure))

(define (implemented-primitive-procedure? procedure)
  (guarantee primitive-procedure? procedure 'implemented-primitive-procedure?)
  (%primitive-procedure-implemented? procedure))

;;;; Compiled Procedures

(define (compiled-procedure-frame-size procedure)
  (guarantee compiled-procedure? procedure 'compiled-procedure-frame-size)
  (let ((max (system-triple-third (compiled-entry-kind procedure))))
    ;; max = (-1)^tail? * (1 + req + opt + tail?)
    ;; frame = req + opt + tail?
    (if (< max 0)
	(- -1 max)
	(- max 1))))

(define-integrable compiled-entry-kind
  (ucode-primitive compiled-entry-kind 1))

(define (compiled-closure->entry closure)
  (guarantee compiled-closure? closure 'compiled-closure->entry)
  ((ucode-primitive compiled-closure->entry 1) closure))

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

(define (make-entity procedure extra)
  (guarantee procedure? procedure 'make-entity)
  (%make-entity procedure extra))

(define (entity-procedure entity)
  (guarantee entity? entity 'entity-procedure)
  (%entity-procedure entity))

(define (entity-extra entity)
  (guarantee entity? entity 'entity-extra)
  (%entity-extra entity))

(define (set-entity-procedure! entity procedure)
  (guarantee entity? entity 'set-entity-procedure!)
  (if (procedure-chains-to? procedure entity)
      (error:bad-range-argument procedure 'set-entity-procedure!))
  (%set-entity-procedure! entity procedure))

(define (set-entity-extra! entity extra)
  (guarantee entity? entity 'set-entity-extra!)
  (%set-entity-extra! entity extra))

(define (make-apply-hook procedure extra)
  (guarantee procedure? procedure 'make-apply-hook)
  (%make-apply-hook procedure extra))

(define (apply-hook-procedure apply-hook)
  (guarantee apply-hook? apply-hook 'apply-hook-procedure)
  (%apply-hook-procedure apply-hook))

(define (apply-hook-extra apply-hook)
  (guarantee apply-hook? apply-hook 'apply-hook-extra)
  (%apply-hook-extra apply-hook))

(define (set-apply-hook-procedure! apply-hook procedure)
  (guarantee apply-hook? apply-hook 'set-apply-hook-procedure!)
  (if (procedure-chains-to? procedure apply-hook)
      (error:bad-range-argument procedure 'set-apply-hook-procedure!))
  (%set-apply-hook-procedure! apply-hook procedure))

(define (set-apply-hook-extra! apply-hook extra)
  (guarantee apply-hook? apply-hook 'set-apply-hook-extra!)
  (%set-apply-hook-extra! apply-hook extra))

;;;; Arity dispatched entities

(define (make-arity-dispatched-procedure default . dispatched-cases)
  ;; DISPATCHED-CASES are the procedures to invoke for 0, 1, 2 etc
  ;; arguments, or #F if the DEFAULT is to be used.  The DEFAULT has a
  ;; SELF argument.
  (make-entity (or default
		   (lambda args
		     (error "Unsupported arguments:" args)))
	       (list->vector
		(cons arity-dispatcher-tag
		      dispatched-cases))))

(define arity-dispatched-procedure?
  (restrict-type entity?
    (lambda (entity)
      (let ((extra (entity-extra entity)))
	(and (vector? extra)
	     (fxpositive? (vector-length extra))
	     (eq? arity-dispatcher-tag (vector-ref extra 0)))))))

(define-integrable arity-dispatcher-tag
  '|#[(microcode)arity-dispatcher-tag]|)

(seq:after-microcode-tables 'add-action!
  (lambda ()
    (set-fixed-objects-item! 'arity-dispatcher-tag arity-dispatcher-tag)))

(define (procedure-chains-to? p1 p2)
  (let loop ((p1 p1))
    (if (eq? p1 p2)
	#t
	(cond ((%entity? p1)
	       (if (arity-dispatched-procedure? p1)
		   (let ((v (%entity-extra p1)))
		     (let ((n (vector-length v)))
		       (let per-arity ((i 1))
			 (if (< i n)
			     (if (let ((p (vector-ref v i)))
				   (and p
					(loop p)))
				 #t
				 (per-arity (fix:+ i 1)))
			     #f))))
		   (loop (%entity-procedure p1))))
	      ((%apply-hook? p1) (loop (%apply-hook-procedure p1)))
	      ((applicable-record? p1) (loop (record-applicator p1)))
	      (else #f)))))