#| -*-Scheme-*-

$Id: uproc.scm,v 1.12 2002/11/20 19:46:24 cph Exp $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

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

(define (procedure-lambda procedure)
  (discriminate-procedure procedure
			  (lambda (procedure) procedure false)
			  %compound-procedure-lambda
			  compiled-procedure/lambda))

(define (procedure-environment procedure)
  (discriminate-procedure
   procedure
   (lambda (procedure)
     (error "primitive procedures have no closing environment" procedure))
   %compound-procedure-environment
   compiled-procedure/environment))

(define (procedure-components procedure receiver)
  (discriminate-procedure
   procedure
   (lambda (procedure)
     (error "primitive procedures have no components" procedure))
   (lambda (procedure)
     (receiver (%compound-procedure-lambda procedure)
	       (%compound-procedure-environment procedure)))
   (lambda (procedure)
     (receiver (compiled-procedure/lambda procedure)
	       (compiled-procedure/environment procedure)))))

(define (discriminate-procedure procedure if-primitive if-compound if-compiled)
  (let ((procedure* (skip-entities procedure)))
    (cond ((%primitive-procedure? procedure*) (if-primitive procedure*))
	  ((%compound-procedure? procedure*) (if-compound procedure*))
	  ((%compiled-procedure? procedure*) (if-compiled procedure*))
	  (else (error:wrong-type-argument procedure "procedure" #F)))))

(define (skip-entities object)
  (if (%entity? object)
      (skip-entities (if (%entity-is-apply-hook? object)
			 (apply-hook-procedure object)
			 (entity-procedure object)))
      object))

(define (procedure-arity procedure)
  (let loop ((p procedure) (e 0))
    (cond ((%primitive-procedure? p)
	   (let ((arity (primitive-procedure-arity p)))
	     (cond ((negative? arity)
		    (cons 0 false))
		   ((<= e arity)
		    (let ((arity (- arity e)))
		      (cons arity arity)))
		   (else
		    (error "illegal arity for entity" procedure)))))
	  ((%compound-procedure? p)
	   (lambda-components (%compound-procedure-lambda p)
	     (lambda (name required optional rest auxiliary decl body)
	       name auxiliary decl body
	       (let ((r (- (length required) e)))
		 (cond (rest
			(cons (if (negative? r) 0 r) false))
		       ((not (negative? r))
			(cons r (+ r (length optional))))
		       (else
			(error "illegal arity for entity" procedure)))))))
	  ((%compiled-procedure? p)
	   (let ((info (compiled-entry-kind p))
		 (e+1 (1+ e)))
	     ;; max = (-1)^tail? * (1 + req + opt + tail?)
	     ;; min = (1 + req)
	     (let ((min (- (system-hunk3-cxr1 info) e+1))
		   (max (system-hunk3-cxr2 info)))
	       (cond ((negative? max)
		      (cons (if (negative? min) 0 min) false))
		     ((not (negative? min))
		      (cons min (- max e+1)))
		     (else
		      (error "illegal arity for entity" procedure))))))
	  ((%entity? p)
	   (if (%entity-is-apply-hook? p)
	       (loop (apply-hook-procedure p) e)
	       (loop (entity-procedure p) (1+ e))))
	  (else
	   (error:wrong-type-argument procedure "procedure"
				      'PROCEDURE-ARITY)))))

(define (procedure-arity-valid? procedure n-arguments)
  (let ((arity (procedure-arity procedure)))
    (and (<= (car arity) n-arguments)
	 (if (cdr arity)
	     (<= n-arguments (cdr arity))
	     true))))

;;;; Interpreted Procedures

(define-integrable (%primitive-procedure? object)
  (object-type? (ucode-type primitive) object))

(define-integrable (%primitive-procedure-name procedure)
  (intern ((ucode-primitive get-primitive-name) procedure)))

(define-integrable (%primitive-procedure-implemented? procedure)
  ((ucode-primitive get-primitive-address)
   (%primitive-procedure-name procedure)
   false))

(define (primitive-procedure? object)
  (%primitive-procedure? (skip-entities object)))

(define (make-primitive-procedure name #!optional arity)
  (let ((arity (if (default-object? arity) false arity)))
    (let ((result ((ucode-primitive get-primitive-address) name arity)))
      (if (not (or (object-type? (ucode-type primitive) result)
		   (eq? arity true)))
	  (if (false? result)
	      (error "MAKE-PRIMITIVE-PROCEDURE: unknown name" name)
	      (error "MAKE-PRIMITIVE-PROCEDURE: inconsistent arity" name
		     (error-irritant/noise " new:") arity
		     (error-irritant/noise " old:") result)))
      result)))

(define (primitive-procedure-name procedure)
  (%primitive-procedure-name (%primitive-procedure-arg procedure)))

(define (implemented-primitive-procedure? procedure)
  (%primitive-procedure-implemented? (%primitive-procedure-arg procedure)))

(define (%primitive-procedure-arg procedure)
  (let ((procedure* (skip-entities procedure)))
    (if (not (%primitive-procedure? procedure*))
	(error:wrong-type-datum  procedure "primitive procedure"))
    procedure*))

(define-integrable (%compound-procedure? object)
  (or (object-type? (ucode-type procedure) object)
      (object-type? (ucode-type extended-procedure) object)))

(define-integrable (%compound-procedure-lambda procedure)
  (system-pair-car procedure))

(define-integrable (%compound-procedure-environment procedure)
  (system-pair-cdr procedure))

(define (compound-procedure? object)
  (let ((object (skip-entities object)))
    (%compound-procedure? object)))

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
	     (if (negative? max)
		 (- -1 max)
		 (-1+ max))))
	  ((%entity? p)
	   (if (%entity-is-apply-hook? p)
	       (loop (apply-hook-procedure p))
	       (1+ (loop (entity-procedure p)))))
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

(define-integrable (set-entity-procedure! entity procedure)
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
  ;; Ziggy cares about this one.
  (and (object-type? (ucode-type hunk3) extra)
       (eq? apply-hook-tag (system-hunk3-cxr0 extra))))

(define apply-hook-tag
  "apply-hook-tag")

(define-integrable (apply-hook-procedure apply-hook)
  (system-hunk3-cxr1 (entity-extra apply-hook)))

(define-integrable (apply-hook-extra apply-hook)
  (system-hunk3-cxr2 (entity-extra apply-hook)))

(define-integrable (set-apply-hook-procedure! apply-hook procedure)
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
       (< 0 (vector-length (entity-extra object)))
       (eq? (vector-ref (entity-extra object) 0)
	    (fixed-objects-item 'ARITY-DISPATCHER-TAG))))