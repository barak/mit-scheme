#| -*-Scheme-*-

$Id: generic.scm,v 1.6 2003/03/10 20:53:34 cph Exp $

Copyright 1996,2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Generic Procedures

(declare (usual-integrations)
	 (integrate-external "gentag" "gencache"))

;;;; Generic Procedures

(define (make-generic-procedure arity #!optional name tag generator)
  (let ((name (if (default-object? name) #f name))
	(tag (if (default-object? tag) #f tag))
	(generator (if (default-object? generator) #f generator)))
    (if (and name (not (symbol? name)))
	(error:wrong-type-argument name "symbol" 'MAKE-GENERIC-PROCEDURE))
    (if tag (guarantee-dispatch-tag tag 'MAKE-GENERIC-PROCEDURE))
    (if (not (or (and (exact-integer? arity)
		      (> arity 0))
		 (and (pair? arity)
		      (exact-integer? (car arity))
		      (> (car arity) 0)
		      (or (not (cdr arity))
			  (and (exact-integer? (cdr arity))
			       (>= (cdr arity) (car arity)))))))
	(error:wrong-type-argument arity "arity"
				   'MAKE-GENERIC-PROCEDURE))
    (guarantee-generator generator 'MAKE-GENERIC-PROCEDURE)
    (let ((record
	   (make-generic-record (or tag standard-generic-procedure-tag)
				(if (and (pair? arity)
					 (eqv? (car arity) (cdr arity)))
				    (car arity)
				    arity)
				generator
				name
				(new-cache
				 (if (pair? arity)
				     (car arity)
				     arity)))))
      (let ((generic (compute-apply-generic record)))
	(set-generic-record/procedure! record generic)
	(eqht/put! generic-procedure-records generic record)
	generic))))

(define-structure (generic-record
		   (conc-name generic-record/)
		   (constructor make-generic-record
				(tag arity generator name cache)))
  (tag #f read-only #t)
  (arity #f read-only #t)
  (generator #f)
  (name #f read-only #t)
  cache
  procedure)

(define (generic-record/min-arity record)
  (arity-min (generic-record/arity record)))

(define (generic-record/max-arity record)
  (arity-max (generic-record/arity record)))

(define (arity-min arity)
  (if (pair? arity) (car arity) arity))

(define (arity-max arity)
  (if (pair? arity) (cdr arity) arity))

(define (generic-procedure? object)
  (if (eqht/get generic-procedure-records object #f) #t #f))

(define (generic-procedure-arity generic)
  (generic-record/arity
   (guarantee-generic-procedure generic 'GENERIC-PROCEDURE-ARITY)))

(define (generic-procedure-name generic)
  (generic-record/name
   (guarantee-generic-procedure generic 'GENERIC-PROCEDURE-NAME)))

(define (generic-procedure-generator generic)
  (generic-record/generator
   (guarantee-generic-procedure generic 'GENERIC-PROCEDURE-GENERATOR)))

(define (set-generic-procedure-generator! generic generator)
  (let ((record
	 (guarantee-generic-procedure generic
				      'SET-GENERIC-PROCEDURE-GENERATOR!)))
    (guarantee-generator generator 'SET-GENERIC-PROCEDURE-GENERATOR!)
    (without-interrupts
     (lambda ()
       (set-generic-record/generator! record generator)
       (%reset-generic-procedure-cache! record)))))

(define (purge-generic-procedure-cache generic #!optional filter)
  (let ((operator
	 (if (or (default-object? filter)
		 (eq? 'ALL-ENTRIES filter))
	     (lambda (generic record)
	       generic
	       (%reset-generic-procedure-cache! record))
	     (lambda (generic record)
	       (%purge-generic-procedure-cache! generic record filter)))))
    (if (eq? 'ALL-PROCEDURES generic)
	(eqht/for-each generic-procedure-records operator)
	(operator
	 generic
	 (guarantee-generic-procedure generic
				      'PURGE-GENERIC-PROCEDURE-CACHE)))))

(define (%reset-generic-procedure-cache! record)
  (set-generic-record/cache! record
			     (new-cache (generic-record/min-arity record))))

(define (%purge-generic-procedure-cache! generic record filter)
  ;; This might have interrupts locked for a long time, and thus is an
  ;; argument for using something like a semaphore to control access.
  (without-interrupts
   (lambda ()
     (set-generic-record/cache!
      record
      (purge-cache-entries (generic-record/cache record)
			   (lambda (tags) (filter generic tags)))))))

(define (guarantee-generic-procedure generic caller)
  (or (eqht/get generic-procedure-records generic #f)
      (error:wrong-type-argument generic "generic procedure" caller)))

(define (guarantee-generator generator caller)
  (if (not (or (not generator)
	       (and (procedure? generator)
		    (procedure-arity-valid? generator 2))))
      (error:wrong-type-argument generator
				 "generic procedure generator"
				 caller)))

;;;; Generic Procedure Application

(define (compute-apply-generic record)
  (let ((arity (generic-record/arity record)))
    (cond ((pair? arity) (apply-generic record))
	  ((= 1 arity) (apply-generic-1 record))
	  ((= 2 arity) (apply-generic-2 record))
	  ((= 3 arity) (apply-generic-3 record))
	  ((= 4 arity) (apply-generic-4 record))
	  (else (apply-generic record)))))

(define (apply-generic record)
  (let ((min-arity (generic-record/min-arity record))
	(max-arity (generic-record/max-arity record)))
    (let ((extra (and max-arity (- max-arity min-arity))))
      (letrec
	  ((generic
	    (lambda args
	      (let loop ((args* args) (n min-arity) (tags '()))
		(if (fix:= n 0)
		    (begin
		      (if (and extra
			       (let loop ((args* args*) (n extra))
				 (and (not (null? args*))
				      (or (fix:= n 0)
					  (loop (cdr args*)
						(fix:- n 1))))))
			  (wna args))
		      (let ((procedure
			     (probe-cache (generic-record/cache record) tags)))
			(if procedure
			    (apply procedure args)
			    (compute-method-and-store record args))))
		    (begin
		      (if (null? args*)
			  (wna args))
		      (loop (cdr args*)
			    (fix:- n 1)
			    (cons (dispatch-tag (car args*)) tags)))))))
	   (wna
	    (lambda (args)
	      (error:wrong-number-of-arguments generic
					       (generic-record/arity record)
					       args))))
	generic))))

(define (generic-procedure-applicable? procedure arguments)
  (let ((record
	 (guarantee-generic-procedure procedure
				      'GENERIC-PROCEDURE-APPLICABLE?))
	(tags (map dispatch-tag arguments)))
    (let ((generator (generic-record/generator record))
	  (arity (generic-record/arity record))
	  (n-args (length tags)))
      (and generator
	   (if (pair? arity)
	       (let ((min-arity (arity-min arity))
		     (max-arity (arity-max arity)))
		 (if (fix:= n-args min-arity)
		     (generator procedure tags)
		     (and (fix:> n-args min-arity)
			  (or (not max-arity)
			      (fix:<= n-args max-arity))
			  (generator procedure (list-head tags min-arity)))))
	       (and (fix:= arity n-args)
		    (generator procedure tags)))))))

(define (apply-generic-1 record)
  (lambda (a1)
    (declare (integrate-operator dispatch-tag))
    (let ((procedure
	   (probe-cache-1 (generic-record/cache record)
			  (dispatch-tag a1))))
      (if procedure
	  (procedure a1)
	  (compute-method-and-store record (list a1))))))

(define (apply-generic-2 record)
  (lambda (a1 a2)
    (declare (integrate-operator dispatch-tag))
    (let ((procedure
	   (probe-cache-2 (generic-record/cache record)
			  (dispatch-tag a1)
			  (dispatch-tag a2))))
      (if procedure
	  (procedure a1 a2)
	  (compute-method-and-store record (list a1 a2))))))

(define (apply-generic-3 record)
  (lambda (a1 a2 a3)
    (declare (integrate-operator dispatch-tag))
    (let ((procedure
	   (probe-cache-3 (generic-record/cache record)
			  (dispatch-tag a1)
			  (dispatch-tag a2)
			  (dispatch-tag a3))))
      (if procedure
	  (procedure a1 a2 a3)
	  (compute-method-and-store record (list a1 a2 a3))))))

(define (apply-generic-4 record)
  (lambda (a1 a2 a3 a4)
    (declare (integrate-operator dispatch-tag))
    (let ((procedure
	   (probe-cache-4 (generic-record/cache record)
			  (dispatch-tag a1)
			  (dispatch-tag a2)
			  (dispatch-tag a3)
			  (dispatch-tag a4))))
      (if procedure
	  (procedure a1 a2 a3 a4)
	  (compute-method-and-store record (list a1 a2 a3 a4))))))

(define (compute-method-and-store record args)
  (let ((tags (map dispatch-tag args)))
    (let ((procedure
	   (let ((generator (generic-record/generator record))
		 (generic (generic-record/procedure record)))
	     (or (and generator (generator generic tags))
		 (error:no-applicable-methods generic args)))))
      (without-interrupts
       (lambda ()
	 (set-generic-record/cache!
	  record
	  (fill-cache (generic-record/cache record) tags procedure))))
      (apply procedure args))))

;;;; Object Tags

;;; We assume that most new data types will be constructed from tagged
;;; vectors, and therefore we should optimize the path for such
;;; structures as much as possible.

(define (dispatch-tag object)
  (declare (integrate object))
  (declare (ignore-reference-traps (set microcode-type-tag-table
					microcode-type-method-table)))
  (if (and (%record? object)
	   (%record? (%record-ref object 0))
	   (eq? dispatch-tag-marker (%record-ref (%record-ref object 0) 0)))
      (%record-ref object 0)
      (or (vector-ref microcode-type-tag-table (object-type object))
	  ((vector-ref microcode-type-method-table (object-type object))
	   object))))

(define (make-built-in-tag name)
  (let ((entry (assq name built-in-tag-table)))
    (if entry
	(cdr entry)
	(let ((tag (make-dispatch-tag name)))
	  (set! built-in-tag-table (cons (cons name tag) built-in-tag-table))
	  tag))))

(define (built-in-dispatch-tags)
  (map cdr built-in-tag-table))

(define (built-in-dispatch-tag name)
  (let ((entry (assq name built-in-tag-table)))
    (and entry
	 (cdr entry))))

(define condition-type:no-applicable-methods)
(define error:no-applicable-methods)

(define (initialize-conditions!)
  (set! condition-type:no-applicable-methods
	(make-condition-type 'NO-APPLICABLE-METHODS condition-type:error
	    '(OPERATOR OPERANDS)
	  (lambda (condition port)
	    (write-string "No applicable methods for " port)
	    (write (access-condition condition 'OPERATOR) port)
	    (write-string " with these arguments: " port)
	    (write (access-condition condition 'OPERANDS) port)
	    (write-string "." port))))
  (set! error:no-applicable-methods
	(condition-signaller condition-type:no-applicable-methods
			     '(OPERATOR OPERANDS)
			     standard-error-handler))
  unspecific)

;;;; Initialization

(define standard-generic-procedure-tag)
(define generic-procedure-records)
(define built-in-tag-table)
(define microcode-type-tag-table)
(define microcode-type-method-table)

(define (initialize-generic-procedures!)
  (set! standard-generic-procedure-tag
	(make-dispatch-tag 'STANDARD-GENERIC-PROCEDURE))
  (set! generic-procedure-records (make-eqht))

  ;; Initialize the built-in tag tables.
  (set! built-in-tag-table '())
  (set! microcode-type-tag-table
	(make-initialized-vector (microcode-type/code-limit)
	  (lambda (code)
	    (make-built-in-tag
	     (or (microcode-type/code->name code) 'OBJECT)))))
  (set! microcode-type-method-table
	(make-vector (microcode-type/code-limit) #f))
  (let ((assign-type
	 (lambda (name get-method)
	   (let ((code (microcode-type/name->code name)))
	     (vector-set! microcode-type-method-table code
			  (get-method
			   (vector-ref microcode-type-tag-table code)))
	     (vector-set! microcode-type-tag-table code #f)))))
    (define-integrable (maybe-generic object default-tag)
      (let ((record (eqht/get generic-procedure-records object #f)))
	(if record
	    (generic-record/tag record)
	    default-tag)))
    (let ((procedure-type
	   (lambda (default-tag)
	     (lambda (object)
	       (maybe-generic object default-tag)))))
      (assign-type 'EXTENDED-PROCEDURE procedure-type)
      (assign-type 'PROCEDURE procedure-type))
    (assign-type
     'COMPILED-ENTRY
     (let ((procedure-tag (make-built-in-tag 'COMPILED-PROCEDURE))
	   (return-address-tag (make-built-in-tag 'COMPILED-RETURN-ADDRESS))
	   (expression-tag (make-built-in-tag 'COMPILED-EXPRESSION)))
       (lambda (default-tag)
	 (lambda (object)
	   (case (system-hunk3-cxr0
		  ((ucode-primitive compiled-entry-kind 1) object))
	     ((0) (maybe-generic object procedure-tag))
	     ((1) return-address-tag)
	     ((2) expression-tag)
	     (else default-tag))))))
    (let ((boolean-tag (make-built-in-tag 'BOOLEAN)))
      (if (> microcode-id/version 11)
	  (assign-type 'CONSTANT
		       (lambda (default-tag)
			 (lambda (object)
			   (if (or (eq? #f object) (eq? #t object))
			       boolean-tag
			       default-tag))))
	  (begin
	    (assign-type 'FALSE
			 (lambda (default-tag)
			   (lambda (object)
			     (if (eq? #f object)
				 boolean-tag
				 default-tag))))
	    (assign-type 'CONSTANT
			 (lambda (default-tag)
			   (lambda (object)
			     (if (eq? #t object)
				 boolean-tag
				 default-tag)))))))
    (assign-type 'FLONUM
		 (let ((flonum-vector-tag
			(make-built-in-tag 'FLONUM-VECTOR)))
		   (lambda (default-tag)
		     (lambda (object)
		       (if (fix:= 2 (system-vector-length object))
			   default-tag
			   flonum-vector-tag)))))
    (assign-type 'RECORD
		 (let ((dt-tag (make-built-in-tag 'DISPATCH-TAG)))
		   (lambda (default-tag)
		     (lambda (object)
		       (if (eq? dispatch-tag-marker (%record-ref object 0))
			   dt-tag
			   default-tag)))))))