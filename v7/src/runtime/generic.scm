#| -*-Scheme-*-

$Id: generic.scm,v 1.12 2005/04/16 04:05:18 cph Exp $

Copyright 1996,2003,2005 Massachusetts Institute of Technology

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
    (guarantee-procedure-arity arity 'MAKE-GENERIC-PROCEDURE)
    (if (not (fix:> (procedure-arity-min arity) 0))
	(error:bad-range-argument arity 'MAKE-GENERIC-PROCEDURE))
    (guarantee-generator generator 'MAKE-GENERIC-PROCEDURE)
    (let ((record
	   (make-generic-record (or tag standard-generic-procedure-tag)
				(procedure-arity-min arity)
				(procedure-arity-max arity)
				generator
				name
				(new-cache (procedure-arity-min arity)))))
      (let ((generic (compute-apply-generic record)))
	(set-generic-record/procedure! record generic)
	(eqht/put! generic-procedure-records generic record)
	generic))))

(define-structure (generic-record
		   (conc-name generic-record/)
		   (constructor make-generic-record
				(tag arity-min arity-max generator name
				     cache)))
  (tag #f read-only #t)
  (arity-min #f read-only #t)
  (arity-max #f read-only #t)
  (generator #f)
  (name #f read-only #t)
  cache
  procedure)

(define (generic-procedure? object)
  (if (eqht/get generic-procedure-records object #f) #t #f))

(define (generic-procedure-arity generic)
  (let ((record
	 (guarantee-generic-procedure generic 'GENERIC-PROCEDURE-ARITY)))
    (make-procedure-arity (generic-record/arity-min record)
			  (generic-record/arity-max record))))

(define (generic-procedure-arity-min generic)
  (generic-record/arity-min
   (guarantee-generic-procedure generic 'GENERIC-PROCEDURE-ARITY-MIN)))

(define (generic-procedure-arity-max generic)
  (generic-record/arity-max
   (guarantee-generic-procedure generic 'GENERIC-PROCEDURE-ARITY-MAX)))

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
			     (new-cache (generic-record/arity-min record))))

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
  (let ((arity (generic-record/arity-min record)))
    (if (and (eqv? arity (generic-record/arity-max record))
	     (fix:<= arity 4))
	(case arity
	  ((1) (apply-generic-1 record))
	  ((2) (apply-generic-2 record))
	  ((3) (apply-generic-3 record))
	  (else (apply-generic-4 record)))
	(apply-generic record))))

(define (apply-generic record)
  (let ((arity-min (generic-record/arity-min record))
	(arity-max (generic-record/arity-max record)))
    (let ((extra (and arity-max (fix:- arity-max arity-min))))
      (letrec
	  ((generic
	    (lambda args
	      (let loop ((args* args) (n arity-min) (tags '()))
		(if (fix:= n 0)
		    (begin
		      (if (and extra
			       (let loop ((args* args*) (n extra))
				 (and (pair? args*)
				      (or (fix:= n 0)
					  (loop (cdr args*)
						(fix:- n 1))))))
			  (wna args))
		      (let ((procedure
			     (probe-cache (generic-record/cache record)
					  tags)))
			(if procedure
			    (apply procedure args)
			    (compute-method-and-store record args))))
		    (begin
		      (if (not (pair? args*))
			  (wna args))
		      (loop (cdr args*)
			    (fix:- n 1)
			    (cons (dispatch-tag (car args*)) tags)))))))
	   (wna
	    (lambda (args)
	      (error:wrong-number-of-arguments generic
					       (make-procedure-arity arity-min
								     arity-max)
					       args))))
	generic))))

(define (generic-procedure-applicable? procedure arguments)
  (let ((record
	 (guarantee-generic-procedure procedure
				      'GENERIC-PROCEDURE-APPLICABLE?))
	(tags (map dispatch-tag arguments)))
    (let ((generator (generic-record/generator record))
	  (arity-min (generic-record/arity-min record))
	  (arity-max (generic-record/arity-max record))
	  (n-args (length tags)))
      (and generator
	   (if (fix:= n-args arity-min)
	       (generator procedure tags)
	       (and (fix:> n-args arity-min)
		    (or (not arity-max)
			(fix:<= n-args arity-max))
		    (generator procedure (list-head tags arity-min))))))))

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
  (let ((tags
	 (let ((p (list 'TAGS)))
	   (do ((args args (cdr args))
		(p p (cdr p)))
	       ((not (pair? args)))
	     (set-cdr! p (list (dispatch-tag (car args)))))
	   (cdr p))))
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
      (if (vector-ref microcode-type-tag-table (object-type object))
	  (vector-ref microcode-type-tag-table (object-type object))
	  ((vector-ref microcode-type-method-table (object-type object))
	   object))))

(define (make-built-in-tag names)
  (let ((tags (map built-in-dispatch-tag names)))
    (if (there-exists? tags (lambda (tag) tag))
	(let ((tag (car tags)))
	  (if (not (and (for-all? (cdr tags)
			  (lambda (tag*)
			    (eq? tag* tag)))
			(let ((names* (dispatch-tag-contents tag)))
			  (and (for-all? names
				 (lambda (name)
				   (memq name names*)))
			       (for-all? names*
				 (lambda (name)
				   (memq name names)))))))
	      (error "Illegal built-in tag redefinition:" names))
	  tag)
	(let ((tag (make-dispatch-tag (list-copy names))))
	  (set! built-in-tags (cons tag built-in-tags))
	  tag))))

(define (built-in-dispatch-tags)
  (list-copy built-in-tags))

(define (built-in-dispatch-tag name)
  (find-matching-item built-in-tags
    (lambda (tag)
      (memq name (dispatch-tag-contents tag)))))

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
(define built-in-tags)
(define microcode-type-tag-table)
(define microcode-type-method-table)

(define (initialize-generic-procedures!)
  (set! standard-generic-procedure-tag
	(make-dispatch-tag 'STANDARD-GENERIC-PROCEDURE))
  (set! generic-procedure-records (make-eqht))

  ;; Initialize the built-in tag tables.
  (set! built-in-tags '())
  (set! microcode-type-tag-table
	(make-initialized-vector (microcode-type/code-limit)
	  (lambda (code)
	    (make-built-in-tag
	     (let ((names (microcode-type/code->names code)))
	       (if (pair? names)
		   names
		   '(OBJECT)))))))
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
     (let ((procedure-tag (make-built-in-tag '(COMPILED-PROCEDURE)))
	   (return-address-tag (make-built-in-tag '(COMPILED-RETURN-ADDRESS)))
	   (expression-tag (make-built-in-tag '(COMPILED-EXPRESSION))))
       (lambda (default-tag)
	 (lambda (object)
	   (case (system-hunk3-cxr0
		  ((ucode-primitive compiled-entry-kind 1) object))
	     ((0) (maybe-generic object procedure-tag))
	     ((1) return-address-tag)
	     ((2) expression-tag)
	     (else default-tag))))))
    (let ((boolean-tag (make-built-in-tag '(BOOLEAN))))
      (assign-type 'FALSE
		   (lambda (default-tag)
		     (lambda (object)
		       (if (eq? object #f)
			   boolean-tag
			   default-tag))))
      (assign-type 'CONSTANT
		   (let ((null-tag (make-built-in-tag '(NULL)))
			 (eof-tag (make-built-in-tag '(EOF)))
			 (default-tag (make-built-in-tag '(DEFAULT)))
			 (keyword-tag (make-built-in-tag '(LAMBDA-KEYWORD))))
		     (lambda (constant-tag)
		       (lambda (object)
			 (cond ((eq? object #t) boolean-tag)
			       ((null? object) null-tag)
			       ((eof-object? object) eof-tag)
			       ((default-object? object) default-tag)
			       ((memq object '(#!optional #!rest #!key #!aux))
				keyword-tag)
			       (else constant-tag)))))))
    (assign-type 'FLONUM
		 (let ((flonum-vector-tag
			(make-built-in-tag '(FLONUM-VECTOR))))
		   (lambda (default-tag)
		     (lambda (object)
		       (if (fix:= 2 (system-vector-length object))
			   default-tag
			   flonum-vector-tag)))))
    (assign-type 'RECORD
		 (let ((dt-tag (make-built-in-tag '(DISPATCH-TAG))))
		   (lambda (default-tag)
		     (lambda (object)
		       (if (eq? dispatch-tag-marker (%record-ref object 0))
			   dt-tag
			   default-tag)))))))