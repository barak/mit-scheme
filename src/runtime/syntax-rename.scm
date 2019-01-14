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

;;;; Syntaxer variable renaming
;;; package: (runtime syntax rename)

;;; The classifier replaces each locally-bound identifier with a
;;; freshly-generated rename, and binds the identifier to a variable item whose
;;; name is the rename.  Since each syntactic environment has its own renamer,
;;; this means that an identifier's rename is governed by what environment it is
;;; closed in.
;;;
;;; Top-level identifiers are never renamed.
;;;
;;; After code generation, in which the program's local-variable identifiers
;;; have all been renamed, there is a post-pass that looks at each of the
;;; locally-bound identifiers to determine whether it is safe to restore it to
;;; its original name.  For each bound identifier in a given frame, we determine
;;; whether it can be restored to its original name by looking for name
;;; collisions.  A name collision occurs when one of the bound identifiers has
;;; an original name that is the same as the original name of one of the free
;;; identifiers.  If there are no name collisions, then it is safe to restore
;;; the original name.  Otherwise the bound identifier is replaced by an
;;; interned symbol with a computed name that's designed to be unique.

(declare (usual-integrations))

(define (make-local-identifier-renamer)
  ((rdb:identifier-renamer (rename-db)) new-identifier))

(define (with-identifier-renaming thunk)
  (parameterize ((rename-db (initial-rename-db)))
    (post-process-output (thunk))))

(define-deferred rename-db
  (make-unsettable-parameter 'unbound))

(define-record-type <rename-db>
    (make-rename-db identifier-renamer lookup-rename)
    rename-db?
  (identifier-renamer rdb:identifier-renamer)
  (lookup-rename rdb:lookup-rename))

(define (initial-rename-db)
  (let ((frame-id 0)
	(mapping-table (make-strong-eq-hash-table))
	(unmapping-table (make-strong-eq-hash-table)))

    (define (identifier-renamer get-rename)
      (let ((delayed-frame-id (delay (new-frame-id))))
	(lambda (identifier)
	  (guarantee identifier? identifier)
	  (let ((bucket
		 (hash-table-intern! mapping-table
				     identifier
				     (lambda () (list 'bucket)))))
	    (let ((entry (assq delayed-frame-id (cdr bucket))))
	      (if entry
		  (cdr entry)
		  (let ((rename (get-rename identifier)))
		    (set-cdr! bucket
			      (cons (cons delayed-frame-id rename)
				    (cdr bucket)))
		    (hash-table-set! unmapping-table
				     rename
				     (cons identifier delayed-frame-id))
		    rename)))))))

    (define (new-frame-id)
      (let ((n (+ frame-id 1)))
	(set! frame-id n)
	n))

    (define (lookup-rename rename)
      (hash-table-ref/default unmapping-table rename #f))

    (make-rename-db identifier-renamer lookup-rename)))

(define (rename->original rename)
  (let ((entry ((rdb:lookup-rename (rename-db)) rename)))
    (if entry
	(car entry)
	rename)))

;;;; Post processing

(define (post-process-output expression)
  (let ((safe-set (make-strong-eq-hash-table)))
    (compute-substitution expression
			  (lambda (rename original)
			    (hash-table-set! safe-set rename original)))
    (alpha-substitute (make-final-substitution safe-set) expression)))

(define (mark-local-bindings bound body mark-safe!)
  (let ((free
	 (lset-difference eq?
			  (compute-substitution body mark-safe!)
			  bound)))
    (for-each (lambda (rename)
		(let ((original (rename->original rename)))
		  (if (not (any (lambda (rename*)
				  (eq? original (rename->original rename*)))
				free))
		      (mark-safe! rename original))))
	      bound)
    free))

(define (make-final-substitution safe-set)
  (let ((lookup-rename (rdb:lookup-rename (rename-db)))
	(uninterned-table (make-strong-eq-hash-table)))

    (define (finalize-renamed-identifier rename)
      (guarantee identifier? rename 'finalize-renamed-identifier)
      (let ((entry (lookup-rename rename)))
	(if entry
	    (let ((original (car entry))
		  (frame-id (force (cdr entry))))
	      (if (interned-symbol? original)
		  (symbol "." original "." frame-id)
		  (finalize-uninterned original frame-id)))
	    rename)))

    (define (finalize-uninterned original frame-id)
      (let ((bucket
	     (hash-table-intern! uninterned-table
				 original
				 (lambda () (list 'bucket)))))
	(let ((entry (assv frame-id (cdr bucket))))
	  (if entry
	      (cdr entry)
	      (let ((finalized
		     (symbol "." original
			     "." frame-id
			     "-" (length (cdr bucket)))))
		(set-cdr! bucket
			  (cons (cons original finalized)
				(cdr bucket)))
		finalized)))))

    (lambda (rename)
      (or (hash-table-ref/default safe-set rename #f)
	  (finalize-renamed-identifier rename)))))

;;;; Compute substitution

(define compute-substitution)
(add-boot-init!
 (lambda ()
   (define (define-cs-handler predicate handler)
     (define-predicate-dispatch-handler compute-substitution
       (list predicate any-object?)
       handler))

   (define (simple-subexpression get-subexpression)
     (lambda (expression mark-safe!)
       (compute-substitution (get-subexpression expression) mark-safe!)))

   (define (simple-subexpressions get-subexpressions)
     (lambda (expression mark-safe!)
       (reduce (lambda (s1 s2)
		 (lset-union eq? s1 s2))
	       '()
	       (map (lambda (expression)
		      (compute-substitution expression mark-safe!))
		    (get-subexpressions expression)))))

   (set! compute-substitution
	 (cached-standard-predicate-dispatcher 'compute-substitution 2))

   (define-predicate-dispatch-default-handler compute-substitution
     (lambda (expression mark-safe!)
       (declare (ignore expression mark-safe!))
       '()))

   (define-cs-handler scode-variable?
     (lambda (expression mark-safe!)
       (declare (ignore mark-safe!))
       (list (scode-variable-name expression))))

   (define-cs-handler scode-assignment?
     (lambda (expression mark-safe!)
       (lset-adjoin eq?
		    (compute-substitution (scode-assignment-value expression)
					  mark-safe!)
		    (scode-assignment-name expression))))

   (define-cs-handler scode-unassigned??
     (lambda (expression mark-safe!)
       (declare (ignore mark-safe!))
       (list (scode-unassigned?-name expression))))

   (define-cs-handler scode-lambda?
     (lambda (expression mark-safe!)
       (lambda-components** expression
	 (lambda (pattern bound body)
	   (declare (ignore pattern))
	   (mark-local-bindings bound body mark-safe!)))))

   (define-cs-handler scode-open-block?
     (lambda (expression mark-safe!)
       (mark-local-bindings (scode-open-block-names expression)
			    (scode-open-block-actions expression)
			    mark-safe!)))

   (define-cs-handler quoted-identifier?
     (simple-subexpression quoted-identifier-identifier))

   (define-cs-handler scode-access?
     (simple-subexpression scode-access-environment))

   (define-cs-handler scode-combination?
     (simple-subexpressions
      (lambda (expr)
	(cons (scode-combination-operator expr)
	      (scode-combination-operands expr)))))

   (define-cs-handler scode-comment?
     (simple-subexpression scode-comment-expression))

   (define-cs-handler scode-conditional?
     (simple-subexpressions
      (lambda (expr)
	(list (scode-conditional-predicate expr)
	      (scode-conditional-consequent expr)
	      (scode-conditional-alternative expr)))))

   (define-cs-handler scode-definition?
     (simple-subexpression scode-definition-value))

   (define-cs-handler scode-delay?
     (simple-subexpression scode-delay-expression))

   (define-cs-handler scode-disjunction?
     (simple-subexpressions
      (lambda (expr)
	(list (scode-disjunction-predicate expr)
	      (scode-disjunction-alternative expr)))))

   (define-cs-handler scode-sequence?
     (simple-subexpressions scode-sequence-actions))

   ))

;;;; Alpha substitution

(define alpha-substitute)
(add-boot-init!
 (lambda ()

   (define (define-as-handler predicate handler)
     (define-predicate-dispatch-handler alpha-substitute
       (list any-object? predicate)
       handler))

   (define (simple-substitution reconstruct . parts)
     (lambda (substitution expression)
       (apply reconstruct
	      (map (lambda (part)
		     (alpha-substitute substitution (part expression)))
		   parts))))

   (define (partial-substitution selector reconstruct . parts)
     (lambda (substitution expression)
       (apply reconstruct
	      (map (lambda (substitute? part)
		     (if substitute?
			 (alpha-substitute substitution (part expression))
			 (part expression)))
		   selector
		   parts))))

   (define (list-substitution reconstruct get-subexpressions)
     (lambda (substitution expression)
       (reconstruct
	(map (lambda (expression)
	       (alpha-substitute substitution expression))
	     (get-subexpressions expression)))))

   (set! alpha-substitute
	 (cached-standard-predicate-dispatcher 'alpha-substitute 2))

   (define-predicate-dispatch-default-handler alpha-substitute
     (lambda (substitution expression)
       (declare (ignore substitution))
       expression))

   (define-as-handler scode-variable?
     (lambda (substitution expression)
       (make-scode-variable (substitution (scode-variable-name expression)))))

   (define-as-handler quoted-identifier?
     (lambda (substitution expression)
       (substitution (quoted-identifier-identifier expression))))

   (define-as-handler scode-assignment?
     (lambda (substitution expression)
       (make-scode-assignment
	(substitution (scode-assignment-name expression))
	(alpha-substitute substitution (scode-assignment-value expression)))))

   (define-as-handler scode-unassigned??
     (lambda (substitution expression)
       (make-scode-unassigned?
	(substitution (scode-unassigned?-name expression)))))

   (define-as-handler scode-lambda?
     (lambda (substitution expression)
       (lambda-components** expression
	 (lambda (pattern bound body)
	   (make-lambda** pattern
			  (map substitution bound)
			  (alpha-substitute substitution body))))))

   (define-as-handler scode-open-block?
     (lambda (substitution expression)
       (make-scode-open-block
	(map substitution (scode-open-block-names expression))
	(map (lambda (declaration)
	       (map-decl-ids (lambda (id selector)
			       (declare (ignore selector))
			       (substitution id))
			     declaration))
	     (scode-open-block-declarations expression))
	(alpha-substitute substitution (scode-open-block-actions expression)))))

   (define-as-handler scode-declaration?
     (lambda (substitution expression)
       (make-scode-declaration
	(map (lambda (declaration)
	       (map-decl-ids (lambda (id selector)
			       (declare (ignore selector))
			       (substitution id))
			     declaration))
	     (scode-declaration-text expression))
	(alpha-substitute substitution
			  (scode-declaration-expression expression)))))

   (define-as-handler scode-access?
     (partial-substitution '(#t #f)
			   make-scode-access
			   scode-access-environment
			   scode-access-name))

   (define-as-handler scode-combination?
     (list-substitution
      (lambda (subexpressions)
	(make-scode-combination (car subexpressions) (cdr subexpressions)))
      (lambda (expression)
	(cons (scode-combination-operator expression)
	      (scode-combination-operands expression)))))

   (define-as-handler scode-comment?
     (partial-substitution '(#f #t)
			   make-scode-comment
			   scode-comment-text
			   scode-comment-expression))

   (define-as-handler scode-conditional?
     (simple-substitution make-scode-conditional
			  scode-conditional-predicate
			  scode-conditional-consequent
			  scode-conditional-alternative))

   (define-as-handler scode-definition?
     (partial-substitution '(#f #t)
			   make-scode-definition
			   scode-definition-name
			   scode-definition-value))

   (define-as-handler scode-delay?
     (simple-substitution make-scode-delay
			  scode-delay-expression))

   (define-as-handler scode-disjunction?
     (simple-substitution make-scode-disjunction
			  scode-disjunction-predicate
			  scode-disjunction-alternative))

   (define-as-handler scode-sequence?
     (list-substitution make-scode-sequence scode-sequence-actions))

   ))