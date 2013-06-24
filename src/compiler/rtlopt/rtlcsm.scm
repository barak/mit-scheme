#| -*-Scheme-*-

$Id: rtlcsm.scm,v 1.6 2007/01/05 21:19:23 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; RTL Common Suffix Merging

(declare (usual-integrations))

(define (merge-common-suffixes! rgraphs)
  (for-each merge-suffixes-of-rgraph! rgraphs))

(define (merge-suffixes-of-rgraph! rgraph)
  (let loop ()
    (let ((suffix-classes (rgraph-matching-suffixes rgraph)))
      (if (not (null? suffix-classes))
	  (begin
	    ;; Because many of the original bblocks can be discarded
	    ;; by the merging process, processing of one suffix class
	    ;; can make the information in the subsequent suffix
	    ;; classes incorrect.  However, reanalysis will still
	    ;; reproduce the remaining suffix classes.  So, process
	    ;; one class and reanalyze before continuing.
	    (merge-suffixes! rgraph (car suffix-classes))
	    (loop))))))

(define (merge-suffixes! rgraph suffixes)
  (with-values
      (lambda ()
	(discriminate-items suffixes
	  (lambda (suffix)
	    (eq? (cdr suffix) (bblock-instructions (car suffix))))))
    (lambda (total-suffixes partial-suffixes)
      (if (not (null? total-suffixes))
	  (let ((new-bblock (caar total-suffixes)))
	    (for-each (lambda (suffix)
			(replace-suffix-block! rgraph suffix new-bblock))
		      (cdr total-suffixes))
	    (replace-suffixes! rgraph new-bblock partial-suffixes))
	  (let ((suffix (car partial-suffixes)))
	    (split-suffix-block! rgraph suffix)
	    (replace-suffixes! rgraph (car suffix) (cdr partial-suffixes)))))))

(define (replace-suffixes! rgraph new-bblock partial-suffixes)
  (for-each (lambda (suffix)
	      (split-suffix-block! rgraph suffix)
	      (replace-suffix-block! rgraph suffix new-bblock))
	    partial-suffixes))

(define (split-suffix-block! rgraph suffix)
  (let ((old-bblock (car suffix))
	(instructions (cdr suffix)))
    (rinst-disconnect-previous! old-bblock instructions)
    (let ((sblock (make-sblock (bblock-instructions old-bblock))))
      (node-insert-snode! old-bblock sblock)
      (add-rgraph-bblock! rgraph sblock))
    (set-bblock-instructions! old-bblock instructions)))

(define (replace-suffix-block! rgraph suffix new-bblock)
  (let ((old-bblock (car suffix)))
    (node-replace-on-right! old-bblock new-bblock)
    (node-disconnect-on-left! old-bblock)
    (delete-rgraph-bblock! rgraph old-bblock)))

(define (rgraph-matching-suffixes rgraph)
  (append-map (lambda (bblock-class)
		(suffix-classes (initial-bblock-matches bblock-class)))
	      (rgraph/bblock-classes rgraph)))

(define (rgraph/bblock-classes rgraph)
  (let ((sblock-classes (list false))
	(pblock-classes (list false)))
    (for-each (lambda (bblock)
		(if (sblock? bblock)
		    (add-sblock-to-classes! sblock-classes bblock)
		    (add-pblock-to-classes! pblock-classes bblock)))
	      (rgraph-bblocks rgraph))
    (let ((singleton? (lambda (x) (null? (cdr x)))))
      (append! (list-transform-negative (cdr sblock-classes) singleton?)
	       (list-transform-negative (cdr pblock-classes) singleton?)))))

(define (add-sblock-to-classes! classes sblock)
  (let ((next (snode-next sblock)))
    (let loop ((previous classes) (classes (cdr classes)))
      (if (null? classes)
	  (set-cdr! previous (list (list sblock)))
	  (if (eq? next (snode-next (caar classes)))
	      (set-car! classes (cons sblock (car classes)))
	      (loop classes (cdr classes)))))))

(define (add-pblock-to-classes! classes pblock)
  (let ((consequent (pnode-consequent pblock))
	(alternative (pnode-alternative pblock)))
    (let loop ((previous classes) (classes (cdr classes)))
      (if (null? classes)
	  (set-cdr! previous (list (list pblock)))
	  (if (let ((pblock* (caar classes)))
		(and (eq? consequent (pnode-consequent pblock*))
		     (eq? alternative (pnode-alternative pblock*))))
	      (set-car! classes (cons pblock (car classes)))
	      (loop classes (cdr classes)))))))

(define (initial-bblock-matches bblocks)
  (let loop ((bblocks bblocks))
    (if (null? bblocks)
	'()
	(let ((entries (find-matching-bblocks (car bblocks) (cdr bblocks))))
	  (if (null? entries)
	      (loop (cdr bblocks))
	      (append! entries (loop (cdr bblocks))))))))

(define (suffix-classes entries)
  (let ((classes '())
	(class-member?
	 (lambda (class suffix)
	   (list-search-positive class
	     (lambda (suffix*)
	       (and (eq? (car suffix) (car suffix*))
		    (eq? (cdr suffix) (cdr suffix*))))))))
    (for-each (lambda (entry)
		(let ((class
		       (list-search-positive classes
			 (lambda (class)
			   (class-member? class (car entry))))))
		  (if class
		      (if (not (class-member? class (cdr entry)))
			  (set-cdr! class (cons (cdr entry) (cdr class))))
		      (let ((class
			     (list-search-positive classes
			       (lambda (class)
				 (class-member? class (cdr entry))))))
			(if class
			    (set-cdr! class (cons (car entry) (cdr class)))
			    (set! classes
				  (cons (list (car entry) (cdr entry))
					classes))))))
		unspecific)
	      entries)
    (map cdr
	 (sort (map (lambda (class) (cons (rinst-length (cdar class)) class))
		    classes)
	       (lambda (x y)
		 (< (car x) (car y)))))))

;;;; Basic Block Matching

(define (find-matching-bblocks bblock bblocks)
  (let loop ((bblocks bblocks))
    (if (null? bblocks)
	'()
	(with-values (lambda () (matching-suffixes bblock (car bblocks)))
	  (lambda (sx sy adjustments)
	    (if (or (interesting-suffix? bblock sx)
		    (interesting-suffix? (car bblocks) sy))
		(begin
		  (for-each (lambda (adjustment) (adjustment)) adjustments)
		  (cons (cons (cons bblock sx) (cons (car bblocks) sy))
			(loop (cdr bblocks))))
		(loop (cdr bblocks))))))))

(define (interesting-suffix? bblock rinst)
  (and rinst
       (or (rinst-next rinst)
	   (eq? rinst (bblock-instructions bblock))
	   (and (sblock? bblock)
		(snode-next bblock))
	   (let ((rtl (rinst-rtl rinst)))
	     (let ((type (rtl:expression-type rtl)))
	       (if (eq? type 'INVOCATION:PRIMITIVE)
		   (let ((procedure (rtl:invocation:primitive-procedure rtl)))
		     (and (not (eq? compiled-error-procedure procedure))
			  (negative? (primitive-procedure-arity procedure))))
		   (memq type
			 '(INTERPRETER-CALL:ACCESS
			   INTERPRETER-CALL:DEFINE
			   INTERPRETER-CALL:LOOKUP
			   INTERPRETER-CALL:SET!
			   INTERPRETER-CALL:UNASSIGNED?
			   INTERPRETER-CALL:UNBOUND
			   INTERPRETER-CALL:CACHE-ASSIGNMENT
			   INTERPRETER-CALL:CACHE-REFERENCE
			   INTERPRETER-CALL:CACHE-UNASSIGNED?
			   INVOCATION:COMPUTED-LEXPR
			   INVOCATION:CACHE-REFERENCE
			   INVOCATION:LOOKUP))))))))

(define (matching-suffixes x y)
  (let loop
      ((rx (bblock-reversed-instructions x))
       (ry (bblock-reversed-instructions y))
       (wx false)
       (wy false)
       (e '())
       (adjustments '()))
    (if (or (null? rx) (null? ry))
	(values wx wy adjustments)
	(with-values
	    (lambda ()
	      (match-rtl (rinst-rtl (car rx)) (rinst-rtl (car ry)) e))
	  (lambda (e adjustment)
	    (if (eq? e 'FAILURE)
		(values wx wy adjustments)
		(let ((adjustments
		       (if adjustment
			   (cons adjustment adjustments)
			   adjustments)))
		  (if (for-all? e (lambda (b) (eqv? (car b) (cdr b))))
		      (loop (cdr rx) (cdr ry)
			    (car rx) (car ry)
			    e adjustments)
		      (loop (cdr rx) (cdr ry)
			    wx wy
			    e adjustments)))))))))

;;;; RTL Instruction Matching

(define (match-rtl x y e)
  (cond ((not (eq? (rtl:expression-type x) (rtl:expression-type y)))
	 (values 'FAILURE false))
	((rtl:assign? x)
	 (values
	  (let ((ax (rtl:assign-address x)))
	    (let ((e (match ax (rtl:assign-address y) e)))
	      (if (eq? e 'FAILURE)
		  'FAILURE
		  (match (rtl:assign-expression x)
			 (rtl:assign-expression y)
			 (remove-from-environment!
			  e
			  (if (rtl:pseudo-register-expression? ax)
			      (list (rtl:register-number ax))
			      '()))))))
	  false))
	((and (rtl:invocation? x)
	      (not (eqv? (rtl:invocation-continuation x)
			 (rtl:invocation-continuation y))))
	 (let ((x* (rtl:map-subexpressions x identity-procedure))
	       (y* (rtl:map-subexpressions y identity-procedure)))
	   (rtl:set-invocation-continuation! x* false)
	   (rtl:set-invocation-continuation! y* false)
	   (values (match x* y* e)
		   (lambda ()
		     (rtl:set-invocation-continuation! x false)
		     (rtl:set-invocation-continuation! y false)))))
	(else
	 (values (match x y e) false))))

(define (remove-from-environment! e keys)
  (if (null? keys)
      e
      (remove-from-environment! (del-assv! (car keys) e) (cdr keys))))

(define (match x y e)
  (cond ((pair? x)
	 (let ((type (car x)))
	   (if (and (pair? y) (eq? type (car y)))
	       (case type
		 ((CONSTANT)
		  (if (eqv? (cadr x) (cadr y))
		      e
		      'FAILURE))
		 ((REGISTER)
		  (let ((rx (cadr x))
			(ry (cadr y)))
		    (if (pseudo-register? rx)
			(if (pseudo-register? ry)
			    (let ((entry (assv rx e)))
			      (cond ((not entry) (cons (cons rx ry) e))
				    ((eqv? (cdr entry) ry) e)
				    (else 'FAILURE)))
			    'FAILURE)
			(if (pseudo-register? ry)
			    'FAILURE
			    (if (eqv? rx ry)
				e
				'FAILURE)))))
		 (else
		  (let loop ((x (cdr x)) (y (cdr y)) (e e))
		    (cond ((pair? x)
			   (if (pair? y)
			       (let ((e (match (car x) (car y) e)))
				 (if (eq? e 'FAILURE)
				     'FAILURE
				     (loop (cdr x) (cdr y) e)))
			       'FAILURE))
			  ((eqv? x y) e)
			  (else 'FAILURE)))))
	       'FAILURE)))
	((eqv? x y) e)
	(else 'FAILURE)))