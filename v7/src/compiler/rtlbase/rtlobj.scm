#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rtlobj.scm,v 4.3 1988/06/14 08:37:16 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Register Transfer Language: Object Datatypes

(declare (usual-integrations))

(define-structure (rtl-expr
		   (conc-name rtl-expr/)
		   (constructor make-rtl-expr (rgraph label entry-edge))
		   (print-procedure
		    (standard-unparser "RTL-EXPR"
		      (lambda (state expression)
			(unparse-object state (rtl-expr/label expression))))))
  (rgraph false read-only true)
  (label false read-only true)
  (entry-edge false read-only true))

(set-type-object-description!
 rtl-expr
 (lambda (expression)
   `((RTL-EXPR/RGRAPH ,(rtl-expr/rgraph expression))
     (RTL-EXPR/LABEL ,(rtl-expr/label expression))
     (RTL-EXPR/ENTRY-EDGE ,(rtl-expr/entry-edge expression)))))

(define-integrable (rtl-expr/entry-node expression)
  (edge-right-node (rtl-expr/entry-edge expression)))

(define-structure (rtl-procedure
		   (conc-name rtl-procedure/)
		   (constructor make-rtl-procedure
				(rgraph label entry-edge name n-required
					n-optional rest? closure? type))
		   (print-procedure
		    (standard-unparser "RTL-PROCEDURE"
		      (lambda (state procedure)
			(unparse-object state
					(rtl-procedure/label procedure))))))
  (rgraph false read-only true)
  (label false read-only true)
  (entry-edge false read-only true)
  (name false read-only true)
  (n-required false read-only true)
  (n-optional false read-only true)
  (rest? false read-only true)
  (closure? false read-only true)
  (type false read-only true)
  (%external-label false))

(set-type-object-description!
 rtl-procedure
 (lambda (procedure)
   `((RTL-PROCEDURE/RGRAPH ,(rtl-procedure/rgraph procedure))
     (RTL-PROCEDURE/LABEL ,(rtl-procedure/label procedure))
     (RTL-PROCEDURE/ENTRY-EDGE ,(rtl-procedure/entry-edge procedure))
     (RTL-PROCEDURE/NAME ,(rtl-procedure/name procedure))
     (RTL-PROCEDURE/N-REQUIRED ,(rtl-procedure/n-required procedure))
     (RTL-PROCEDURE/N-OPTIONAL ,(rtl-procedure/n-optional procedure))
     (RTL-PROCEDURE/REST? ,(rtl-procedure/rest? procedure))
     (RTL-PROCEDURE/CLOSURE? ,(rtl-procedure/closure? procedure))
     (RTL-PROCEDURE/TYPE ,(rtl-procedure/type procedure))
     (RTL-PROCEDURE/%EXTERNAL-LABEL
      ,(rtl-procedure/%external-label procedure)))))

(define-integrable (rtl-procedure/entry-node procedure)
  (edge-right-node (rtl-procedure/entry-edge procedure)))

(define (rtl-procedure/external-label procedure)
  (or (rtl-procedure/%external-label procedure)
      (let ((label (generate-label (rtl-procedure/name procedure))))
	(set-rtl-procedure/%external-label! procedure label)
	label)))

(define-structure (rtl-continuation
		   (conc-name rtl-continuation/)
		   (constructor make-rtl-continuation
				(rgraph label entry-edge))
		   (print-procedure
		    (standard-unparser "RTL-CONTINUATION"		      (lambda (state continuation)
			(unparse-object
			 state
			 (rtl-continuation/label continuation))))))
  (rgraph false read-only true)
  (label false read-only true)
  (entry-edge false read-only true))

(set-type-object-description!
 rtl-continuation
 (lambda (continuation)
   `((RTL-CONTINUATION/RGRAPH ,(rtl-continuation/rgraph continuation))
     (RTL-CONTINUATION/LABEL ,(rtl-continuation/label continuation))
     (RTL-CONTINUATION/ENTRY-EDGE
      ,(rtl-continuation/entry-edge continuation)))))

(define-integrable (rtl-continuation/entry-node continuation)
  (edge-right-node (rtl-continuation/entry-edge continuation)))

(define (make/label->object expression procedures continuations)
  (let ((hash-table
	 (symbol-hash-table/make
	  (1+ (+ (length procedures) (length continuations))))))
    (symbol-hash-table/insert! hash-table
			       (rtl-expr/label expression)
			       expression)    (for-each (lambda (procedure)
		(symbol-hash-table/insert! hash-table
					   (rtl-procedure/label procedure)
					   procedure))
	      procedures)
    (for-each (lambda (continuation)
		(symbol-hash-table/insert!
		 hash-table
		 (rtl-continuation/label continuation)
		 continuation))
	      continuations)
    (make/label->object* hash-table)))

(define (make/label->object* hash-table)
  (lambda (label)
    (symbol-hash-table/lookup hash-table label)))