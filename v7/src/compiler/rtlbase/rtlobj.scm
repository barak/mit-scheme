#| -*-Scheme-*-

$Id: rtlobj.scm,v 4.16 2007/01/05 21:19:23 cph Exp $

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

;;;; Register Transfer Language: Object Datatypes

(declare (usual-integrations))

(define-structure (rtl-expr
		   (conc-name rtl-expr/)
		   (constructor make-rtl-expr
				(rgraph label entry-edge debugging-info))
		   (print-procedure
		    (standard-unparser (symbol->string 'RTL-EXPR)
		      (lambda (state expression)
			(unparse-object state (rtl-expr/label expression))))))
  (rgraph false read-only true)
  (label false read-only true)
  (entry-edge false read-only true)
  (debugging-info false read-only true))

(define-integrable (rtl-expr/entry-node expression)
  (edge-right-node (rtl-expr/entry-edge expression)))

(define-structure (rtl-procedure
		   (conc-name rtl-procedure/)
		   (constructor make-rtl-procedure
				(rgraph label entry-edge name n-required
					n-optional rest? closure?
					dynamic-link? type
					debugging-info
					next-continuation-offset stack-leaf?))
		   (print-procedure
		    (standard-unparser (symbol->string 'RTL-PROCEDURE)
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
  (dynamic-link? false read-only true)
  (type false read-only true)
  (%external-label false)
  (debugging-info false read-only true)
  (next-continuation-offset false read-only true)
  (stack-leaf? false read-only true))

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
				(rgraph label entry-edge
					next-continuation-offset
					debugging-info))
		   (print-procedure
		    (standard-unparser (symbol->string 'RTL-CONTINUATION)
		      (lambda (state continuation)
			(unparse-object
			 state
			 (rtl-continuation/label continuation))))))
  (rgraph false read-only true)
  (label false read-only true)
  (entry-edge false read-only true)
  (next-continuation-offset false read-only true)
  (debugging-info false read-only true))

(define-integrable (rtl-continuation/entry-node continuation)
  (edge-right-node (rtl-continuation/entry-edge continuation)))

(define (make/label->object expression procedures continuations)
  (let ((hash-table
	 (make-eq-hash-table
	  (+ (if expression 1 0)
	     (length procedures)
	     (length continuations)))))
    (if expression
	(hash-table/put! hash-table
			 (rtl-expr/label expression)
			 expression))
    (for-each (lambda (procedure)
		(hash-table/put! hash-table
				 (rtl-procedure/label procedure)
				 procedure))
	      procedures)
    (for-each (lambda (continuation)
		(hash-table/put! hash-table
				 (rtl-continuation/label continuation)
				 continuation))
	      continuations)
    (lambda (label)
      (let ((datum (hash-table/get hash-table label #f)))
	(if (not datum)
	    (error "Undefined label:" label))
	datum))))