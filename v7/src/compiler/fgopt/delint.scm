#| -*-Scheme-*-

$Id: delint.scm,v 1.7 2007/01/05 15:33:03 cph Exp $

Copyright (c) 1989, 1990, 1999 Massachusetts Institute of Technology

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

;;;; Delete integrated parameters

(declare (usual-integrations))

(define (delete-integrated-parameters blocks)
  (for-each
   (lambda (block)
     (if (stack-block? block)
	 (delete-integrated-parameters! block)))
   blocks))

(define (delete-integrated-parameters! block)
  (let ((deletions '())
	(procedure (block-procedure block)))
    (let ((delete-integrations
	   (lambda (get-names set-names!)
	     (with-values
		 (lambda ()
		   (find-integrated-variables (get-names procedure)))
	       (lambda (not-integrated integrated)
		 (if (not (null? integrated))
		     (begin
		       (set-names! procedure not-integrated)
		       (set! deletions
			     (eq-set-union deletions integrated)))))))))
      (delete-integrations (lambda (procedure)
			     (cdr (procedure-required procedure)))
			   (lambda (procedure required)
			     (set-cdr! (procedure-required procedure)
				       required)))
      (delete-integrations procedure-optional set-procedure-optional!))
    (let ((rest (procedure-rest procedure)))
      (if (and rest (variable-unused? rest))
	  (begin
	    (set! deletions (eq-set-adjoin deletions rest))
	    (set-procedure-rest! procedure false))))
    (with-values
	(lambda ()
	  (find-integrated-bindings (procedure-names procedure)
				    (procedure-values procedure)))
      (lambda (names vals integrated)
	(set-procedure-names! procedure names)
	(set-procedure-values! procedure vals)
	(set! deletions (eq-set-union deletions integrated))))
    (if (not (null? deletions))
	(set-block-bound-variables!
	 block
	 (eq-set-difference (block-bound-variables block) deletions)))))

(define (find-integrated-bindings names vals)
  (if (null? names)
      (values '() '() '())
      (with-values
	  (lambda ()
	    (find-integrated-bindings (cdr names) (cdr vals)))
	(lambda (names* values* integrated)
	  (if (variable-unused? (car names))
	      (values names* values* (cons (car names) integrated))
	      (values (cons (car names) names*)
		      (cons (car vals) values*)
		      integrated))))))

(define (find-integrated-variables variables)
  (if (null? variables)
      (values '() '())
      (with-values
	  (lambda ()
	    (find-integrated-variables (cdr variables)))
	(lambda (not-integrated integrated)
	  (if (or (variable-register (car variables))
		  (variable-unused? (car variables)))
	      (values not-integrated
		      (cons (car variables) integrated))
	      (values (cons (car variables) not-integrated)
		      integrated))))))



