#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/delint.scm,v 1.2 1989/10/26 07:36:48 cph Exp $

Copyright (c) 1989 Massachusetts Institute of Technology

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
    (if (procedure-interface-optimizible? procedure)
	(begin
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
		  (set-procedure-rest! procedure false))))))
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



