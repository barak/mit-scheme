#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/mips/lapopt.scm,v 1.1 1991/07/25 02:42:11 cph Exp $

Copyright (c) 1991 Massachusetts Institute of Technology

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

;;;; LAP Optimizer for MIPS.

(declare (usual-integrations))

(define (optimize-linear-lap instructions)
  ;; Find occurrences of LW/LBU/LWC1 followed by NOP, and delete the
  ;; NOP if the instruction following it has no reference to the
  ;; target register of the load.

  ;; **** This is pretty fragile. ****
  (letrec
      ((find-load
	(lambda (instructions)
	  (cond ((null? instructions) '())
		((and (pair? (car instructions))
		      (or (eq? 'LW (caar instructions))
			  (eq? 'LBU (caar instructions))
			  (eq? 'LWC1 (caar instructions))))
		 instructions)
		(else (find-load (cdr instructions))))))
       (get-next
	(lambda (instructions)
	  (let ((instructions (cdr instructions)))
	    (cond ((null? instructions) '())
		  ((or (not (pair? (car instructions)))
		       (eq? 'LABEL (caar instructions))
		       (eq? 'COMMENT (caar instructions)))
		   (get-next instructions))
		  (else instructions)))))
       (refers-to-register?
	(lambda (instruction register)
	  (let loop ((x instruction))
	    (if (pair? x)
		(or (loop (car x))
		    (loop (cdr x)))
		(eqv? register x))))))
    (let loop ((instructions instructions))
      (let ((first (find-load instructions)))
	(if (not (null? first))
	    (let ((second (get-next first)))
	      (if (not (null? second))
		  (let ((third (get-next second)))
		    (if (not (null? third))
			(if (and (equal? '(NOP) (car second))
				 ;; This is a crude way to test for a
				 ;; reference to the target register
				 ;; -- it will sometimes incorrectly
				 ;; say that there is a reference, but
				 ;; it will never incorrectly say that
				 ;; there is no reference.
				 (not (refers-to-register? (car third)
							   (cadar first)))
				 (or (not (and (eq? 'LWC1 (caar first))
					       (odd? (cadar first))))
				     (not (refers-to-register?
					   (car third)
					   (- (cadar first) 1)))))
			    (begin
			      (let loop ((this (cdr first)) (prev first))
				(if (eq? second this)
				    (set-cdr! prev (cdr this))
				    (loop (cdr this) this)))
			      (loop (if (equal? '(NOP) (car third))
					first
					third)))
			    (loop second))))))))))
  instructions)