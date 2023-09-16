#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Debugger state
;;; package: (runtime new-debugger-state)

(declare (usual-integrations))

(define-record-type <dstate>
    make-dstate
    dstate?
  (frames dstate-frames)
  (stack dstate-stack)
  (reduction-index %dstate-reduction-index)
  (hist-state dstate-hist-state)
  (env-list dstate-env-list)
  (condition dstate-condition))

(define (dstate-frame dstate)
  (stream-car (dstate-frames dstate)))

(define (dstate-subproblem-index dstate)
  (length (dstate-stack dstate)))

(define (dstate-n-subproblems dstate)
  (stream-length (dstate-all-subproblems dstate)))

(define (dstate-all-subproblems dstate)
  (stream-filter cframe-subproblem?
		 (if (pair? (dstate-stack dstate))
		     (car (last-pair (dstate-stack dstate)))
		     (dstate-frames dstate))))

(define (dstate-expression dstate)
  (cframe-dbg-expression (dstate-frame dstate)))

(define (dstate-subexpression dstate)
  (cframe-dbg-subexpression (dstate-frame dstate)))

(define (dstate-has-environment? dstate)
  (pair? (dstate-env-list dstate)))

(define (dstate-environment dstate)
  (assert (dstate-has-environment? dstate))
  (car (last-pair (dstate-env-list dstate))))

(define (dstate-current-environment dstate)
  (assert (dstate-has-environment? dstate))
  (car (dstate-env-list dstate)))

(define (dstate-current-environment-index dstate)
  (assert (dstate-has-environment? dstate))
  (length (cdr (dstate-env-list dstate))))

(define (dstate-use-history? dstate)
  (and (hist-state-using-history? (dstate-hist-state dstate))
       (dstate-has-reductions? dstate)))

(define (dstate-auto-toggle? dstate)
  (not (eq? (dstate-hist-state dstate) 'disabled)))

(define (dstate-enabled? dstate)
  (eq? (dstate-hist-state dstate) 'enabled))

(define (dstate-reduction dstate index)
  (cframe-reduction (dstate-frame dstate) index))

(define (dstate-has-reductions? dstate)
  (cframe-has-reductions? (dstate-frame dstate)))

(define (dstate-reduction-index dstate)
  (let ((index (%dstate-reduction-index dstate)))
    (assert index)
    index))

(define (dstate-n-reductions dstate)
  (cframe-n-reductions (dstate-frame dstate)))

(define (dstate-current-reduction dstate)
  (and (%dstate-reduction-index dstate)
       (dstate-reduction dstate (%dstate-reduction-index dstate))))

(define (dstate-other-thread dstate)
  (let ((condition (dstate-condition dstate)))
    (and condition
	 (condition/other-thread condition))))

(define (initial-dstate continuation condition)
  (let ((frames
	 (cframe-stream-first-subproblem
	  (continuation->cframe-stream continuation))))
    (assert (stream-pair? frames))
    (new-subproblem frames
		    '()
		    (cond (debugger:use-history? 'always)
			  (debugger:auto-toggle? 'enabled)
			  (else 'disabled))
		    condition)))

(define (dstate-earlier-subproblem? dstate)
  (stream-pair? (cframe-stream-next-subproblem (dstate-frames dstate))))

(define (dstate-earlier-subproblem dstate)
  (let* ((frames (dstate-frames dstate))
	 (next (cframe-stream-next-subproblem frames)))
    (and (stream-pair? next)
	 (select-subproblem next
			    (cons frames (dstate-stack dstate))
			    dstate))))

(define (dstate-later-subproblem dstate)
  (let ((stack (dstate-stack dstate)))
    (and (pair? stack)
	 (select-subproblem (car stack) (cdr stack) dstate))))

(define (dstate-nth-subproblem dstate index)
  (let-values (((frames stack) (subproblem-ref dstate index)))
    (and (stream-pair? frames)
	 (select-subproblem frames stack dstate))))

(define (dstate-earlier-reduction dstate)
  (assert (dstate-has-reductions? dstate))
  (let ((index (%dstate-reduction-index dstate)))
    (and (fix:< (fix:+ index 1) (dstate-n-reductions dstate))
	 (select-reduction (fix:+ index 1) dstate))))

(define (dstate-later-reduction dstate)
  (assert (dstate-has-reductions? dstate))
  (let ((index (%dstate-reduction-index dstate)))
    (and (fix:> index 0)
	 (select-reduction (fix:- index 1) dstate))))

(define (dstate-latest-reduction dstate)
  (assert (dstate-has-reductions? dstate))
  (select-reduction 0 dstate))

(define (dstate-earliest-reduction dstate)
  (assert (dstate-has-reductions? dstate))
  (select-reduction (fix:- (dstate-n-reductions dstate) 1) dstate))

(define (dstate-nth-reduction dstate index)
  (assert (and (fix:>= index 0) (fix:< index (dstate-n-reductions dstate))))
  (select-reduction index dstate))

(define (dstate-parent-environment dstate)
  (assert (dstate-has-environment? dstate))
  (let ((env (dstate-current-environment dstate)))
    (and (eq? #t (environment-has-parent? env))
	 (select-environment (cons (environment-parent env)
				   (dstate-env-list dstate))
			     dstate))))

(define (dstate-child-environment dstate)
  (assert (dstate-has-environment? dstate))
  (let ((env-list (cdr (dstate-env-list dstate))))
    (and (pair? env-list)
	 (select-environment env-list dstate))))

(define (dstate-start-using-history dstate)
  (if (eq? (dstate-hist-state dstate) 'enabled)
      (new-hist-state 'now dstate)
      dstate))

(define (dstate-stop-using-history dstate)
  (if (eq? (dstate-hist-state dstate) 'now)
      (new-hist-state 'enabled dstate)
      dstate))

(define (dstate-continuation dstate)
  (cframe-stream->continuation (dstate-frames dstate)))

(define (select-subproblem frames stack dstate)
  (new-subproblem frames
		  stack
		  (dstate-hist-state dstate)
		  (dstate-condition dstate)))

(define (new-subproblem frames stack hist-state condition)
  (let-values (((index env-list) (index-and-env-list frames hist-state)))
    (make-dstate frames
		 stack
		 index
		 hist-state
		 env-list
		 condition)))

(define (select-reduction index dstate)
  (let ((frames (dstate-frames dstate)))
    (make-dstate frames
		 (dstate-stack dstate)
		 index
		 (dstate-hist-state dstate)
		 (reduction-env-list (stream-car frames) index)
		 (dstate-condition dstate))))

(define (select-environment env-list dstate)
  (make-dstate (dstate-frames dstate)
	       (dstate-stack dstate)
	       (%dstate-reduction-index dstate)
	       (dstate-hist-state dstate)
	       env-list
	       (dstate-condition dstate)))

(define (new-hist-state hist-state dstate)
  (let ((frames (dstate-frames dstate)))
    (let-values (((index env-list) (index-and-env-list frames hist-state)))
      (make-dstate frames
		   (dstate-stack dstate)
		   index
		   hist-state
		   env-list
		   (dstate-condition dstate)))))

(define (index-and-env-list frames hist-state)
  (let ((frame (stream-car frames)))
    (if (and (hist-state-using-history? hist-state)
	     (cframe-has-reductions? frame))
	(values 0 (reduction-env-list frame 0))
	(let ((env (cframe-stream-dbg-environment frames)))
	  (if (cframe-dbg-environment-undefined? env)
	      (values #f '())
	      (values 0 (list env)))))))

(define (hist-state-using-history? hist-state)
  (or (eq? hist-state 'always)
      (eq? hist-state 'now)))

(define (reduction-env-list frame index)
  (list
   (history-reduction-environment
    (cframe-reduction frame index))))

(define (subproblem-ref dstate index)
  (let loop ((i 0) (frames (dstate-all-subproblems dstate)) (stack '()))
    (if (fix:< i index)
	(let ((frames* (stream-cdr frames))
	      (stack* (cons frames stack)))
	  (if (stream-pair? frames*)
	      (loop (fix:+ i 1) frames* stack*)
	      (values frames* stack*)))
	(values frames stack))))