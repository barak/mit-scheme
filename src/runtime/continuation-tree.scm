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

;;;; Continuation tree
;;; package: (runtime continuation-tree)

(declare (usual-integrations))

(define-record-type <ctree>
    %make-ctree
    ctree?
  (snodes %ctree-subproblems)
  (continuation ctree-continuation)
  (condition ctree-condition))

(define (make-ctree continuation condition)
  (letrec
      ((ctree
	(%make-ctree (delay
		       (make-snodes
			(cframe-stream-first-subproblem
			 (continuation->cframe-stream continuation))
			ctree))
		     continuation
		     condition)))
    ctree))

(define (->ctree object)
  (cond ((ctree? object)
	 object)
	((condition? object)
	 (make-ctree (condition/continuation object) object))
	((continuation? object)
	 (make-ctree object #f))
	((ctree-subproblem? object)
	 (ctree-subproblem->ctree object))
	((ctree-reduction? object)
	 (ctree-subproblem->ctree (ctree-reduction->subproblem object)))
	(else
	 (error "Can't coerce to a ctree:" object))))

(define (ctree-subproblems ctree)
  (force (%ctree-subproblems ctree)))

(define (ctree-n-subproblems ctree)
  (let loop ((snode (ctree-subproblems ctree)) (n 0))
    (if snode
	(loop (ctree-subproblem-earlier snode) (fix:+ n 1))
	n)))

(define (ctree-nth-subproblem ctree index)
  (let loop ((snode (ctree-subproblems ctree)) (i 0))
    (and snode
	 (if (fix:< i index)
	     (loop (ctree-subproblem-earlier snode) (fix:+ i 1))
	     snode))))

;;;; Subproblem nodes

(define-record-type <ctree-subproblem>
    make-snode
    ctree-subproblem?
  (frames snode-frames)
  (index ctree-subproblem-index)
  (tree ctree-subproblem->ctree)
  (later ctree-subproblem-later)
  (earlier %ctree-subproblem-earlier)
  (expression ctree-subproblem-expression)
  (environment ctree-subproblem-environment)
  (subexpression ctree-subproblem-subexpression)
  (rnodes %ctree-subproblem-reductions))

(define (make-snodes frames ctree)
  (let loop ((frames frames) (index 0) (later #f))
    (letrec
	((snode
	  (make-snode frames index ctree later
		      (delay
			(let ((frames (cframe-stream-next-subproblem frames)))
			  (and (stream-pair? frames)
			       (loop frames (fix:+ index 1) snode))))
		      (cframe-dbg-expression (stream-car frames))
		      (Cframe-stream-dbg-environment frames)
		      (cframe-dbg-subexpression (stream-car frames))
		      (delay (make-rnodes (stream-car frames) snode)))))
      snode)))

(define (ctree-subproblem-earlier snode)
  (force (%ctree-subproblem-earlier snode)))

(define (ctree-subproblem-reductions snode)
  (force (%ctree-subproblem-reductions snode)))

(define (ctree-subproblem-has-expression? snode)
  (let ((exp (ctree-subproblem-expression snode)))
    (not (or (dbg-expression-compiled? exp)
	     (dbg-expression-undefined? exp)
	     (dbg-printer? exp)))))

(define (ctree-subproblem-has-environment? snode)
  (not (dbg-environment-undefined? (ctree-subproblem-environment snode))))

(define (ctree-subproblem-has-subexpression? snode)
  (not (dbg-expression-undefined? (ctree-subproblem-subexpression snode))))

(define (snode-frame snode)
  (stream-car (snode-frames snode)))

(define (ctree-subproblem-return-address snode)
  (cframe-return-address (snode-frame snode)))

(define (ctree-subproblem-system-boundary? snode)
  (eq? (cframe-tracked-item-value (snode-frame snode) 'system-frame?)
       'boundary))

(define (ctree-subproblem-cc-frame? snode)
  (cframe-compiled-address? (snode-frame snode)))

(define (ctree-subproblem-raw-frame snode)
  (cframe-raw (snode-frame snode)))

(define (ctree-subproblem-n-reductions snode)
  (cframe-n-reductions (snode-frame snode)))

(define (ctree-subproblem-nth-reduction snode index)
  (let loop ((rnode (ctree-subproblem-reductions snode)) (i 0))
    (and rnode
	 (if (fix:< i index)
	     (loop (ctree-reduction-earlier rnode) (fix:+ i 1))
	     rnode))))

(define (ctree-subproblem->continuation snode)
  (cframe-stream->continuation (snode-frames snode)))

;;;; Reduction nodes

(define-record-type <ctree-reduction>
    make-rnode
    ctree-reduction?
  (index ctree-reduction-index)
  (snode ctree-reduction->subproblem)
  (later ctree-reduction-later)
  (earlier %ctree-reduction-earlier)
  (expression ctree-reduction-expression)
  (environment ctree-reduction-environment))

(define (make-rnodes frame snode)
  (let ((n (cframe-n-reductions frame)))
    (let loop ((index 0) (later #f))
      (and (fix:< index n)
	   (letrec
	       ((rnode
		 (let ((reduction (cframe-reduction frame index)))
		   (make-rnode index snode later
			       (delay (loop (fix:+ index 1) rnode))
			       (history-reduction-expression reduction)
			       (history-reduction-environment reduction)))))
	     rnode)))))

(define (ctree-reduction-earlier rnode)
  (force (%ctree-reduction-earlier rnode)))

(define (ctree-reduction-latest rnode)
  (ctree-subproblem-reductions (ctree-reduction->subproblem rnode)))

(define (ctree-reduction-earliest rnode)
  (let loop ((rnode rnode))
    (let ((rnode* (ctree-reduction-earlier rnode)))
      (if rnode*
	  (and rnode* (loop rnode*))
	  rnode))))

(define (ctree-subproblem object)
  (cond ((ctree? object) (ctree-subproblems object))
	((ctree-subproblem? object) object)
	((ctree-reduction? object) (ctree-reduction->subproblem object))
	(else (error "Not a ctree object:" object))))