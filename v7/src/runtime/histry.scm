#| -*-Scheme-*-

$Id: histry.scm,v 14.10 2007/01/05 21:19:28 cph Exp $

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

;;;; History Manipulation
;;; package: (runtime history)

(declare (usual-integrations))

;;; Vertebrae

(define-integrable (make-vertebra rib deeper shallower)
  (history:unmark (hunk3-cons rib deeper shallower)))

(define-integrable vertebra-rib system-hunk3-cxr0)
(define-integrable deeper-vertebra system-hunk3-cxr1)
(define-integrable shallower-vertebra system-hunk3-cxr2)
(define-integrable set-vertebra-rib! system-hunk3-set-cxr0!)
(define-integrable set-deeper-vertebra! system-hunk3-set-cxr1!)
(define-integrable set-shallower-vertebra! system-hunk3-set-cxr2!)

(define-integrable (marked-vertebra? vertebra)
  (history:marked? (system-hunk3-cxr1 vertebra)))

(define (mark-vertebra! vertebra)
  (system-hunk3-set-cxr1! vertebra
			  (history:mark (system-hunk3-cxr1 vertebra))))

(define (unmark-vertebra! vertebra)
  (system-hunk3-set-cxr1! vertebra
			  (history:unmark (system-hunk3-cxr1 vertebra))))

(define-integrable (same-vertebra? x y)
  (= (object-datum x) (object-datum y)))

(define (link-vertebrae previous next)
  (set-deeper-vertebra! previous next)
  (set-shallower-vertebra! next previous))

;;; Reductions

(define-integrable (make-reduction expression environment next)
  (history:unmark (hunk3-cons expression environment next)))

(define-integrable reduction-expression system-hunk3-cxr0)
(define-integrable reduction-environment system-hunk3-cxr1)
(define-integrable next-reduction system-hunk3-cxr2)
(define-integrable set-reduction-expression! system-hunk3-set-cxr0!)
(define-integrable set-reduction-environment! system-hunk3-set-cxr1!)
(define-integrable set-next-reduction! system-hunk3-set-cxr2!)

(define-integrable (marked-reduction? reduction)
  (history:marked? (system-hunk3-cxr2 reduction)))

(define (mark-reduction! reduction)
  (system-hunk3-set-cxr2! reduction
			  (history:mark (system-hunk3-cxr2 reduction))))

(define (unmark-reduction! reduction)
  (system-hunk3-set-cxr2! reduction
			  (history:unmark (system-hunk3-cxr2 reduction))))

(define-integrable (same-reduction? x y)
  (= (object-datum x) (object-datum y)))

;;; Marks

(define-integrable (history:unmark object)
  (object-new-type (ucode-type unmarked-history) object))

(define-integrable (history:mark object)
  (object-new-type (ucode-type marked-history) object))

(define-integrable (history:marked? object)
  (object-type? (ucode-type marked-history) object))

;;;; History Initialization

(define (create-history depth width)
  (let ((new-vertebra
	 (lambda ()
	   (let ((head (make-reduction false false '())))
	     (set-next-reduction!
	      head
	      (let reduction-loop ((n (-1+ width)))
		(if (zero? n)
		    head
		    (make-reduction false false (reduction-loop (-1+ n))))))
	     (make-vertebra head '() '())))))
    (if (not (and (exact-integer? depth) (positive? depth)))
	(error "CREATE-HISTORY: invalid depth" depth))
    (if (not (and (exact-integer? width) (positive? width)))
	(error "CREATE-HISTORY: invalid width" width))
    (let ((head (new-vertebra)))
      (let subproblem-loop ((n (-1+ depth)) (previous head))
	(if (zero? n)
	    (link-vertebrae previous head)
	    (let ((next (new-vertebra)))
	      (link-vertebrae previous next)
	      (subproblem-loop (-1+ n) next))))
      head)))

;;; The PUSH-HISTORY! accounts for the pop which happens after
;;; SET-CURRENT-HISTORY! is run.

(define (with-new-history thunk)
  ((ucode-primitive with-history-disabled)
    (lambda ()
      ((ucode-primitive set-current-history!)
       (let ((history
	      (push-history! (create-history max-subproblems
					     max-reductions))))
	 (if (zero? max-subproblems)

	     ;; In this case, we want the history to appear empty,
	     ;; so when it pops up, there is nothing in it.
	     history

	     ;; Otherwise, record a dummy reduction, which will appear
	     ;; in the history.
	     (begin (record-dummy-reduction-in-history! history)
		    (push-history! history)))))
      (thunk))))

(define max-subproblems 10)
(define max-reductions 5)

;;;; Primitive History Operations
;;;  These operations mimic the actions of the microcode.
;;;  The history motion operations all return the new history.

(define (record-evaluation-in-history! history expression environment)
  (let ((current-reduction (vertebra-rib history)))
    (set-reduction-expression! current-reduction expression)
    (set-reduction-environment! current-reduction environment)))

(define (set-history-to-next-reduction! history)
  (let ((next-reduction (next-reduction (vertebra-rib history))))
    (set-vertebra-rib! history next-reduction)
    (unmark-reduction! next-reduction)
    history))

(define (push-history! history)
  (let ((deeper-vertebra (deeper-vertebra history)))
    (mark-vertebra! deeper-vertebra)
    (mark-reduction! (vertebra-rib deeper-vertebra))
    deeper-vertebra))

(define (pop-history! history)
  (unmark-vertebra! history)
  (shallower-vertebra history))

;;;; Side-Effectless Examiners

(define (history-transform history)
  (let loop ((current history))
    (cons current
	  (if (marked-vertebra? current)
	      (cons (delay (unfold-and-reverse-rib (vertebra-rib current)))
		    (delay (let ((next (shallower-vertebra current)))
			     (if (same-vertebra? next history)
				 the-empty-history
				 (loop next)))))
	      '()))))

(define the-empty-history)

(define (unfold-and-reverse-rib rib)
  (let loop ((current (next-reduction rib)) (output 'WRAP-AROUND))
    (let ((step
	   (let ((tail
		  (if (marked-reduction? current)
		      '()
		      output)))
	     (if (dummy-reduction? current)
		 tail
		 (cons (list (reduction-expression current)
			     (reduction-environment current))
		       tail)))))
      (if (same-reduction? current rib)
	  step
	  (loop (next-reduction current) step)))))

(define (dummy-reduction? reduction)
  (and (false? (reduction-expression reduction))
       (eq? (ucode-return-address pop-from-compiled-code)
	    (reduction-environment reduction))))				 

(define (record-dummy-reduction-in-history! history)
  (record-evaluation-in-history!
   history
   false
   (ucode-return-address pop-from-compiled-code)))

(define (history-superproblem history)
  (if (null? (cdr history))
      history
      (force (cddr history))))

(define (history-reductions history)
  (if (null? (cdr history))
      '()
      (force (cadr history))))

(define-integrable (history-untransform history)
  (car history))

(define (initialize-package!)
  (set! the-empty-history
	(cons (vector-ref (get-fixed-objects-vector)
			  (fixed-objects-vector-slot 'DUMMY-HISTORY))
	      '()))
  unspecific)