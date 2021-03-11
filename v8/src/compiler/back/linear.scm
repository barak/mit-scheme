#| -*-Scheme-*-

Copyright (c) 1987-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; LAP linearizer
;;; package: (compiler lap-syntaxer linearizer)

(declare (usual-integrations))

(define *strongly-heed-branch-preferences?* false)

;; `Lazy-LAP' operator.  We collect a tree of the liste that we would
;; have appended and rewrite them later, avoiding much consing.

(define-integrable (LLAP x y)
  (vector x y))

(define (bblock-linearize-lap bblock queue-continuations!)
  (define (linearize-bblock bblock)
    (LLAP (linearize-bblock-1 bblock)
	  (linearize-next bblock)))

  (define (linearize-bblock-1 bblock)
    (node-mark! bblock)
    (queue-continuations! bblock)
    (if (and (not (bblock-label bblock))
	     (let loop ((bblock bblock))
	       (or (node-previous>1? bblock)
		   (and (node-previous=1? bblock)
			(let ((previous (node-previous-first bblock)))
			  (and (sblock? previous)
			       (null? (bblock-instructions previous))
			       (loop previous)))))))
	(bblock-label! bblock))
    (let ((kernel
	   (lambda ()
	     (bblock-instructions bblock))))
      (if (bblock-label bblock)
	  (LLAP (lap:make-label-statement (bblock-label bblock)) (kernel))
	  (kernel))))

  (define (linearize-next bblock)
    (if (sblock? bblock)
	(let ((next (find-next (snode-next bblock))))
	  (if next
	      (linearize-sblock-next next (bblock-label next))
	      (let ((bblock (sblock-continuation bblock)))
		(if (and bblock (not (node-marked? bblock)))
		    (linearize-bblock bblock)
		    (LAP)))))
	(linearize-pblock
	 bblock
	 (find-next (pnode-consequent bblock))
	 (find-next (pnode-alternative bblock)))))

  (define (linearize-sblock-next bblock label)
    (if (node-marked? bblock)
	(lap:make-unconditional-branch label)
	(linearize-bblock bblock)))

  (define (linearize-pblock pblock cn an)
    (if (node-marked? cn)
	(if (node-marked? an)
	    (heed-preference pblock cn an
	      (lambda (generator cn an)
		(LLAP (generator (bblock-label cn))
		     (lap:make-unconditional-branch (bblock-label an)))))
	    (LLAP ((pblock-consequent-lap-generator pblock)
		    (bblock-label cn))
		 (linearize-bblock an)))
	(if (node-marked? an)
	    (LLAP ((pblock-alternative-lap-generator pblock)
		    (bblock-label an))
		 (linearize-bblock cn))
	    (linearize-pblock-1 pblock cn an))))

  (define (linearize-pblock-1 pblock cn an)
    (let ((finish
	   (lambda (generator cn an)
	     (let ((clabel (bblock-label! cn))
		   (alternative (linearize-bblock an)))
	       (LLAP (LLAP (generator clabel)
			   alternative)
		     (if (node-marked? cn)
			 (LAP)
			 (linearize-bblock cn)))))))
      (let ((consequent-first
	     (lambda ()
	       (finish (pblock-alternative-lap-generator pblock) an cn)))
	    (alternative-first
	     (lambda ()
	       (finish (pblock-consequent-lap-generator pblock) cn an)))
	    (unspecial
	     (lambda ()
	       (heed-preference pblock cn an finish)))
	    (diamond
	     (lambda ()
	       (let ((jlabel (generate-label)))
		 (heed-preference pblock cn an
		   (lambda (generator cn an)
		     (let ((clabel (bblock-label! cn)))
		       (let ((consequent (linearize-bblock-1 cn))
			     (alternative (linearize-bblock-1 an)))
			 (LLAP
			  (LLAP
			   (LLAP (LLAP (LLAP (generator clabel)
					     alternative)
				       (lap:make-unconditional-branch jlabel))
				 consequent)
			   (lap:make-label-statement jlabel))
			  (linearize-next cn))))))))))

	(lap:mark-preferred-branch! pblock cn an)
	(cond ((eq? cn an)
	       (warn "bblock-linearize-lap: Identical branches" pblock)
	       (unspecial))
	      ((and *strongly-heed-branch-preferences?*
		    (pnode/preferred-branch pblock))
	       (unspecial))
	      ((sblock? cn)
	       (let ((cnn (find-next (snode-next cn))))
		 (cond ((eq? cnn an)
			(consequent-first))
		       ((sblock? an)
			(let ((ann (find-next (snode-next an))))
			  (cond ((eq? ann cn)
				 (alternative-first))
				((not cnn)
				 (if ann
				     (consequent-first)
				     (if (null? (bblock-continuations cn))
					 (if (null? (bblock-continuations an))
					     (unspecial)
					     (consequent-first))
					 (if (null? (bblock-continuations an))
					     (alternative-first)
					     (unspecial)))))
				((not ann)
				 (alternative-first))
				((eq? cnn ann)
				 (diamond))
				(else
				 (unspecial)))))
		       ((not cnn)
			(consequent-first))
		       (else
			(unspecial)))))
	      ((and (sblock? an)
		    (let ((ann (find-next (snode-next an))))
		      (or (not ann)
			  (eq? ann cn))))
	       (alternative-first))
	      (else
	       (unspecial))))))

  (define (heed-preference pblock cn an finish)
    (if (eq? 'CONSEQUENT (pnode/preferred-branch pblock))
	(finish (pblock-alternative-lap-generator pblock) an cn)
	(finish (pblock-consequent-lap-generator pblock) cn an)))

  (define (find-next bblock)
    (let loop ((bblock bblock) (previous false))
      (cond ((not bblock)
	     previous)
	    ((and (sblock? bblock)
		  (null? (bblock-instructions bblock)))
	     (loop (snode-next bblock) bblock))
	    (else
	     bblock))))

  (linearize-bblock bblock))

(define-integrable (set-current-branches! consequent alternative)
  (set-pblock-consequent-lap-generator! *current-bblock* consequent)
  (set-pblock-alternative-lap-generator! *current-bblock* alternative))

(define *end-of-block-code*)

(define-structure (extra-code-block
		   (conc-name extra-code-block/)
		   (constructor extra-code-block/make
				(name constraint xtra)))
  (name false read-only true)
  (constraint false read-only true)
  (code (LAP) read-only false)
  (xtra false read-only false))

(define linearize-lap
  (make-linearizer bblock-linearize-lap
    (lambda () (LAP))
    (lambda (x y) (LLAP x y))
    (lambda (linearized-lap)
      (let ((end-code *end-of-block-code*))
	(set! *end-of-block-code* '())
	(let ((final-linearized-lap
	       (LLAP linearized-lap
		     (let process ((end-code end-code))
		       (if (null? end-code)
			   (LAP)
			   (LLAP (extra-code-block/code (car end-code))
				 (process (cdr end-code))))))))
	  (let process ((x '()) (y final-linearized-lap) (tail '()))
	    (cond ((vector? y)
		   (let ((prefix (vector-ref y 0))
			 (suffix (vector-ref y 1)))
		     (process (vector x prefix) suffix tail)))
		  ((vector? x)
		   (let ((prefix (vector-ref x 0))
			 (suffix (vector-ref x 1)))
		     (process prefix suffix (append y tail))))
		  (else (append x (append y tail))))))))))

(define (find-extra-code-block name)
  (let loop ((end-code *end-of-block-code*))
    (cond ((null? end-code) false)
	  ((eq? name (extra-code-block/name (car end-code)))
	   (car end-code))
	  (else
	   (loop (cdr end-code))))))

(define (declare-extra-code-block! name constraint xtra)
  (if (find-extra-code-block name)
      (error "declare-extra-code-block!: Multiply defined block"
	     name)
      (let ((new (extra-code-block/make name constraint xtra))
	    (all *end-of-block-code*))

	(define (constraint-violation new old)
	  (error "declare-extra-code-block!: Inconsistent constraints"
		 new old))

	(case constraint
	  ((FIRST)
	   (if (and (not (null? all))
		    (eq? 'FIRST
			 (extra-code-block/constraint (car all))))
	       (constraint-violation new (car all)))
	   (set! *end-of-block-code* (cons new all)))
	  ((ANYWHERE)
	   (if (or (null? all)
		   (not (eq? 'FIRST
			     (extra-code-block/constraint (car all)))))
	       (set! *end-of-block-code* (cons new all))
	       (set-cdr! all (cons new (cdr all)))))
	  ((LAST)
	   (if (null? all)
	       (set! *end-of-block-code* (list new))
	       (let* ((lp (last-pair all))
		      (old (car lp)))
		 (if (eq? 'LAST (extra-code-block/constraint old))
		     (constraint-violation new old))
		 (set-cdr! lp (cons new '())))))
	  (else
	   (error "declare-extra-code-block!: Unknown constraint"
		  constraint)))
	new)))

(define (add-extra-code! block new-code)
  (set-extra-code-block/code!
   block
   (LLAP (extra-code-block/code block)
	 new-code)))

(define (add-end-of-block-code! code-thunk)
  (add-extra-code!
   (or (find-extra-code-block 'END-OF-BLOCK)
       (declare-extra-code-block! 'END-OF-BLOCK 'ANYWHERE false))
   (code-thunk)))