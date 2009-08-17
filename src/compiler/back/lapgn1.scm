#| -*-Scheme-*-

$Id: 71dfdc18cbb4b616cb2c7645af945713b0d83f3d $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; LAP Generator: top level
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define *current-bblock*)
(define *pending-bblocks*)
(define *insert-rtl?*)

(define (generate-lap rgraphs remote-links process-constants-block)
  (pre-lapgen-analysis rgraphs)
  (fluid-let ((*insert-rtl?*
	       (and compiler:generate-lap-files?
		    compiler:intersperse-rtl-in-lap?)))
    (with-new-node-marks
      (lambda ()
	(for-each cgen-rgraph rgraphs)
	(let ((link-info
	       (and compiler:compress-top-level?
		    (not (null? remote-links))
		    (not (null? (cdr remote-links)))
		    (let* ((index->vector
			    (lambda (index)
			      (list->vector
			       (map (lambda (remote-link)
				      (vector-ref remote-link index))
				    remote-links))))
			   (index->constant-label
			    (lambda (index)
			      (constant->label (index->vector index)))))
		      (list (length remote-links)
			    ;; cc blocks
			    (index->constant-label 0)
			    ;; number of linker sections
			    (index->vector 3))))))

	  (if (not link-info)
	      (for-each (lambda (remote-link)
			  (vector-set! remote-link
				       0
				       (constant->label
					(vector-ref remote-link 0)))
			  unspecific)
			remote-links))
	    
	  (with-values prepare-constants-block
	    (or process-constants-block
		(lambda (constants-code environment-label free-ref-label
					n-sections)
		  (LAP ,@constants-code
		       ,@(generate/quotation-header environment-label
						    (or free-ref-label
							environment-label)
						    n-sections)
		       ,@(if link-info
			     (generate/remote-links (car link-info)
						    (cadr link-info)
						    (caddr link-info))
			     (let loop ((remote-links remote-links))
			       (if (null? remote-links)
				   (LAP)
				   (LAP
				    ,@(let ((remote-link (car remote-links)))
					(generate/remote-link
					 (vector-ref remote-link 0)
					 (vector-ref remote-link 1)
					 (or (vector-ref remote-link 2)
					     (vector-ref remote-link 1))
					 (vector-ref remote-link 3)))
				    ,@(loop (cdr remote-links)))))))))))))))

(define (cgen-rgraph rgraph)
  (fluid-let ((*current-rgraph* rgraph)
	      (*pending-bblocks* '()))
    (for-each (lambda (edge)
		(if (not (node-marked? (edge-right-node edge)))
		    (cgen-entry rgraph edge)))
	      (rgraph-entry-edges rgraph))
    (if (not (null? *pending-bblocks*))
	(error "CGEN-RGRAPH: pending blocks left at end of pass"))))

(define (cgen-entry rgraph edge)
  (define (loop bblock map)
    (cgen-bblock bblock map)
    (if (sblock? bblock)
	(cgen-right (snode-next-edge bblock))
	(begin
	  (cgen-right (pnode-consequent-edge bblock))
	  (cgen-right (pnode-alternative-edge bblock)))))

  (define (cgen-right edge)
    (let ((next (edge-next-node edge)))
      (if (and next (not (node-marked? next)))
	  (let ((previous (node-previous-edges next)))
	    (cond ((for-all? previous
		     (lambda (edge)
		       (memq edge (rgraph-entry-edges rgraph))))
		   ;; Assumption: no action needed to clear existing
		   ;; register map at this point.
		   (loop next (empty-register-map)))
		  ((and (null? (cdr previous))
			(edge-left-node (car previous)))
		   (loop
		    next
		    (let ((previous (edge-left-node edge)))
		      (delete-pseudo-registers
		       (bblock-register-map previous)
		       (regset->list
			(regset-difference (bblock-live-at-exit previous)
					   (bblock-live-at-entry next)))))))
		  (else
		   (let ((entry
			  (or (assq next *pending-bblocks*)
			      (let ((entry
				     (cons next
					   (list-transform-positive
					       previous
					     edge-left-node))))
				(set! *pending-bblocks*
				      (cons entry
					    *pending-bblocks*))
				entry))))
		     (let ((dependencies (delq! edge (cdr entry))))
		       (if (not (null? dependencies))
			   (set-cdr! entry dependencies)
			   (begin
			     (set! *pending-bblocks*
				   (delq! entry *pending-bblocks*))
			     (loop next (adjust-maps-at-merge! next))))))))))))

  (loop (edge-right-node edge) (empty-register-map)))

(define (cgen-bblock bblock map)
  ;; This procedure is coded out of line to facilitate debugging.
  (node-mark! bblock)
  (fluid-let ((*current-bblock* bblock)
	      (*register-map* map))
    (set-bblock-instructions! bblock
			      (let loop ((rinst (bblock-instructions bblock)))
				(if (rinst-next rinst)
				    (let ((instructions (cgen-rinst rinst)))
				      (LAP ,@instructions
					   ,@(loop (rinst-next rinst))))
				    (cgen-rinst rinst))))
    (set-bblock-register-map! bblock *register-map*)))

(define (cgen-rinst rinst)
  (let ((rtl (rinst-rtl rinst)))
    ;; LOOP is for easy restart while debugging.
    (let loop ()
      (let ((match-result (lap-generator/match-rtl-instruction rtl)))
	(if match-result
	    (let ((dead-registers (rinst-dead-registers rinst)))
	      (fluid-let ((*dead-registers* dead-registers)
			  (*registers-to-delete* dead-registers)
			  (*prefix-instructions* (LAP))
			  (*suffix-instructions* (LAP))
			  (*needed-registers* '()))
		(let ((instructions (match-result)))
		  (delete-dead-registers!)
		  (LAP ,@(if *insert-rtl?*
			     (LAP (COMMENT (RTL ,rtl)))
			     (LAP))
		       ,@*prefix-instructions*
		       ,@instructions
		       ,@*suffix-instructions*))))
	    (begin (error "CGEN-RINST: No matching rules" rtl)
		   (loop)))))))

(define (adjust-maps-at-merge! bblock)
  (let ((edges
	 (list-transform-positive (node-previous-edges bblock)
	   edge-left-node)))
    (let ((maps
	   (map
	    (let ((live-registers (bblock-live-at-entry bblock)))
	      (lambda (edge)
		(register-map:keep-live-entries
		 (bblock-register-map (edge-left-node edge))
		 live-registers)))
	    edges)))
      (let ((target-map (merge-register-maps maps #f)))
	(for-each
	 (lambda (class)
	   (let ((instructions
		  (coerce-map-instructions (cdar class) target-map)))
	     (if (not (null? instructions))
		 (let ((sblock (make-sblock instructions)))
		   (node-mark! sblock)
		   (edge-insert-snode! (caar class) sblock)
		   (for-each (lambda (x)
			       (let ((edge (car x)))
				 (edge-disconnect-right! edge)
				 (edge-connect-right! edge sblock)))
			     (cdr class))))))
	 (equivalence-classes (map cons edges maps)
			      (lambda (x y) (map-equal? (cdr x) (cdr y)))))
	target-map))))

(define (equivalence-classes objects predicate)
  (let ((find-class (association-procedure predicate car)))
    (let loop ((objects objects) (classes '()))
      (if (null? objects)
	  classes
	  (let ((class (find-class (car objects) classes)))
	    (if (not class)
		(loop (cdr objects)
		      (cons (list (car objects)) classes))
		(begin
		  (set-cdr! class (cons (car objects) (cdr class)))
		  (loop (cdr objects) classes))))))))

(define *cgen-rules* '())
(define *assign-rules* '())
(define *assign-variable-rules* '())

(define (add-statement-rule! pattern matcher)
  (cond ((not (eq? (car pattern) 'ASSIGN))
	 (let ((entry (assq (car pattern) *cgen-rules*)))
	   (if entry
	       (set-cdr! entry (cons matcher (cdr entry)))
	       (set! *cgen-rules*
		     (cons (list (car pattern) matcher)
			   *cgen-rules*)))))
	((not (pattern-variable? (cadr pattern)))
	 (let ((entry (assq (caadr pattern) *assign-rules*)))
	   (if entry
	       (set-cdr! entry (cons matcher (cdr entry)))
	       (set! *assign-rules*
		     (cons (list (caadr pattern) matcher)
			   *assign-rules*)))))
	(else
	 (set! *assign-variable-rules*
	       (cons matcher *assign-variable-rules*))))
  pattern)

(define (lap-generator/match-rtl-instruction rtl)
  ;; Match a single RTL instruction, returning a thunk to generate the
  ;; LAP.  This is used in the RTL optimizer at certain points to
  ;; determine if a rewritten instruction is valid.
  (if (not (rtl:assign? rtl))
      (let ((rules (assq (rtl:expression-type rtl) *cgen-rules*)))
	(and rules (pattern-lookup (cdr rules) rtl)))
      (let ((rules
	     (assq (rtl:expression-type (rtl:assign-address rtl))
		   *assign-rules*)))
	(or (and rules (pattern-lookup (cdr rules) rtl))
	    (pattern-lookup *assign-variable-rules* rtl)))))

;;; Instruction sequence sharing mechanisms

(define *block-associations*)

(define (block-association token)
  (let ((place (assq token *block-associations*)))
    (and place (cdr place))))

(define (block-associate! token frob)
  (set! *block-associations*
	(cons (cons token frob)
	      *block-associations*))
  unspecific)

;; This can only be used when the instruction sequences are bit-wise identical.
;; In other words, no variable registers, constants, etc.

(define (share-instruction-sequence! name if-shared generator)
  (cond ((block-association name)
	 => if-shared)
	(else
	 (let ((label (generate-label name)))
	   (block-associate! name label)
	   (generator label)))))

(define (make-new-sblock instructions)
  (let ((bblock (make-sblock instructions)))
    (node-mark! bblock)
    bblock))

(define (current-bblock-continue! bblock)
  (let ((current-bblock *current-bblock*))
    (if (sblock-continuation current-bblock)
	(error "current-bblock-continue! bblock already has a continuation"
	       current-bblock)
	(begin
	  (create-edge! current-bblock set-snode-next-edge! bblock)
	  (set-bblock-continuations! current-bblock (list bblock))
	  (set-sblock-continuation! current-bblock bblock)))))

(define (lap:comment comment)
  (if compiler:generate-lap-files?
      (LAP (COMMENT (LAP ,comment)))
      (LAP)))