#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/lapgn1.scm,v 4.7 1989/08/21 19:30:23 cph Exp $

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

;;;; LAP Generator: top level

(declare (usual-integrations))

(define *current-bblock*)
(define *pending-bblocks*)

(define (generate-bits rgraphs remote-links process-constants-block)
  (with-new-node-marks
   (lambda ()
     (for-each cgen-rgraph rgraphs)
     (for-each (lambda (remote-link)
		 (vector-set! remote-link
			      0
			      (constant->label (vector-ref remote-link 0)))
		 unspecific)
	       remote-links)
     (with-values
	 (lambda ()
	   (generate/constants-block *interned-constants*
				     *interned-variables*
				     *interned-assignments*
				     *interned-uuo-links*))
       (or process-constants-block
	   (lambda (constants-code environment-label free-ref-label n-sections)
	     (LAP ,@constants-code
		  ,@(if free-ref-label
			(generate/quotation-header environment-label
						   free-ref-label
						   n-sections)
			(LAP))
		  ,@(let loop ((remote-links remote-links))
		      (if (null? remote-links)
			  (LAP)
			  (LAP ,@(let ((remote-link (car remote-links)))
				   (if (vector-ref remote-link 2)
				       (generate/remote-link
					(vector-ref remote-link 0)
					(vector-ref remote-link 1)
					(vector-ref remote-link 2)
					(vector-ref remote-link 3))
				       (LAP)))
			       ,@(loop (cdr remote-links))))))))))))

(define (cgen-rgraph rgraph)
  (fluid-let ((*current-rgraph* rgraph)
	      (*pending-bblocks* '()))
    (for-each (lambda (edge)
		(if (not (node-marked? (edge-right-node edge)))
		    (cgen-entry edge)))
	      (rgraph-entry-edges rgraph))
    (if (not (null? *pending-bblocks*))
	(error "CGEN-RGRAPH: pending blocks left at end of pass"))))

(define (cgen-entry edge)
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
	    (cond ((not (for-all? previous edge-left-node))
		   ;; Assumption: no action needed to clear existing
		   ;; register map at this point.
		   (loop next (empty-register-map)))
		  ((null? (cdr previous))
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
	    (fluid-let ((*dead-registers* (rinst-dead-registers rinst))
			(*prefix-instructions* (LAP))
			(*needed-registers* '()))
	      (let ((instructions (match-result)))
		(delete-dead-registers!)
		(LAP ,@*prefix-instructions* ,@instructions)))
	    (begin (error "CGEN-RINST: No matching rules" rtl)
		   (loop)))))))

(define (adjust-maps-at-merge! bblock)
  (let ((edges (node-previous-edges bblock)))    (let ((maps
	   (map
	    (let ((live-registers (bblock-live-at-entry bblock)))
	      (lambda (edge)
		(register-map:keep-live-entries
		 (bblock-register-map (edge-left-node edge))
		 live-registers)))
	    edges)))
      (let ((target-map (merge-register-maps maps false)))
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

(define (add-statement-rule! pattern result-procedure)
  (let ((result (cons pattern result-procedure)))
    (cond ((not (eq? (car pattern) 'ASSIGN))
	   (let ((entry (assq (car pattern) *cgen-rules*)))
	     (if entry
		 (set-cdr! entry (cons result (cdr entry)))
		 (set! *cgen-rules*
		       (cons (list (car pattern) result)
			     *cgen-rules*)))))
	  ((not (pattern-variable? (cadr pattern)))
	   (let ((entry (assq (caadr pattern) *assign-rules*)))
	     (if entry
		 (set-cdr! entry (cons result (cdr entry)))
		 (set! *assign-rules*
		       (cons (list (caadr pattern) result)
			     *assign-rules*)))))
	  (else
	   (set! *assign-variable-rules*
		 (cons result *assign-variable-rules*)))))
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