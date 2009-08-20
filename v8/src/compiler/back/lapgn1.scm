#| -*-Scheme-*-

$Id$

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

;;;; LAP Generator: top level
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define *current-bblock*)
(define *pending-bblocks*)
(define *insert-rtl?*)

(define (generate-lap rgraphs remote-links process-constants-block)
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

  (define (delay-block bblock edge)
    (let ((entry
	   (or (assq bblock *pending-bblocks*)
	       (let ((entry
		      (cons bblock
			    (list-transform-positive
				(node-previous-edges bblock)
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
	      (loop bblock
		    (adjust-maps-at-merge! rgraph bblock)))))))

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
		   (delay-block next edge)))))))

  (let ((bblock (edge-right-node edge)))
    (if (not (there-exists? (node-previous-edges bblock) edge-left-node))
	(loop bblock (empty-register-map))
	(delay-block bblock edge))))

(define (cgen-bblock bblock map)
  ;; This procedure is coded out of line to facilitate debugging.
  (node-mark! bblock)
  (fluid-let ((*current-bblock* bblock)
	      (*register-map* map)
	      (*preserved-registers* '())
	      (*recomputed-registers* '()))
    (profile-info/start)
    (set-bblock-instructions! bblock
			      (let loop ((rinst (bblock-instructions bblock)))
				(if (rinst-next rinst)
				    (let ((instructions (cgen-rinst rinst)))
				      (LAP ,@instructions
					   ,@(loop (rinst-next rinst))))
				    (cgen-rinst rinst))))
    (profile-info/end)
    (set-bblock-register-map! bblock *register-map*)))

(define (cgen-rinst rinst)
  (let loop ((rtl (rinst-rtl rinst))
	     (dead-registers (rinst-dead-registers rinst)))
    (let ((match-result (lap-generator/match-rtl-instruction rtl)))
      (cond (match-result
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
	    ;; The following presumes that PRESERVE and RESTORE do
	    ;; not match, or, if they do, they are completely handled
	    ;; by the back end.
	    ((eq? (car rtl) 'PRESERVE)
	     (preserve-register!
	      (rtl:register-number (rtl:preserve-register rtl))
	      (rtl:preserve-how rtl))
	     (if *insert-rtl?*
		 (LAP (COMMENT (RTL ,rtl)))
		 (LAP)))
	    ((eq? (car rtl) 'RESTORE)
	     (cgen-restore rtl loop dead-registers))
	    ((eq? (car rtl) 'PROFILE-DATA)
	     (profile-info/add (second (second rtl)))
	     (if *insert-rtl?*
		 (LAP (COMMENT (RTL ,rtl)))
		 (LAP)))
	    (else
	     (error "CGEN-RINST: No matching rules" rtl)
	     (loop rtl dead-registers))))))

(define (cgen-restore rtl loop dead-registers)
  (let ((restore-reg (rtl:restore-register rtl))
	(restore-value (rtl:restore-value rtl)))
    (call-with-values
     (lambda ()
       (restored-register-home (rtl:register-number restore-reg)))
     (lambda (available? reg-where-desired)
       (let ((instrs
	      (LAP
	       (COMMENT (RESTORING ,restore-reg ,restore-value ,available? ,reg-where-desired))
	       ,@(cond (available?
			;; Either has aliases or in register home.
			(LAP))
		       ((not reg-where-desired)
			(loop `(ASSIGN ,restore-reg ,restore-value)
			      dead-registers))
		       (else
			(let* ((code1 (loop
				       `(ASSIGN (REGISTER ,reg-where-desired)
						,restore-value)
				       dead-registers))
			       (code2 (loop
				       `(ASSIGN ,restore-reg
						(REGISTER ,reg-where-desired))
				       '()))
			       (instrs (LAP ,@code1
					    ,@code2)))
			  (release-register! reg-where-desired)
			  instrs))))))
	 (if *insert-rtl?*
	     (LAP (COMMENT (RTL ,rtl))
		  ,@instrs)
	     instrs))))))

(define (adjust-maps-at-merge! rgraph bblock)
  (let* ((edges (list-transform-positive (node-previous-edges bblock)
		  edge-left-node))
	 (maps (map (let ((live-registers (bblock-live-at-entry bblock)))
		      (lambda (edge)
			(register-map:keep-live-entries
			 (bblock-register-map (edge-left-node edge))
			 live-registers)))
		    edges))
	 (pairs (map cons edges maps))
	 #|
	 (target-map (merge-register-maps maps false))
	 |#
	 (target-map (choose-register-map rgraph pairs)))
    (for-each
     (lambda (class)
       (let ((instructions
	      (coerce-map-instructions (cdar class) target-map)))
	 (if (not (null? instructions))
	     (let ((sblock (make-sblock (LAP (COMMENT MAP MERGE:)
					     ,@instructions))))
	       (node-mark! sblock)
	       (edge-insert-snode! (caar class) sblock)
	       (for-each (lambda (x)
			   (let ((edge (car x)))
			     (edge-disconnect-right! edge)
			     (edge-connect-right! edge sblock)))
			 (cdr class))))))
     (equivalence-classes pairs
			  (lambda (x y) (map-equal? (cdr x) (cdr y)))))
    target-map))

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

(define (choose-register-map rgraph edges&maps)
  ;; Choose the map corresponding to the "best" edge,
  ;; and coerce the rest to that shape.
  ;; For now, a very simple decision,
  ;; plus, the labels are removed!!
  rgraph				; ignored
  (let ((non-continuations (list-transform-positive edges&maps
			     (lambda (edge&map)
			       (let* ((edge (car edge&map))
				      (bblock (edge-left-node edge)))
				 (for-all? (node-previous-edges bblock)
				   edge-left-node))))))
    (register-map:without-labels
     (cdr (car (if (null? non-continuations)
		   edges&maps
		   non-continuations))))))

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
