#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; Debugging Information
;;; package: (compiler debugging-information)

(declare (usual-integrations))

(define (info-generation-phase-1 expression procedures)
  (fluid-let ((*integrated-variables* '()))
    (set-expression-debugging-info!
     expression
     (make-dbg-expression (block->dbg-block (expression-block expression))
			  (expression-label expression)))
    (for-each
     (lambda (procedure)
       (if (procedure-continuation? procedure)
	   (set-continuation/debugging-info!
	    procedure
	    (let ((block (block->dbg-block (continuation/block procedure))))
	      (let ((continuation
		     (make-dbg-continuation
		      block
		      (continuation/label procedure)
		      (enumeration/index->name continuation-types
					       (continuation/type procedure))
		      (continuation/offset procedure)
		      (continuation/debugging-info procedure))))
		(set-dbg-block/procedure! block continuation)
		continuation)))
	   (set-procedure-debugging-info!
	    procedure
	    (let ((block (block->dbg-block (procedure-block procedure))))
	      (let ((procedure
		     (make-dbg-procedure
		      block
		      (procedure-label procedure)
		      (procedure/type procedure)
		      (procedure-name procedure)
		      (map variable->dbg-variable
			   (cdr (procedure-original-required procedure)))
		      (map variable->dbg-variable
			   (procedure-original-optional procedure))
		      (let ((rest (procedure-original-rest procedure)))
			(and rest (variable->dbg-variable rest)))
		      (map variable->dbg-variable (procedure-names procedure))
		      (procedure-debugging-info procedure))))
		(set-dbg-block/procedure! block procedure)
		procedure)))))
     procedures)
    (for-each process-integrated-variable! *integrated-variables*)))

(define (generated-dbg-continuation context label)
  (let ((block
	 (make-dbg-block/continuation (reference-context/block context)
				      false)))
    (let ((continuation
	   (make-dbg-continuation block
				  label
				  'GENERATED
				  (reference-context/offset context)
				  false)))
      (set-dbg-block/procedure! block continuation)
      continuation)))

(define (block->dbg-block block)
  (and block
       (or (block-debugging-info block)
	   (let ((dbg-block
		  (enumeration-case block-type (block-type block)
		    ((STACK) (stack-block->dbg-block block))
		    ((CONTINUATION) (continuation-block->dbg-block block))
		    ((CLOSURE) (closure-block->dbg-block block))
		    ((IC) (ic-block->dbg-block block))
		    (else
		     (error "BLOCK->DBG-BLOCK: Illegal block type" block)))))
	     (set-block-debugging-info! block dbg-block)
	     dbg-block))))

(define (stack-block->dbg-block block)
  (let ((parent (block-parent block))
	(frame-size (block-frame-size block))
	(procedure (block-procedure block)))
    (let ((layout (make-layout frame-size)))
      (for-each (lambda (variable)
		  (if (not (continuation-variable? variable))
		      (layout-set! layout
				   (variable-normal-offset variable)
				   (variable->dbg-variable variable))))
		(block-bound-variables block))
      (if (procedure/closure? procedure)
	  (if (closure-procedure-needs-operator? procedure)
	      (layout-set! layout
			   (procedure-closure-offset procedure)
			   dbg-block-name/normal-closure))
	  (if (stack-block/static-link? block)
	      (layout-set! layout
			   (-1+ frame-size)
			   dbg-block-name/static-link)))
      (make-dbg-block 'STACK
		      (block->dbg-block parent)
		      (if (procedure/closure? procedure)
			  (block->dbg-block
			   (reference-context/block
			    (procedure-closure-context procedure)))
			  (block->dbg-block
			   (procedure-target-block procedure)))
		      layout
		      (block->dbg-block (block-stack-link block))))))

(define (continuation-block->dbg-block block)
  (make-dbg-block/continuation
   (block-parent block)
   (continuation/always-known-operator? (block-procedure block))))

(define (make-dbg-block/continuation parent always-known?)
  (let ((dbg-parent (block->dbg-block parent)))
    (make-dbg-block
     'CONTINUATION
     dbg-parent
     false
     (let ((names
	    (append (if always-known?
			'()
			(list dbg-block-name/return-address))
		    (if (block/dynamic-link? parent)
			(list dbg-block-name/dynamic-link)
			'())
		    (if (ic-block? parent)
			(list dbg-block-name/ic-parent)
			'()))))
       (let ((layout (make-layout (length names))))
	 (do ((names names (cdr names))
	      (index 0 (1+ index)))
	     ((null? names))
	   (layout-set! layout index (car names)))
	 layout))
     dbg-parent)))

(define (closure-block->dbg-block block)
  (let ((parent (block-parent block))
	(start-offset
	 (closure-object-first-offset
	  (block-entry-number (block-shared-block block))))
	(offsets
	 (map (lambda (offset)
		(cons (car offset)
		      (- (cdr offset)
			 (closure-block-first-offset block))))
	      (block-closure-offsets block))))
    (let ((layout (make-layout (1+ (apply max (map cdr offsets))))))
      (for-each (lambda (offset)
		  (layout-set! layout
			       (cdr offset)
			       (variable->dbg-variable (car offset))))
		offsets)
      (if (and parent (ic-block/use-lookup? parent))
	  (layout-set! layout 0 dbg-block-name/ic-parent))
      (make-dbg-block 'CLOSURE (block->dbg-block parent) false
		      (cons start-offset layout)
		      false))))

(define (ic-block->dbg-block block)
  (make-dbg-block 'IC (block->dbg-block (block-parent block))
		  false false false))

(define-integrable (make-layout length)
  (make-vector length false))

(define (layout-set! layout index name)
  (let ((name* (vector-ref layout index)))
    (if name* (error "LAYOUT-SET!: reusing layout slot" name* name)))
  (vector-set! layout index name)
  unspecific)

(define *integrated-variables*)

(define (variable->dbg-variable variable)
  (or (lvalue-get variable dbg-variable-tag)
      (let ((integrated? (lvalue-integrated? variable))
	    (indirection (variable-indirection variable)))
	(let ((dbg-variable
	       (make-dbg-variable
		(variable-name variable)
		(cond (integrated? 'INTEGRATED)
		      (indirection 'INDIRECTED)
		      ((variable-in-cell? variable) 'CELL)
		      (else 'NORMAL))
		(cond (integrated?
		       (lvalue-known-value variable))
		      (indirection
		       ;; This currently does not examine whether it is a
		       ;; simple indirection, or a closure indirection.
		       ;; The value displayed will be incorrect if it
		       ;; is a closure indirection, but...
		       (variable->dbg-variable (car indirection)))
		      (else
		       false)))))
	  (if integrated?
	      (set! *integrated-variables*
		    (cons dbg-variable *integrated-variables*)))
	  (lvalue-put! variable dbg-variable-tag dbg-variable)
	  dbg-variable))))

(define dbg-variable-tag
  "dbg-variable-tag")

(define (process-integrated-variable! variable)
  (set-dbg-variable/value!
   variable
   (let ((rvalue (dbg-variable/value variable)))
     (cond ((rvalue/constant? rvalue) (constant-value rvalue))
	   ((rvalue/procedure? rvalue) (procedure-debugging-info rvalue))
	   (else (error "Illegal variable value" rvalue))))))

(define (info-generation-phase-2 expression procedures continuations)
  (let ((debug-info
	 (lambda (selector object)
	   (or (selector object)
	       (error "Missing debugging info" object)))))
    (values
     (and expression (debug-info rtl-expr/debugging-info expression))
     (map (lambda (procedure)
	    (let ((info (debug-info rtl-procedure/debugging-info procedure)))
	      (set-dbg-procedure/external-label!
	       info
	       (rtl-procedure/%external-label procedure))
	      info))
	  procedures)
     (map (lambda (continuation)
	    (debug-info rtl-continuation/debugging-info continuation))
	  continuations))))

(define (info-generation-phase-3 expression procedures continuations
				 label-bindings external-labels)
  (let ((label-bindings (labels->dbg-labels label-bindings))
	(no-datum '(NO-DATUM)))
    (let ((labels (make-string-hash-table)))
      (for-each (lambda (label-binding)
		  (for-each (lambda (key)
			      (let ((datum
				     (hash-table/get labels key no-datum)))
				(if (not (eq? datum no-datum))
				    (error "Redefining label:" key datum)))
			      (hash-table/put! labels
					       key
					       (cdr label-binding)))
			    (car label-binding)))
		label-bindings)
      (let ((map-label/fail
	     (lambda (label)
	       (let ((key (system-pair-car label)))
		 (let ((datum (hash-table/get labels key no-datum)))
		   (if (eq? datum no-datum)
		       (error "Missing label:" key))
		   datum))))
	    (map-label/false
	     (lambda (label)
	       (hash-table/get labels (system-pair-car label) #f))))
	(for-each (lambda (label)
		    (set-dbg-label/external?! (map-label/fail label) true))
		  external-labels)
	(if expression
	    (set-dbg-expression/label!
	     expression
	     (map-label/fail (dbg-expression/label expression))))
	(for-each
	 (lambda (procedure)
	   (let* ((internal-label (dbg-procedure/label procedure))
		  (mapped-label (map-label/false internal-label)))
	     (set-dbg-procedure/label! procedure mapped-label)
	     (cond ((dbg-procedure/external-label procedure)
		    => (lambda (label)
			 (set-dbg-procedure/external-label!
			  procedure
			  (map-label/fail label))))
		   ((not mapped-label)
		    (error "Missing label" internal-label)))))
	 procedures)
	(for-each
	 (lambda (continuation)
	   (set-dbg-continuation/label!
	    continuation
	    (map-label/fail (dbg-continuation/label continuation))))
	 continuations)))
    (make-dbg-info
     expression
     (list->vector (sort procedures dbg-procedure<?))
     (list->vector (sort continuations dbg-continuation<?))
     (list->vector (map cdr label-bindings)))))

(define (labels->dbg-labels label-bindings)
  (map (lambda (offset-binding)
	 (let ((names (cdr offset-binding)))
	   (cons names
		 (make-dbg-label-2 (choose-distinguished-label names)
				   (car offset-binding)))))
       (let ((offsets (make-rb-tree = <)))
	 (for-each (lambda (binding)
		     (let ((offset (cdr binding))
			   (name (system-pair-car (car binding))))
		       (let ((datum (rb-tree/lookup offsets offset #f)))
			 (if datum
			     (set-cdr! datum (cons name (cdr datum)))
			     (rb-tree/insert! offsets offset (list name))))))
		   label-bindings)
	 (rb-tree->alist offsets))))

(define (choose-distinguished-label names)
  (if (null? (cdr names))
      (car names)
      (let ((distinguished
	     (list-transform-negative names
	       (lambda (name)
		 (or (standard-name? name "label")
		     (standard-name? name "end-label"))))))
	(cond ((null? distinguished)
	       (min-suffix names))
	      ((null? (cdr distinguished))
	       (car distinguished))
	      (else
	       (min-suffix distinguished))))))

(define char-set:label-separators
  (char-set #\- #\_))

(define (min-suffix names)
  (let ((suffix-number
	 (lambda (name)
	   (let ((index (string-find-previous-char-in-set
			 name
			 char-set:label-separators)))
	     (if (not index)
		 (error "Illegal label name" name))
	     (let ((suffix (string-tail name (1+ index))))
	       (let ((result (string->number suffix)))
		 (if (not result)
		     (error "Illegal label suffix" suffix))
		 result))))))
    (car (sort names (lambda (x y)
		       (< (suffix-number x)
			  (suffix-number y)))))))

(define (standard-name? string prefix)
  (let ((index (string-match-forward-ci string prefix))
	(end (string-length string)))
    (and (= index (string-length prefix))
	 (>= (- end index) 2)
	 (let ((next (string-ref string index)))
	   (or (char=? #\- next)
	       (char=? #\_ next)))
	 (let loop ((index (1+ index)))
	   (or (= index end)
	       (and (char-numeric? (string-ref string index))
		    (loop (1+ index))))))))