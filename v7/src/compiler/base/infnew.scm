#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/infnew.scm,v 4.3 1988/12/30 07:02:35 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Debugging Information

(declare (usual-integrations))

(define (info-generation-phase-1 expression procedures)
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
		   (make-dbg-continuation block
					  (continuation/label procedure)
					  (enumeration/index->name
					   continuation-types
					   (continuation/type procedure))
					  (continuation/offset procedure))))
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
		    (symbol->string (procedure-name procedure))
		    (map variable->dbg-name
			 (cdr (procedure-required procedure)))
		    (map variable->dbg-name (procedure-optional procedure))
		    (let ((rest (procedure-rest procedure)))
		      (and rest (variable->dbg-name rest)))
		    (map variable->dbg-name (procedure-names procedure)))))
	      (set-dbg-block/procedure! block procedure)
	      procedure)))))
   procedures))

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
				   (variable->dbg-name variable))))
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
	(offsets
	 (map (lambda (offset)
		(cons (car offset)
		      (- (cdr offset) closure-block-first-offset)))
	      (block-closure-offsets block))))
    (let ((layout (make-layout (1+ (apply max (map cdr offsets))))))
      (for-each (lambda (offset)
		  (layout-set! layout
			       (cdr offset)
			       (variable->dbg-name (car offset))))
		offsets)
      (if (and parent (ic-block/use-lookup? parent))
	  (layout-set! layout 0 dbg-block-name/ic-parent))
      (make-dbg-block 'CLOSURE (block->dbg-block parent) layout false))))

(define (ic-block->dbg-block block)
  (make-dbg-block 'IC (block->dbg-block (block-parent block)) false false))

(define-integrable (make-layout length)
  (make-vector length false))

(define (layout-set! layout index name)
  (let ((name* (vector-ref layout index)))
    (if name* (error "LAYOUT-SET!: reusing layout slot" name* name)))
  (vector-set! layout index name)
  unspecific)

(define-integrable (variable->dbg-name variable)
  (symbol->dbg-name (variable-name variable)))

(define (generated-dbg-continuation context label)
  (let ((block
	 (make-dbg-block/continuation (reference-context/block context)
				      false)))
    (let ((continuation
	   (make-dbg-continuation block
				  label
				  'GENERATED
				  (reference-context/offset context))))
      (set-dbg-block/procedure! block continuation)
      continuation)))

(define (info-generation-phase-2 expression procedures continuations)
  (let ((debug-info
	 (lambda (selector object)
	   (or (selector object)
	       (error "Missing debugging info" object)))))
    (values
     (debug-info rtl-expr/debugging-info expression)
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
  (let ((dbg-labels (labels->dbg-labels label-bindings)))
    (let ((labels (make-btree)))
      (for-each (lambda (dbg-label)
		  (for-each (lambda (name)
			      (btree-insert! labels string<? car name
				(lambda (name)
				  (cons name dbg-label))
				(lambda (association)
				  (error "redefining label" association))
				(lambda (association)
				  association
				  unspecific)))
			    (dbg-label/names dbg-label)))
		dbg-labels)
      (let ((map-label
	     (lambda (label)
	       (btree-lookup labels string<? car (system-pair-car label)
		 cdr
		 (lambda (name)
		   (error "Missing label" name))))))
	(for-each (lambda (label)
		    (set-dbg-label/external?! (map-label label) true))
		  external-labels)
	(set-dbg-expression/label!
	 expression
	 (map-label (dbg-expression/label expression)))	(for-each
	 (lambda (procedure)
	   (set-dbg-procedure/label!
	    procedure
	    (map-label (dbg-procedure/label procedure)))
	   (let ((label (dbg-procedure/external-label procedure)))
	     (if label
		 (set-dbg-procedure/external-label! procedure
						    (map-label label)))))
	 procedures)
	(for-each
	 (lambda (continuation)
	   (set-dbg-continuation/label!
	    continuation
	    (map-label (dbg-continuation/label continuation))))
	 continuations)))
    (make-dbg-info
     expression
     (list->vector (sort procedures dbg-procedure<?))
     (list->vector (sort continuations dbg-continuation<?))
     (list->vector dbg-labels))))

(define (labels->dbg-labels label-bindings)
  (let ((dbg-labels
	 (let ((labels (make-btree)))
	   (for-each
	    (lambda (binding)
	      (let ((name (system-pair-car (car binding))))
		(btree-insert! labels < dbg-label/offset (cdr binding)
		  (lambda (offset)
		    (make-dbg-label name offset))
		  (lambda (dbg-label)
		    (set-dbg-label/names!
		     dbg-label
		     (cons name (dbg-label/names dbg-label))))
		  (lambda (dbg-label)
		    dbg-label
		    unspecific))))
	    label-bindings)
	   (btree-fringe labels))))
    (for-each (lambda (dbg-label)
		(set-dbg-label/name!
		 dbg-label
		 (choose-distinguished-label (dbg-label/names dbg-label))))
	      dbg-labels)
    dbg-labels))

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

(define (min-suffix names)
  (let ((suffix-number
	 (lambda (name)
	   (let ((index (string-find-previous-char name #\-)))
	     (if (not index)
		 (error "Illegal label name" name))
	     (let ((suffix (string-tail name (1+ index))))
	       (let ((result (string->number suffix)))
		 (if (not result)
		     (error "Illegal label suffix" suffix))
		 result))))))
    (car (sort names (lambda (x y) (< (suffix-number x) (suffix-number y)))))))

(define (standard-name? string prefix)
  (let ((index (string-match-forward-ci string prefix))
	(end (string-length string)))
    (and (= index (string-length prefix))
	 (>= (- end index) 2)
	 (char=? #\- (string-ref string index))
	 (let loop ((index (1+ index)))
	   (or (= index end)
	       (and (char-numeric? (string-ref string index))
		    (loop (1+ index))))))))