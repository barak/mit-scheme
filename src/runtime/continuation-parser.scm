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

;;;; Continuation Parser
;;; package: (runtime new-continuation-parser)

(declare (usual-integrations))

(add-boot-deps! '(runtime microcode-tables) '(runtime history))

(define-primitives
  (return-frame-type 2))

(define-deferred return-frame-types
  (microcode-return-frame-types))

(define (continuation->cframe-generator continuation)
  (let ((graw
	 (control-point->raw-frame-generator
	  (continuation/control-point continuation)))
	(tracked-items
	 (initial-tracked-items
	  'dynamic-state
	  (continuation/dynamic-state continuation)
	  'block-thread-events?
	  (continuation/block-thread-events? continuation))))

    (define (generator)
      (let ((raw-result (graw)))
	(if (eof-object? raw-result)
	    raw-result
	    (let ((frame (decode-raw-result raw-result tracked-items)))
	      (set! tracked-items (update-tracked-items tracked-items frame))
	      frame))))

    generator))

(define (continuation->cframe-stream continuation)
  (generator->stream (continuation->cframe-generator continuation)))

(define (decode-raw-result raw-result bindings)
  (let ((findex (vector-ref raw-result 0))
	(cpend (vector-ref raw-result 1))
	(raw (vector-ref raw-result 2)))
    (let ((info (vector-ref return-frame-types (return-frame-type raw 0)))
	  (return-code-name
	   (let ((address (vector-ref raw 0)))
	     (and (interpreter-return-address? address)
		  (return-address/name address)))))

      (define (make ftype . alist)
	(make-cframe ftype findex cpend raw info alist bindings))

      (define-integrable (name index)
	(vector-ref info (fix:+ 3 (fix:* 2 index))))

      (define-integrable (val-loc index)
	(vector-ref info (fix:+ 4 (fix:* 2 index))))

      (define-integrable (elt index)
	(cons (name index) (vector-ref raw (val-loc index))))

      (define-integrable (rest-elts index)
	(cons (name index) (vector->list raw (val-loc index))))

      (let ((frame-type-name (vector-ref info 0)))
	(case frame-type-name
	  ((return-to-interpreter)
	   (make frame-type-name))
	  ((cc-restore-interrupt-mask)
	   (make frame-type-name (elt 0)))
	  ((exp+env history stack-marker)
	   (make return-code-name (elt 0) (elt 1)))
	  ((cc-stack-marker)
	   (make frame-type-name (elt 0) (elt 1)))
	  ((apply combination-apply)
	   (make return-code-name (elt 0) (rest-elts 1)))
	  ((cc-internal-apply cc-bkpt cc-invocation)
	   (make frame-type-name (elt 0) (rest-elts 1)))
	  ((with-arg with-arg-subproblem)
	   (let ((name
		  (case return-code-name
		    ((join-stacklets) 'control-point)
		    ((access-continue) 'expression)
		    ((force-snap-thunk) 'delayed)
		    ((normal-garbage-collect-done) 'gc-result)
		    ((restore-value pop-return-error) 'value)
		    ((restore-interrupt-mask) 'interrupt-mask)
		    ((halt) 'termination-code)
		    (else #f))))
	     (if name
		 (make return-code-name
		       (cons name (vector-ref raw (val-loc 0))))
		 (make return-code-name))))
	  ((return-to-compiled-code return-to-compiled-code-subproblem)
	   (apply make return-code-name (elt 0)
		  (return-to-cc-extra-fields return-code-name raw)))
	  ((compiled-address)
	   (apply make frame-type-name (cc-address-extra-fields raw 0)))
	  ((combination-save)
	   (let ((n-blanks (manifest-nmv-datum (vector-ref raw (val-loc 2)))))
	     (make return-code-name
		   (elt 0)
		   (elt 1)
		   (cons (name 2) n-blanks)
		   (cons (name 3)
			 (vector->list raw (fix:+ (val-loc 3) n-blanks))))))
	  ((hardware-trap)
	   (make return-code-name (elt 0) (elt 1) (elt 2) (elt 3)
		 (elt 4) (elt 5) (elt 6) (elt 7)))
	  (else
	   (error "Unknown return-frame-type code:" frame-type-name)))))))

(define (cc-address-extra-fields raw index)
  (let ((entry (vector-ref raw index)))
    (if (compiled-procedure? entry)
	(list (cons 'procedure entry)
	      (cons 'arguments (vector->list raw (fix:+ index 1))))
	(list (cons 'entry entry)))))

(define (return-to-cc-extra-fields return-code-name raw)
  (case return-code-name
    ((compiler-reference-trap-restart
      compiler-safe-reference-trap-restart
      compiler-unassigned?-trap-restart)
     (list (cons 'variable (vector-ref raw 2))
	   (cons 'environment (vector-ref raw 3))))
    ((compiler-assignment-trap-restart)
     (list (cons 'variable (vector-ref raw 2))
	   (cons 'environment (vector-ref raw 3))
	   (cons 'value (vector-ref raw 4))))
    ((compiler-lookup-apply-trap-restart
      compiler-operator-lookup-trap-restart)
     (list (cons 'variable (vector-ref raw 2))
	   (cons 'environment (vector-ref raw 3))
	   (cons 'arguments (vector->list raw 6))))
    ((compiler-error-restart)
     (list (cons 'primitive (vector-ref raw 2))))
    ((compiler-interrupt-restart)
     (cons (cons 'state (vector-ref raw 2))
	   (cc-address-extra-fields raw 3)))
    (else
     '())))

;;;; Frame abstraction

(define-record-type <cframe>
    make-cframe
    cframe?
  (type cframe-type)
  (start cframe-start)			;index of frame within control point
  (cpoint-end cframe-cpoint-end)	;length of control point
  (raw cframe-raw)
  (info cframe-info)
  (fields cframe-fields)
  (tracked-items cframe-tracked-items))

(define-print-method cframe?
  (standard-print-method 'cframe
    (lambda (frame)
      (list (cframe-type frame)))))

(define-pp-describer cframe?
  (lambda (frame)
    (list (list 'start (cframe-start frame))
	  (list 'length (cframe-length frame))
	  (list 'raw (cframe-raw frame))
	  (cons 'fields
		(map (lambda (field)
		       (list (car field) (cdr field)))
		     (cframe-fields frame)))
	  (cons 'tracked-items
		(map (lambda (item)
		       (list (tracked-item-name item)
			     (tracked-item-value item)))
		     (cframe-tracked-items frame))))))

(define (cframe-end frame)
  (fix:+ (cframe-start frame)
	 (cframe-length frame)))

(define (cframe-length frame)
  (vector-length (cframe-raw frame)))

(define (cframe-ref frame index)
  (vector-ref (cframe-raw frame) index))

(define (cframe-return-address frame)
  (vector-ref (cframe-raw frame) 0))

(define (cframe-return-code frame)
  (let ((return-address (cframe-return-address frame)))
    (and (interpreter-return-address? return-address)
	 (return-address/code return-address))))

(define (cframe-compiled-code? frame)
  (compiled-return-address? (cframe-return-address frame)))

(define (cframe-return-type frame)
  (vector-ref (cframe-info frame) 0))

(define (cframe-subproblem? frame)
  (or (vector-ref (cframe-info frame) 1)
      (cframe:stack-marker-of-type? with-repl-eval-boundary frame)))

(define (cframe-history-subproblem? frame)
  (vector-ref (cframe-info frame) 2))

(define (cframe-field-value frame name #!optional default)
  (let ((p (assq name (cframe-fields frame))))
    (if p
	(cdr p)
	(begin
	  (if (default-object? default)
	      (error "Unknown frame field name:" name))
	  default))))

(define (cframe-field-name? frame name)
  (and (assq name (cframe-fields frame)) #t))

(define (cframe-field-names frame)
  (map car (cframe-fields frame)))

(define (cframe-tracked-item-value frame name)
  (let ((item (find-tracked-item (cframe-tracked-items frame) name)))
    (if (not item)
	(error "Unknown tracked item name:" name))
    (tracked-item-value item)))

(define (cframe-tracked-item-names frame)
  (map tracked-item-name (cframe-tracked-items frame)))

(define (cframe-tracked-item-name? frame name)
  (and (find-tracked-item (cframe-tracked-items frame) name) #t))

(define (cframe:join-stacklets? frame)
  (eq? (cframe-type frame) 'join-stacklets))

(define (cframe:restore-interrupt-mask? frame)
  (let ((type (cframe-type frame)))
    (or (eq? type 'restore-interrupt-mask)
	(eq? type 'cc-restore-interrupt-mask))))

(define (cframe:restore-history? frame)
  (let ((type (cframe-type frame)))
    (or (eq? type 'restore-history)
	(eq? type 'restore-dont-copy-history))))

(define (cframe:stack-marker? frame)
  (let ((type (cframe-type frame)))
    (or (eq? type 'stack-marker)
	(eq? type 'cc-stack-marker))))

(define (cframe:stack-marker-of-type? marker-type frame)
  (and (cframe:stack-marker? frame)
       (eq? marker-type (cframe-field-value frame 'marker-type))))

;;;; Frame-stream operations

(define (cframe-stream-next-subproblem frames)
  (if (cframe-subproblem? (stream-car frames))
      (let ((frames* (stream-cdr frames)))
	(and (stream-pair? frames*)
	     (cframe-stream-skip-non-subproblems frames*)))
      (cframe-stream-skip-non-subproblems frames)))

(define (cframe-stream-skip-non-subproblems frames)
  (if (cframe-subproblem? (stream-car frames))
      frames
      (let ((frames* (stream-cdr frames)))
	(if (stream-pair? frames*)
	    (cframe-stream-skip-non-subproblems frames*)
	    frames*))))

(define (cframe-stream-ref frames index)
  (guarantee non-negative-fixnum? index 'cframe-stream-ref)
  (let-values (((frames* index*) (find-frame-with-index frames index)))
    (if (not (stream-pair? frames*))
	(error:bad-range-argument index 'cframe-stream-ref))
    (cframe-ref (stream-car frames*) index*)))

(define (cframe-stream-resolve-stack-address frames address)
  (let* ((offset (stack-address-offset address))
	 (index
	  (fix:- (let ((frame (stream-car frames)))
		   (fix:- (cframe-cpoint-end frame)
			  (cframe-start frame)))
		 offset)))
    (assert (fix:>= index 0))
    (let-values (((frames* index*) (find-frame-with-index frames index)))
      (if (not (stream-pair? frames*))
	  (error:bad-range-argument frames
				    'cframe-stream-resolve-stack-address))
      (values frames* index*))))

(define (find-frame-with-index frames index)
  (let ((n (cframe-length (stream-car frames))))
    (if (fix:< index n)
	(values frames index)
	(let ((frames* (stream-cdr frames)))
	  (if (stream-pair? frames*)
	      (find-frame-with-index frames* (fix:- index n))
	      (values frames* index))))))

(define (cframe-stream->continuation frames)
  (let ((frame (stream-car frames)))
    (make-continuation
     (make-control-point*
      (old-raw-frames (cframe-stream-raw-prefix frames)
		      (cframe-tracked-item-value frame 'interrupt-mask)
		      (cframe-tracked-item-value frame 'history)
		      (cframe-previous-restore-history-offset frame)))
     (cframe-tracked-item-value frame 'dynamic-state)
     (cframe-tracked-item-value frame 'block-thread-events?))))

(define (cframe-stream->control-point frames)
  (make-control-point* (cframe-stream-raw-prefix frames)))

(define (cframe-stream-raw-prefix frames)
  (let loop ((frames frames) (raw '()))
    (let ((frame (stream-car frames))
	  (frames* (stream-cdr frames)))
      (let ((raw (cons (cframe-raw frame) raw)))
	(if (or (cframe:join-stacklets? frame)
		(not (stream-pair? frames*)))
	    (reverse raw)
	    (loop frames* raw))))))

(define (old-raw-frames frames interrupt-mask history
			previous-restore-history-offset)
  (if (and (pair? frames)
	   (pair? (cdr frames))
	   (let ((f1 (car frames))
		 (f2 (car frames)))
	     (and (eq? (vector-ref f1 0)
		       (ucode-return-address restore-interrupt-mask))
		  (eqv? (vector-ref f1 1) interrupt-mask)
		  (eq? (vector-ref f2 0) (ucode-return-address restore-history))
		  (eq? (vector-ref f2 1) history)
		  (eqv? (vector-ref f2 2) previous-restore-history-offset))))
      frames
      (cons* (vector (ucode-return-address restore-interrupt-mask)
		     interrupt-mask)
	     (vector (ucode-return-address restore-history)
		     history
		     previous-restore-history-offset
		     #f)
	     frames)))

;;;; Tracked items

(define (initial-tracked-items . inits)
  (map (lambda (entry)
	 (cons entry
	       (let ((value (get-keyword-value inits (vector-ref entry 0))))
		 (if (default-object? value)
		     (vector-ref entry 1)
		     value))))
       defined-items))

(define (update-tracked-items items frame)
  (map (lambda (item)
	 (cons (car item)
	       ((tracked-item-updater item)
		(tracked-item-value item)
		frame)))
       items))

(define (find-tracked-item items name)
  (find (lambda (item)
	  (eq? (tracked-item-name item) name))
	items))

(define-integrable (tracked-item-name item)
  (vector-ref (car item) 0))

(define-integrable (tracked-item-updater item)
  (vector-ref (car item) 2))

(define-integrable (tracked-item-value item)
  (cdr item))

(define (define-item name initial-value updater)
  (let ((entry (vector name initial-value updater))
	(tail
	 (find-tail (lambda (entry)
		      (eq? (vector-ref entry 0) name))
		    defined-items)))
    (if tail
	(set-car! tail entry)
	(set! defined-items (cons entry defined-items)))))

(define defined-items
  '())

(define (simple-item-updater filter)
  (lambda (value frame)
    (let ((keyword (filter frame)))
      (if keyword
	  (cframe-field-value frame keyword)
	  value))))

(define (stack-marker-type-filter marker-type)
  (lambda (frame)
    (and (cframe:stack-marker-of-type? marker-type frame)
	 'marker-instance)))

(define-item 'previous-type #f
  (lambda (value frame)
    (if (cframe:join-stacklets? frame)
	value
	(cframe-type frame))))

(define-item 'dynamic-state #f
  (simple-item-updater (stack-marker-type-filter %translate-to-state-point)))

(define-item 'block-thread-events? #f
  (simple-item-updater (stack-marker-type-filter 'with-thread-events-blocked)))

(define-item 'interrupt-mask #f
  (simple-item-updater
   (lambda (frame)
     (cond ((cframe:restore-interrupt-mask? frame)
	    'interrupt-mask)
	   ((cframe:stack-marker-of-type? 'set-interrupt-enables! frame)
	    'marker-instance)
	   (else #f)))))

(define-item 'history #f
  (lambda (value frame)
    (if (cframe:restore-history? frame)
	(history-transform (cframe-field-value frame 'history))
	value)))

(define-item 'next-restore-history 0
  (lambda (value frame)
    (if (cframe:restore-history? frame)
	(begin
	  (assert (or (fix:= value 0)
		      (fix:= value (cframe-start frame))))
	  (let ((index (cframe-next-restore-history frame)))
	    (assert (or (fix:= index 0)
			(fix:>= index (cframe-end frame))))
	    index))
	(begin
	  (assert (or (fix:= value 0)
		      (fix:>= value (cframe-end frame))))
	  value))))

(define (cframe-next-restore-history frame)
  (let ((offset (cframe-field-value frame 'previous-restore-history-offset)))
    (if (fix:= offset 0)
	0
	(fix:- (cframe-cpoint-end frame) offset))))

(define (cframe-previous-restore-history-offset frame)
  (let ((index (cframe-tracked-item-value frame 'next-restore-history)))
    (if (fix:= index 0)
	0
	(fix:- (cframe-cpoint-end frame) index))))

(define-item 'next-return-code #f
  (lambda (value frame)
    (if (cframe-compiled-code? frame)
	(begin
	  (assert (and value (fix:>= value (cframe-end frame))))
	  value)
	(begin
	  (assert (or (not value) (fix:= value (cframe-start frame))))
	  (if (eq? (cframe-type frame) 'reenter-compiled-code)
	      (let ((index
		     (fix:+ (cframe-end frame)
			    (cframe-field-value frame 'last-return-code))))
		(assert (fix:< index (cframe-cpoint-end frame)))
		index)
	      #f)))))

;;;; Debugging info

(define (cframe-dbg-expression frame)
  (call-frame-generator frame 'expression undefined-exp))

(define (cframe-dbg-environment frame)
  (call-frame-generator frame 'environment undefined-env))

(define (cframe-dbg-subexpression frame)
  (if (eq? (cframe-tracked-item-value frame 'previous-type)
	   'pop-return-error)
      undefined-exp
      (call-frame-generator frame 'subexpression undefined-exp)))

(define (define-return-code-generators type . keylist)
  (alist-table-set! return-code-generators type (keyword-list->alist keylist)))

(define (define-return-type-generators frame-type . keylist)
  (alist-table-set! return-type-generators frame-type
		    (keyword-list->alist keylist)))

(define (call-frame-generator frame keyword default)
  (let* ((alist
	 (or (let ((code (cframe-return-code frame)))
	       (and code
		    (alist-table-ref return-code-generators
				     (microcode-return/code->name code)
				     (lambda () #f))))
	     (alist-table-ref return-type-generators
			      (cframe-return-type frame)
			      (lambda () '()))))
	 (p (assq keyword alist)))
    (if p
	((cdr p) frame)
	default)))

(define return-code-generators (alist-table eq?))
(define return-type-generators (alist-table eq?))

(define (select-subexp exp)
  (cond ((scode-access? exp) (scode-access-environment exp))
	((scode-assignment? exp) (scode-assignment-value exp))
	((scode-conditional? exp) (scode-conditional-predicate exp))
	((scode-definition? exp) (scode-definition-value exp))
	((scode-disjunction? exp) (scode-disjunction-predicate exp))
	((scode-sequence? exp) (scode-sequence-first exp))
	(else (error "Can't select subexpression:" exp))))

(define (squote object)
  (if (scode-constant? object)
      object
      (make-scode-quotation object)))

(define (standard-expression frame)
  (cframe-field-value frame 'expression))

(define (standard-environment frame)
  (cframe-field-value frame 'environment))

(define (standard-subexpression frame)
  (select-subexp (cframe-field-value frame 'expression)))

(define (default-expression frame)
  (make-printer
   (lambda (verbose? port)
     (write-string "Unknown " port)
     (if verbose?
	 (pp frame port)
	 (write frame port)))))

(define (apply-expression frame)
  (make-scode-combination (squote (cframe-field-value frame 'procedure))
			  (map squote (cframe-field-value frame 'arguments))))

;; TODO: requires changes in "environment.scm".
(define (cframe-environment frame undefined-env)
  (declare (ignore frame))
  undefined-env)

(define-record-type <printer>
    make-printer
    cframe-dbg-printer?
  (procedure printer-procedure))

(define (cframe-dbg-printer-apply printer verbose? port)
  ((printer-procedure printer) verbose? port))

(define-record-type <undefined-expression>
    make-undefined-expression
    cframe-dbg-expression-undefined?)

(define undefined-exp (make-undefined-expression))

(define-record-type <undefined-environment>
    make-undefined-environment
    cframe-dbg-environment-undefined?)

(define undefined-env (make-undefined-environment))

(define-record-type <compiled-expression>
    make-compiled-expression
    cframe-dbg-expression-compiled?)

(define compiled-exp (make-compiled-expression))

(define-return-code-generators 'access-continue
  'expression standard-expression
  'subexpression standard-subexpression)

(define-return-type-generators 'exp+env
  'expression standard-expression
  'environment standard-environment
  'subexpression standard-subexpression)

(define-return-code-generators 'eval-error
  'expression standard-expression
  'environment standard-environment)

(define-return-type-generators 'apply
  'expression apply-expression)

(define-return-type-generators 'cc-bkpt
  'expression apply-expression)

(define-return-type-generators 'combination-apply
  'expression standard-expression
  'subexpression
  (lambda (frame)
    (scode-combination-operator (cframe-field-value frame 'expression))))

(define-return-type-generators 'combination-save
  'expression standard-expression
  'environment standard-environment
  'subexpression
  (lambda (frame)
    (scode-combination-element (cframe-field-value frame 'expression)
			       (cframe-field-value frame 'number-of-blanks))))

(define-return-code-generators 'compiler-assignment-trap-restart
  'expression
  (lambda (frame)
    (make-scode-assignment (cframe-field-value frame 'variable)
			   (squote (cframe-field-value frame 'value))))
  'environment standard-environment)

(define-return-code-generators 'compiler-error-restart
  'expression
  (lambda (frame)
    (let ((prim (cframe-field-value frame 'primitive)))
      (if (primitive-procedure? prim)
	  (make-scode-combination (make-scode-variable 'apply)
				  (list prim))
	  undefined-exp))))

(define (cc-lookup-apply-exp frame)
  (make-scode-combination
   (make-scode-variable (cframe-field-value frame 'variable))
   (map squote (cframe-field-value frame 'arguments))))

(define-return-code-generators 'compiler-lookup-apply-trap-restart
  'expression cc-lookup-apply-exp
  'environment standard-environment)

(define-return-code-generators 'compiler-operator-lookup-trap-restart
  'expression cc-lookup-apply-exp
  'environment standard-environment)

(define-return-code-generators 'compiler-reference-trap-restart
  'expression
  (lambda (frame)
    (make-scode-variable (cframe-field-value frame 'variable)))
  'environment standard-environment)

(define-return-code-generators 'compiler-safe-reference-trap-restart
  'expression
  (lambda (frame)
    (make-scode-variable (cframe-field-value frame 'variable) #t))
  'environment standard-environment)

(define-return-code-generators 'compiler-unassigned?-trap-restart
  'expression
  (lambda (frame)
    (make-scode-unassigned? (cframe-field-value frame 'variable) #t))
  'environment standard-environment)

(define ((cc-accessor cont-accessor proc-accessor default) frame)
  (let ((dbg (compiled-entry/dbg-object (cframe-return-address frame))))
    (cond ((dbg-continuation? dbg)
	   (if (let ((source (dbg-continuation/source-code dbg)))
		 (and (vector? source)
		      (fix:>= (vector-length source) 2)))
	       (cont-accessor dbg frame)
	       undefined-exp))
	  ((dbg-procedure? dbg) (proc-accessor dbg frame))
	  (else default))))

(define cc-exp
  (cc-accessor (lambda (dbg frame)
		 (declare (ignore frame))
		 (let ((source (dbg-continuation/source-code dbg)))
		   (case (vector-ref source 0)
		     ((access-continue assignment-continue combination-element
				       combination-operand conditional-decide
				       conditional-predicate
				       conditional-predicate definition-continue
				       sequence-continue sequence-element)
		      (vector-ref source 1))
		     (else compiled-exp))))
	       (lambda (dbg frame)
		 (declare (ignore frame))
		 (scode-lambda-body (dbg-procedure/source-code dbg)))
	       compiled-exp))

(define cc-env
  (cc-accessor (lambda (dbg frame)
		 (let ((source (dbg-continuation/source-code dbg)))
		   (case (vector-ref source 0)
		     ((access-continue assignment-continue combination-operand
				       conditional-decide conditional-predicate
				       definition-continue sequence-continue)
		      (cframe-environment frame undefined-env))
		     (else undefined-env))))
	       (lambda (dbg frame)
		 (if (dbg-procedure/block dbg)
		     (cframe-environment frame undefined-env)
		     undefined-env))
	       undefined-env))

(define cc-subexp
  (cc-accessor (lambda (dbg frame)
		 (declare (ignore frame))
		 (let ((source (dbg-continuation/source-code dbg)))
		   (case (vector-ref source 0)
		     ((access-continue assignment-continue conditional-decide
				       conditional-predicate definition-continue
				       sequence-continue)
		      (select-subexp (vector-ref source 1)))
		     ((combination-operand)
		      (scode-combination-element (vector-ref source 1)
						 (vector-ref source 2)))
		     ((combination-element conditional-predicate
					   sequence-element)
		      (vector-ref source 2))
		     (else undefined-exp))))
	       (lambda (dbg frame)
		 (declare (ignore frame))
		 (scode-lambda-body (dbg-procedure/source-code dbg)))
	       undefined-exp))

(define-return-code-generators 'compiler-interrupt-restart
  'expression cc-exp
  'environment cc-env
  'subexpression cc-subexp)

(define-return-type-generators 'compiled-address
  'expression cc-exp
  'environment cc-env
  'subexpression cc-subexp)

(define-return-type-generators 'hardware-trap
  'expression
  (lambda (frame)
    (make-printer
     (lambda (verbose? port)
       (describe-hardware-trap-frame frame verbose? port)))))

(define (describe-hardware-trap-frame frame verbose? port)

  (define (write-hex value port)
    (if (< value #x10)
	(write value port)
	(begin
	  (write-string "#x" port)
	  (write-string (number->string value #x10) port))))

  (let ((name (cframe-field-value frame 'signal-name))
	(state (cframe-field-value frame 'recovery-state)))
    (if (not name)
	(write-string "User microcode reset" port)
	(let ((code (cframe-field-value frame 'code-name)))
	  (write-string "Hardware trap " port)
	  (write-string name port)
	  (write-string " (")
	  (if (and (pair? code) (cdr code))
	      (write-string (cdr code) port)
	      (begin
		(write-string "code = " port)
		(write-hex (if (pair? code) (car code) code) port)))
	  (write-string ")" port)))
    (if verbose?
	(let ((pc-info-1 (cframe-field-value frame 'pc-info-1))
	      (pc-info-2 (cframe-field-value frame 'pc-info-2)))
	  (case state
	    ((0)				; unknown
	     (write-string " at an unknown location." port))
	    ((1)				; primitive
	     (write-string " within " port)
	     (write pc-info-1 port))
	    ((2)				; compiled code
	     (write-string " at offset " port)
	     (write-hex pc-info-2 port)
	     (newline port)
	     (write-string "within " port)
	     (let ((block pc-info-1))
	       (write block port)
	       (let-values (((filename index library)
			     (compiled-code-block/filename-and-index block)))
		 (declare (ignore index library))
		 (if filename
		     (begin
		       (write-string " (" port)
		       (write filename port)
		       (write-string ")" port))))))
	    ((3)				; probably compiled-code
	     (write-string " at an unknown compiled-code location." port))
	    ((4)				; builtin (i.e. hook)
	     (let ((name ((ucode-primitive builtin-index->name 1) pc-info-1)))
	       (if name
		   (begin
		     (write-string " in assembly-language utility " port)
		     (write-string name port))
		   (begin
		     (write-string " in unknown assembly-language utility "
				   port)
		     (write-hex pc-info-1 port)))))
	    ((5)				; utility
	     (let ((name ((ucode-primitive utility-index->name 1) pc-info-1)))
	       (if name
		   (begin
		     (write-string " in compiled-code utility " port)
		     (write-string name port))
		   (begin
		     (write-string " in unknown compiled-code utility " port)
		     (write-hex pc-info-1 port)))))
	    (else
	     (error "Unknown state:" state)))))))