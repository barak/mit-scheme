#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; NFA regular-expression implementation
;;; package: (runtime regexp nfa)

(declare (usual-integrations))

;;;; Compiler

(define (compile-matcher thunk)
  (parameterize ((compiler-shared-state (make-compiler-shared-state)))
    (let ((insn (thunk)))
      (let ((initial-node (link-insn insn (terminal-node))))
	(make-matcher initial-node
		      (number-of-nodes)
		      (number-of-groups)
		      (gcb-insns-seen? (compiler-shared-state)))))))

(define-record-type <matcher>
    (make-matcher initial-node n-nodes n-groups need-gcb?)
    matcher?
  (initial-node matcher-initial-node)
  (n-nodes matcher-n-nodes)
  (n-groups matcher-n-groups)
  (need-gcb? matcher-need-gcb?))

(define compiler-shared-state
  (make-parameter #f))

(define-record-type <compiler-shared-state>
    (%make-compiler-shared-state node-indices group-indices gcb-insns-seen?)
    compiler-shared-state?
  (node-indices node-indices)
  (group-indices group-indices)
  (gcb-insns-seen? gcb-insns-seen? %set-gcb-insns-seen!))

(define (make-compiler-shared-state)
  (%make-compiler-shared-state (node-index-generator)
			       (group-index-generator)
			       #f))

(define (make-index-generator n)
  (lambda ()
    (let ((n* n))
      (set! n (fix:+ n 1))
      n*)))

(define (node-index-generator)
  (make-index-generator 0))

(define (next-node-index)
  ((node-indices (compiler-shared-state))))

(define (number-of-nodes)
  (next-node-index))

(define (group-index-generator)
  (make-index-generator 1))

(define (next-group-index)
  ((group-indices (compiler-shared-state))))

(define (number-of-groups)
  (- (next-group-index) 1))

(define (gcb-insns-seen!)
  (%set-gcb-insns-seen! (compiler-shared-state) #t))

;;;; Instructions

(define-record-type <insn>
    (make-insn linker)
    insn?
  (linker insn-linker))

(define (link-insn insn next-node)
  ((insn-linker insn) next-node))

(define (normal-insn type id procedure datum)
  (make-insn
   (lambda (next-node)
     (make-node type id procedure datum next-node))))

(define (char-zero-width-insn id predicate)
  (normal-insn 'char-zero-width id predicate #f))

(define (string-zero-width-insn id predicate)
  (normal-insn 'string-zero-width id predicate #f))

(define (full-zero-width-insn id predicate)
  (normal-insn 'full-zero-width id predicate #f))

(define (ctx-only-insn id procedure)
  (normal-insn 'ctx-only id procedure #f))

(define (match-insn predicate datum)
  (normal-insn 'match (list datum) predicate datum))

(define null-insn
  (make-insn
   (lambda (next-node)
     next-node)))

(define fail-insn
  (make-insn
   (lambda (next-node)
     (declare (ignore next-node))
     (make-node 'fail '() #f #f #f))))

;;;; Nodes

(define-record-type <node>
    (%make-node type index id procedure datum next)
    node?
  (type node-type)
  (index node-index)
  (id %node-id)
  (procedure node-procedure)
  (datum node-datum)
  (next node-next %set-node-next!))

(define (make-node type id procedure datum nodes)
  (%make-node type (next-node-index) id procedure datum nodes))

(define (node-id node)
  (cons* (node-type node)
	 (node-index node)
	 (%node-id node)))

(define-print-method node?
  (standard-print-method 'node node-id))

(define (terminal-node)
  (make-node 'terminal '() #f #f #f))

(define (fork-node nodes)
  (make-node 'fork '() #f #f nodes))

(define (cyclic-fork-node get-nodes)
  (let ((node (fork-node '())))
    (%set-node-next! node (get-nodes node))
    node))

(define (epsilon-node? node)
  (let ((type (node-type node)))
    (or (eq? type 'fork)
	(eq? type 'char-zero-width)
	(eq? type 'string-zero-width)
	(eq? type 'full-zero-width)
	(eq? type 'ctx-only))))

(define (normal-node? node)
  (let ((type (node-type node)))
    (or (eq? type 'char-zero-width)
	(eq? type 'string-zero-width)
	(eq? type 'full-zero-width)
	(eq? type 'ctx-only)
	(eq? type 'match))))

(define (fork-node? node)
  (eq? 'fork (node-type node)))

(define (terminal-node? node)
  (eq? 'terminal (node-type node)))

(define (final-node? node)
  (let ((type (node-type node)))
    (or (eq? type 'terminal)
	(eq? type 'fail))))

;;;; Graph printer

(define (matcher->nfa matcher)
  (let ((table (make-strong-eq-hash-table)))

    (define (handle-node node)
      (maybe-call (lambda (node)
		    (cond ((normal-node? node) (handle-normal node))
			  ((fork-node? node) (handle-fork node))
			  (else '())))
		  node))

    (define (maybe-call proc node)
      (if (hash-table-exists? table node)
	  '()
	  (begin
	    (hash-table-set! table node #t)
	    (proc node))))

    (define (handle-normal node)
      (let loop ((node node) (chain '()))
	(let ((chain (cons node chain)))
	  (cond ((normal-node? node)
		 (loop (node-next node) chain))
		((fork-node? node)
		 (cons (reverse chain)
		       (maybe-call handle-fork node)))
		(else
		 (list (reverse chain)))))))

    (define (handle-fork node)
      (cons (cons node (node-next node))
	    (append-map handle-node (node-next node))))

    (let ((node (matcher-initial-node matcher)))
      (if (final-node? node)
	  (list (list node))
	  (handle-node node)))))

;;;; Instruction builders

(define (insn:char-zero-width pred)
  (char-zero-width-insn 'general pred))

(define (insn:string-zero-width pred)
  (string-zero-width-insn 'general pred))

(define (insn:string-start)
  (char-zero-width-insn '(bos)
    (lambda (next-char prev-char)
      (declare (ignore next-char))
      (not prev-char))))

(define (insn:string-end)
  (char-zero-width-insn '(eos)
    (lambda (next-char prev-char)
      (declare (ignore prev-char))
      (not next-char))))

(define (insn:start-boundary char-set)
  (char-zero-width-insn (list 'start-boundary char-set)
    (lambda (next-char prev-char)
      (and (matches? char-set next-char)
	   (not (matches? char-set prev-char))))))

(define (insn:end-boundary char-set)
  (char-zero-width-insn (list 'end-boundary char-set)
    (lambda (next-char prev-char)
      (and (not (matches? char-set next-char))
	   (matches? char-set prev-char)))))

(define (insn:boundary char-set)
  (char-zero-width-insn (list 'boundary char-set)
    (lambda (next-char prev-char)
      (if (matches? char-set next-char)
	  (not (matches? char-set prev-char))
	  (matches? char-set prev-char)))))

(define (insn:non-boundary char-set)
  (char-zero-width-insn (list 'non-boundary char-set)
    (lambda (next-char prev-char)
      (if (matches? char-set next-char)
	  (matches? char-set prev-char)
	  (not (matches? char-set prev-char))))))

(define (insn:bog)
  (gcb-insns-seen!)
  (full-zero-width-insn '(bog)
    (lambda (index string start end ctx)
      (declare (ignore string start))
      (and (fix:< index end)
	   (let ((gcbs (ctx-gcbs ctx)))
	     (and (pair? gcbs)
		  (fix:= (car gcbs) index)))))))

(define (insn:eog)
  (gcb-insns-seen!)
  (full-zero-width-insn '(eog)
    (lambda (index string start end ctx)
      (declare (ignore string end))
      (and (fix:> index start)
	   (let ((gcbs (ctx-gcbs ctx)))
	     (and (pair? gcbs)
		  (fix:= (car gcbs) index)))))))

(define (matches? char-set char)
  (and char
       (char-set-contains? char-set char)))

(define (insn:char char ci?)
  (if ci?
      (match-insn char-ci=-predicate (cons 'ci char))
      (match-insn (char=-predicate char) char)))

(define (insn:char-set char-set ci?)
  (case (char-set-size char-set)
    ((0) fail-insn)
    ((1) (insn:char (char-set-ref char-set (char-set-cursor char-set)) ci?))
    (else
     (match-insn (if ci?
		     (char-set-ci-predicate char-set)
		     (char-set-predicate char-set))
		 char-set))))

(define (insn:string string ci?)
  (insn:seq
   (map (lambda (char)
	  (insn:char char ci?))
	(string->list string))))

(define (insn:seq insns)
  (case (length insns)
    ((0) null-insn)
    ((1) (car insns))
    (else
     (make-insn
      (lambda (next)
	(fold-right (lambda (insn next)
		      (link-insn insn next))
		    next
		    insns))))))

(define (insn:alt insns)
  (case (length insns)
    ((0) fail-insn)
    ((1) (car insns))
    (else
     (make-insn
      (lambda (next)
	(fork-node
	 (map (lambda (insn)
		(link-insn insn next))
	      insns)))))))

(define (insn:? insn)
  (insn:alt (list insn null-insn)))

(define (insn:?? insn)
  (insn:alt (list null-insn insn)))

(define (insn:* insn)
  (make-insn
   (lambda (next)
     (cyclic-fork-node
      (lambda (node)
	(list (link-insn insn node) next))))))

(define (insn:*? insn)
  (make-insn
   (lambda (next)
     (cyclic-fork-node
      (lambda (node)
	(list next (link-insn insn node)))))))

(define (insn:= n insn)
  (insn:seq (make-list n insn)))

(define (insn:>= n insn)
  (insn:seq (list (insn:= n insn) (insn:* insn))))

(define (insn:>=? n insn)
  (insn:seq (list (insn:= n insn) (insn:*? insn))))

(define (insn:** n m insn)
  (insn:seq
   (cons (insn:= n insn)
	 (make-list (- m n) (insn:? insn)))))

(define (insn:**? n m insn)
  (insn:seq
   (cons (insn:= n insn)
	 (make-list (- m n) (insn:?? insn)))))

(define (insn:group key insn)
  (let ((n (next-group-index)))
    (insn:seq
     (list (start-group-insn n key)
	   insn
	   (end-group-insn n key)))))

(define (start-group-insn n key)
  (ctx-only-insn (list 'start-group n key)
    (lambda (ctx)
      (start-group ctx))))

(define (end-group-insn n key)
  (ctx-only-insn (list 'end-group n key)
    (lambda (ctx)
      (end-group ctx key))))

;;;; Interpreter

(define (run-matcher matcher match-all? capture? index string start end)
  (parameterize ((run-shared-state (make-run-shared-state matcher)))
    (let ((initial
	   (make-state (matcher-initial-node matcher)
		       (initial-ctx index
				    (if (matcher-need-gcb? matcher)
					(string-gcb-stream string start end)
					'())
				    capture?))))
      (trace-matcher (lambda (port) (write (list 'initial-state initial) port)))
      (let ((final (match-nodes initial match-all? index string start end)))
	(trace-matcher (lambda (port) (write (list 'final-state final) port)))
	(and final
	     (all-groups string index (state-ctx final)))))))

(define run-shared-state
  (make-parameter #f))

(define-record-type <run-shared-state>
    (%make-run-shared-state state-memoizer)
    run-shared-state?
  (state-memoizer %state-memoizer))

(define (make-run-shared-state matcher)
  (%make-run-shared-state (make-state-memoizer matcher)))

(define (state-memoizer)
  (%state-memoizer (run-shared-state)))

(define (succeed next-node ctx)
  (make-state next-node ctx))

(define (fail)
  #f)

(define (trace-matcher proc)
  (if (param:trace-regexp-nfa?)
      (let ((port (current-output-port)))
	(fresh-line port)
	(proc port))))

(define param:trace-regexp-nfa?
  (make-settable-parameter #f))

(define (match-nodes initial-state match-all? index string start end)
  (let* ((seen (make-strong-eq-hash-table))
	 (follow-epsilons
	  (follow-epsilons match-all? string start end seen)))
    (let loop
	((states (list initial-state))
	 (index index)
	 (prev-char
	  (and (fix:> index start)
	       (fix:<= index end)
	       (string-ref string (fix:- index 1)))))
      (trace-matcher
       (lambda (port)
	 (pp (cons* 'match-nodes index prev-char states) port)))
      (let ((next-char
	     (and (fix:< index end)
		  (string-ref string index))))
	(let ((states (follow-epsilons next-char states index prev-char)))
	  (cond ((not (pair? states)) #f)
		((terminal-state? (car states)) (car states))
		(else
		 (let ((states (follow-matchers states next-char)))
		   (if next-char
		       (loop states (fix:+ index 1) next-char)
		       (let ((states
			      (follow-epsilons next-char
					       states
					       index
					       prev-char)))
			 (and (pair? states)
			      ;; If a terminal state is present, it's always
			      ;; last.  The follow-X procedures guarantee this.
			      (let ((state (last states)))
				(and (terminal-state? state)
				     state)))))))))))))

(define (follow-matchers states next-char)
  (trace-matcher (lambda (port) (pp (cons 'follow-matchers states) port)))

  (define (loop inputs outputs)
    (if (pair? inputs)
	(follow-state (car inputs) (cdr inputs) outputs)
	(reverse! outputs)))

  (define (follow-state state inputs outputs)
    (trace-matcher (lambda (port) (write state port)))
    (let ((node (state-node state))
	  (ctx (state-ctx state)))
      (case (node-type node)
	((match) (loop inputs (match node ctx outputs)))
	((fail) (loop inputs outputs))
	((terminal) (reverse! (cons state outputs)))
	(else (error "Unknown node type:" node)))))

  (define (match node ctx outputs)
    (if (and next-char ((node-procedure node) next-char))
	(cons (make-state (node-next node) (++index ctx)) outputs)
	outputs))

  (loop states '()))

(define (follow-epsilons match-all? string start end seen)
  (lambda (next-char states index prev-char)
    (trace-matcher
     (lambda (port)
       (pp (cons* 'follow-epsilons index prev-char next-char states) port)))

    (define (loop inputs outputs)
      (if (pair? inputs)
	  (follow-state (car inputs) (cdr inputs) outputs)
	  (done outputs)))

    (define (follow-state state inputs outputs)
      (if (seen? state)
	  (loop inputs outputs)
	  (let ((node (state-node state))
		(ctx (state-ctx state)))
	    (trace-matcher (lambda (port) (write state port)))
	    (case (node-type node)
	      ((fork)
	       (loop (fold-right (lambda (node* inputs)
				   (cons (make-state node* ctx)
					 inputs))
				 inputs
				 (node-next node))
		     outputs))
	      ((char-zero-width)
	       (if ((node-procedure node) next-char prev-char)
		   (follow-state (make-state (node-next node) ctx)
				 inputs
				 outputs)
		   (loop inputs outputs)))
	      ((string-zero-width)
	       (if ((node-procedure node) index string start end)
		   (follow-state (make-state (node-next node) ctx)
				 inputs
				 outputs)
		   (loop inputs outputs)))
	      ((full-zero-width)
	       (if ((node-procedure node) index string start end ctx)
		   (follow-state (make-state (node-next node) ctx)
				 inputs
				 outputs)
		   (loop inputs outputs)))
	      ((ctx-only)
	       (follow-state (make-state (node-next node)
					 ((node-procedure node) ctx))
			     inputs
			     outputs))
	      ((terminal)
	       (if (and match-all? next-char)
		   (loop inputs outputs)
		   (done (cons state outputs))))
	      ((fail) (loop inputs outputs))
	      (else (loop inputs (cons state outputs)))))))

    (define (seen? state)
      (let ((p (hash-table-intern! seen state (lambda () (list #f)))))
	(if (car p)
	    #t
	    (begin
	      (set-car! p #t)
	      #f))))

    (define (done outputs)
      (trace-matcher
       (lambda (port) (pp (cons 'follow-epsilons:done outputs) port)))
      (hash-table-clear! seen)
      (reverse! outputs))

    (loop states '())))

;;;; States

(define-record-type <state>
    (%make-state node ctx)
    state?
  (node state-node)
  (ctx state-ctx))

(define (make-state node ctx)
  (let ((memoizer (state-memoizer)))
    (let ((states (vector-ref memoizer (node-index node))))
      (or (find (lambda (state)
		  (eq? ctx (state-ctx state)))
		states)
	  (let ((state (%make-state node ctx)))
	    (vector-set! memoizer (node-index node) (cons state states))
	    state)))))

(define (make-state-memoizer matcher)
  (make-vector (matcher-n-nodes matcher) '()))

(define (state=? s1 s2)
  (eq? s1 s2))

(define-print-method state?
  (standard-print-method 'state
    (lambda (state)
      (node-id (state-node state)))))

(define (terminal-state? state)
  (terminal-node? (state-node state)))

;;;; Context

(define-record-type <ctx>
    (make-ctx index gcbs stack groups)
    ctx?
  (index ctx-index)
  (gcbs ctx-gcbs)
  (stack ctx-stack)
  (groups ctx-groups))

(define (initial-ctx index gcbs capture?)
  (make-ctx index
	    (chase-gcbs index gcbs)
	    '()
	    (if capture? '() #f)))

(define (++index ctx)
  (let ((index* (fix:+ (ctx-index ctx) 1)))
    (make-ctx index*
	      (chase-gcbs index* (ctx-gcbs ctx))
	      (ctx-stack ctx)
	      (ctx-groups ctx))))

(define (start-group ctx)
  (if (ctx-groups ctx)
      (let ((index (ctx-index ctx)))
	(make-ctx index
		  (ctx-gcbs ctx)
		  (cons index (ctx-stack ctx))
		  (ctx-groups ctx)))
      ctx))

(define (end-group ctx key)
  (if (ctx-groups ctx)
      (let ((index (ctx-index ctx))
	    (stack (ctx-stack ctx)))
	(make-ctx index
		  (ctx-gcbs ctx)
		  (cdr stack)
		  (cons (let ((start (car stack)))
			  (lambda (string)
			    (make-group key string start index)))
			(ctx-groups ctx))))
      ctx))

(define (chase-gcbs index gcbs)
  (if (and (pair? gcbs) (fix:< (car gcbs) index))
      (chase-gcbs index (force (cdr gcbs)))
      gcbs))

(define (all-groups string index ctx)
  (cons (make-group 0 string index (ctx-index ctx))
	(map (lambda (p) (p string))
	     (reverse (ctx-groups ctx)))))

(define-record-type <group>
    (make-group key string start end)
    group?
  (key group-key)
  (string group-string)
  (start group-start)
  (end group-end))

(define (group-value group)
  (string-slice (group-string group)
		(group-start group)
		(group-end group)))

(define (group-empty? group)
  (fix:= (group-start group) (group-end group)))

(define (group-length group)
  (fix:- (group-end group) (group-start group)))

(define-print-method group?
  (standard-print-method 'group
    (lambda (group)
      (list (group-key group) (group-value group)))))