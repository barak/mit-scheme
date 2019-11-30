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
		      (number-of-groups))))))

(define-record-type <matcher>
    (make-matcher initial-node n-nodes n-groups)
    matcher?
  (initial-node matcher-initial-node)
  (n-nodes matcher-n-nodes)
  (n-groups matcher-n-groups))

(define compiler-shared-state
  (make-parameter #f))

(define-record-type <compiler-shared-state>
    (%make-compiler-shared-state node-indices group-indices)
    compiler-shared-state?
  (node-indices node-indices)
  (group-indices group-indices))

(define (make-compiler-shared-state)
  (%make-compiler-shared-state (node-index-generator)
			       (group-index-generator)))

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

;;;; Instructions

(define-record-type <insn>
    (make-insn linker)
    insn?
  (linker insn-linker))

(define (link-insn insn next-node)
  ((insn-linker insn) next-node))

(define (normal-insn type id datum)
  (make-insn
   (lambda (next-node)
     (make-node type id datum next-node))))

(define (lookaround-insn id predicate)
  (normal-insn 'lookaround id predicate))

(define (ctx-only-insn id procedure)
  (normal-insn 'ctx-only id procedure))

(define (char-insn char)
  (normal-insn 'char (list char) char))

(define (char-ci-insn char)
  (normal-insn 'char-ci (list char) char))

(define (char-set-insn char-set)
  (normal-insn 'char-set (list char-set) char-set))

(define null-insn
  (make-insn
   (lambda (next-node)
     next-node)))

(define fail-insn
  (make-insn
   (lambda (next-node)
     (declare (ignore next-node))
     (make-node 'fail '() #f #f))))

;;;; Nodes

(define-record-type <node>
    (%make-node type index id datum next)
    node?
  (type node-type)
  (index node-index)
  (id %node-id)
  (datum node-datum)
  (next node-next %set-node-next!))

(define (make-node type id datum nodes)
  (%make-node type (next-node-index) id datum nodes))

(define (node-id node)
  (cons* (node-type node)
	 (node-index node)
	 (%node-id node)))

(define-print-method node?
  (standard-print-method 'node node-id))

(define (terminal-node)
  (make-node 'terminal '() #f #f))

(define (fork-node nodes)
  (make-node 'fork '() #f nodes))

(define (cyclic-fork-node get-nodes)
  (let ((node (fork-node '())))
    (%set-node-next! node (get-nodes node))
    node))

(define (epsilon-node? node)
  (let ((type (node-type node)))
    (or (eq? type 'fork)
	(eq? type 'lookaround)
	(eq? type 'ctx-only))))

(define (normal-node? node)
  (let ((type (node-type node)))
    (or (eq? type 'lookaround)
	(eq? type 'ctx-only)
	(eq? type 'char)
	(eq? type 'char-ci)
	(eq? type 'char-set))))

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

(define (insn:string-start)
  (lookaround-insn '(bos)
    (lambda (next-char prev-char)
      (declare (ignore next-char))
      (not prev-char))))

(define (insn:string-end)
  (lookaround-insn '(eos)
    (lambda (next-char prev-char)
      (declare (ignore prev-char))
      (not next-char))))

(define (insn:line-start)
  (lookaround-insn '(bol)
    (lambda (next-char prev-char)
      (declare (ignore next-char))
      (or (not prev-char)
	  (char-newline? prev-char)))))

(define (insn:line-end)
  (lookaround-insn '(eol)
    (lambda (next-char prev-char)
      (declare (ignore prev-char))
      (or (not next-char)
	  (char-newline? next-char)))))

(define (insn:char char ci?)
  (if ci?
      (char-ci-insn char)
      (char-insn char)))

(define (insn:char-set char-set)
  (case (char-set-size char-set)
    ((0) fail-insn)
    ((1) (insn:char (integer->char (car (char-set->code-points char-set))) #f))
    (else (char-set-insn char-set))))

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
      (let ((index (ctx-index ctx)))
	(make-ctx index
		  (cons index (ctx-stack ctx))
		  (ctx-groups ctx))))))

(define (end-group-insn n key)
  (ctx-only-insn (list 'end-group n key)
    (lambda (ctx)
      (let ((index (ctx-index ctx))
	    (stack (ctx-stack ctx)))
	(make-ctx index
		  (cdr stack)
		  (cons (let ((start (car stack)))
			  (lambda (string)
			    (make-group key string start index)))
			(ctx-groups ctx)))))))

;;;; Interpreter

(define (run-matcher matcher string start end)
  (parameterize ((run-shared-state (make-run-shared-state matcher)))
    (let ((initial
	   (make-state (matcher-initial-node matcher)
		       (initial-ctx start))))
      (trace-matcher (lambda (port) (write (list 'initial-state initial) port)))
      (let ((final (match-nodes initial string start end)))
	(trace-matcher (lambda (port) (write (list 'final-state final) port)))
	(and final
	     (all-groups string start (state-ctx final)))))))

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

(define (match-nodes initial-state string start end)
  (let loop ((states (list initial-state)) (index start) (prev-char #f))
    (trace-matcher (lambda (port) (write (cons* 'index index states) port)))
    (let ((next-char
	   (and (fix:< index end)
		(string-ref string index))))
      (let ((states (follow-epsilons states next-char prev-char)))
	(cond ((not (pair? states)) #f)
	      ((terminal-state? (car states)) (car states))
	      (else
	       (let ((states (follow-matchers states next-char)))
		 (if next-char
		     (loop states (fix:+ index 1) next-char)
		     (let ((states
			    (follow-epsilons states next-char prev-char)))
		       (and (pair? states)
			    ;; If a terminal state is present, it's always last.
			    ;; The follow-X procedures guarantee this.
			    (let ((state (last states)))
			      (and (terminal-state? state)
				   state))))))))))))

(define (follow-epsilons states next-char prev-char)
  (trace-matcher (lambda (port) (pp (cons 'follow-epsilons states) port)))
  (let ((seen (make-strong-eq-hash-table)))

    (define (loop inputs outputs)
      (if (pair? inputs)
	  (follow-state (car inputs) (cdr inputs) outputs)
	  (reverse! outputs)))

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
	      ((lookaround)
	       (if ((node-datum node) next-char prev-char)
		   (follow-state (make-state (node-next node) ctx)
				 inputs
				 outputs)
		   (loop inputs outputs)))
	      ((ctx-only)
	       (follow-state (make-state (node-next node)
					 ((node-datum node) ctx))
			     inputs
			     outputs))
	      ((fail) (loop inputs outputs))
	      ((terminal) (reverse! (cons state outputs)))
	      (else (loop inputs (cons state outputs)))))))

    (define (seen? state)
      (let ((p (hash-table-intern! seen state (lambda () (list #f)))))
	(if (car p)
	    #t
	    (begin
	      (set-car! p #t)
	      #f))))

    (loop states '())))

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
	((char) (match char=? node ctx inputs outputs))
	((char-ci) (match char-ci=? node ctx inputs outputs))
	((char-set) (match char-set-contains? node ctx inputs outputs))
	((fail) (loop inputs outputs))
	((terminal) (reverse! (cons state outputs)))
	(else (error "Unknown node type:" node)))))

  (define (match pred node ctx inputs outputs)
    (loop inputs
	  (if (and next-char (pred (node-datum node) next-char))
	      (cons (make-state (node-next node) (++index ctx)) outputs)
	      outputs)))

  (loop states '()))

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
    (make-ctx index stack groups)
    ctx?
  (index ctx-index)
  (stack ctx-stack)
  (groups ctx-groups))

(define (initial-ctx start)
  (make-ctx start '() '()))

(define (++index ctx)
  (make-ctx (fix:+ (ctx-index ctx) 1)
	    (ctx-stack ctx)
	    (ctx-groups ctx)))

(define (all-groups string start ctx)
  (cons (make-group 0 string start (ctx-index ctx))
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

(define-print-method group?
  (standard-print-method 'group
    (lambda (group)
      (list (group-key group) (group-value group)))))