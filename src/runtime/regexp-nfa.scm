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

(define (generate-matcher thunk)
  (parameterize ((shared-state (make-shared-state)))
    (let ((insn (thunk)))
      (make-matcher (link-insn insn (terminal-node))))))

;; This structure is overkill for now but allows adding additional information
;; from the compiler that can be used to make interpretation more efficient.
(define-record-type <matcher>
    (make-matcher initial-node)
    matcher?
  (initial-node matcher-initial-node))

(define shared-state
  (make-parameter #f))

(define-record-type <shared-state>
    (%make-shared-state group-indices)
    shared-state?
  (group-indices group-indices))

(define (make-shared-state)
  (%make-shared-state (make-index-generator 1)))

(define (make-index-generator n)
  (lambda ()
    (let ((n* n))
      (set! n (fix:+ n 1))
      n*)))

(define (next-group-index)
  ((group-indices (shared-state))))

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
    (%make-node type id datum next)
    node?
  (type node-type)
  (id %node-id)
  (datum node-datum)
  (next node-next %set-node-next!))

(define (make-node type id datum nodes)
  (%make-node type id datum nodes))

(define (node-id node)
  (cons (node-type node) (%node-id node)))

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

  (define (finish state)
    (trace-matcher (lambda (port) (write (list 'success state) port)))
    (all-groups string start (state-ctx state)))

  (let per-index
      ((states
	(append-state (make-state (matcher-initial-node matcher)
				  (initial-ctx start))
		      (make-state-set)))
       (index start)
       (prev-char #f))
    (trace-matcher (lambda (port) (pp (cons index (all-elts states)) port)))
    (cond ((no-elts? states)
	   #f)
	  ((let ((state (first-elt states)))
	     (and (terminal-state? state)
		  state))
	   => finish)
	  ((fix:< index end)
	   (let ((next-char (string-ref string index)))
	     (per-index (interpret-states states next-char prev-char)
			(fix:+ index 1)
			next-char)))
	  (else
	   (let ((state
		  (find terminal-state?
			(all-elts (interpret-states states #f prev-char)))))
	     (and state
		  (finish state)))))))

(define (interpret-states states next-char prev-char)

  (define (loop inputs outputs)
    (if (no-elts? inputs)
	outputs
	(interpret-state (first-elt inputs) (rest-elts inputs) outputs)))

  (define (interpret-state state inputs outputs)
    (trace-matcher (lambda (port) (write state port)))
    (let ((node (state-node state))
	  (ctx (state-ctx state)))
      (case (node-type node)
	((lookaround)
	 (if ((node-datum node) next-char prev-char)
	     (continue node ctx inputs outputs)
	     (loop inputs outputs)))
	((ctx-only)
	 (continue node ((node-datum node) ctx) inputs outputs))
	((char)
	 (match char=? node ctx inputs outputs))
	((char-ci)
	 (match char-ci=? node ctx inputs outputs))
	((char-set)
	 (match char-set-contains? node ctx inputs outputs))
	((fail)
	 (loop inputs outputs))
	((terminal)
	 (loop inputs (append-state state outputs)))
	(else
	 (error "Unknown node type:" node)))))

  (define (continue node ctx inputs outputs)
    (let* ((next (node-next node))
	   (state (make-state next ctx)))
      (if (fork-node? next)
	  (loop (prepend-state state inputs) outputs)
	  (interpret-state state inputs outputs))))

  (define (match pred node ctx inputs outputs)
    (if (and next-char (pred (node-datum node) next-char))
	(loop inputs
	      (append-state (make-state (node-next node) (++index ctx))
			    outputs))
	(loop inputs outputs)))

  (loop states (make-state-set)))

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

(define-record-type <state>
    (make-state node ctx)
    state?
  (node state-node)
  (ctx state-ctx))

(define-print-method state?
  (standard-print-method 'state
    (lambda (state)
      (cons (hash-object (state-node state))
	    (node-id (state-node state))))))

(define (terminal-state? state)
  (terminal-node? (state-node state)))

(define (fork-state? state)
  (fork-node? (state-node state)))

(define (fork-state-threads state)
  (map (let ((ctx (state-ctx state)))
	 (lambda (node)
	   (make-state node ctx)))
       (node-next (state-node state))))

(define (make-state-set)
  (let loop ((seen '()) (states '()))

    (define (add-to-end state)
      (if (fork-state? state)
	  (if (seen? state)
	      this
	      (fold append-state
		    (loop (cons state seen) states)
		    (fork-state-threads state)))
	  (if (seen? state)
	      this
	      (loop (cons state seen)
		    (cons state states)))))

    (define (add-to-start state)
      (if (fork-state? state)
	  (if (seen? state)
	      this
	      (fold-right prepend-state
			  (loop (cons state seen) states)
			  (fork-state-threads state)))
	  (if (seen? state)
	      this
	      (loop (cons state seen)
		    (append states (list state))))))

    (define (seen? state)
      (any (lambda (state*)
	     (state=? state state*))
	   seen))

    (define (empty?)
      (null? states))

    (define (first)
      (last states))

    (define (rest)
      (loop seen (except-last-pair states)))

    (define (all)
      (reverse states))

    (define this
      (%make-state-set add-to-end add-to-start empty? first rest all))

    this))

(define-record-type <state-set>
    (%make-state-set append prepend empty? first rest all)
    state-set?
  (append %state-set-append)
  (prepend %state-set-prepend)
  (empty? %state-set-empty?)
  (first %state-set-first)
  (rest %state-set-rest)
  (all %state-set-all))

(define (append-state state states) ((%state-set-append states) state))
(define (prepend-state state states) ((%state-set-prepend states) state))
(define (no-elts? states) ((%state-set-empty? states)))
(define (first-elt states) ((%state-set-first states)))
(define (rest-elts states) ((%state-set-rest states)))
(define (all-elts states) ((%state-set-all states)))

(define (state=? s1 s2)
  (and (eq? (state-node s1) (state-node s2))
       (eq? (state-ctx s1) (state-ctx s2))))

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