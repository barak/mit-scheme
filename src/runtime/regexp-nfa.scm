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
    (%make-shared-state node-indices group-indices)
    shared-state?
  (node-indices node-indices)
  (group-indices group-indices))

(define (make-shared-state)
  (%make-shared-state (make-index-generator 0)
		      (make-index-generator 1)))

(define (make-index-generator n)
  (lambda ()
    (let ((n* n))
      (set! n (fix:+ n 1))
      n*)))

(define (next-node-index)
  ((node-indices (shared-state))))

(define (next-group-index)
  ((group-indices (shared-state))))

(define-record-type <insn>
    (make-insn linker)
    insn?
  (linker insn-linker))

(define (link-insn insn next-node)
  ((insn-linker insn) next-node))

(define (matcher-insn id procedure)
  (make-insn
   (lambda (next-node)
     (normal-node 'matcher id procedure next-node))))

(define (looker-insn id procedure)
  (make-insn
   (lambda (next-node)
     (normal-node 'looker id procedure next-node))))

(define null-insn
  (make-insn
   (lambda (next-node)
     next-node)))

(define fail-insn
  (looker-insn '(fail)
    (lambda (next-node next-char prev-char ctx)
      (declare (ignore next-node next-char prev-char ctx))
      (fail))))

(define-record-type <node>
    (%make-node type id procedure nodes)
    node?
  (type node-type)
  (id %node-id)
  (procedure node-procedure)
  (nodes next-nodes %set-next-nodes!))

(define (make-node type id procedure nodes)
  (%make-node type (cons (next-node-index) id) procedure nodes))

(define (node-id node)
  (cons (node-type node) (%node-id node)))

(define-print-method node?
  (standard-print-method 'node node-id))

(define (normal-node type id procedure next-node)
  (make-node type id procedure (list next-node)))

(define (terminal-node)
  (make-node 'terminal '() #f '()))

(define (fork-node nodes)
  (make-node 'fork '() #f nodes))

(define (cyclic-fork-node get-nodes)
  (let ((node (fork-node '())))
    (%set-next-nodes! node (get-nodes node))
    node))

(define (matcher->nfa matcher)
  (let ((table (make-strong-eq-hash-table)))

    (define (handle-node node)
      (maybe-call (lambda (node)
		    (case (node-type node)
		      ((matcher looker) (handle-normal node))
		      ((fork) (handle-fork node))
		      ((terminal) '())
		      (else (error "Unknown node type:" node))))
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
	  (case (node-type node)
	    ((matcher looker)
	     (loop (car (next-nodes node)) chain))
	    ((fork)
	     (cons (reverse chain)
		   (maybe-call handle-fork node)))
	    ((terminal)
	     (list (reverse chain)))
	    (else
	     (error "Unknown node type:" node))))))

    (define (handle-fork node)
      (cons (cons node (next-nodes node))
	    (append-map handle-node (next-nodes node))))

    (let ((node (matcher-initial-node matcher)))
      (if (eq? 'terminal (node-type node))
	  (list (list node))
	  (handle-node node)))))

;;;; Instructions

(define (insn:string-start)
  (looker-insn '(bos)
    (lambda (next-node next-char prev-char ctx)
      (declare (ignore next-char))
      (if (not prev-char)
	  (succeed next-node ctx)
	  (fail)))))

(define (insn:string-end)
  (looker-insn '(eos)
    (lambda (next-node next-char prev-char ctx)
      (declare (ignore prev-char))
      (if (not next-char)
	  (succeed next-node ctx)
	  (fail)))))

(define (insn:line-start)
  (looker-insn '(bol)
    (lambda (next-node next-char prev-char ctx)
      (declare (ignore next-char))
      (if (or (not prev-char)
	      (char-newline? prev-char))
	  (succeed next-node ctx)
	  (fail)))))

(define (insn:line-end)
  (looker-insn '(eol)
    (lambda (next-node next-char prev-char ctx)
      (declare (ignore prev-char))
      (if (or (not next-char)
	      (char-newline? next-char))
	  (succeed next-node ctx)
	  (fail)))))

(define (insn:char char ci?)
  (matcher-insn (ci-id char ci?)
    (let ((pred (if ci? char-ci=? char=?)))
      (lambda (next-node next-char prev-char ctx)
	(declare (ignore prev-char))
	(if (and next-char (pred char next-char))
	    (succeed next-node (++index ctx))
	    (fail))))))

(define (insn:char-set char-set ci?)
  (case (char-set-size char-set)
    ((0) fail-insn)
    ((1) (insn:char (integer->char (car (char-set->code-points char-set))) ci?))
    (else
     (matcher-insn (ci-id char-set ci?)
       (lambda (next-node next-char prev-char ctx)
	 (declare (ignore prev-char))
	 (if (and next-char (char-in-set? next-char char-set))
	     (succeed next-node (++index ctx))
	     (fail)))))))

(define (ci-id object ci?)
  (if ci?
      (list 'ci object)
      (list object)))

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
     (list (looker-insn (list 'start-group n key)
	     (lambda (next-node next-char prev-char ctx)
	       (declare (ignore next-char prev-char))
	       (succeed next-node (start-group ctx))))
	   insn
	   (looker-insn (list 'end-group n key)
	     (lambda (next-node next-char prev-char ctx)
	       (declare (ignore next-char prev-char))
	       (succeed next-node (finish-group key ctx))))))))

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
    (case (state-type state)
      ((matcher) (interpret-matcher state inputs outputs))
      ((looker) (interpret-looker state inputs outputs))
      (else (loop inputs (append-state state outputs)))))

  (define (interpret-matcher state inputs outputs)
    (let ((state* (run-normal-state state)))
      (trace-matcher (lambda (port) (write (list '-> state*) port)))
      (loop inputs
	    (if state*
		(append-state state* outputs)
		outputs))))

  (define (interpret-looker state inputs outputs)
    (let ((state* (run-normal-state state)))
      (if state*
	  (if (fork-state? state*)
	      (loop (prepend-state state* inputs) outputs)
	      (interpret-state state* inputs outputs))
	  (loop inputs outputs))))

  (define (run-normal-state state)
    (let ((node (state-node state)))
      ((node-procedure node)
       (car (next-nodes node))
       next-char
       prev-char
       (state-ctx state))))

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
      (node-id (state-node state)))))

(define (state-type state)
  (node-type (state-node state)))

(define (terminal-state? state)
  (eq? 'terminal (state-type state)))

(define (fork-state? state)
  (eq? 'fork (state-type state)))

(define (fork-state-threads state)
  (map (let ((ctx (state-ctx state)))
	 (lambda (node)
	   (make-state node ctx)))
       (next-nodes (state-node state))))

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

(define (start-group ctx)
  (let ((index (ctx-index ctx)))
    (make-ctx index
	      (cons index (ctx-stack ctx))
	      (ctx-groups ctx))))

(define (finish-group key ctx)
  (let ((index (ctx-index ctx))
	(stack (ctx-stack ctx)))
    (make-ctx index
	      (cdr stack)
	      (cons (let ((start (car stack)))
		      (lambda (string)
			(make-group key string start index)))
		    (ctx-groups ctx)))))

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