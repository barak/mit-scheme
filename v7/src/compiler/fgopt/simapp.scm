#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/simapp.scm,v 1.2 1987/06/30 19:50:45 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; Dataflow Analysis: Simulate Application

(declare (usual-integrations))

(package (simulate-application)

;;;; Simulate Application

(define-export (simulate-application vnodes combinations)
  (for-each (lambda (vnode)
	      (set-vnode-procedures-cache! vnode
					   (vnode-initial-procedures vnode)))
	    vnodes)
  (for-each (lambda (combination)
	      (set-combination-procedures! combination '()))
	    combinations)
  (transitive-closure process-combination combinations)
  (for-each (lambda (vnode)
	      (set-vnode-procedures-cache! vnode 'NOT-CACHED))
	    vnodes))

(define (process-combination combination)
  (set-combination-procedures!
   combination
   (let ((operator (subproblem-value (combination-operator combination)))
	 (old (combination-procedures combination))
	 (apply-procedure
	  (procedure-applicator (combination-operands combination)
				(combination-value combination))))
     (define (process-vnode vnode)
       (let ((new (vnode-procedures-cache vnode)))
	 (define (loop procedures)
	   ;; We can use `eq?' here because we assume that
	   ;; (eq? (list-tail (eq-set-union x y) n) y) for some n.
	   ;; This is also noted at the definition of `eq-set-union'.
	   (if (eq? procedures old)
	       new
	       (begin (apply-procedure (car procedures))
		      (loop (cdr procedures)))))
	 (loop new)))
     (cond ((vnode? operator)
	    (process-vnode operator))
	   ((reference? operator)
	    (process-vnode (reference-variable operator)))
	   ((not (null? old))
	    (error "Encountered constant-operator combination twice"
		   combination))
	   (else
	    (if (procedure? operator)
		(apply-procedure operator))
	    true)))))

(define (procedure-applicator operands combination-value)
  (let ((number-supplied (length operands)))
    (lambda (procedure)
      (let ((number-required (length (procedure-required procedure)))
	    (number-optional (length (procedure-optional procedure)))
	    (rest (procedure-rest procedure)))
	(cond ((< number-supplied number-required)
	       (warn "Too few arguments" procedure operands))
	      (rest
	       (if (<= number-supplied (+ number-required number-optional))
		   ((vnode-connect!:constant (make-constant '())) rest)
		   ;; Can make this a LIST rvalue when that is implemented.
		   (vnode-unknowable! rest)))
	      ((> number-supplied (+ number-required number-optional))
	       (warn "Too many arguments" procedure operands))))
      (for-each vnode-connect!
		(append (procedure-required procedure)
			(procedure-optional procedure))
		operands)
      ((vnode-connect!:vnode (procedure-value procedure)) combination-value))))

(define-integrable (vnode-connect! vnode operand)
  ((&vnode-connect! (subproblem-value operand)) vnode))

(define ((vnode-connect!:procedure procedure) vnode)
  (let ((procedures (vnode-initial-procedures vnode)))
    (if (not (memq procedure procedures))
	(set-vnode-initial-procedures! vnode (cons procedure procedures))))
  (let loop ((vnode vnode))
    (let ((procedures (vnode-procedures-cache vnode)))
      (if (not (memq procedure procedures))
	  (begin (enqueue-nodes! (vnode-combinations vnode))
		 (set-vnode-procedures-cache! vnode
					      (cons procedure procedures))
		 (for-each loop (vnode-forward-links vnode)))))))

(define (vnode-connect!:vnode from)
  (define (self to)
    (if (not (memq from (vnode-backward-links to)))
	(begin (enqueue-nodes! (vnode-combinations to))
	       (set-vnode-backward-links! to
					  (cons from
						(vnode-backward-links to)))
	       (set-vnode-forward-links! from
					 (cons to (vnode-forward-links from)))
	       (set-vnode-procedures-cache!
		to
		(eq-set-union (vnode-procedures-cache from)
			      (vnode-procedures-cache to)))
	       (for-each (lambda (backward)
			   ((vnode-connect!:vnode backward) to))
			 (vnode-backward-links from))
	       (for-each self (vnode-forward-links to)))))
  self)

(define &vnode-connect!
  (standard-rvalue-operation vnode-connect!:constant vnode-connect!:procedure
			     vnode-connect!:vnode))

)