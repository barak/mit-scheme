#| -*-Scheme-*-

$Id: debug.scm,v 4.16 2001/10/17 03:26:55 cph Exp $

Copyright (c) 1988-1990, 1999, 2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Compiler Debugging Support

(declare (usual-integrations))

(define (po object)
  (let ((object (->tagged-vector object)))
    (write-line object)
    (for-each pp ((tagged-vector/description object) object))))

(define (debug/find-procedure name)
  (let loop ((procedures *procedures*))
    (and (pair? procedures)
	 (if (and (not (procedure-continuation? (car procedures)))
		  (or (eq? name (procedure-name (car procedures)))
		      (eq? name (procedure-label (car procedures)))))
	     (car procedures)
	     (loop (cdr procedures))))))

(define (debug/find-continuation number)
  (let ((label
	 (intern (string-append "continuation-" (number->string number)))))
    (let loop ((procedures *procedures*))
      (and (pair? procedures)
	   (if (and (procedure-continuation? (car procedures))
		    (eq? label (procedure-label (car procedures))))
	       (car procedures)
	       (loop (cdr procedures)))))))

(define (debug/find-entry-node node)
  (let ((node (->tagged-vector node)))
    (if (eq? (expression-entry-node *root-expression*) node)
	(write-line *root-expression*))
    (for-each (lambda (procedure)
		(if (eq? (procedure-entry-node procedure) node)
		    (write-line procedure)))
	      *procedures*)))

(define (debug/where object)
  (cond ((compiled-code-block? object)
	 (write-line (compiled-code-block/debugging-info object)))
	((compiled-code-address? object)
	 (write-line
	  (compiled-code-block/debugging-info
	   (compiled-code-address->block object)))
	 (write-string "Offset: ")
	 (write-string
	  (number->string (compiled-code-address->offset object) 16))
	 (newline))
	(else
	 (error "debug/where -- what?" object))))

(define (write-rtl-instructions rtl port)
  (write-instructions
   (lambda ()
     (with-output-to-port port
       (lambda ()
	 (for-each show-rtl-instruction rtl))))))

(define (dump-rtl filename)
  (write-instructions
   (lambda ()
     (with-output-to-file (pathname-new-type (->pathname filename) "rtl")
       (lambda ()
	 (for-each show-rtl-instruction (linearize-rtl *rtl-graphs*)))))))

(define (show-rtl rtl)
  (pp-instructions
   (lambda ()
     (for-each show-rtl-instruction rtl)))
  (newline))

(define (show-bblock-rtl bblock)
  (pp-instructions
   (lambda ()
     (bblock-walk-forward (->tagged-vector bblock)
       (lambda (rinst)
	 (show-rtl-instruction (rinst-rtl rinst))))))
  (newline))

(define (write-instructions thunk)
  (fluid-let ((*show-instruction* write)
	      (*unparser-radix* 16)
	      (*unparse-uninterned-symbols-by-name?* #t))
    (thunk)))

(define (pp-instructions thunk)
  (fluid-let ((*show-instruction* pretty-print)
	      (*pp-primitives-by-name* #f)
	      (*unparser-radix* 16)
	      (*unparse-uninterned-symbols-by-name?* #t))
    (thunk)))

(define *show-instruction*)

(define (show-rtl-instruction rtl)
  (if (memq (car rtl)
	    '(LABEL CONTINUATION-ENTRY CONTINUATION-HEADER IC-PROCEDURE-HEADER
		    OPEN-PROCEDURE-HEADER PROCEDURE-HEADER CLOSURE-HEADER))
      (newline))
  (*show-instruction* rtl)
  (newline))

(define procedure-queue)
(define procedures-located)

(define (show-fg)
  (fluid-let ((procedure-queue (make-queue))
	      (procedures-located '()))
    (write-string "---------- Expression ----------")
    (newline)
    (fg/print-object *root-expression*)
    (with-new-node-marks
     (lambda ()
       (fg/print-entry-node (expression-entry-node *root-expression*))
       (queue-map!/unsafe procedure-queue
	 (lambda (procedure)
	   (newline)
	   (if (procedure-continuation? procedure)
	       (write-string "---------- Continuation ----------")
	       (write-string "---------- Procedure ----------"))
	   (newline)
	   (fg/print-object procedure)
	   (fg/print-entry-node (procedure-entry-node procedure))))))
    (newline)
    (write-string "---------- Blocks ----------")
    (newline)
    (fg/print-blocks (expression-block *root-expression*))))

(define (show-fg-node node)
  (fluid-let ((procedure-queue #f))
    (with-new-node-marks
     (lambda ()
       (fg/print-entry-node
	(let ((node (->tagged-vector node)))
	  (if (procedure? node)
	      (procedure-entry-node node)
	      node)))))))

(define (fg/print-entry-node node)
  (if node
      (fg/print-node node)))

(define (fg/print-object object)
  (po object)
  (newline))

(define (fg/print-blocks block)
  (fg/print-object block)
  (for-each fg/print-object (block-bound-variables block))
  (if (not (block-parent block))
      (for-each fg/print-object (block-free-variables block)))
  (for-each fg/print-blocks (block-children block))
  (for-each fg/print-blocks (block-disowned-children block)))

(define (fg/print-node node)
  (if (and node
	   (not (node-marked? node)))
      (begin
	(node-mark! node)
	(fg/print-object node)
	(cfg-node-case (tagged-vector/tag node)
	  ((PARALLEL)
	   (for-each fg/print-subproblem (parallel-subproblems node))
	   (fg/print-node (snode-next node)))
	  ((APPLICATION)
	   (fg/print-rvalue (application-operator node))
	   (for-each fg/print-rvalue (application-operands node)))
	  ((VIRTUAL-RETURN)
	   (fg/print-rvalue (virtual-return-operand node))
	   (fg/print-node (snode-next node)))
	  ((POP)
	   (fg/print-rvalue (pop-continuation node))
	   (fg/print-node (snode-next node)))
	  ((ASSIGNMENT)
	   (fg/print-rvalue (assignment-rvalue node))
	   (fg/print-node (snode-next node)))
	  ((DEFINITION)
	   (fg/print-rvalue (definition-rvalue node))
	   (fg/print-node (snode-next node)))
	  ((TRUE-TEST)
	   (fg/print-rvalue (true-test-rvalue node))
	   (fg/print-node (pnode-consequent node))
	   (fg/print-node (pnode-alternative node)))
	  ((STACK-OVERWRITE FG-NOOP)
	   (fg/print-node (snode-next node)))))))

(define (fg/print-rvalue rvalue)
  (if procedure-queue
      (let ((rvalue (rvalue-known-value rvalue)))
	(if (and rvalue
		 (rvalue/procedure? rvalue)
		 (not (memq rvalue procedures-located)))
	    (begin
	      (set! procedures-located (cons rvalue procedures-located))
	      (enqueue!/unsafe procedure-queue rvalue))))))

(define (fg/print-subproblem subproblem)
  (fg/print-object subproblem)
  (if (subproblem-canonical? subproblem)
      (fg/print-rvalue (subproblem-continuation subproblem)))
  (let ((prefix (subproblem-prefix subproblem)))
    (if (not (cfg-null? prefix))
	(fg/print-node (cfg-entry-node prefix)))))