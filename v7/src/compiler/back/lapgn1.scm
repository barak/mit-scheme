#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/lapgn1.scm,v 1.39 1987/07/08 22:00:41 jinx Exp $

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

;;;; LAP Generator

(declare (usual-integrations))

(define *block-start-label*)
(define *code-object-label*)
(define *code-object-entry*)
(define *current-rnode*)
(define *dead-registers*)
(define *continuation-queue*)

(define (generate-bits quotations procedures continuations receiver)
  (with-new-node-marks
   (lambda ()
     (fluid-let ((*next-constant* 0)
		 (*interned-constants* '())
		 (*interned-variables* '())
		 (*interned-uuo-links* '())
		 (*block-start-label* (generate-label))
		 (*code-object-label*)
		 (*code-object-entry*)
		 (*continuation-queue* (make-queue)))
       (for-each (lambda (quotation)
		   (cgen-entry quotation quotation-rtl-entry))
		 quotations)
       (for-each (lambda (procedure)
		   (cgen-entry procedure procedure-rtl-entry))
		 procedures)
       (queue-map! *continuation-queue*
	 (lambda (continuation)
	   (cgen-entry continuation continuation-rtl-entry)))
       (for-each (lambda (continuation)
		   (if (not (continuation-frame-pointer-offset continuation))
		       (error "GENERATE-LAP: Continuation not processed"
			      continuation)))
		 *continuations*)
       (receiver *block-start-label*
		 (generate/quotation-header *block-start-label*
					    *interned-constants*
					    *interned-variables*
					    *interned-uuo-links*))))))

(define (cgen-entry object extract-entry)
  (set! *code-object-label* (code-object-label-initialize object))
  (let ((rnode (extract-entry object)))
    (set! *code-object-entry* rnode)
    (cgen-rnode rnode)))

(define (cgen-rnode rnode)
  (let ((offset (cgen-rnode-1 rnode)))
    (define (cgen-right-node edge)
      (let ((next (edge-next-node edge)))
	(if next
	    (begin
	      (record-rnode-frame-pointer-offset! next offset)
	      (if (node-previous>1? next)
		  (let ((snode (statement->snode '(NOOP))))
		    (set-rnode-lap! snode
				    (clear-map-instructions
				     (rnode-register-map rnode)))
		    (node-mark! snode)
		    (edge-insert-snode! edge snode)))
	      (if (not (node-marked? next))
		  (cgen-rnode next))))))
    (if (rtl-snode? rnode)
	(cgen-right-node (snode-next-edge rnode))
	(begin (cgen-right-node (pnode-consequent-edge rnode))
	       (cgen-right-node (pnode-alternative-edge rnode))))))

(define (cgen-rnode-1 rnode)
  ;; This procedure is coded out of line to facilitate debugging.
  (node-mark! rnode)
  ;; LOOP is for easy restart while debugging.
  (let loop ()
    (let ((match-result
	   (pattern-lookup
	    (cdr (or (if (eq? (car (rnode-rtl rnode)) 'ASSIGN)
			 (assq (caadr (rnode-rtl rnode)) *assign-rules*)
			 (assq (car (rnode-rtl rnode)) *cgen-rules*))
		     (error "CGEN-RNODE: Unknown keyword" rnode)))
	    (rnode-rtl rnode))))
      (if match-result
	  (fluid-let ((*current-rnode* rnode)
		      (*dead-registers* (rnode-dead-registers rnode))
		      (*register-map* (rnode-input-register-map rnode))
		      (*prefix-instructions* '())
		      (*needed-registers* '())
		      (*frame-pointer-offset*
		       (rnode-frame-pointer-offset rnode)))
	    (let ((instructions (match-result)))
	      (set-rnode-lap! rnode
			      (LAP ,@*prefix-instructions* ,@instructions)))
	    (delete-dead-registers!)
	    (set-rnode-register-map! rnode *register-map*)
	    *frame-pointer-offset*)
	  (begin (error "CGEN-RNODE: No matching rules" (rnode-rtl rnode))
		 (loop))))))

(define (rnode-input-register-map rnode)
  (if (or (eq? rnode *code-object-entry*)
	  (not (node-previous=1? rnode)))
      (empty-register-map)
      (let ((previous (node-previous-first rnode)))
	(let ((map (rnode-register-map previous)))
	  (if (rtl-pnode? previous)
	      (delete-pseudo-registers
	       map
	       (regset->list
		(regset-difference (bblock-live-at-exit (node-bblock previous))
				   (bblock-live-at-entry (node-bblock rnode))))
	       (lambda (map aliases) map))
	      map)))))

(define *cgen-rules* '())
(define *assign-rules* '())

(define (add-statement-rule! pattern result-procedure)
  (let ((result (cons pattern result-procedure)))
    (if (eq? (car pattern) 'ASSIGN)
	(let ((entry (assq (caadr pattern) *assign-rules*)))
	  (if entry
	      (set-cdr! entry (cons result (cdr entry)))
	      (set! *assign-rules*
		    (cons (list (caadr pattern) result)
			  *assign-rules*))))
	(let ((entry (assq (car pattern) *cgen-rules*)))
	  (if entry
	      (set-cdr! entry (cons result (cdr entry)))
	      (set! *cgen-rules*
		    (cons (list (car pattern) result)
			  *cgen-rules*))))))
  pattern)