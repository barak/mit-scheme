#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/lapgn1.scm,v 4.1 1987/12/30 06:53:23 cph Exp $

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
(define *entry-bblock*)
(define *current-bblock*)
(define *dead-registers*)

(define (generate-bits rgraphs receiver)
  (with-new-node-marks
   (lambda ()
     (fluid-let ((*next-constant* 0)
		 (*interned-constants* '())
		 (*interned-variables* '())
		 (*interned-assignments* '())
		 (*interned-uuo-links* '())
		 (*block-start-label* (generate-label)))
       (for-each cgen-rgraph rgraphs)
       (receiver *block-start-label*
		 (generate/quotation-header *block-start-label*
					    *interned-constants*
					    *interned-variables*
					    *interned-assignments*
					    *interned-uuo-links*))))))

(define (cgen-rgraph rgraph)
  (fluid-let ((*current-rgraph* rgraph))
    (for-each (lambda (edge)
		(if (not (node-marked? (edge-right-node edge)))
		    (cgen-entry edge)))
	      (rgraph-entry-edges rgraph))))

(define (cgen-entry edge)
  (let ((bblock (edge-right-node edge)))
    (fluid-let ((*entry-bblock* bblock))
      (let loop ((bblock bblock))
	(cgen-bblock bblock)
	(let ((cgen-right
	       (lambda (edge)
		 (let ((next (edge-next-node edge)))
		   (if next
		       (begin
			 (if (node-previous>1? next)
			     (clear-map-between bblock edge next))
			 (if (not (node-marked? next))
			     (loop next))))))))
	  (if (sblock? bblock)
	      (cgen-right (snode-next-edge bblock))
	      (begin (cgen-right (pnode-consequent-edge bblock))
		     (cgen-right (pnode-alternative-edge bblock)))))))))

(define (clear-map-between bblock edge bblock*)
  (let ((map
	 (let ((map (bblock-register-map bblock))
	       (live-at-entry (bblock-live-at-entry bblock*)))
	   (let ((deletions
		  (list-transform-negative (register-map-live-homes map)
		    (lambda (pseudo-register)
		      (regset-member? live-at-entry pseudo-register)))))
	     (if (not (null? deletions))
		 (delete-pseudo-registers map
					  deletions
					  (lambda (map aliases) map))
		 map)))))
    (if (not (register-map-clear? map))
	(let ((sblock (make-sblock (clear-map-instructions map))))
	  (node-mark! sblock)
	  (edge-insert-snode! edge sblock)))))

(define (cgen-bblock bblock)
  ;; This procedure is coded out of line to facilitate debugging.
  (node-mark! bblock)
  (fluid-let ((*current-bblock* bblock)
	      (*register-map* (bblock-input-register-map bblock)))
    (set-bblock-instructions! bblock
			      (let loop ((rinst (bblock-instructions bblock)))
				(if (rinst-next rinst)
				    (let ((instructions (cgen-rinst rinst)))
				      (LAP ,@instructions
					   ,@(loop (rinst-next rinst))))
				    (cgen-rinst rinst))))
    (set-bblock-register-map! bblock *register-map*)))

(define (cgen-rinst rinst)
  (let ((rtl (rinst-rtl rinst)))
    ;; LOOP is for easy restart while debugging.
    (let loop ()
      (let ((match-result
	     (let ((rule
		    (if (eq? (car rtl) 'ASSIGN)
			(assq (caadr rtl) *assign-rules*)
			(assq (car rtl) *cgen-rules*))))
	       (and rule
		    (pattern-lookup (cdr rule) rtl)))))
	(if match-result
	    (fluid-let ((*dead-registers* (rinst-dead-registers rinst))
			(*prefix-instructions* '())
			(*needed-registers* '()))
	      (let ((instructions (match-result)))
		(delete-dead-registers!)
		(LAP ,@*prefix-instructions* ,@instructions)))
	    (begin (error "CGEN-BBLOCK: No matching rules" rtl)
		   (loop)))))))

(define (bblock-input-register-map bblock)
  (if (or (eq? bblock *entry-bblock*)
	  (not (node-previous=1? bblock)))
      (empty-register-map)
      (let ((previous (node-previous-first bblock)))
	(let ((map (bblock-register-map previous)))
	  (if (sblock? previous)
	      map
	      (delete-pseudo-registers
	       map
	       (regset->list
		(regset-difference (bblock-live-at-exit previous)
				   (bblock-live-at-entry bblock)))
	       (lambda (map aliases) map)))))))

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