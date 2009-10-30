#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Interrupt Checks
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;; The first two procedures are the interface.
;; GET-EXIT-INTERRUPT-CHECKS and GET-ENTRY-INTERRUPT-CHECKS get a list
;; of kinds interrupt check.  An empty list implies no check is
;; required.  The list can contain these symbols:
;;
;;    STACK      stack check required here
;;    HEAP       heap check required here
;;    INTERRUPT  check required here to avoid loops without checks.
;;
;; The traversal and decision making is done immediately prior to LAP
;; generation (from PRE-LAPGEN-ANALYSIS.)

(define (get-entry-interrupt-checks)
  (get-interupt-checks 'ENTRY-INTERRUPT-CHECKS))

(define (get-exit-interrupt-checks)
  (get-interupt-checks 'EXIT-INTERRUPT-CHECKS))

(define (expect-no-entry-interrupt-checks)
  (if (not (null? (get-entry-interrupt-checks)))
      (error "No entry interrupt checks expected here" *current-bblock*)))

(define (expect-no-exit-interrupt-checks)
  (if (not (null? (get-exit-interrupt-checks)))
      (error "No exit interrupt checks expected here" *current-bblock*)))

(define (get-interupt-checks kind)
  (or (cfg-node-get *current-bblock* kind)
      (error "DETERMINE-INTERRUPT-CHECKS failed" kind)))

;; This algorithm finds leaf-procedure-like paths in the rtl control
;; flow graph.  If a procedure entry point can only reach a return, it
;; is leaf-like.  If a return can only be reached from a procedure
;; entry, it too is leaf-like.
;;
;; If a procedure reaches a procedure call, that could be a loop, so
;; it is not leaf-like.  Similarly, if a continuation entry reaches
;; return, that could be a long unwinding of recursion, so a check is
;; needed in case the unwinding does allocation.
;;
;; Typically, true leaf procedures avoid both checks, and trivial
;; cases (like MAP returning '()) avoid the exit check.
;;
;; This could be a lot smarter.  For example, a procedure entry does
;; not need to check for interrupts if it reaches call sites of
;; strictly lesser arity; or it could analyze the cycles in the CFG
;; and select good places to break them
;;
;; The algorithm has three phases: (1) explore the CFG to find all
;; entry and exit points, (2) propagate entry (exit) information so
;; that each potential interrupt check point knows what kinds of exits
;; (entrys) it reaches (is reached from), and (3) decide on the kinds
;; of interrupt check that are required at each entry and exit.

(define (determine-interrupt-checks bblock)
  (let ((entries '())
	(exits '()))

    (define (explore bblock)
      (or (cfg-node-get bblock 'INTERRUPT-CHECK-EXPLORE)
	  (begin
	    (cfg-node-put! bblock 'INTERRUPT-CHECK-EXPLORE #T)
	    (if (node-previous=0? bblock)
		(set! entries (cons bblock entries))
		(if (rtl:continuation-entry?
		     (rinst-rtl (bblock-instructions bblock)))
		    ;; previous block is invocation:special-primitive
		    ;; so it is just an out of line instruction
		    (cfg-node-put! bblock 'ENTRY-INTERRUPT-CHECKS '())))
	    (for-each-previous-node bblock explore)
	    (for-each-subsequent-node bblock explore)
	    (if (and (snode? bblock)
		     (or (not (snode-next bblock))
			 (let ((last (last-insn bblock)))
			   (or (rtl:invocation:special-primitive? last)
			       (rtl:invocation:primitive? last)))))
		(set! exits (cons bblock exits))))))

    (define (for-each-subsequent-node node procedure)
      (if (snode? node)
	  (if (snode-next node)
	      (procedure (snode-next node)))
	  (begin
	    (procedure (pnode-consequent node))
	    (procedure (pnode-alternative node)))))

    (define (propagator for-each-link)
      (lambda (node update place)
	(let propagate ((node node))
	  (let ((old (cfg-node-get node place)))
	    (let ((new (update old)))
	      (if (not (equal? old new))
		  (begin
		    (cfg-node-put! node place new)
		    (for-each-link node propagate))))))))

    (define upward   (propagator for-each-previous-node))
    (define downward (propagator for-each-subsequent-node))

    (define (setting-flag old) old #T)

    (define (propagate-entry-info bblock)
      (let ((insn (rinst-rtl (bblock-instructions bblock))))
	(cond ((or (rtl:continuation-entry? insn)
		   (rtl:continuation-header? insn))
	       (downward bblock setting-flag 'REACHED-FROM-CONTINUATION))
	      ((or (rtl:closure-header? insn)
		   (rtl:ic-procedure-header? insn)
		   (rtl:open-procedure-header? insn)
		   (rtl:procedure-header? insn))
	       (downward bblock setting-flag 'REACHED-FROM-PROCEDURE))
	      (else unspecific))))

    (define (propagate-exit-info exit-bblock)
      (let ((insn (last-insn exit-bblock)))
	(cond ((rtl:pop-return? insn)
	       (upward exit-bblock setting-flag 'REACHES-POP-RETURN))
	      (else
	       (upward exit-bblock setting-flag 'REACHES-INVOCATION)))))

    (define (decide-entry-checks bblock)
      (define (checks! types)
	(cfg-node-put! bblock 'ENTRY-INTERRUPT-CHECKS types))
      (define (decide-label internal-label)
	(let ((object (label->object internal-label)))
	  (let ((stack?
		 (if (and (rtl-procedure? object)
			  (not (rtl-procedure/stack-leaf? object))
			  compiler:generate-stack-checks?)
		     '(STACK)
		     '())))
	    (if (or (cfg-node-get bblock 'REACHES-INVOCATION)
		    (pair? stack?))
		(checks! (cons* 'HEAP 'INTERRUPT stack?))
		(checks! '())))))

      (let ((insn (rinst-rtl (bblock-instructions bblock))))
	(cond ((rtl:continuation-entry? insn)  (checks! '()))
	      ((rtl:continuation-header? insn) (checks! '()))
	      ((rtl:closure-header? insn)
	       (decide-label (rtl:closure-header-procedure insn)))
	      ((rtl:ic-procedure-header? insn)
	       (decide-label (rtl:ic-procedure-header-procedure insn)))
	      ((rtl:open-procedure-header? insn)
	       (decide-label (rtl:open-procedure-header-procedure insn)))
	      ((rtl:procedure-header? insn)
	       (decide-label (rtl:procedure-header-procedure insn)))
	      (else
	       (checks! '(INTERRUPT))))))

    (define (last-insn bblock)
      (rinst-rtl (rinst-last (bblock-instructions bblock))))

    (define (decide-exit-checks bblock)
      (define (checks! types)
	(cfg-node-put! bblock 'EXIT-INTERRUPT-CHECKS types))
      (if (rtl:pop-return? (last-insn bblock))
	  (if (cfg-node-get bblock 'REACHED-FROM-CONTINUATION)
	      (checks! '(INTERRUPT))
	      (checks! '()))
	  (checks! '())))

    (explore bblock)

    (for-each propagate-entry-info entries)
    (for-each propagate-exit-info exits)
    (for-each decide-entry-checks entries)
    (for-each decide-exit-checks exits)))