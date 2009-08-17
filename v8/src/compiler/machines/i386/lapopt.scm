#| -*-Scheme-*-

$Id: 3e759592b80fb26b0295543bf763b4017500ea28 $

Copyright (c) 1992, 1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; LAP Optimizer for Intel i386.

(declare (usual-integrations))


(define (lap:mark-preferred-branch! pblock cn an)
  ;; This can leave pblock unchanged
  (define (single-instruction bblock other)
    (and (sblock? bblock)
	 (let ((next (snode-next bblock)))
	   (or (not next)
	       (eq? next other)))
	 (let find-first ((instrs (bblock-instructions bblock)))
	   (and (not (null? instrs))
		(let ((instr (car instrs)))
		  (if (eq? 'COMMENT (car instr))
		      (find-first (cdr instrs))
		      (and (let find-next ((instrs (cdr instrs)))
			     (or (null? instrs)
				 (and (eq? 'COMMENT (car (car instrs)))
				      (find-next (cdr instrs)))))
			   instr)))))))
  
  (define (try branch bblock other)
    (let ((instr (single-instruction bblock other)))
      (and instr
	   (not (instr-expands? instr))
	   (pnode/prefer-branch! pblock branch)
	   true)))

  (let ((branch-instr
	 (car (last-pair ((pblock-consequent-lap-generator pblock) 'FOO)))))
    (and (memq (car branch-instr)
	       '(COMB COMBT COMBF COMIB COMIBT COMIBF COMBN COMIBTN COMIBFN))
	 (or (try 'CONSEQUENT cn an)
	     (try 'ALTERNATIVE an cn)))))

(define (optimize-linear-lap instructions)
#|
  ;; The following returns a list of information about the instruction:
  ;; 1. timing -- how many cycles
  ;; 2. pipelining -- which pipes 1 - first pipe, 2 - second pipe, 12 - both pipes, #f - unpipable
  ;; 3. list of registers modified
  ;; 4. list of registers read
  ;; 5. list of registers used in addressing
  (define (timing-of-inst inst)
    (first inst))
  (define (piping-of-inst inst)
    (second inst))
  (define (regs-mod-of-inst inst)
    (third inst))
  (define (regs-use-of-inst inst)
    (fourth inst))
  (define (regs-addr-of-inst inst)
    (fifth inst))

  ;; Checks whether two lists have any items in common
  (define (intersect? a b)
    (if (pair? a)
	(and (memq (car a) b)
	     (intersect? (cdr a) b))
	#f))
  
  (define (get-instruction-info inst)
    (define ins-vars '())
    (define instruction-data
      '((1 12 (a) (b) () mov ? (? a) (? b))
	(1 12 (a) () () lea (? a) ?)
	(1 12 (a) (b) () add ? (R a) (? b))
	(2 #f () (a) () jmp (R a))
	(#f #f () () () comment ?)
	(#f #f () () () scheme-object ? ?)
	(#f #f () () () label ?)
	(#f #f () () () block-offset ?)
	(#f #f () () () entry-point ?)
	(#f #f () () () word ? ?)))
    (define (find-var v)
      (let loop ((data ins-vars))
	(if (null? data)
	    #f
	    (if (eq? (car (car data))
		     v)
		(cdr (car data))
		(loop (cdr data))))))
    
    ;; Given a list of registers/variables from the instruction data,
    ;; this procedure creates a list containing all the registers referenced
    ;; If the list specifies a variable, then that variable is looked up to
    ;; find out if it is a register or an addressing mode (we only care
    ;; about registers).  A register can also be explicitly stated in the
    ;; list passed to make-reg-list
    (define (make-reg-list a)
      (if (pair? a)
	  (if (number? (car a))
	      (cons (car a)
		    (make-reg-list (cdr a)))
	      (let ((data (find-var (car a))))
		(if data
		    (if (eq? (car data) 'R)
			(cons (cadr data)
			      (make-reg-list (cdr a)))
			(make-reg-list (cdr a)))
		    (begin
		      (pp (car a))
		      ()))))
	  a))
    
    (define (make-ea-list a inst)
      (define (get-regs-from-ea ea)
	(if (pair? ea)
	    (cond ((eq? '@R (car ea))
		   (list (second ea)))
		  ((eq? '@RI (car ea))
		   (list (second ea) (third ea)))
		  ((eq? '@ROI (car ea))
		   (list (third ea) (fifth ea)))
		  ((eq? '@RO (car ea))
		   (list (third ea)))
		  (else ()))
	    ()))
      (if (pair? inst)
	  (append (get-regs-from-ea (car inst))
		  (make-ea-list a
				(cdr inst)))
	  (if (pair? a)
	      (if (number? (car a))
		  (cons (car a)
			(make-ea-list (cdr a) inst))
		  (let ((data (find-var (car a))))
		    (if data
			(append (get-regs-from-ea data)
				(make-ea-list (cdr a) inst))
			(begin
			  (pp (car a))
			  ()))))
	      ())))
    
    ;; Checks to see if the the pattern matches given data
    (define (is-all-match? pattern data)
      (define (is-match? pattern data)
	(cond ((eq? '? pattern)
	       #t)
	      ((pair? pattern)
	       (if (or (eq? '? (car pattern))
		       (eq? (car pattern)
			    (car data)))
		   (begin
		     ;; Add data to variable list
		     (set! ins-vars
			   (cons (cons (cadr pattern)
				       data)
				 ins-vars))
		     #t)
		   #f))
	      ((eq? pattern data)
	       #t)
	      (else
	       #f)))
      (cond ((and (pair? pattern) (pair? data))
	     (and (is-match? (car pattern)
			     (car data))
		  (is-all-match? (cdr pattern)
				 (cdr data))))
	    (else
	     (eq? pattern data))))

    (let loop ((data instruction-data))
      (set! ins-vars '())
      (if (null? data)
	  (begin
	    (pp inst)
	    '(0 0 () () ()))
	  (if (is-all-match? (cdr (cdr (cdr (cdr (cdr (car data))))))
			     inst)
	      (list (timing-of-inst (car data))
		    (piping-of-inst (car data))
		    (make-reg-list (regs-mod-of-inst (car data)))
		    (make-reg-list (regs-use-of-inst (car data)))
		    (make-ea-list (regs-addr-of-inst (car data))
				  inst))
	      (loop (cdr data))))))
  
  
  (define (get-pentium-timing instructions)
    (let loop ((inst instructions)
	       (time 0)
	       (pipe-1-filled? #f)
	       (pipe-1-data ())
	       (last-mod-regs ()))
      (define (flush-pipe-1)
	(if pipe-1-filled?
	    (begin
	      (set! time (+ time (timing-of-inst pipe-1-data)))
	      (set! pipe-1-filled? #f)
	      (set! last-mod-regs (regs-mod-of-inst pipe-1-data))
	      (set! pipe-1-data ()))))
	      
      (if (null? inst)
	  (begin
	    (if pipe-1-filled?
		(flush-pipe-1))
	    time)
	  (let ((info (get-instruction-info (car inst))))
	    (if (not (timing-of-inst info))
		(loop (cdr inst)
		      time
		      pipe-1-filled?
		      pipe-1-data
		      last-mod-regs)
		(begin
		  (if (and pipe-1-filled?
			   (or (eq? (piping-of-inst info) #f)
			       (= (piping-of-inst info) 1)
			       (intersect? (append (regs-use-of-inst info)
						   (regs-addr-of-inst info))
					   (regs-mod-of-inst pipe-1-data))
			       (intersect? (regs-addr-of-inst info)
					   last-mod-regs)))
		      (flush-pipe-1))
		  (if (intersect? last-mod-regs
				  (regs-addr-of-inst info))
		      (set! time (+ time 1)))
		  (if pipe-1-filled?
		      (loop (cdr inst)
			    (+ time (if (> (timing-of-inst info)
					   (timing-of-inst pipe-1-data))
					(timing-of-inst info)
					(timing-of-inst pipe-1-data)))
			    #f
			    ()
			    (append (regs-mod-of-inst info)
				    (regs-mod-of-inst pipe-1-data)))
		      (loop (cdr inst)
			    time
			    #t
			    info
			    last-mod-regs))))))))

  (pp (get-pentium-timing instructions))
|#
  instructions)







