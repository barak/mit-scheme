#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/compiler/machines/i386/lapopt.scm,v 1.8 1995/01/12 17:29:24 ssmith Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

;;;; LAP Optimizer for Intel i386.

(declare (usual-integrations))

(define (optimize-linear-lap instructions)
  ;; The following returns a list of information about the instruction:
  ;; 1. timing -- how many cycles
  ;; 2. pipelining -- which pipes 1 - first pipe, 2 - second pipe, 12 - both pipes, #f - unpipable
  ;; 3. list of registers modified
  ;; 4. list of registers read
  ;; 5. list of registers used in addressing
  (define (timing-of-inst inst)
    (car inst))
  (define (piping-of-inst inst)
    (cadr inst))
  (define (regs-mod-of-inst inst)
    (caddr inst))
  (define (regs-read-of-inst inst)
    (cadddr inst))
  (define (regs-addr-of-inst inst)
    (caddddr inst))
  
  (define (get-instruction-info inst)
    (define ins-vars '())
    (define instruction-data
      '((1 12 (a) (b) () mov ? (? a) (? b))
	(1 12 (a) () () lea (? a) ?)
	(1 12 (a) (b) () add ? (R a) (? b))
	(2 #f () (a) () jmp (R a))
	(0 #f () () () comment ?)
	(0 #f () () () scheme-object ? ?)
	(0 #f () () () label ?)
	(0 #f () () () block-offset ?)
	(0 #f () () () entry-point ?)
	(0 #f () () () word ? ?)))
      
    ;; Given a list of registers/variables from the instruction data,
    ;; this procedure creates a list containing all the registers referenced
    ;; If the list specifies a variable, then that variable is looked up to
    ;; find out if it is a register or an addressing mode (we only care
    ;; about registers).  A register can also be explicitly stated in the
    ;; list passed to make-reg-list
    (define (make-reg-list a)
      (define (find-var v)
	(let loop ((data ins-vars))
	  (if (null? data)
	      #f
	      (if (eq? (car (car data))
		       v)
		  (cdr (car data))
		  (loop (cdr data))))))
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
		     #t)))
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
	      (list (car (car data))
		    (cadr (car data))
		    (make-reg-list (caddr (car data)))
		    (make-reg-list (cadddr (car data)))
		    ())
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
	      (set! time (+ time (car pipe-1-data)))
	      (set! pipe-1-filled? #f)
	      (set! last-mod-regs (regs-mod-of-inst pipe-1-data))
	      (set! pipe-1-data ()))))
	      
      (if (null? inst)
	  (begin
	    (if pipe-1-filled?
		(flush-pipe-1))
	    time)
	  (let ((info (get-instruction-info (car inst))))
	    (if (and pipe-1-filled?
		     (or (= (piping-of-inst info) 1)
			 (eq? (piping-of-inst info) #f)
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
		      last-mod-regs)))))))

  (pp (get-pentium-timing instructions))
  instructions)







