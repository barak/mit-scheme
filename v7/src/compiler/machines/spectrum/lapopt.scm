#| -*-Scheme-*-

$Id: lapopt.scm,v 1.3 1993/02/14 06:26:55 gjr Exp $

Copyright (c) 1991-1993 Massachusetts Institute of Technology

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

;;;; LAP Optimizer for HP Precision Archtecture.
;; package: (compiler lap-optimizer)

(declare (usual-integrations))

;;;; An instruction classifier and decomposer

(define (classify-instruction instr)
  ;; returns: type target source-1 source-2
  ;; This needs the following:
  ;; - Base modification (LDWM/STWM)
  ;; - Third source (indexed loads)
  ;; - Floats
  (let ((opcode (car instr)))
    (cond ((memq opcode '(ANDCM AND OR XOR UXOR SUB DS SUBT
				SUBB ADD SH1ADD SH2ADD SH3ADD ADDC
				COMCLR UADDCM UADDCMT ADDL SH1ADDL
				SH2ADDL SH3ADDL SUBO SUBTO SUBBO
				ADDO SH1ADDO SH2ADDO SH3ADDO ADDCO
				VSHD SHD))
	   ;; source source ... target
	   (values 'ALU
		   ;; not (list-ref instr 4)
		   (car (last-pair instr))
		   (list-ref instr 2)
		   (list-ref instr 3)))
	  ((memq opcode '(ADDI ADDIO ADDIT ADDITO SUBI SUBIO COMICLR))
	   ;; immed source target
	   (values 'ALU
		   (list-ref instr 4)
		   (list-ref instr 3)
		   false))
	  ((memq opcode '(COPY))
	   ;; source target
	   (values 'ALU
		   (list-ref instr 3)
		   (list-ref instr 2)
		   false))
	  ((memq opcode '(LDW LDB LDO LDH))
	   ;; (offset n m source) target
	   (values 'MEMORY
		   (list-ref instr 3)
		   (cadddr (list-ref instr 2))
		   false))
	  ((memq opcode '(STW STB STH))
	   ;; source1 (offset n m source2)
	   (values 'MEMORY
		   false
		   (list-ref instr 2)
		   (cadddr (list-ref instr 3))))
	  ((memq opcode '(LDI LDIL))
	   ;; immed target
	   (values 'ALU
		   (list-ref instr 3)
		   false
		   false))
	  ((memq opcode '(ADDIL))
	   ;; immed source
	   (values 'ALU
		   regnum:addil-result
		   (list-ref instr 3)
		   false))
	  ((memq opcode '(NOP))
	   (values 'ALU false false false))

	  #|
	  ((memq opcode '(B BL GATE))
	   <>)
	  ((memq opcode '(BV BLR))
	   ;; source-1 source-2
	   (values 'CONTROL
		   false
		   (list-ref instr 2)
		   (list-ref instr 3)))
	  ((memq opcode '(BLR))
	   ;; source target
	   (values 'CONTROL
		   (list-ref instr 3)
		   (list-ref instr 2)
		   false))
	  ((memq opcode '(BV))
	   ;; source-1 source-2
	   (values 'CONTROL
		   false
		   (list-ref instr 2)
		   (list-ref instr 3)))
	  ((memq opcode '(BE))
	   <>)
	  ((memq opcode '(COMB ...))
	   <>)
	  |#
	  ((memq opcode '(LABEL EQUATE ENTRY-POINT
				EXTERNAL-LABEL BLOCK-OFFSET))
	   (values 'DIRECTIVE false false false))
	  (else
	   (values 'UNKNOWN false false false)))))

(define (instruction-type instr)
  (with-values (lambda () (classify-instruction instr))
    (lambda (type tgt src1 src2)
      tgt src1 src2			; ignored
      type)))

;;;; Utilities

;; A trivial pattern matcher

(define (match pattern instance)
  (let ((dict '(("empty" . empty))))

    (define (match-internal pattern instance)
      (cond ((not (pair? pattern))
	     (eqv? pattern instance))
	    ((eq? (car pattern) '?)
	     (let ((var (cadr pattern))
		   (val instance))
	       (cond ((eq? var '?)	; quoting ?
		      (eq? val '?))
		     ((assq var dict)
		      => (lambda (place)
			   (equal? (cdr place) val)))
		     (else
		      (set! dict (cons (cons var val) dict))
		      true))))
	    (else
	     (and (pair? instance)
		  (match-internal (car pattern) (car instance))
		  (match-internal (cdr pattern) (cdr instance))))))

    (and (match-internal pattern instance)
	 dict)))

(define (skips? instr)
  ;; Not really true, for example
  ;; (COMBT (<) ...)
  (and (pair? (cadr instr))
       (not (memq (car instr)
		  '(B BL BV BLR BLE BE)))))

(define return-pattern			; reversed
  (cons
   `(LDO () (OFFSET (? frame) 0 ,regnum:stack-pointer) ,regnum:stack-pointer)
   `((BV (N) 0 (? ret))
     (DEP () ,regnum:quad-bitmask
	  ,(-1+ scheme-type-width)
	  ,scheme-type-width
	  (? ret))
     (LDWM () (OFFSET 4 0 ,regnum:stack-pointer) (? ret))
     . (? more-insts))))

(define (find-or-label instrs)
  (and (not (null? instrs))
       (if (memq (caar instrs) '(COMMENT SCHEME-OBJECT EQUATE))
	   (find-or-label (cdr instrs))
	   instrs)))

(define (find-non-label instrs)
  (and (not (null? instrs))
       (if (memq (caar instrs) '(LABEL COMMENT SCHEME-OBJECT EQUATE))
	   (find-non-label (cdr instrs))
	   instrs)))

(define (list-difference whole suffix)
  (if (eq? whole suffix)
      '()
      (cons (car whole)
	    (list-difference (cdr whole) suffix))))

(define (optimize-linear-lap instructions)
  (define (fix-complex-return ret frame junk instr avoid)
    (let ((ret (list-search-positive
		   (list ret regnum:first-arg regnum:second-arg
			 regnum:third-arg regnum:fourth-arg)
		 (lambda (reg)
		   (not (memq reg avoid))))))
      `(,@(reverse junk)
	(LDW () (OFFSET ,frame 0 ,regnum:stack-pointer) ,ret)
	,instr
	(DEP () ,regnum:quad-bitmask
	     ,(-1+ scheme-type-width)
	     ,scheme-type-width
	     ,ret)
	(BV () 0 ,ret)
	(LDO () (OFFSET ,(+ frame 4) 0 ,regnum:stack-pointer)
	     ,regnum:stack-pointer))))

  (define (fix-simple-return ret frame junk)
    `(,@(reverse junk)
      (LDW () (OFFSET ,frame 0 ,regnum:stack-pointer) ,ret)
      (LDO () (OFFSET ,(+ frame 4) 0 ,regnum:stack-pointer)
	   ,regnum:stack-pointer)
      (DEP () ,regnum:quad-bitmask
	   ,(-1+ scheme-type-width)
	   ,scheme-type-width
	   ,ret)
      (BV (N) 0 ,ret)))

  (define (fix-a-return dict1 junk dict2 rest)
    (let* ((next (find-or-label rest))
	   (next* (and next (find-non-label next)))
	   (frame (cdr (assq 'frame dict2)))
	   (ret (cdr (assq 'ret dict1))))
      (cond ((or (not next)
		 (and (eq? (caar next) 'LABEL)
		      (or (not next*)
			  (not (skips? (car next*))))))
	     (values (fix-simple-return ret frame junk)
		     rest))
	    ((or (memq (caar next)
		       '(LABEL ENTRY-POINT EXTERNAL-LABEL BLOCK-OFFSET))
		 (skips? (car next)))
	     (values '() false))
	    (else
	     (with-values
		 (lambda () (classify-instruction (car next)))
	       (lambda (type target src1 src2)
		 (if (or (not (memq type '(MEMORY ALU)))
			 (eq? target regnum:stack-pointer))
		     (values (fix-simple-return ret frame junk)
			     rest)
		     (values
		      (fix-complex-return ret frame
					  (append junk
						  (list-difference rest next))
					  (car next)
					  (list target src1 src2))
		      (cdr next)))))))))

  (define (fix-sequences instrs tail)
    (cond ((null? instrs)
	   tail)
	  ((and (eq? 'BV (caar instrs))
		(match (cdr return-pattern) instrs))
	   => (lambda (dict1)
		(let* ((tail* (cdddr instrs))
		       (next (find-or-label tail*))
		       (fail
			(fix-sequences tail*
				       (append (reverse (list-head instrs 3))
					       tail)))
		       (dict2
			(and next
			     (match (car return-pattern) (car next)))))
			     
		  (if (not dict2)
		      (fail)
		      (with-values
			  (lambda ()
			    (fix-a-return dict1
					  (list-difference tail* next)
					  dict2
					  (cdr next)))
			(lambda (frobbed untouched)
			  (if (null? frobbed)
			      (fail)
			      (fix-sequences untouched
					     (append frobbed tail)))))))))
	  ((and (eq? 'B (caar instrs))
		(equal? '(N) (cadar instrs)))
	   (let* ((next (find-or-label (cdr instrs)))
		  (next* (and next (find-non-label (cdr next)))))
	     (if (and next
		      (memq (instruction-type (car next) '(MEMORY ALU)))
		      (not (skips? (car next)))
		      (or (not next*)
			  (not (skips? (car next*)))))
		 (fix-sequences (cdr next)
				`(,@(reverse
				     (list-difference (cdr instrs) next))
				  (B () ,@(cddar instrs))
				  ,(car next)
				  ,@tail))
		 (fix-sequences (cdr instrs)
				(cons (car instrs) tail)))))	  
	  (else
	   (fix-sequences (cdr instrs)
			  (cons (car instrs) tail)))))

  (fix-sequences (reverse instructions) '()))