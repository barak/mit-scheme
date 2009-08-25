#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; LAP Optimizer for HP Precision Archtecture.
;; package: (compiler lap-optimizer)

(declare (usual-integrations))

;;;; An instruction classifier and decomposer

(define-integrable (float-reg reg)
  (+ 32 reg))

(define (classify-instruction instr)
  ;; (values type target source-1 source-2 offset)
  ;; This needs the following:
  ;; - Loads with base modification (LDWM)
  ;; - Third source (indexed loads)
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
		   (list-ref instr 3)
		   false))
	  ((memq opcode '(ADDI ADDIO ADDIT ADDITO SUBI SUBIO COMICLR))
	   ;; immed source target
	   (values 'ALU
		   (list-ref instr 4)
		   (list-ref instr 3)
		   false
		   false))
	  ((memq opcode '(COPY))
	   ;; source target
	   (values 'ALU
		   (list-ref instr 3)
		   (list-ref instr 2)
		   false
		   false))
	  ((memq opcode '(LDW LDB LDO LDH))
	   ;; (offset n m source) target
	   (let ((offset (list-ref instr 2)))
	     (values 'MEMORY
		     (list-ref instr 3)
		     (cadddr offset)
		     false
		     (cadr offset))))
	  ((memq opcode '(STW STB STH))
	   ;; source1 (offset n m source2)
	   (let ((offset (list-ref instr 3)))
	     (values 'MEMORY
		     false
		     (list-ref instr 2)
		     (cadddr offset)
		     (cadr offset))))
	  ((memq opcode '(STWM STWS))
	   ;; source1 (offset n m target/source)
	   (let* ((offset (list-ref instr 3))
		  (base (cadddr offset)))
	     (values 'MEMORY
		     base
		     (list-ref instr 2)
		     base
		     (cadr offset))))

	  ((memq opcode '(LDI LDIL))
	   ;; immed target
	   (values 'ALU
		   (list-ref instr 3)
		   false
		   false
		   false))
	  ((memq opcode '(ADDIL))
	   ;; immed source
	   (values 'ALU
		   regnum:addil-result
		   (list-ref instr 3)
		   false
		   false))
	  ((memq opcode '(NOP))
	   (values 'ALU false false false false))
	  ((memq opcode '(VDEPI DEPI ZVDEPI ZDEPI))
	   (values 'ALU
		   (car (last-pair instr))
		   false
		   false
		   false))
	  ((memq opcode '(EXTRU EXTRS DEP ZDEP))
	   (values 'ALU
		   (list-ref instr 5)
		   (list-ref instr 2)
		   false
		   false))
	  ((memq opcode '(VEXTRU VEXTRS VDEP ZVDEP))
	   (values 'ALU
		   (list-ref instr 4)
		   (list-ref instr 2)
		   false
		   false))
	  ((memq opcode '(FCPY FABS FSQRT FRND))
	   ;; source target
	   (values 'FALU
		   (float-reg (list-ref instr 3))
		   (float-reg (list-ref instr 2))
		   false
		   false))
	  ((memq opcode '(FADD FSUB FMPY FDIV FREM))
	   ;; source1 source2 target
	   (values 'FALU
		   (float-reg (list-ref instr 4))
		   (float-reg (list-ref instr 2))
		   (float-reg (list-ref instr 3))
		   false))
	  ((eq? opcode 'FSTDS)
	   ;; source (offset n m base)
	   (let* ((offset (list-ref instr 3))
		  (base (cadddr offset)))
	     (values 'MEMORY
		     (and (or (memq 'MA (cadr instr))
			      (memq 'MB (cadr instr)))
			  base)
		     base
		     (float-reg (list-ref instr 2))
		     (cadr offset))))

	  #|
	  ((memq opcode '(B BL GATE))
	   <>)
	  ((memq opcode '(BV BLR))
	   ;; source-1 source-2
	   (values 'CONTROL
		   false
		   (list-ref instr 2)
		   (list-ref instr 3)
		   false))
	  ((memq opcode '(BLR))
	   ;; source target
	   (values 'CONTROL
		   (list-ref instr 3)
		   (list-ref instr 2)
		   false
		   false))
	  ((memq opcode '(BV))
	   ;; source-1 source-2
	   (values 'CONTROL
		   false
		   (list-ref instr 2)
		   (list-ref instr 3)
		   false))
	  ((memq opcode '(BE))
	   <>)
	  ((memq opcode '(BLE))
	   <>)
	  ((memq opcode '(COMB ...))
	   <>)
	  ((memq opcode '(PCR-HOOK))
	   <>)
	  ((memq opcode '(LABEL EQUATE ENTRY-POINT
				EXTERNAL-LABEL BLOCK-OFFSET
				SCHEME-OBJECT SCHEME-EVALUATION PADDING))
	   (values 'DIRECTIVE false false false false))
	  |#
	  (else
	   (values 'UNKNOWN false false false false)))))

(define (offset-fits? offset opcode)
  (and (number? offset)
       (memq opcode '(LDW LDB LDO LDH STW STB STH STWM LDWM
			  STWS LDWS FLDWS FLDDS FSTWS FSTDS))
       (<= -8192 offset 8191)))

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

(define (pc-sensitive? instr)
  (or (eq? instr '*PC*)
      (and (pair? instr)
	   (or (pc-sensitive? (car instr))
	       (pc-sensitive? (cdr instr))))))

(define (skips? instr)
  ;; Not really true, for example
  ;; (COMBT (<) ...)
  (and (pair? (cadr instr))
       (not (memq (car instr)
		  '(B BL BV BLR BLE BE
		      LDWS LDHS LDBS LDCWS
		      STWS STHS STBS STBYS
		      FLDWS FLDDS FSTWS FSTDS)))
       ;; or SGL, or QUAD, but not used now.
       (not (memq 'DBL (cadr instr)))))

(define (find-or-label instrs)
  (and (not (null? instrs))
       (if (memq (caar instrs)
		 '(COMMENT SCHEME-OBJECT SCHEME-EVALUATION EQUATE))
	   (find-or-label (cdr instrs))
	   instrs)))

(define (find-non-label instrs)
  (and (not (null? instrs))
       (if (memq (caar instrs)
		 '(LABEL COMMENT SCHEME-OBJECT SCHEME-EVALUATION EQUATE))
	   (find-non-label (cdr instrs))
	   instrs)))

(define (list-difference whole suffix)
  (if (eq? whole suffix)
      '()
      (cons (car whole)
	    (list-difference (cdr whole) suffix))))

(define (fix-complex-return ret frame junk instr avoid)
  (let ((syll `(OFFSET ,frame 0 ,regnum:stack-pointer)))
    (if (and (eq? (car instr) 'STW)
	     (equal? (cadddr instr) syll))
	;; About to store return address.  Forego store completely
	(let ((ret (caddr instr)))
	  `(,@(reverse junk)
	    (DEP () ,regnum:quad-bitmask
		 ,(-1+ scheme-type-width)
		 ,scheme-type-width
		 ,ret)
	    (BV () 0 ,ret)
	    (LDO () (OFFSET ,(+ frame 4) 0 ,regnum:stack-pointer)
		 ,regnum:stack-pointer)))
	(let ((ret (list-search-positive
		       (list ret regnum:first-arg regnum:second-arg
			     regnum:third-arg regnum:fourth-arg)
		     (lambda (reg)
		       (not (memq reg avoid))))))
	  `(,@(reverse junk)
	    (LDW () ,syll ,ret)
	    ,instr
	    (DEP () ,regnum:quad-bitmask
		 ,(-1+ scheme-type-width)
		 ,scheme-type-width
		 ,ret)
	    (BV () 0 ,ret)
	    (LDO () (OFFSET ,(+ frame 4) 0 ,regnum:stack-pointer)
		 ,regnum:stack-pointer))))))

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
	       (pc-sensitive? (car next))
	       (memq (caar next)
		     '(ENTRY-POINT EXTERNAL-LABEL BLOCK-OFFSET PCR-HOOK))
	       (and (eq? (caar next) 'LABEL)
		    (or (not next*)
			(not (skips? (car next*))))))
	   (values (fix-simple-return ret frame junk)
		   rest))
	  ((or (eq? (caar next) 'LABEL)
	       (skips? (car next)))
	   (values '() false))
	  (else
	   (with-values
	       (lambda () (classify-instruction (car next)))
	     (lambda (type target src1 src2 offset)
	       offset			; ignored
	       (if (or (not (memq type '(MEMORY ALU FALU)))
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
  (define-integrable (fail)
    (fix-sequences (cdr instrs)
		   (cons (car instrs) tail)))

  (if (null? instrs)
      tail
      (let* ((instr (car instrs))
	     (opcode (car instr)))
	(case opcode
	  ((BV)
	   (let ((dict1 (match (cdr return-pattern) instrs)))
	     (if (not dict1)
		 (fail)
		 (let* ((tail* (cdddr instrs))
			(next (find-or-label tail*))
			(fail*
			 (lambda ()
			   (fix-sequences
			    tail*
			    (append (reverse (list-head instrs 3))
				    tail))))
			(dict2
			 (and next
			      (match (car return-pattern) (car next)))))
			     
		   (if (not dict2)
		       (fail*)
		       (with-values
			   (lambda ()
			     (fix-a-return dict1
					   (list-difference tail* next)
					   dict2
					   (cdr next)))
			 (lambda (frobbed untouched)
			   (if (null? frobbed)
			       (fail*)
			       (fix-sequences untouched
					      (append frobbed tail))))))))))
	  ((B BE BLE)
	   (let ((completer (cadr instr)))
	     (if (or (not (pair? completer))
		     (not (eq? 'N (car completer)))
		     (not (null? (cdr completer))))
		 (fail)
		 (with-values (lambda () (find-movable-instr (cdr instrs)))
		   (lambda (movable junk rest)
		     (if (not movable)
			 (fail)
			 (fix-sequences
			  rest
			  `(,@(reverse junk)
			    (,opcode () ,@(cddr instr))
			    ,movable
			    ,@tail))))))))

	  ((NOP)
	   (let ((dict (match hook-pattern instrs)))
	     (if (not dict)
		 (fail)
		 (with-values (lambda () (find-movable-instr (cddr instrs)))
		   (lambda (movable junk rest)
		     (if (not movable)
			 (fail)
			 (fix-sequences
			  rest
			  `(,@(reverse junk)
			    ,(cadr instrs)
			    ,movable
			    ,@tail))))))))
	  (else
	   (fail))))))

(define (find-movable-instr instrs)
  (let* ((next (find-or-label instrs))
	 (instr (and next (car next)))
	 (next* (and next (find-non-label (cdr next)))))
    (if (and instr
	     (with-values (lambda () (classify-instruction instr))
	       (lambda (type tgt src1 src2 offset)
		 tgt src1 src2		; ignored
		 (or (memq type '(ALU FALU))
		     (and (eq? type 'MEMORY)
			  (offset-fits? offset (car instr))))))
	     (not (skips? instr))
	     (not (pc-sensitive? instr))
	     (or (not next*)
		 (not (skips? (car next*)))))
	(values instr
		(list-difference instrs next)
		(cdr next))
	(values false false false))))

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

(define hook-pattern
  `((NOP ())
    (BLE () (OFFSET (? hook) 4 ,regnum:scheme-to-interface-ble))
    . (? more-insts)))

(define (optimize-linear-lap instructions)
  (fix-sequences (reverse! instructions) '()))