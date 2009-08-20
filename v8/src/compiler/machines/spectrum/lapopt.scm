#| -*-Scheme-*-

$Id$

Copyright (c) 1991-1999 Massachusetts Institute of Technology

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

;;;; LAP Optimizer for HP Precision Archtecture.
;; package: (compiler lap-optimizer)

(declare (usual-integrations))

;;;; An instruction classifier and decomposer

(define-integrable (float-reg reg)
  (+ 32 reg))

(define (classify-instruction instr)
  ;; (values type writes reads offset)
  ;; The types are ALU, MEMORY, FALU (floating ALU), CONTROL
  (let ((opcode (car instr)))
    (case opcode
      ((ANDCM AND OR XOR UXOR SUB DS SUBT
	      SUBB ADD SH1ADD SH2ADD SH3ADD ADDC
	      COMCLR UADDCM UADDCMT ADDL SH1ADDL
	      SH2ADDL SH3ADDL SUBO SUBTO SUBBO
	      ADDO SH1ADDO SH2ADDO SH3ADDO ADDCO
	      VSHD SHD)
       ;; operator conditions source source ... target
       (values 'ALU
	       ;; not (list-ref instr 4)
	       (list (car (last-pair instr))) ; Skip the "..."
	       (list (list-ref instr 2) (list-ref instr 3))
	       false))
      ((ADDI ADDIO ADDIT ADDITO SUBI SUBIO COMICLR)
       ;; operator conditions immed source target
       (values 'ALU
	       (list (list-ref instr 4))
	       (list (list-ref instr 3))
	       false))
      ((COPY)
       ;; operator conditions source target
       (values 'ALU
	       (list (list-ref instr 3))
	       (list (list-ref instr 2))
	       false))

      ((LDW LDB LDO LDH)
       ;; operator completer (offset bytes space source) target
       ;;   the completer isn't actually used!
       (let ((offset (list-ref instr 2)))
	 (values (if (eq? opcode 'LDO)
		     'ALU
		     'MEMORY)
		 (list (list-ref instr 3))
		 (list (cadddr offset))
		 (cadr offset))))
      ((LDWM)
       ;; operator completer (offset bytes space target/source) target
       ;;   Notice that this writes BOTH registers: one from memory
       ;;   contents, the other by adding the offset to the register
       (let* ((offset (list-ref instr 2))
	      (base (cadddr offset)))
	 (values 'MEMORY
		 (list base (list-ref instr 3))
		 (list base)
		 (cadr offset))))
      ((LDWS LDHS LDBS LDWAS LDCWS)
       ;; operator completer (offset bytes space target/source) target
       (let* ((completer (cadr instr))
	      (offset (list-ref instr 2))
	      (base (cadddr offset)))
	 (values 'MEMORY
		 (cons (list-ref instr 3)
		       (if (or (memq 'MA completer)
			       (memq 'MB completer))
			   (list base)
			   '()))
		 (list base)
		 (cadr offset))))

      ((LDWX LDHX LDBX LDWAX LDCWX)
       ;; operator completer (INDEX source1 m source2/target) target
       (let* ((completer (cadr instr))
	      (index (list-ref instr 2))
	      (base (cadddr index)))
	 (values 'MEMORY
		 (cons (list-ref instr 3)
		       (if (or (memq 'M completer)
			       (memq 'SM completer))
			   (list base)
			   '()))
		 (list (cadr index) base)
		 false)))
      ((STW STB STH)
       ;; operator completer source1 (offset bytes space source2)
       (let ((offset (list-ref instr 3)))
	 (values 'MEMORY
		 '()
		 (list (list-ref instr 2) (cadddr offset))
		 (cadr offset))))
      ((STWM)
       ;; operator completer source1 (offset n m target/source)
       (let* ((offset (list-ref instr 3))
	      (base (cadddr offset)))
	 (values 'MEMORY
		 (list base)
		 (list (list-ref instr 2) base)
		 (cadr offset))))
      ((STWS STHS STBS STWAS)
       ;; operator completer source1 (offset n m target/source)
       (let* ((offset (list-ref instr 3))
	      (base (cadddr offset)))
	 (values 'MEMORY
		 (if (or (memq 'MA (cadr instr))
			 (memq 'MB (cadr instr)))
		     (list base)
		     '())
		 (list base (list-ref instr 2))
		 (cadr offset))))
      ((LDI LDIL)
       ;; immed target
       (values 'ALU
	       (list (list-ref instr 3))
	       '()
	       (list-ref instr 2)))
      ((ADDIL)
       ;; immed source
       (values 'ALU
	       (list regnum:addil-result)
	       (list (list-ref instr 3))
	       (list-ref instr 2)))
      ((NOP SKIP)
       (values 'ALU '() '() false))
      ((VDEPI DEPI)
       (values 'ALU
	       (list (car (last-pair instr)))
	       (list (car (last-pair instr)))
	       false))
      ((ZVDEPI ZDEPI)
       (values 'ALU
	       (list (car (last-pair instr)))
	       '()
	       false))
      ((EXTRU EXTRS ZDEP)
       (values 'ALU
	       (list (list-ref instr 5))
	       (list (list-ref instr 2))
	       false))

      ((DEP)
       (values 'ALU
	       (list (list-ref instr 5))
	       (list (list-ref instr 5) (list-ref instr 2))
	       false))
      ((VEXTRU VEXTRS VDEP ZVDEP)
       (values 'ALU
	       (list (list-ref instr 4))
	       (list (list-ref instr 2))
	       false))
      ((FCPY FABS FSQRT FRND)
       ;; source target
       (values 'FALU
	       (list (float-reg (list-ref instr 3)))
	       (list (float-reg (list-ref instr 2)))
	       false))
      ((FADD FSUB FMPY FDIV FREM)
       ;; source1 source2 target
       (values 'FALU
	       (list (float-reg (list-ref instr 4)))
	       (list (float-reg (list-ref instr 2))
		     (float-reg (list-ref instr 3)))
	       false))
      ((FSTDS)
       ;; source (offset n m base)
       (let* ((offset (list-ref instr 3))
	      (base (cadddr offset)))
	 (values 'MEMORY
		 (if (or (memq 'MA (cadr instr))
			 (memq 'MB (cadr instr)))
		     (list base)
		     '())
		 (list base
		       (float-reg (list-ref instr 2)))
		 (cadr offset))))
      ((COMBT COMBF COMB COMBN)
       ;; source1 source2
       (values 'CONTROL
	       '()
	       (list (list-ref instr 2) (list-ref instr 3))
	       false))
      ((COMIBT COMIBF COMIB COMIBTN COMIBFN)
       ;; immediate source
       (values 'CONTROL
	       '()
	       (list (list-ref instr 3))
	       false))
      ((BL)
       ;; target
       (values 'CONTROL
	       (list (list-ref instr 2))
	       '()
	       false))
      ((B)
       ;; target
       (values 'CONTROL
	       '()
	       '()
	       false))
      ((BV)
       ;; source-1 source-2
       (values 'CONTROL
	       '()
	       (list (list-ref instr 2) (list-ref instr 3))
	       false))

      ((BLR)
       ;; source target
       (values 'CONTROL
	       (list (list-ref instr 3))
	       (list (list-ref instr 2))
	       false))
      ((BLE)
       (let ((offset-expr (list-ref instr 2)))
	 (values 'CONTROL
		 (list 31)
		 (list (list-ref offset-expr 3))
		 (list-ref offset-expr 1))))
      ((BE)
       (let ((offset-expr (list-ref instr 2)))
	 (values 'CONTROL
		 '()
		 (list (list-ref offset-expr 3))
		 (list-ref offset-expr 1))))
      #|
      ((ADDBT ADDBF ADDB)
       ;; source1 source2/target
       (let ((target (list-ref instr 3)))
	 (values 'CONTROL
		 (list target)
		 (list (list-ref instr 2) target)
		 false)))
      ((ADDIBT ADDIBF ADDIB)
       ;; immediate source/target
       (let ((target (list-ref instr 3)))
	 (values 'CONTROL
		 (list target)
		 (list target)
		 false)))
      ((GATE)
       <>)
      ((MOVB ...)
       <>)
      ((PCR-HOOK)
       <>)
      ((LABEL EQUATE ENTRY-POINT
	      EXTERNAL-LABEL BLOCK-OFFSET
	      SCHEME-OBJECT SCHEME-EVALUATION PADDING)
       (values 'DIRECTIVE '() '() false))
      |#
      (else
       (values 'UNKNOWN '() '() false)))))

(define (offset-fits? offset opcode)
  (and (number? offset)
       (or (and	;; These opcodes require a small offset otherwise they are
                ;; assembled as two instructions.
	        (memq opcode '(LDW LDB LDO LDI LDH STW STB STH STWM LDWM
			       STWS LDWS FLDWS FLDDS FSTWS FSTDS))
		(<= -8192 offset 8191))
	   (eq? opcode 'LDIL))))

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

(define (directive? instr)
  (memq (car instr)
	'(COMMENT
	  LABEL EQUATE ENTRY-POINT
	  EXTERNAL-LABEL BLOCK-OFFSET
	  SCHEME-OBJECT SCHEME-EVALUATION PADDING)))

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
	;; FORMAT: (STW () ret (OFFSET frame 0 regnum:stack-pointer))
	(let ((ret (caddr instr)))
	  `(,@(reverse junk)
	    ,@(entry->address ret)
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
	    ,@(entry->address ret)
	    (BV () 0 ,ret)
	    (LDO () (OFFSET ,(+ frame 4) 0 ,regnum:stack-pointer)
		 ,regnum:stack-pointer))))))

(define (fix-simple-return ret frame junk)
  ;; JSM: Why can't the LDO be in the delay slot of the BV?
  `(,@(reverse junk)
    (LDW () (OFFSET ,frame 0 ,regnum:stack-pointer) ,ret)
    (LDO () (OFFSET ,(+ frame 4) 0 ,regnum:stack-pointer)
	 ,regnum:stack-pointer)
    ,@(entry->address ret)
    (BV (N) 0 ,ret)))

(define (fix-a-return dict1 junk dict2 rest)
  (let* ((next (find-or-label rest))
	 (next* (and next (find-non-label next)))
	 (frame (cdr (assq 'frame dict2)))
	 (ret (cdr (assq 'ret dict1))))
    (cond ((or (not next)
	       (instr-pc-sensitive? (car next))
	       (memq (caar next)
		     '(ENTRY-POINT EXTERNAL-LABEL BLOCK-OFFSET PCR-HOOK))
	       (and (eq? (caar next) 'LABEL)
		    (or (not next*)
			(not (instr-skips? (car next*))))))
	   (values (fix-simple-return ret frame junk)
		   rest))
	  ((or (eq? (caar next) 'LABEL)
	       (instr-skips? (car next)))
	   (values '() false))
	  (else
	   (call-with-values
	    (lambda () (classify-instruction (car next)))
	    (lambda (type writes reads offset)
	      offset			; ignored
	      (if (or (not (memq type '(ALU MEMORY FALU)))
		      (equal? writes (list regnum:stack-pointer)))
		  (values (fix-simple-return ret frame junk)
			  rest)
		  (values
		   (fix-complex-return ret frame
				       (append junk
					       (list-difference rest next))
				       (car next)
				       (append writes reads))
		   (cdr next)))))))))

(define (fix-sequences instrs tail)
  (define-integrable (single instr)
    (fix-sequences (cdr instrs)
		   (cons instr tail)))

  (define-integrable (fail)
    (single (car instrs)))

  (if (null? instrs)
      tail
      (let* ((instr (car instrs))
	     (opcode (car instr)))

	(define (try-skip)
	  (let ((label (let ((address (list-ref instr 4)))
			 (and (eq? (car address) '@PCR)
			      (cadr address)))))
	    (if (not label)
		(fail)
		(let* ((next (find-non-label tail))
		       (instr* (and next
				    (not (directive? (car next)))
				    (car next)))
		       (next* (and instr* (find-or-label (cdr next))))
		       (instr** (and next* (car next*))))
		  (if (or (not instr**)
			  (not (eq? (car instr**) 'LABEL))
			  (not (eq? (cadr instr**) label))
			  (instr-expands? instr*))
		      (fail)
		      (case opcode
			 ((COMB COMBT COMBN)
			  (single
			   `(COMCLR ,(delq 'N (cadr instr))
				    ,(caddr instr)
				    ,(cadddr instr)
				    0)))
			 ((COMIB COMIBT COMIBTN)
			  (single
			   `(COMICLR ,(delq 'N (cadr instr))
				     ,(caddr instr)
				     ,(cadddr instr)
				     0)))
			 ((COMBF)
			  (single
			   `(COMCLR ,(map invert-condition
					  (delq 'N (cadr instr)))
				    ,(caddr instr)
				    ,(cadddr instr)
				    0)))
			 ((COMIBF COMIBFN)
			  (single
			   `(COMICLR ,(map invert-condition
					  (delq 'N (cadr instr)))
				    ,(caddr instr)
				    ,(cadddr instr)
				    0)))
			 (else "LAPOPT: try-skip bad case" instr)))))))

	(define (fix-unconditional-branch)
	  (if (not (equal? (cadr instr) '(N)))
	      (fail)
	      (call-with-values
	       (lambda ()
		 (find-movable-instr/delay instr (cdr instrs)))
	       (lambda (movable junk rest)
		 (if (not movable)
		     (fail)
		     (fix-sequences
		      rest
		      `(,@(reverse junk)
			(,opcode () ,@(cddr instr))
			,movable
			,@tail)))))))

	(define (drop-instr)
	  (fix-sequences (cdr instrs)
			 (cons '(COMMENT (branch removed))
			       tail)))

	(define (generate-skip)
	  (let* ((default (lambda () (single `(SKIP (TR)))))
		 (previous (find-or-label (cdr instrs)))
		 (skipify
		  (lambda (instr*)
		    (fix-sequences
		     (cdr previous)
		     (cons instr*
			   (append
			    (reverse (list-difference (cdr instrs) previous))
			    tail)))))
		 (instr (and previous (car previous)))
		 (previous* (and previous (find-non-label (cdr previous)))))
	    (if (or (not instr)
		    (not (null? (cadr instr)))
		    (directive? instr)
		    (and previous*
			 (instr-skips? (car previous*))))
		(default)
		(call-with-values
		 (lambda ()
		   (classify-instruction instr))
		 (lambda (type writes reads offset)
		   (cond ((or (not (eq? type 'ALU))
			      (memq (car instr) '(LDIL ADDIL)))
			  (default))
			 ((not (memq (car instr) '(LDO LDI)))
			  (skipify
			   `(,(car instr) (TR) ,@(cddr instr))))
			 ((not (fits-in-11-bits-signed? offset))
			  (default))
			 (else
			  (skipify
			   `(ADDI (TR)
				  ,offset
				  ,(if (null? reads)
				       0
				       (car reads))
				  ,(car writes))))))))))

	(case opcode
	  ((BV)
	   (let ((dict1 (match (cdr return-pattern) instrs)))
	     (if (not dict1)
		 (fix-unconditional-branch)
		 (let* ((tail* (cddr instrs))
			(next (find-or-label tail*))
			(fail*
			 (lambda ()
			   (fix-sequences
			    tail*
			    (append (reverse (list-head instrs 2))
				    tail))))
			(dict2
			 (and next
			      (match (car return-pattern) (car next)))))
			     
		   (if (not dict2)
		       (fail*)
		       (call-with-values
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

	  ((B)
	   (let ((address (caddr instr)))
	     (if (not (eq? (car address) '@PCR))
		 (fix-unconditional-branch)
		 (let ((label (cadr address)))
		   (if (equal? (cadr instr) '(N))
		       ;; Branch with nullification
		       (let* ((next (find-or-label tail))
			      (instr* (and next (car next))))
			  (cond ((not instr*)
				 (fix-unconditional-branch))
				((eq? (car instr*) 'LABEL)
				 (if (not (eq? (cadr instr*) label))
				     (fix-unconditional-branch)
				     (drop-instr)))
				((eq? (car instr*) 'EXTERNAL-LABEL)
				 (let ((address* (list-ref instr* 3)))
				   (if (or (not (eq? (car address*) '@PCR))
					   (not (eq? label (cadr address*))))
				       (fix-unconditional-branch)
				       (generate-skip))))
				(else
				 (fix-unconditional-branch))))
		       ;; Branch with no nullification
		       (let* ((next (find-non-label tail))
			      (instr* (and next (car next)))
			      (next* (and next (find-or-label (cdr next))))
			      (instr** (and next* (car next*))))
			 (cond ((not instr**)
				(fix-unconditional-branch))
			       ((and (eq? (car instr**) 'LABEL)
				     (eq? (cadr instr**) label)
				     (not (instr-expands? instr*)))
				(drop-instr))
			       (else
				(fix-unconditional-branch)))))))))

	  ((BE)
	   (fix-unconditional-branch))
	  ((BLE)
	   (if (and (equal? (second instr) '(N))
		    (eqv? (second (third instr)) hook:compiler-profile-count))
	       ;; (BLE (N) (OFFSET profile 4 3)) has a data word following it
	       (fail)
	       (fix-unconditional-branch)))
	  ((NOP)
	   (let ((dict (match hook-pattern instrs)))
	     (if (not dict)
		 (fail)
		 (call-with-values
		  (lambda ()
		    (find-movable-instr/delay (cadr instrs) ; The BLE
					      (cddr instrs)))
		  (lambda (movable junk rest)
		    (if (not movable)
			(fail)
			(fix-sequences
			 rest
			 `(,@(reverse junk)
			   ,(cadr instrs)
			   ,movable
			   ,@tail))))))))
	  ((LDW LDB LDH)
	   #|
	   ;; yyy
	   ;; LD[WB] ... Rx
	   ;; use Rx
	   ;; =>
	   ;; LD[WB] ... Rx
	   ;; yyy
	   ;; use Rx
	   |#
	   (let* ((writes (fourth instr))
		  (next (find-non-label tail)))
	     (if (or (not next)
		     (not (instr-uses? (car next) writes)))
		 (fail)
		 (call-with-values
		  (lambda ()
		    (find-movable-instr/load (cdr instrs)
					     (list (fourth (third instr)))
					     (list writes)
					     (car next)))
		  (lambda (movable junk rest)
		    (if (not movable)
			(fix-sequences
			 (cdr instrs)
			 (cons* instr '(COMMENT *load-stall*) tail))
			(fix-sequences
			 rest
			 `(,@(reverse junk)
			   (COMMENT (moved for load scheduling))
			   ,instr
			   ,movable
			   ,@tail))))))))

	  #|
	  (else
	   (cond (;; Load scheduling
		  ;;    xxx
		  ;;    LD[WB] ... Rx
		  ;;    use Rx
		  ;;   =>
		  ;;    LD[WB] ... Rx
		  ;;    xxx
		  ;;    use Rx
		  (and (pair? (cdr instrs))
		       ;; `use Rx' is not, say, a comment
		       (not (directive? instr))
		       (eq? instrs (find-or-label instrs))
		       (memq (caar (find-or-label (cdr instrs))) '(LDW LDB))
		       (instr-uses?
			instr
			(fourth (car (find-or-label (cdr instrs))))))
		  (call-with-values
		      (lambda ()
			(find-movable-instr-for-load-slot
			 (cdr (find-or-label (cdr instrs)))))
		    (lambda (movable junk rest)
		      (if (or (not movable)
			      (memq (car movable) '(LDWM STWM)))
			  ;; This annotates them, otherwise eqv to (fail):
			  (fix-sequences (cdr instrs)
					 (cons* '(COMMENT *load-stall*)
						(car instrs) tail))
			  (fix-sequences
			   rest
			   `(,@(reverse junk)
			     ,(car (find-or-label (cdr instrs)))
			     (COMMENT (moved for load scheduling))
			     ,movable
			     ,(car instrs)
			     ,@tail))))))
		 (else
		  (fail))))
	  |#
	  ((COMB COMBT COMBF COMIB COMIBT COMIBF)
	   (if (not (memq 'N (cadr instr)))
	       (fail)
	       (try-skip)))
	  ((COMBN COMIBTN COMIBFN)
	   (try-skip))
	  (else
	   (fail))))))

(define (fits-in-11-bits-signed? value)
  (and (number? value)			; i.e. not a symbolic expressions
       (< value 1024)
       (>= value -1024)))

(define (instr-skips? instr)
  ;; Not really true, for example
  ;; (COMBT (<) ...)
  (or (and (pair? (cadr instr))
	   (not (memq (car instr)
		      '(B BL BV BLR BLE BE
			  LDWS LDHS LDBS LDCWS
			  STWS STHS STBS STBYS
			  FLDWS FLDDS FSTWS FSTDS
			  COMBN COMIBTN COMIBFN)))
	   ;; or SGL, or QUAD, but not used now.
	   (not (memq 'DBL (cadr instr))))

      ;; A jump with a non-nullified delay slot
      (and (memq (car instr) '(B BL BV BLR BLE BE))
	   (null? (cadr instr)))))

(define (instr-uses? instr reg)
  ;; Might INSTR have a data dependency on REG?
  (call-with-values
   (lambda () (classify-instruction instr))
   (lambda (type writes reads offset)
     writes offset			; ignored
     (or (eq? type 'UNKNOWN)
	 (eq? type 'DIRECTIVE)
	 (memq reg reads)))))

(define (instr-expands? instr)
  (call-with-values
   (lambda () (classify-instruction instr))
   (lambda (type writes reads offset)
     writes reads			; ignored
     (or (eq? type 'UNKNOWN)
	 (eq? type 'DIRECTIVE)
	 (cond (offset
		(not (offset-fits? offset (car instr))))
	       ((eq? type 'CONTROL)
		(instr-pc-sensitive? instr))
	       (else
		false))))))

(define (instr-pc-sensitive? instr)
  (let walk ((instr instr))
    (or (memq instr '(*PC* @PCR))
	(and (pair? instr)
	     (or (walk (car instr))
		 (walk (cdr instr)))))))

(define (find-movable-instr/delay instr instrs)
  (let* ((next   (find-or-label instrs))
	 (instr* (and next (car next)))
	 (next*  (and next (find-non-label (cdr next)))))
    (if (and instr*
	     (call-with-values
	      (lambda () (classify-instruction instr*))
	      (lambda (type writes reads offset)
		(and (memq type '(ALU MEMORY FALU))
		     (or (not offset)
			 (offset-fits? offset (car instr*)))
		     (call-with-values
		      (lambda () (classify-instruction instr))
		      (lambda (type* writes* reads* offset*)
			type* offset*	; ignored
			;;(pp `((,instr* writes ,writes reads ,reads)
			;;      (,instr writes* ,writes* reads* ,reads*)))
			(and (null? (eq-set-intersection writes reads*))
			     (null? (eq-set-intersection reads writes*))))))))
	     (not (instr-skips? instr*))
	     (not (instr-pc-sensitive? instr*))
	     (or (not next*)
		 (not (instr-skips? (car next*)))))
	(values instr*
		(list-difference instrs next)
		(cdr next))
	(values false false false))))

;; Certainly dont try (equal? instr recache-memtop) in above as it causes the
;; branch for which we are seeking an instruction to fill its delay slot to
;; be put in the delay slot of the COMB instruction.

#|
(define (find-movable-instr-for-load-slot instrs)
  ;; This needs to be taught about dependencies between instructiions.
  ;; Currently it will only reschedule the recaching of memtop as that has no
  ;; dependencies at all.
  (let* ((next (find-or-label instrs))
	 (instr (and next (car next))))
    (if (or (equal? instr recache-memtop)
	    #F)
	(values instr
		(list-difference instrs next)
		(cdr next))
	(values false false false))))
|#

(define (find-movable-instr/load instrs reads writes next**)
  (let* ((next   (find-or-label instrs))
	 (instr  (and next (car next)))
	 (next*  (and next (find-non-label (cdr next)))))
    (if (and instr
	     (not (instr-skips? instr))
	     (call-with-values
	      (lambda () (classify-instruction instr))
	      (lambda (type writes* reads* offset)
		offset			; ignored
		(and (memq type '(ALU MEMORY FALU))
		     (null? (eq-set-intersection writes* reads))
		     (null? (eq-set-intersection writes reads*))
		     (or (null? writes*)
			 (not (there-exists? writes*
				(lambda (tgt)
				  (instr-uses? next** tgt))))))))
	     (or (not (memq (car instr)
			    '(STW STB STH STWM STWS STHS STBS STWAS)))
		 ;; Don't move a memory store instruction past
		 ;; a load.  There are cases where this is OK,
		 ;; but we're not going to handle them now. -- JSM
		 (begin
		   ;;(write-line (list 'FIND-MOVABLE-INSTR/LOAD instr))
		   #F))
	     (or (not next*)
		 (not (instr-skips? (car next*)))
		 (equal? instr recache-memtop)))
	(values instr
		(list-difference instrs next)
		(cdr next))
	(values false false false))))

(define return-pattern			; reversed
  (cons
   `(LDO () (OFFSET (? frame) 0 ,regnum:stack-pointer) ,regnum:stack-pointer)
   `((BV (N) 0 (? ret))
     (LDWM () (OFFSET 4 0 ,regnum:stack-pointer) (? ret))
     . (? more-insts))))

(define hook-pattern
  `((NOP ())
    (BLE () (OFFSET (? hook) 4 ,regnum:scheme-to-interface-ble))
    . (? more-insts)))

(define recache-memtop '(LDW () (OFFSET 0 0 4) #x14))

(define (old-optimize-linear-lap instructions)
  (fix-sequences (reverse! instructions) '()))

#|
** I believe that I have fixed this - there are cdd..drs and list
   indexes in the code that assume that the return pattern has a
   certain length.

;; At the moment the code removes the assignment to r2 in the following:

((entry-point fixmul-5)
 (scheme-object CONSTANT-0 debugging-info)
 (scheme-object CONSTANT-1 environment)
 (comment (rtl (procedure-header fixmul-0 3 3)))
 (equate fixmul-5 fixmul-0)
 (label label-4)
 (ble () (offset 0 4 3))
 (ldi () 26 28)
 (external-label () 771 (@pcr fixmul-0))
 (label fixmul-0)
 (comb (>=) 21 20 (@pcr label-4))
 (ldw () (offset 0 0 4) 20)
 (comment
  (rtl (assign (register 65) (offset (register 22) (machine-constant 0)))))
 (ldw () (offset 0 0 22) 6)
 (comment
  (rtl (assign (register 66) (offset (register 22) (machine-constant 1)))))
 (ldw () (offset 4 0 22) 7)
 (comment
  (rtl
   (assign
    (register 2)
    (fixnum-2-args multiply-fixnum (register 65) (register 66) #f))))
 (copy () 6 26)
 (copy () 7 25)
 (ble () (offset 116 4 3))
 (nop ())
 (comment
  (rtl
   (assign
    (register 22)
    (offset-address (register 22) (machine-constant 2)))))
 (ldo () (offset 8 0 22) 22)
 (comment (rtl (pop-return)))
 (copy () 26 2)
 (ldwm () (offset 4 0 22) 6)
 (bv (n) 0 6))
|#
(define (optimize-linear-lap instructions)
  (old-optimize-linear-lap instructions))

;;;; This works in conjuction with try-skip in fix-sequences.

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