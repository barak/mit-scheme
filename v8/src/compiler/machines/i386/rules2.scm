#| -*-Scheme-*-

$Id$

Copyright (c) 1992-1999 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Predicates
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define (set-equal-branches!)
  (set-current-branches! (lambda (label)
			   (LAP (JE (@PCR ,label))))
			 (lambda (label)
			   (LAP (JNE (@PCR ,label))))))

(define (set-specific-branches! truejump falsejump)
  (set-current-branches! (lambda (label)
			   (LAP (,truejump (@PCR ,label))))
			 (lambda (label)
			   (LAP (,falsejump (@PCR ,label))))))

(define-rule predicate
  (TYPE-TEST (REGISTER (? register)) (? type))
  (set-equal-branches!)
  (LAP (CMP B ,(reference-alias-register! register 'GENERAL) (&U ,type))))

(define-rule predicate
  ;; Branch if virtual register contains the specified type number
  (TYPE-TEST (OBJECT->TYPE (REGISTER (? register))) 0)
  (set-specific-branches! 'JB 'JAE)
  (LAP (CMP W ,(reference-alias-register! register 'GENERAL) (&U ,signed-fixnum/upper-limit))))


(define-rule predicate
  (EQ-TEST (REGISTER (? register-1)) (REGISTER (? register-2)))
  (set-equal-branches!)
  (compare/register*register register-1 register-2))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (? expression rtl:simple-offset?))
  (set-equal-branches!)
  (LAP (CMP W ,(source-register-reference register)
	    ,(offset->reference! expression))))



(define-rule predicate
  (PRED-1-ARG GENERIC-ADDITIVE-TEST (REGISTER (? source)))
  (let ((temp (allocate-temporary-register! 'GENERAL))
	(src (standard-source! source))
	(osize (if (> scheme-datum-width 7)
		   'W
		   'B)))
    (set-equal-branches!)
    (LAP (LEA (R ,temp) (@RO ,osize ,src ,(expt 2 (-1+ scheme-datum-width))))
	 (SHR W (R ,temp) ,scheme-datum-width))))

(define-rule predicate
  (PRED-1-ARG FIXNUM? (REGISTER (? source)))
  (let ((temp (allocate-temporary-register! 'GENERAL))
	(src (standard-source! source))
	(osize (if (> scheme-datum-width 6)
		   'W
		   'B)))
    (set-equal-branches!)
    (LAP (LEA (R ,temp) (@RO ,osize ,src ,(expt 2 scheme-datum-width)))
	 (SHR W (R ,temp) (& ,(1+ scheme-datum-width))))))

(define-rule predicate
  (PRED-1-ARG FALSE? (REGISTER (? source)))
  (if compiler:generate-trap-on-null-valued-conditional?
      (error "unsupported compiler option: generate-trap-on-null-valued-conditional?")
      (begin
	(set-equal-branches!)
	(LAP (CMP W (R ,(standard-source! source))
		  (& ,(make-non-pointer-literal (386-object-type #f)
						(386-object-datum #f))))))))

(define-rule predicate
  (PRED-1-ARG NULL? (REGISTER (? source)))
  (set-equal-branches!)
  (LAP (CMP W (R ,(standard-source! source)) ,(get-regblock-ea register-block/empty-list))))

(define-rule predicate
  (PRED-2-ARGS WORD-LESS-THAN-UNSIGNED?
	       (REGISTER (? smaller))
	       (REGISTER (? larger)))
  (set-special-branches! 'JB 'JAE)
  (LAP (CMP W (R ,(standard-source! smaller)) (R ,(standard-source! larger)))))

(define-rule predicate
  (PRED-2-ARGS WORD-LESS-THAN-UNSIGNED?
	       (CONSTANT (? smaller))
	       (REGISTER (? larger)))
  (set-special-branches! 'JB 'JAE)
  (LAP (CMP W (& ,smaller) (R ,(standard-source! larger)))))

(define-rule predicate
  (PRED-2-ARGS WORD-LESS-THAN-UNSIGNED?
	       (REGISTER (? smaller))
	       (CONSTANT (? larger)))
  (set-special-branches! 'JB 'JAE)
  (LAP (CMP W (R ,(standard-source! smaller)) (& ,larger))))

(define-rule predicate
  (PRED-2-ARGS SMALL-FIXNUM?
	       (REGISTER (? source))
	       (MACHINE-CONSTANT (? nbits)))
  (let* ((src (standard-source! source))
	 (temp (allocate-temporary-register! 'GENERAL))
	 (osize (if (> (- scheme-datum-width nbits) 6)
		    'W
		    'B)))
    (set-equal-branches!)
    ;; There are several ways to do this:
    ;; assuming you want to check that the number is 16 bits + sign extension:
 
    ;; lea eax,[ebx+32768]
    ;; shr eax,16
    ;; jz blat
    ;; This is good because it is two instructions and will execute quickly,
    ;; but be careful for stalling because of the addressing mode!
    ;; Also, it is about 6+3=9 bytes (for the arithmetic)
   
    ;; Or:
    ;; mov eax,ebx
    ;; sar eax,16
    ;; adc eax,0
    ;; jz blat
    ;; This is good because it doesn't use [ebx] in addressing, plus it is
    ;; only 2+3+3=8 bytes.  NOTE: We originally thought that you could do
    ;; an ADC AL,0; but realize there are 16 bits you are testing.  Besides,
    ;; that would only gain you a byte, assuming you got the EAX register
    ;; This is also good because it can pull from memory or from a register

    (LAP (LEA (R ,temp) (@RO ,osize ,src ,(expt 2 (- scheme-datum-width nbits))))
	 (SHR W (R ,temp) (& ,(- (+ scheme-datum-width 1) nbits))))))


(define-rule predicate
  (EQ-TEST (? expression rtl:simple-offset?) (REGISTER (? register)))
  (set-equal-branches!)
  (LAP (CMP W ,(offset->reference! expression)
	    ,(source-register-reference register))))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (REGISTER (? register)))
  (QUALIFIER (non-pointer-object? constant))
  (set-equal-branches!)
  (LAP (CMP W ,(any-reference register)
	    (&U ,(non-pointer->literal constant)))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (CONSTANT (? constant)))
  (QUALIFIER (non-pointer-object? constant))
  (set-equal-branches!)
  (LAP (CMP W ,(any-reference register)
	    (&U ,(non-pointer->literal constant)))))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (? expression rtl:simple-offset?))
  (QUALIFIER (non-pointer-object? constant))
  (set-equal-branches!)
  (LAP (CMP W ,(offset->reference! expression)
	    (&U ,(non-pointer->literal constant)))))

(define-rule predicate
  (EQ-TEST (? expression rtl:simple-offset?) (CONSTANT (? constant)))
  (QUALIFIER (non-pointer-object? constant))
  (set-equal-branches!)
  (LAP (CMP W ,(offset->reference! expression)
	    (&U ,(non-pointer->literal constant)))))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant-1)) (CONSTANT (? constant-2)))
  (let ((always-jump
	 (lambda (label)
	   (LAP (JMP (@PCR ,label)))))
	(always-fall-through
	 (lambda (label)
	   label			; ignored
	   (LAP))))
    (if (eq? constant-1 constant-2)
	(set-current-branches! always-jump always-fall-through)
	(set-current-branches! always-fall-through always-jump)))
  (LAP))

(define-rule predicate
  (EQ-TEST (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum)))
	   (REGISTER (? register)))
  (set-equal-branches!)
  (LAP (CMP W ,(any-reference register)
	    (&U ,(make-non-pointer-literal type datum)))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register))
	   (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum))))
  (set-equal-branches!)
  (LAP (CMP W ,(any-reference register)
	    (&U ,(make-non-pointer-literal type datum)))))

(define-rule predicate
  (EQ-TEST (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum)))
	   (? expression rtl:simple-offset?))
  (set-equal-branches!)
  (LAP (CMP W ,(offset->reference! expression)
	    (&U ,(make-non-pointer-literal type datum)))))

(define-rule predicate
  (EQ-TEST (? expression rtl:simple-offset?)
	   (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum))))
  (set-equal-branches!)
  (LAP (CMP W ,(offset->reference! expression)
	    (&U ,(make-non-pointer-literal type datum)))))