#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/spectrum/instr1.scm,v 1.2 1990/07/19 04:03:55 jinx Rel $

Copyright (c) 1987, 1989, 1990 Massachusetts Institute of Technology

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

;;;; HP Spectrum instruction utilities
;;; Originally from Walt Hill, who did the hard part.

(declare (usual-integrations))

(define-transformer complx
  (lambda (completer)
    (vector (encode-S/SM completer)
	    (cc-val completer)
	    (m-val completer))))

(define-transformer compls
  (lambda (completer)
    (vector (encode-MB completer)
	    (cc-val completer)
	    (m-val completer))))

(define-transformer compledb
  (lambda (completer)
    (cons (encode-n completer)
	  (extract-deposit-condition completer))))

(define-transformer compled
  (lambda (completer)
    (extract-deposit-condition completer)))

(define-transformer complalb
  (lambda (completer)
    (cons (encode-n completer)
	  (arith-log-condition completer))))

(define-transformer complaltfb
  (lambda (completer)
    (list (encode-n completer)
	  (let ((val (arith-log-condition completer)))
	    (if (not (zero? (cadr val)))
		(error "complaltfb: Bad completer" completer)
		(car val))))))

(define-transformer complal
  (lambda (completer)
    (arith-log-condition completer)))

(define-transformer fpformat
  (lambda (completer)
    (encode-fpformat completer)))

(define-transformer fpcond
  (lambda (completer)
    (encode-fpcond completer)))

(define-transformer sr3
  (lambda (value)
    (let ((place (assq value '((0 . 0) (1 . 2) (2 . 4) (3 . 6)
			       (4 . 1) (5 . 3) (6 . 5) (7 . 7)))))
      (if place
	  (cdr place)
	  (error "sr3: Invalid space register descriptor" value)))))

;;;; Utilities

(define-integrable (branch-extend-pco disp nullify?)
  (if (and (= nullify? 1)
	   (negative? disp))
      4
      0))

(define-integrable (branch-extend-nullify disp nullify?)
  (if (and (= nullify? 1)
	  (not (negative? disp)))
      1
      0))

(define-integrable (branch-extend-disp disp)
  (- disp 4))

(define-integrable (branch-extend-edcc cc)
  (remainder (+ cc 4) 8))

(define-integrable (encode-N completers)
  (if (memq 'N completers)
      1
      0))

(define-integrable (encode-S/SM completers)
  (if (or (memq 'S completers) (memq 'SM completers))
      1
      0))

(define-integrable (encode-MB completers)
  (if (memq 'MB completers)
      1
      0))

(define-integrable (m-val compl-list)
  (if (or (memq 'M compl-list)
	  (memq 'SM compl-list)
	  (memq 'MA compl-list)
	  (memq 'MB compl-list))
      1
      0))

(define-integrable (cc-val compl-list)
  (cond ((memq 'P compl-list) 3)
	((memq 'Q compl-list) 2)
	((memq 'C compl-list) 1)
	(else 0)))

(define (extract-deposit-condition compl)
  (cond ((or (null? compl) (memq 'NV compl)) 0)
	((or (memq 'EQ compl) (memq '= compl)) 1)
	((or (memq 'LT compl) (memq '< compl)) 2)
	((memq 'OD compl) 3)
	((memq 'TR compl) 4)
	((or (memq 'LTGT compl) (memq '<> compl)) 5)
	((or (memq 'GTEQ compl) (memq '>= compl)) 6)
	((memq 'EV compl) 7)
	(else
	 ;; This should really error out, but it's hard to
	 ;; arrange given that the compl includes other
	 ;; fields.
	 0)))

(define-integrable (encode-fpformat compl)
  (case compl
    ((DBL) 1)
    ((SGL) 0)
    ((QUAD) 3)
    (else
     (error "Missing Floating Point Format" compl))))

(define-integrable (encode-fpcond fpcond)
  (let ((place (assq fpcond float-condition-table)))
    (if place
	(cadr place)
	(error "encode-fpcond: Unknown condition" fpcond))))

(define float-condition-table
  '((false?	0)
    (false	1)
    (?		2)
    (!<=>	3)
    (=		4)
    (=T		5)
    (?=		6)
    (!<>	7)
    (!?>=	8)
    (<		9)
    (?<		10)
    (!>=	11)
    (!?>	12)
    (<=		13)
    (?<=	14)
    (!>		15)
    (!?<=	16)
    (>		17)
    (?>		18)
    (!<=	19)
    (!?<	20)
    (>=		21)
    (?>=	22)
    (!<		23)
    (!?=	24)
    (<>		25)
    (!=		26)
    (!=T	27)
    (!?		28)
    (<=>	29)
    (true?	30)
    (true	31)))
    
(define (arith-log-condition compl-list)
  ;; Returns (c f)
  (let loop ((compl-list compl-list))
    (if (null? compl-list)
	'(0 0)
	(let ((val (assq (car compl-list) arith-log-condition-table)))
	  (if val
	      (cadr val)
	      (loop (cdr compl-list)))))))

(define arith-log-condition-table
  '((NV      (0 0))
    (EQ      (1 0))
    (=       (1 0))
    (LT      (2 0))
    (<       (2 0))
    (SBZ     (2 0))
    (LTEQ    (3 0))
    (<=      (3 0))
    (SHZ     (3 0))
    (LTLT    (4 0))
    (<<      (4 0))
    (NUV     (4 0))
    (SDC     (4 0))
    (LTLTEQ  (5 0))
    (<<=     (5 0))
    (ZNV     (5 0))
    (SV      (6 0))
    (SBC     (6 0))
    (OD      (7 0))
    (SHC     (7 0))
    (TR      (0 1))
    (LTGT    (1 1))
    (<>      (1 1))
    (GTEQ    (2 1))
    (>=      (2 1))
    (NBZ     (2 1))
    (GT      (3 1))
    (>       (3 1))
    (NHZ     (3 1))
    (GTGTEQ  (4 1))
    (>>=     (4 1))
    (UV      (4 1))
    (NDC     (4 1))
    (GTGT    (5 1))
    (>>      (5 1))
    (VNZ     (5 1))
    (NSV     (6 1))
    (NBC     (6 1))
    (EV      (7 1))
    (NHC     (7 1))))

(define-integrable (tf-adjust opcode condition)
  (+ opcode (* 2 (cadr condition))))

(define (tf-adjust-inverted opcode condition)
  (+ opcode (* 2 (- 1 (cadr condition)))))

(define (make-operator name handler)
  (lambda (value)
    (if (exact-integer? value)
	(handler value)
	`(,name ,value))))	

(let-syntax ((define-operator
	       (macro (name handler)
		 `(define ,name
		    (make-operator ',name ,handler)))))

(define-operator LEFT
  (lambda (number)
    (bit-string->signed-integer
     (bit-substring (signed-integer->bit-string 32 number) 11 32))))

(define-operator RIGHT
  (lambda (number)
    (bit-string->unsigned-integer
     (bit-substring (signed-integer->bit-string 32 number) 0 11)))))