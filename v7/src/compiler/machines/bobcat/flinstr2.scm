#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/flinstr2.scm,v 1.1 1989/07/25 12:26:27 arthur Rel $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; Instruction set description for 68881 floating point processor
;;; Originally provided courtesy of BBN ACI.

(declare (usual-integrations))

(let-syntax
    ((define-binary-flop
       (macro (name bits)
	 `(define-instruction ,name

	    (((? type float-source-format)
	      (? source ea-d)
	      (? destination float-reg))
	     (WORD (4 #b1111)
		   (3 FPC)
		   (3 #b000)
		   (6 source SOURCE-EA 'L))
	     (EXTENSION-WORD (3 #b010)
			     (3 type)
			     (3 destination)
			     (7 ,bits)))

	    (((? source float-reg) (? destination float-reg))
	     (WORD (4 #b1111)
		   (3 FPC)
		   (3 #b000)
		   (6 #b000000))
	     (EXTENSION-WORD (3 #b000)
			     (3 source)
			     (3 destination)
			     (7 ,bits)))))))

  (define-binary-flop FADD	#b0100010)
  (define-binary-flop FCMP	#b0111000)
  (define-binary-flop FDIV	#b0100000)
  (define-binary-flop FMOD	#b0100001)
  (define-binary-flop FMUL	#b0100011)
  (define-binary-flop FREM	#b0100101)
  (define-binary-flop FSCALE	#b0100110)
  (define-binary-flop FSGLDIV	#b0100100)
  (define-binary-flop FSGLMUL	#b0100111)
  (define-binary-flop FSUB	#b0101000))

(define-instruction FTST

  (((? type float-source-format) (? ea ea-d))
   (WORD (4 #b1111)
	 (3 FPC)
	 (3 #b000)
	 (6 ea SOURCE-EA 'L))
   (EXTENSION-WORD (3 #b010)
		   (3 type)
		   (3 #b000)
		   (7 #b0111010)))

  (((? source float-reg))
   (WORD (4 #b1111)
	 (3 FPC)
	 (3 #b000)
	 (6 #b000000))
   (EXTENSION-WORD (3 #b000)
		   (3 source)
		   (3 #b000)
		   (7 #b0111010))))

(define-instruction FB

  (((? cc float-cc) (@PCR (? target)))
   (GROWING-WORD (disp `(- ,target (+ *PC* 2)))
		 ((-32768 32767)
		  (WORD (4 #b1111)
			(3 FPC)
			(3 #b010)
			(6 cc)
			(16 disp SIGNED)))
		 ((() ())
		  (WORD (4 #b1111)
			(3 FPC)
			(3 #b011)
			(6 cc)
			(32 disp SIGNED))))))