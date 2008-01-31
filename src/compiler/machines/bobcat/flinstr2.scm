#| -*-Scheme-*-

$Id: flinstr2.scm,v 1.9 2008/01/30 20:01:48 cph Exp $

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

;;;; Instruction set description for 68881 floating point processor
;;; Originally provided courtesy of BBN ACI.

(declare (usual-integrations))

(let-syntax
    ((define-binary-flop
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-INSTRUCTION ,(cadr form)

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
			      (7 ,(caddr form))))

	     (((? source float-reg) (? destination float-reg))
	      (WORD (4 #b1111)
		    (3 FPC)
		    (3 #b000)
		    (6 #b000000))
	      (EXTENSION-WORD (3 #b000)
			      (3 source)
			      (3 destination)
			      (7 ,(caddr form)))))))))
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