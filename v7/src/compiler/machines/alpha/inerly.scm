#| -*-Scheme-*-

$Id: inerly.scm,v 1.6 2001/12/23 17:20:57 cph Exp $

Copyright (c) 1992-1999, 2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;; Alpha Instruction Set Macros.  Early version
;;; Package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define-syntax define-instruction
  (non-hygienic-macro-transformer
   (lambda (opcode . patterns)
     `(SET! EARLY-INSTRUCTIONS
	    (CONS
	     (LIST ',opcode
		   ,@(map (lambda (pattern)
			    `(early-parse-rule
			      ',(car pattern)
			      (lambda (pat vars)
				(early-make-rule
				 pat
				 vars
				 (scode-quote
				  (instruction->instruction-sequence
				   ,(parse-instruction (cadr pattern)
						       (cddr pattern)
						       #t)))))))
			  patterns))
		  EARLY-INSTRUCTIONS)))))