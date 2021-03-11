#| -*-Scheme-*-

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

;;;; Compiler Debugging Support

(declare (usual-integrations))

(define (po object)
  (let ((object (->tagged-vector object)))
    (write-line object)
    (for-each pp ((tagged-vector/description object) object))))

(define (debug/where object)
  (cond ((compiled-code-block? object)
	 (write-line (compiled-code-block/debugging-info object)))
	((compiled-code-address? object)
	 (write-line
	  (compiled-code-block/debugging-info
	   (compiled-code-address->block object)))
	 (write-string "\nOffset: ")
	 (write-string
	  (number->string (compiled-code-address->offset object) 16)))
	(else
	 (error "debug/where -- what?" object))))

(define (write-rtl-instructions rtl port)
  (write-instructions
   (lambda ()
     (with-output-to-port port
       (lambda ()
	 (for-each show-rtl-instruction rtl))))))

(define (dump-rtl filename)
  (write-instructions
   (lambda ()
     (with-output-to-file (pathname-new-type (->pathname filename) "rtl")
       (lambda ()
	 (for-each show-rtl-instruction
		   (linearize-rtl *rtl-graphs*
				  '()
				  '()
				  false)))))))

(define (show-rtl rtl)
  (newline)
  (pp-instructions
   (lambda ()
     (for-each show-rtl-instruction rtl))))

(define (show-bblock-rtl bblock)
  (newline)
  (pp-instructions
   (lambda ()
     (bblock-walk-forward (->tagged-vector bblock)
       (lambda (rinst)
	 (show-rtl-instruction (rinst-rtl rinst)))))))

(define (write-instructions thunk)
  (fluid-let ((*show-instruction* write)
	      (*unparser-radix* 16)
	      (*unparse-uninterned-symbols-by-name?* true))
    (thunk)))

(define (pp-instructions thunk)
  (fluid-let ((*show-instruction* pretty-print)
	      (*pp-primitives-by-name* false)
	      (*unparser-radix* 16)
	      (*unparse-uninterned-symbols-by-name?* true))
    (thunk)))

(define *show-instruction*)

(define (show-rtl-instruction rtl)
  (if (memq (car rtl)
	    '(LABEL CONTINUATION-ENTRY CONTINUATION-HEADER IC-PROCEDURE-HEADER
		    OPEN-PROCEDURE-HEADER PROCEDURE-HEADER CLOSURE-HEADER
		    ;; New stuff
		    RETURN-ADDRESS PROCEDURE TRIVIAL-CLOSURE CLOSURE
		    EXPRESSION
		    ))
      (newline))
  (*show-instruction* rtl)
  (newline))

(define procedure-queue)
(define procedures-located)

