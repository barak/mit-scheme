#| -*-Scheme-*-

$Id: insutl.scm,v 1.10 2002/11/20 19:45:51 cph Exp $

Copyright (c) 1988, 1989, 1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; 68000 utility procedures

(declare (usual-integrations))

;;;; Effective Addressing

;;; *** NOTE: If this format changes, inerly.scm must also be changed! ***

(define ea-tag
  "Effective-Address")

(define (make-effective-address keyword mode register extension categories)
  (vector ea-tag keyword mode register extension categories))

(define (effective-address? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? (vector-ref object 0) ea-tag)))

(define-integrable (ea-keyword ea)
  (vector-ref ea 1))

(define-integrable (ea-mode ea)
  (vector-ref ea 2))

(define-integrable (ea-register ea)
  (vector-ref ea 3))

(define-integrable (ea-extension ea)
  (vector-ref ea 4))

(define-integrable (ea-categories ea)
  (vector-ref ea 5))

(define-integrable (with-ea ea receiver)
  (receiver (ea-keyword ea)
	    (ea-mode ea)
	    (ea-register ea)
	    (ea-extension ea)
	    (ea-categories ea)))

;; For completeness

(define (ea-keyword-early ea)
  (vector-ref ea 1))

(define (ea-mode-early ea)
  (vector-ref ea 2))

(define (ea-register-early ea)
  (vector-ref ea 3))

(define (ea-extension-early ea)
  (vector-ref ea 4))

(define (ea-categories-early ea)
  (vector-ref ea 5))

;;;; Effective Address Extensions

(define-integrable (output-16bit-offset o)
  (EXTENSION-WORD (16 o SIGNED)))

(define-integrable (output-16bit-relative l)
  (EXTENSION-WORD (16 `(- ,l *PC*) SIGNED)))

(define-integrable (output-offset-index-register xtype xr s o)
  (EXTENSION-WORD (1 xtype)
		  (3 xr)
		  (1 s)
		  (3 #b000)
		  (8 o SIGNED)))

(define-integrable (output-relative-index-register xtype xr s l)
  (EXTENSION-WORD (1 xtype)
		  (3 xr)
		  (1 s)
		  (3 #b000)
		  (8 `(- ,l *PC*) SIGNED)))

(define-integrable (output-16bit-address a)
  (EXTENSION-WORD (16 a)))

(define-integrable (output-32bit-address a)
  (EXTENSION-WORD (32 a)))

(define (output-immediate-data immediate-size i)
  (case immediate-size
    ((B)  (EXTENSION-WORD (8 #b00000000) (8 i SIGNED)))
    ((UB) (EXTENSION-WORD (8 #b00000000) (8 i UNSIGNED)))
    ((W)  (EXTENSION-WORD (16 i SIGNED)))
    ((UW) (EXTENSION-WORD (16 i UNSIGNED)))
    ((L)  (EXTENSION-WORD (32 i SIGNED)))
    ((UL) (EXTENSION-WORD (32 i UNSIGNED)))
    (else (error "illegal immediate size" immediate-size))))

;;; Support for 68020 addressing modes

(define-integrable (output-brief-format-extension-word
		    index-register-type index-register
		    index-size factor
		    displacement)
  (EXTENSION-WORD (1 index-register-type)
		  (3 index-register)
		  (1 index-size)
		  (2 factor SCALE-FACTOR)
		  (1 #b0)
		  (8 displacement SIGNED)))

(define (output-full-format-extension-word index-register-type index-register
					   index-size factor
					   base-suppress index-suppress
					   base-displacement-size
					   base-displacement
					   indirection-type
					   outer-displacement-size
					   outer-displacement)
  (let ((output-displacement
	 (lambda (size displacement)
	   (case size
	     ((1) false)
	     ((2) (EXTENSION-WORD (16 displacement SIGNED)))
	     ((3) (EXTENSION-WORD (32 displacement SIGNED)))
	     (else (error "illegal displacement-size" size))))))
    (apply
     optimize-group
     (let loop
	 ((items
	   (list
	    (EXTENSION-WORD
	     (1 index-register-type)
	     (3 index-register)
	     (1 index-size)
	     (2 factor SCALE-FACTOR)
	     (1 #b1)
	     (1 base-suppress)
	     (1 index-suppress)
	     (2 base-displacement-size)
	     (1 #b0)
	     (3 (case indirection-type
		  ((#F) #b000)
		  ((PRE) outer-displacement-size)
		  ((POST) (+ #b100 outer-displacement-size))
		  (else (error "illegal indirection-type" indirection-type)))))
	    (output-displacement base-displacement-size base-displacement)
	    (output-displacement outer-displacement-size outer-displacement))))
       (if (null? items)
	   '()
	   (let ((rest (loop (cdr items))))
	     (if (car items)
		 (cons-syntax (car items) rest)
		 rest)))))))

;;;; Common special cases

(define-integrable (output-@D-indirect register)
  (EXTENSION-WORD (1 #b0)		;index register = data
		  (3 register)
		  (1 #b1)		;index size = longword
		  (2 #b00)		;scale factor = 1
		  (1 #b1)
		  (1 #b1)		;suppress base register
		  (1 #b0)		;don't suppress index register
		  (2 #b01)		;null base displacement
		  (1 #b0)
		  (3 #b000)		;no memory indirection
		  ))

(define (output-@DO-indirect register displacement)
  (EXTENSION-WORD (1 #b0)		;index register = data
		  (3 register)
		  (1 #b1)		;index size = 32 bits
		  (2 #b00)		;scale factor = 1
		  (1 #b1)
		  (1 #b1)		;suppress base register
		  (1 #b0)		;don't suppress index register
		  (2 #b10)		;base displacement size = 16 bits
		  (1 #b0)
		  (3 #b000)		;no memory indirection
		  (16 displacement SIGNED)))

(define (output-32bit-offset offset)
  (EXTENSION-WORD (1 #b0)		;index register = data
		  (3 #b000)		;register number = 0
		  (1 #b0)		;index size = 32 bits
		  (2 #b00)		;scale factor = 1
		  (1 #b1)
		  (1 #b0)		;use base register
		  (1 #b1)		;suppress index register
		  (2 #b11)		;base displacement size = 32 bits
		  (1 #b0)
		  (3 #b000)		;no memory indirection
		  (32 offset SIGNED)))

;;;; Operand Syntaxers.

(define (immediate-words data size)
  (case size
    ((B) (immediate-byte data))
    ((W) (immediate-word data))
    ((L) (immediate-long data))
    ((UB) (immediate-unsigned-byte data))
    ((UW) (immediate-unsigned-word data))
    ((UL) (immediate-unsigned-long data))
    (else (error "Illegal size" size))))

(define (immediate-unsigned-words data size)
  (case size
    ((B UB) (immediate-unsigned-byte data))
    ((W UW) (immediate-unsigned-word data))
    ((L UL) (immediate-unsigned-long data))
    (else (error "Illegal size" size))))

(define-integrable (immediate-byte data)
  `(GROUP ,(make-bit-string 8 0)
	  ,(syntax-evaluation data coerce-8-bit-signed)))

(define-integrable (immediate-unsigned-byte data)
  `(GROUP ,(make-bit-string 8 0)
	  ,(syntax-evaluation data coerce-8-bit-unsigned)))

(define-integrable (immediate-word data)
  (syntax-evaluation data coerce-16-bit-signed))

(define-integrable (immediate-unsigned-word data)
  (syntax-evaluation data coerce-16-bit-unsigned))

(define-integrable (immediate-long data)
  (syntax-evaluation data coerce-32-bit-signed))

(define-integrable (immediate-unsigned-long data)
  (syntax-evaluation data coerce-32-bit-unsigned))

(define-integrable (relative-word address)
  (syntax-evaluation `(- ,address *PC*) coerce-16-bit-signed))

(define-integrable (relative-long address)
  (syntax-evaluation `(- ,address *PC*) coerce-32-bit-signed))

(define-integrable (offset-word data)
  (syntax-evaluation data coerce-16-bit-signed))

(define-integrable (output-bit-string bit-string)
  bit-string)

;;;; Randoms

;; Auxiliary procedure for register list transformers

(define (encode-register-list reg-list encoding)
  (let ((bit-string (make-bit-string 16 #!FALSE)))
    (define (loop regs)
      (if (null? regs)
	  bit-string
	  (let ((place (assq (car regs) encoding)))
	    (if (null? place)
		#F
		(begin
		  (bit-string-set! bit-string (cdr place))
		  (loop (cdr regs)))))))
    (loop reg-list)))
