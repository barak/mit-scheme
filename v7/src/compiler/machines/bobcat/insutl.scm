#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/insutl.scm,v 1.1 1987/06/25 10:35:23 jinx Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; 68000 utility procedures

(declare (usual-integrations))

;;;; Effective Addressing

(define (make-effective-address keyword mode register extension categories)
  (vector ea-tag keyword mode register extension categories))

(define (effective-address? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? (vector-ref object 0) ea-tag)))

(define ea-tag
  "Effective-Address")

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
    ((B)
     (EXTENSION-WORD (8 #b00000000)
		     (8 i SIGNED)))
    ((W)
     (EXTENSION-WORD (16 i SIGNED)))
    ((L)
     (EXTENSION-WORD (32 i SIGNED)))
    (else
     (error "OUTPUT-IMMEDIATE-DATA: illegal immediate size"
	    immediate-size))))

;;; New stuff for 68020

;; (? index-register-type da)
;; (? index-size wl)
;; (? scale-factor bwlq)
;; (? base-displacement-size nwl)
;; (? outer-displacement-size nwl)

(define (output-brief-format-extension-word immediate-size
					    index-register-type index-register
					    index-size scale-factor
					    displacement)
  (EXTENSION-WORD (1 index-register-type)
		  (3 index-register)
		  (1 index-size)
		  (2 scale-factor)
		  (1 #b0)
		  (8 displacement SIGNED)))

(define (output-full-format-extension-word immediate-size
					   index-register-type index-register
					   index-size scale-factor
					   base-suppress? index-suppress?
					   base-displacement-size
					   base-displacement
					   memory-indirection-type
					   outer-displacement-size
					   outer-displacement)
  (EXTENSION-WORD (1 index-register-type)
		  (3 index-register)
		  (1 index-size)
		  (2 scale-factor)
		  (1 #b1)
		  (1 (if base-suppress? #b1 #b0))
		  (1 (if index-suppress? #b1 #b0))
		  (2 base-displacement-size)
		  (1 #b0)
		  (3 (case memory-indirection-type
		       ((#F) #b000)
		       ((PRE) outer-displacement-size)
		       ((POST)
			(+ #b100 outer-displacement-size)))))
  (output-displacement base-displacement-size base-displacement)
  (output-displacement outer-displacement-size outer-displacement))

(define (output-displacement size displacement)
  (case size
    ((1))
    ((2) (EXTENSION-WORD (16 displacement SIGNED)))
    ((3) (EXTENSION-WORD (32 displacement SIGNED)))))

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

;;;; Operand Syntaxers.

(define (immediate-words data size)
  (case size
    ((B) (immediate-byte data))
    ((W) (immediate-word data))
    ((L) (immediate-long data))
    (else (error "IMMEDIATE-WORD: Illegal size" size))))

(define-integrable (immediate-byte data)
  `(GROUP ,(make-bit-string 8 0)
	  ,(syntax-evaluation data coerce-8-bit-signed)))

(define-integrable (immediate-word data)
  (syntax-evaluation data coerce-16-bit-signed))

(define-integrable (immediate-long data)
  (syntax-evaluation data coerce-32-bit-signed))

(define-integrable (relative-word address)
  (syntax-evaluation `(- ,address *PC*) coerce-16-bit-signed))

(define-integrable (offset-word data)
  (syntax-evaluation data coerce-16-bit-signed))

(define-integrable (output-bit-string bit-string)
  bit-string)

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