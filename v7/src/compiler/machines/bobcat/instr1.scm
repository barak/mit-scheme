;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; 68000 Instruction Set Description
;;; Originally from GJS (who did the hard part).

(declare (usual-integrations))
(using-syntax (access assembler-syntax-table compiler-package)

;;;; Effective Addressing

(define (make-effective-address keyword mode register extension categories)
  (vector ea-tag keyword mode register extension categories))

(define (effective-address? object)
  (and (vector? object)
       (not (zero? (vector-size object)))
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

(define (ea-all expression)
  (let ((match-result (pattern-lookup ea-database expression)))
    (and match-result (match-result))))

(define ((ea-filtered filter) expression)
  (let ((ea (ea-all expression)))
    (and ea (filter ea) ea)))

(define (ea-filtered-by-category category)
  (ea-filtered
   (lambda (ea)
     (memq category (ea-categories ea)))))

(define ea-d (ea-filtered-by-category 'DATA))
(define ea-a (ea-filtered-by-category 'ALTERABLE))
(define ea-c (ea-filtered-by-category 'CONTROL))

(define (ea-filtered-by-categories categories)
  (ea-filtered
   (lambda (ea)
     (eq?-subset? categories (ea-categories ea)))))

(define (eq?-subset? x y)
  (or (null? x)
      (and (memq (car x) y)
	   (eq?-subset? (cdr x) y))))

(define ea-d&a (ea-filtered-by-categories '(DATA ALTERABLE)))
(define ea-c&a (ea-filtered-by-categories '(CONTROL ALTERABLE)))
(define ea-m&a (ea-filtered-by-categories '(MEMORY ALTERABLE)))

(define ea-d&-&
  (ea-filtered
   (lambda (ea)
     (and (not (eq? (ea-keyword ea) '&))
	  (memq 'DATA (ea-categories ea))))))

;;; These are just predicates, to be used in conjunction with EA-ALL.

(define (ea-b=>-A ea s)
  (not (and (eq? s 'B) (eq? (ea-keyword ea) 'A))))

(define (ea-a&<b=>-A> ea s)
  (and (memq 'ALTERABLE (ea-categories ea)) (ea-b=>-A ea s)))

;;;; Effective Address Description

(define ea-database
  (make-ea-database
   ((D (? r)) (DATA ALTERABLE) #b000 r)

   ((A (? r)) (ALTERABLE) #b001 r)

   ((@A (? r)) (DATA MEMORY CONTROL ALTERABLE) #b010 r)

   ((@D (? r))
    (DATA MEMORY CONTROL ALTERABLE) #b110 #b000
    (output-@D-indirect r))

   ((@A+ (? r)) (DATA MEMORY ALTERABLE) #b011 r)

   ((@-A (? r)) (DATA MEMORY ALTERABLE) #b100 r)

   ((@AO (? r) (? o))
    (DATA MEMORY CONTROL ALTERABLE) #b101 r
    (output-16bit-offset o))

   ((@AR (? r) (? l))
    (DATA MEMORY CONTROL ALTERABLE) #b101 r
    (output-16bit-relative l))

   ((@DO (? r) (? o))
    (DATA MEMORY CONTROL ALTERABLE) #b110 #b000
    (output-@DO-indirect r o))

   ((@AOX (? r) (? o) (? xtype) (? xr) (? s))
    (QUALIFIER (da? xtype) (wl? s))
    (DATA MEMORY CONTROL ALTERABLE) #b110 r
    (output-offset-index-register xtype xr s o))

   ((@ARX (? r) (? l) (? xtype) (? xr) (? s))
    (QUALIFIER (da? xtype) (wl? s))
    (DATA MEMORY CONTROL ALTERABLE) #b110 r
    (output-relative-index-register xtype xr s l))

   ((W (? a))
    (DATA MEMORY CONTROL ALTERABLE) #b111 #b000
    (output-16bit-address a))

   ((L (? a))
    (DATA MEMORY CONTROL ALTERABLE) #b111 #b001
    (output-32bit-address a))

   ((@PCO (? o))
    (DATA MEMORY CONTROL) #b111 #b010
    (output-16bit-offset o))

   ((@PCR (? l))
    (DATA MEMORY CONTROL) #b111 #b010
    (output-16bit-relative l))

   ((@PCOX (? o) (? xtype) (? xr) (? s))
    (QUALIFIER (da? xtype) (wl? s))
    (DATA MEMORY CONTROL) #b111 #b011
    (output-offset-index-register xtype xr s o))

   ((@PCRX (? l) (? xtype) (? xr) (? s))
    (QUALIFIER (da? xtype) (wl? s))
    (DATA MEMORY CONTROL) #b111 #b011
    (output-relative-index-register xtype xr s l))

   ((& (? i))
    (DATA MEMORY) #b111 #b100
    (output-immediate-data immediate-size i))))

;;;; Effective Address Extensions

(define-integrable (output-16bit-offset o)
  (EXTENSION-WORD (16 o SIGNED)))

(define-integrable (output-16bit-relative l)
  (EXTENSION-WORD (16 `(- ,l *PC*) SIGNED)))

(define-integrable (output-offset-index-register xtype xr s o)
  (EXTENSION-WORD (1 (encode-da xtype))
		  (3 xr)
		  (1 (encode-wl s))
		  (3 #b000)
		  (8 o SIGNED)))

(define-integrable (output-relative-index-register xtype xr s l)
  (EXTENSION-WORD (1 (encode-da xtype))
		  (3 xr)
		  (1 (encode-wl s))
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

(define (output-brief-format-extension-word immediate-size
					    index-register-type index-register
					    index-size scale-factor
					    displacement)
  (EXTENSION-WORD (1 (encode-da index-register-type))
		  (3 index-register)
		  (1 (encode-wl index-size))
		  (2 (encode-bwlq scale-factor))
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
  (EXTENSION-WORD (1 (encode-da index-register-type))
		  (3 index-register)
		  (1 (encode-wl index-size))
		  (2 (encode-bwlq scale-factor))
		  (1 #b1)
		  (1 (if base-suppress? #b1 #b0))
		  (1 (if index-suppress? #b1 #b0))
		  (2 (encode-nwl base-displacement-size))
		  (1 #b0)
		  (3 (case memory-indirection-type
		       ((#F) #b000)
		       ((PRE) (encode-nwl outer-displacement-size))
		       ((POST)
			(+ #b100 (encode-nwl outer-displacement-size))))))
  (output-displacement base-displacement-size base-displacement)
  (output-displacement outer-displacement-size outer-displacement))

(define (output-displacement size displacement)
  (case size
    ((N))
    ((W) (EXTENSION-WORD (16 displacement SIGNED)))
    ((L) (EXTENSION-WORD (32 displacement SIGNED)))))

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

;;;; Symbolic Constants

;(declare (integrate symbol-member bwl? bw? wl? rl? us? da? cc? nwl? bwlq?))

(define ((symbol-member list) expression)
;  (declare (integrate list expression))
  (memq expression list))

(define bwl? (symbol-member '(B W L)))
(define bw?  (symbol-member '(B W)))
(define wl?  (symbol-member '(W L)))
(define rl?  (symbol-member '(R L)))
(define us?  (symbol-member '(U S)))
(define da?  (symbol-member '(D A)))
(define nwl? (symbol-member '(N W L)))
(define bwlq? (symbol-member '(B W L Q)))

(define cc?
  (symbol-member
   '(T F HI LS HS LO CC CS NE EQ VC VS PL MI GE LT GT LE)))

;(declare (integrate symbol-mapping encode-bwl encode-blw encode-bw encode-wl
;		    encode-lw encode-rl encode-us encode-da granularity
;		    encode-cc encode-nwl encode-bwlq))

(define ((symbol-mapping alist) expression)
;  (declare (integrate alist expression))
  (cdr (assq expression alist)))

(define encode-bwl  (symbol-mapping '((B . 0) (W . 1) (L . 2))))
(define encode-blw  (symbol-mapping '((B . 1) (W . 3) (L . 2))))
(define encode-bw   (symbol-mapping '((B . 0) (W . 1))))
(define encode-wl   (symbol-mapping '((W . 0) (L . 1))))
(define encode-lw   (symbol-mapping '((W . 1) (L . 0))))
(define encode-rl   (symbol-mapping '((R . 0) (L . 1))))
(define encode-us   (symbol-mapping '((U . 0) (S . 1))))
(define encode-da   (symbol-mapping '((D . 0) (A . 1))))
(define granularity (symbol-mapping '((B . 8) (W . 16) (L . 32))))
(define encode-nwl (symbol-mapping '((N . 1) (W . 2) (L . 3))))
(define encode-bwlq (symbol-mapping '((B . 0) (W . 1) (L . 2) (Q . 3))))

(define encode-cc
  (symbol-mapping
   '((T . 0) (F . 1) (HI . 2) (LS . 3) (HS . 4) (LO . 5)
     (CC . 4) (CS . 5) (NE . 6) (EQ . 7) (VC . 8) (VS . 9)
     (PL . 10) (MI . 11) (GE . 12) (LT . 13) (GT . 14) (LE . 15))))

(define (register-list? expression)
  (eq?-subset? expression '(D0 D1 D2 D3 D4 D5 D6 D7 A0 A1 A2 A3 A4 A5 A6 A7)))

(define ((encode-register-list encoding) registers)
  (let ((bit-string (make-bit-string 16 #!FALSE)))
    (for-each (lambda (register)
		(bit-string-set! bit-string (cdr (assq register encoding))))
	      registers)
    bit-string))

(define encode-c@a+register-list
  (encode-register-list
   '((A7 . 0) (A6 . 1) (A5 . 2) (A4 . 3) (A3 . 4) (A2 . 5) (A1 . 6) (A0 . 7)
	      (D7 . 8) (D6 . 9) (D5 . 10) (D4 . 11) (D3 . 12) (D2 . 13)
	      (D1 . 14) (D0 . 15))))

(define encode-@-aregister-list
  (encode-register-list
   '((D0 . 0) (D1 . 1) (D2 . 2) (D3 . 3) (D4 . 4) (D5 . 5) (D6 . 6) (D7 . 7)
	      (A0 . 8) (A1 . 9) (A2 . 10) (A3 . 11) (A4 . 12) (A5 . 13)
	      (A6 . 14) (A7 . 15))))

(define-instruction DC
  ((W (? expression))
   (WORD (16 expression SIGNED))))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access lap-syntaxer-package compiler-package)
;;; Scheme Syntax Table: (access assembler-syntax-table compiler-package)
;;; End:
   (WORD (16 expression SIGNED))))