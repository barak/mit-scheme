#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; C-output fake object assembler
;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;; Numbers in the string table are encoded by a very simple scheme:
;; The first byte is the least significant byte, and so on.
;; A byte encodes 7 bits of the number being encoded.
;; Any byte whose most-significant bit (bit 7) is 0 denotes the end
;; of the substring encoding the number.
;; Thus, a number below 128 can be encoded in a single byte.
;; A number below 16384 can be encoded in two bytes, and so on.
;; Unlike UTF8, numbers with an arbitrary number of bits can be
;; encoded.  Of course, the string-search properties of UTF8 are not
;; present, but they are not necessary here.

;; String-like objects (strings, bit strings) consist of an encoded
;; length followed by the string contents

;; - For character strings, the string is the string itself
;; - For floats, the string is some C-parseable representation of the
;;   float (e.g. F notation), in double-precision.
;; - For bit strings, the string is numeric value (little endian) of
;;   the contents.

;;; General objects

(define-syntax define-enumeration
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (let ((name (cadr form))
	   (elements (cddr form)))
       `(BEGIN
	  ,@(let loop ((n 0)
		       (elements elements)
		       (code '())
		       (bindings '()))
	      (if (not (pair? elements))
		  (reverse!
		   (cons `(define ,(symbol-append '* name '*)
			    '#(,@(reverse! bindings)))
			 code))
		  (let* ((next (car elements))
			 (suffix (if (pair? next)
				     (car next)
				     next))
			 (n (if (not (pair? next))
				n
				(let ((m (cadr next)))
				  (if (< m n)
				      (error "define-enumeration: Overlap"
					     next)
				      m)))))
		    (let ((name (symbol-append name '/ suffix)))
		      (loop (+ n 1)
			    (cdr elements)
			    (cons `(DEFINE-INTEGRABLE ,name ,n)
				  code)
			    (cons `(,name ,n) bindings)))))))))))

;; Given how ulongs are represented (first add one), and that
;; the 0 opcode is illegal, there should only be null characters
;; in the output string if a component string contained a null
;; character itself.  Obviously these could be escaped, but fortunately,
;; at least gcc allows null characters within strings just fine.
;; Furthermore, if we ever gzip the strings, there will be null characters
;; anyway.

(define-enumeration stackify-opcode

;; General objects

illegal					; Make null characters very rare
escape					; For future growth
push-+fixnum				; magnitude in string table
push--fixnum				; magnitude in string table
push-+integer				; digit string mag. in string table
push--integer				; digit string mag. in string table
push-false
push-true
push-nil
push-flonum				; decimal string in string table
push-cons-ratnum
push-cons-recnum
push-string				; in string table
push-symbol				; name in string table
push-uninterned-symbol			; name in string table
push-char				; char bits + char code in string table
push-bit-string				; length + little-endian
push-empty-cons
pop-and-set-car
pop-and-set-cdr
push-cons*
push-empty-vector			; length in string table
pop-and-vector-set			; length in string table
push-vector				; length in string table
push-empty-record			; length in string table
pop-and-record-set			; length in string table
push-record				; length in string table
push-lookup				; length in string table
store					; length in string table
push-constant				; length in string table
push-unassigned
push-primitive				; arity + name in string table
push-primitive-lexpr			; name in string table
push-nm-header				; length in string table
push-label-entry			; rel. dispatch off. in string table
push-linkage-header-operator		; length in string table
push-linkage-header-reference		; length in string table
push-linkage-header-assignment		; length in string table
push-linkage-header-global		; length in string table
push-linkage-header-closure		; length in string table
push-ulong				; value in string table
push-label-descriptor			; code word + offset in string table
cc-block-to-entry			; entry offset in string table
retag-cc-block				; no arguments
push-return-code			; datum in string table
;; 44

;; Fast fixnums

(push-0 #o200)
push-1
push-2
push-3
push-4
push-5
push-6
push--1
;; 8

;; Fast pairs

(push-cons*-0 #o210)
push-cons*-1
push-cons*-2
push-cons*-3
push-cons*-4
push-cons*-5
push-cons*-6
push-cons*-7
;; 8

;; Fast vectors

(pop-and-vector-set-0 #o220)
pop-and-vector-set-1
pop-and-vector-set-2
pop-and-vector-set-3
pop-and-vector-set-4
pop-and-vector-set-5
pop-and-vector-set-6
pop-and-vector-set-7
push-vector-1
push-vector-2
push-vector-3
push-vector-4
push-vector-5
push-vector-6
push-vector-7
push-vector-8
;; 16

;; Fast records

(pop-and-record-set-0 #o240)
pop-and-record-set-1
pop-and-record-set-2
pop-and-record-set-3
pop-and-record-set-4
pop-and-record-set-5
pop-and-record-set-6
pop-and-record-set-7
push-record-1
push-record-2
push-record-3
push-record-4
push-record-5
push-record-6
push-record-7
push-record-8
;; 16

;; Fast register lookup

(push-lookup-0 #o260)
push-lookup-1
push-lookup-2
push-lookup-3
push-lookup-4
push-lookup-5
push-lookup-6
push-lookup-7
;; 8

;; Fast register assignment

(store-0 #o270)
store-1
store-2
store-3
store-4
store-5
store-6
store-7
;; 8

;; Fast primitives

(push-primitive-0 #o300)		; name in string table
push-primitive-1			; name in string table
push-primitive-2			; name in string table
push-primitive-3			; name in string table
push-primitive-4			; name in string table
push-primitive-5			; name in string table
push-primitive-6			; name in string table
push-primitive-7			; name in string table
;; 8
)

(define stackify/fast-fixnum-opcodes
  (vector stackify-opcode/push-0
	  stackify-opcode/push-1
	  stackify-opcode/push-2
	  stackify-opcode/push-3
	  stackify-opcode/push-4
	  stackify-opcode/push-5
	  stackify-opcode/push-6))

(define stackify/fast-cons*-opcodes
  (vector
   stackify-opcode/push-cons*-0
   stackify-opcode/push-cons*-1
   stackify-opcode/push-cons*-2
   stackify-opcode/push-cons*-3
   stackify-opcode/push-cons*-4
   stackify-opcode/push-cons*-5
   stackify-opcode/push-cons*-6
   stackify-opcode/push-cons*-7))

(define stackify/fast-vector-set-opcodes
  (vector
   stackify-opcode/pop-and-vector-set-0
   stackify-opcode/pop-and-vector-set-1
   stackify-opcode/pop-and-vector-set-2
   stackify-opcode/pop-and-vector-set-3
   stackify-opcode/pop-and-vector-set-4
   stackify-opcode/pop-and-vector-set-5
   stackify-opcode/pop-and-vector-set-6
   stackify-opcode/pop-and-vector-set-7))

(define stackify/fast-vector-opcodes
  (vector
   #f
   stackify-opcode/push-vector-1
   stackify-opcode/push-vector-2
   stackify-opcode/push-vector-3
   stackify-opcode/push-vector-4
   stackify-opcode/push-vector-5
   stackify-opcode/push-vector-6
   stackify-opcode/push-vector-7
   stackify-opcode/push-vector-8))

(define stackify/fast-record-set-opcodes
  (vector
   stackify-opcode/pop-and-record-set-0
   stackify-opcode/pop-and-record-set-1
   stackify-opcode/pop-and-record-set-2
   stackify-opcode/pop-and-record-set-3
   stackify-opcode/pop-and-record-set-4
   stackify-opcode/pop-and-record-set-5
   stackify-opcode/pop-and-record-set-6
   stackify-opcode/pop-and-record-set-7))

(define stackify/fast-record-opcodes
  (vector
   #f
   stackify-opcode/push-record-1
   stackify-opcode/push-record-2
   stackify-opcode/push-record-3
   stackify-opcode/push-record-4
   stackify-opcode/push-record-5
   stackify-opcode/push-record-6
   stackify-opcode/push-record-7
   stackify-opcode/push-record-8))

(define stackify/fast-lookup-opcodes
  (vector
   stackify-opcode/push-lookup-0
   stackify-opcode/push-lookup-1
   stackify-opcode/push-lookup-2
   stackify-opcode/push-lookup-3
   stackify-opcode/push-lookup-4
   stackify-opcode/push-lookup-5
   stackify-opcode/push-lookup-6
   stackify-opcode/push-lookup-7))

(define stackify/fast-store-opcodes
  (vector
   stackify-opcode/store-0
   stackify-opcode/store-1
   stackify-opcode/store-2
   stackify-opcode/store-3
   stackify-opcode/store-4
   stackify-opcode/store-5
   stackify-opcode/store-6
   stackify-opcode/store-7))

(define stackify/fast-primitive-opcodes
  (vector
   stackify-opcode/push-primitive-0
   stackify-opcode/push-primitive-1
   stackify-opcode/push-primitive-2
   stackify-opcode/push-primitive-3
   stackify-opcode/push-primitive-4
   stackify-opcode/push-primitive-5
   stackify-opcode/push-primitive-6
   stackify-opcode/push-primitive-7))

(define *stackify/opcode-name* #f)

(define (stackify/setup-debug!)
  (or *stackify/opcode-name*
      (let* ((result (make-vector 256 #f))
	     (vec *stackify-opcode*)
	     (len (vector-length vec)))
	(do ((i 0 (1+ i)))
	    ((>= i len) unspecific)
	  (let ((binding (vector-ref vec i)))
	    (vector-set! result (cadr binding) (car binding))))
	(set! *stackify/opcode-name* result)
	unspecific)))

(define (stackify/c-quotify str)
  (let* ((len (string-length str))
	 (res (make-string len)))
    (do ((i 0 (1+ i)))
	((>= i len) res)
      (let ((c (string-ref str i)))
	(case c
	  ((#\*)
	   (string-set! res i #\S))
	  ((#\- #\/)
	   (string-set! res i #\_))
	  ((#\+)
	   (string-set! res i #\P))
	  (else
	   (string-set! res i c)))))))

(define (stackify/dump-c-enums output)
  (with-output-to-file output
    (lambda ()
      (for-each
       write-string
       (let ((time (get-decoded-time)))
	 (list "/* Emacs: this is -*- C -*- code. */\n\n"
	       "#ifndef STACKOPS_H\n"
	       "#define STACKOPS_H\n\n"
	       "/* C code produced\n   "
	       (decoded-time/date-string time)
	       " at "
	       (decoded-time/time-string time)
	       "\n */\n\n"
	       "typedef enum\n"
	       "{\n")))
      (let* ((vec *stackify-opcode*)
	     (len (vector-length vec))
	     (max -1))
	(do ((i 0 (1+ i)))
	    ((>= i len) unspecific)
	  (let* ((binding (vector-ref vec i))
		 (value (cadr binding)))
	    (if (> value max)
		(set! max value))
	    (for-each
	     write-string
	     (list "\t"
		   (stackify/C-quotify (symbol-name (car binding)))
		   " = 0"
		   (if (zero? value)
		       ""
		       (number->string value 8))
		   ",\n"))))
	(for-each
	 write-string
	 (list "\t"
	       "N_STACKIFY_OPCODE = "
	       (number->string (1+ max))
	       "\n")))
      (for-each
       write-string
       (list "} stackify_opcode_t;\n\n"
	     "#endif /* STACKOPS_H */\n")))))
