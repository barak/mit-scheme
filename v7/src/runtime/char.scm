#| -*-Scheme-*-

$Id: char.scm,v 14.32 2007/08/10 18:09:39 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Character Abstraction
;;; package: (runtime character)

(declare (usual-integrations))

(define-primitives
  (char? 1)
  char->integer
  integer->char)

(define-integrable char-code-limit #x110000)
(define-integrable char-bits-limit #x10)
(define-integrable char-integer-limit #x2000000)

(define-integrable (%make-char code bits)
  (integer->char (fix:or (fix:lsh bits 21) code)))

(define-integrable (%char-code char)
  (fix:and (char->integer char) #x1FFFFF))

(define-integrable (%char-bits char)
  (fix:lsh (char->integer char) -21))

(define-guarantee char "character")

(define (make-char code bits)
  (guarantee-limited-index-fixnum code char-code-limit 'MAKE-CHAR)
  (guarantee-limited-index-fixnum bits char-bits-limit 'MAKE-CHAR)
  (%make-char code bits))

(define (code->char code)
  (guarantee-limited-index-fixnum code char-code-limit 'CODE->CHAR)
  (integer->char code))

(define (char-code char)
  (guarantee-char char 'CHAR-CODE)
  (%char-code char))

(define (char-bits char)
  (guarantee-char char 'CHAR-BITS)
  (%char-bits char))

(define (char-bits-set? bits char)
  (guarantee-limited-index-fixnum bits char-bits-limit 'CHAR-BITS-SET?)
  (guarantee-char char 'CHAR-BITS-SET?)
  (fix:= bits (fix:and (%char-bits char) bits)))

(define (char-bits-clear? bits char)
  (guarantee-limited-index-fixnum bits char-bits-limit 'CHAR-BITS-CLEAR?)
  (guarantee-char char 'CHAR-BITS-CLEAR?)
  (fix:= 0 (fix:and (%char-bits char) bits)))

(define (set-char-bits bits char)
  (guarantee-limited-index-fixnum bits char-bits-limit 'SET-CHAR-BITS)
  (guarantee-char char 'SET-CHAR-BITS)
  (%make-char (%char-code char)
	      (fix:or (%char-bits char) bits)))

(define (clear-char-bits bits char)
  (guarantee-limited-index-fixnum bits char-bits-limit 'CLEAR-CHAR-BITS)
  (guarantee-char char 'CLEAR-CHAR-BITS)
  (%make-char (%char-code char)
	      (fix:andc (%char-bits char) bits)))

(define (8-bit-char? object)
  (and (char? object)
       (fix:< (char->integer object) 256)))

(define (guarantee-8-bit-char object #!optional caller)
  caller
  (error:not-8-bit-char object))

(define (char-ascii? char)
  (guarantee-char char 'CHAR-ASCII?)
  (let ((n (char->integer char)))
    (and (fix:< n 256)
	 n)))

(define (char->ascii char)
  (guarantee-8-bit-char char 'CHAR->ASCII)
  (char->integer char))

(define (ascii->char code)
  (guarantee-limited-index-fixnum code 256 'ASCII->CHAR)
  (%make-char code 0))

(define (chars->ascii chars)
  (map char->ascii chars))

(define (char=? x y)
  ;; There's no %CHAR=? because the compiler recodes CHAR=? as EQ?.
  (guarantee-char x 'CHAR=?)
  (guarantee-char y 'CHAR=?)
  (fix:= (char->integer x) (char->integer y)))

(define (char<? x y)
  (guarantee-char x 'CHAR<?)
  (guarantee-char y 'CHAR<?)
  (%char<? x y))

(define-integrable (%char<? x y)
  (fix:< (char->integer x) (char->integer y)))

(define (char<=? x y)
  (guarantee-char x 'CHAR<=?)
  (guarantee-char y 'CHAR<=?)
  (%char<=? x y))

(define-integrable (%char<=? x y)
  (fix:<= (char->integer x) (char->integer y)))

(define (char>? x y)
  (guarantee-char x 'CHAR>?)
  (guarantee-char y 'CHAR>?)
  (%char>? x y))

(define-integrable (%char>? x y)
  (fix:> (char->integer x) (char->integer y)))

(define (char>=? x y)
  (guarantee-char x 'CHAR>=?)
  (guarantee-char y 'CHAR>=?)
  (%char>=? x y))

(define-integrable (%char>=? x y)
  (fix:>= (char->integer x) (char->integer y)))

(define (char-ci=? x y)
  (fix:= (char-ci->integer x) (char-ci->integer y)))

(define (char-ci<? x y)
  (fix:< (char-ci->integer x) (char-ci->integer y)))

(define (char-ci<=? x y)
  (fix:<= (char-ci->integer x) (char-ci->integer y)))

(define (char-ci>? x y)
  (fix:> (char-ci->integer x) (char-ci->integer y)))

(define (char-ci>=? x y)
  (fix:>= (char-ci->integer x) (char-ci->integer y)))

(define-integrable (char-ci->integer char)
  (char->integer (char-upcase char)))

(define (char-downcase char)
  (guarantee-char char 'CHAR-DOWNCASE)
  (%char-downcase char))

(define (%char-downcase char)
  (if (fix:< (%char-code char) 256)
      (%make-char (vector-8b-ref downcase-table (%char-code char))
		  (%char-bits char))
      char))

(define (char-upcase char)
  (guarantee-char char 'CHAR-UPCASE)
  (%char-upcase char))

(define (%char-upcase char)
  (if (fix:< (%char-code char) 256)
      (%make-char (vector-8b-ref upcase-table (%char-code char))
		  (%char-bits char))
      char))

(define downcase-table)
(define upcase-table)

(define (initialize-case-conversions!)
  (set! downcase-table (make-string 256))
  (set! upcase-table (make-string 256))
  (do ((i 0 (fix:+ i 1)))
      ((fix:= i 256))
    (vector-8b-set! downcase-table i i)
    (vector-8b-set! upcase-table i i))
  (let ((case-range
	 (lambda (uc-low uc-high lc-low)
	   (do ((i uc-low (fix:+ i 1))
		(j lc-low (fix:+ j 1)))
	       ((fix:> i uc-high))
	     (vector-8b-set! downcase-table i j)
	     (vector-8b-set! upcase-table j i)))))
    (case-range 65 90 97)
    (case-range 192 214 224)
    (case-range 216 222 248)))

(define 0-code)
(define upper-a-code)
(define lower-a-code)

(define (initialize-package!)
  (set! 0-code (char->integer #\0))
  ;; Next two codes are offset by 10 to speed up CHAR->DIGIT.
  (set! upper-a-code (fix:- (char->integer #\A) 10))
  (set! lower-a-code (fix:- (char->integer #\a) 10))
  (initialize-case-conversions!))

(define (radix? object)
  (and (index-fixnum? object)
       (fix:<= 2 object)
       (fix:<= object 36)))

(define-guarantee radix "radix")

(define (digit->char digit #!optional radix)
  (guarantee-limited-index-fixnum digit
				  (if (default-object? radix)
				      10
				      (begin
					(guarantee-radix radix 'DIGIT->CHAR)
					radix))
				  'DIGIT->CHAR)
  (string-ref "0123456789abcdefghijklmnopqrstuvwxyz" digit))

(define (char->digit char #!optional radix)
  (guarantee-char char 'CHAR->DIGIT)
  (let ((code (char->integer char))
	(radix
	 (cond ((default-object? radix)
		10)
	       ((and (fix:fixnum? radix)
		     (fix:<= 2 radix) (fix:<= radix 36))
		radix)
	       (else
		(error:wrong-type-argument radix "radix" 'CHAR->DIGIT)))))
    (let ((n (fix:- code 0-code)))
      (if (and (fix:<= 0 n) (fix:< n radix))
	  n
	  (let ((n (fix:- code upper-a-code)))
	    (if (and (fix:<= 10 n) (fix:< n radix))
		n
		(let ((n (fix:- code lower-a-code)))
		  (if (and (fix:<= 10 n) (fix:< n radix))
		      n
		      #f))))))))

;;;; Character Names

(define (name->char string)
  (let ((end (string-length string))
	(lose (lambda () (error:bad-range-argument string 'NAME->CHAR))))
    (let loop ((start 0) (bits 0))
      (case (fix:- end start)
	((0)
	 (lose))
	((1)
	 (let ((char (string-ref string start)))
	   (if (not (char-graphic? char))
	       (lose))
	   (make-char (char-code char) bits)))
	(else
	 (let ((hyphen (substring-find-next-char string start end #\-)))
	   (if hyphen
	       (let ((bit (->code named-bits string start hyphen)))
		 (if (not (and bit (fix:= 0 (fix:and bit bits))))
		     (lose))
		 (loop (fix:+ hyphen 1) (fix:or bit bits)))
	       (make-char
		(or (->code named-codes string start end)
		    (and (substring-prefix-ci? "U+" 0 1 string start end)
			 (substring->number string (fix:+ start 2) end 16))
		    (lose))
		bits))))))))

(define (char->name char #!optional slashify?)
  (let ((code (char-code char))
	(bits (char-bits char)))
    (string-append
     (bucky-bits->prefix bits)
     (let ((base-char (if (fix:= 0 bits) char (integer->char code))))
       (cond ((->name named-codes code))
	     ((and (if (default-object? slashify?) #f slashify?)
		   (not (fix:= 0 bits))
		   (or (char=? base-char #\\)
		       (char-set-member? char-set/atom-delimiters base-char)))
	      (string-append "\\" (string base-char)))
	     ((char-graphic? base-char)
	      (string base-char))
	     (else
	      (string-append "U+"
			     (let ((s (number->string code 16)))
			       (string-pad-left s
						(let ((l (string-length s)))
						  (let loop ((n 2))
						    (if (fix:<= l n)
							n
							(loop (fix:* 2 n)))))
						#\0)))))))))

;; This procedure used by Edwin.
(define (bucky-bits->prefix bits)
  (let loop ((entries named-bits))
    (if (pair? entries)
	(if (fix:= 0 (fix:and (caar entries) bits))
	    (loop (cdr entries))
	    (string-append (cadar entries) "-" (loop (cdr entries))))
	"")))

(define (->code entries string start end)
  (let ((entry
	 (find-matching-item entries
	   (lambda (entry)
	     (there-exists? (if (cadr entry) (cdr entry) (cddr entry))
	       (lambda (key)
		 (substring-ci=? string start end
				 key 0 (string-length key))))))))
    (and entry
	 (car entry))))

(define (->name entries n)
  (let ((entry (assv n entries)))
    (and entry
	 (cadr entry))))

(define named-codes
  '((#x00 #f "null" "nul")
    (#x01 #f "soh")
    (#x02 #f "stx")
    (#x03 #f "etx")
    (#x04 #f "eot")
    (#x05 #f "enq")
    (#x06 #f "ack")
    (#x07 #f "bel")
    (#x08 "backspace" "bs")
    (#x09 "tab" "ht")
    (#x0A "newline" "linefeed" "lfd" "lf")
    (#x0B #f "vt")
    (#x0C "page" "ff" "np")
    (#x0D "return" "ret" "cr")
    (#x0E #f "so")
    (#x0F #f "si")
    (#x10 #f "dle")
    (#x11 #f "dc1")
    (#x12 #f "dc2")
    (#x13 #f "dc3")
    (#x14 #f "dc4")
    (#x15 #f "nak")
    (#x16 #f "syn")
    (#x17 #f "etb")
    (#x18 #f "can")
    (#x19 #f "em")
    (#x1A #f "sub" "call")
    (#x1B "escape" "esc" "altmode")
    (#x1C #f "fs")
    (#x1D #f "gs")
    (#x1E #f "rs")
    (#x1F #f "us" "backnext")
    (#x20 "space" "spc" "sp")
    (#x7F "delete" "del" "rubout")
    (#xA0 "nbsp")
    (#xFEFF "bom")))

(define named-bits
  '((#x01 "M" "meta")
    (#x02 "C" "control" "ctrl")
    (#x04 "S" "super")
    (#x08 "H" "hyper")))

(define char-bit:meta #x01)
(define char-bit:control #x02)
(define char-bit:super #x04)
(define char-bit:hyper #x08)