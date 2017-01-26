#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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
  (char->integer 1)
  (integer->char 1))

(define-integrable char-code-limit #x110000)
(define-integrable char-bits-limit #x10)
(define-integrable char-integer-limit #x2000000)

(define-guarantee char "character")

(define (make-char code bits)
  (guarantee-limited-index-fixnum code char-code-limit 'MAKE-CHAR)
  (guarantee-limited-index-fixnum bits char-bits-limit 'MAKE-CHAR)
  (%make-char code bits))

(define-integrable (%make-char code bits)
  (integer->char (fix:or (fix:lsh bits 21) code)))

(define (code->char code)
  (guarantee-limited-index-fixnum code char-code-limit 'CODE->CHAR)
  (integer->char code))

(define (char-code char)
  (fix:and (char->integer char) #x1FFFFF))

(define (char-bits char)
  (fix:lsh (char->integer char) -21))

(define (char-bits-set? bits char)
  (guarantee-limited-index-fixnum bits char-bits-limit 'CHAR-BITS-SET?)
  (fix:= bits (fix:and (char-bits char) bits)))

(define (char-bits-clear? bits char)
  (guarantee-limited-index-fixnum bits char-bits-limit 'CHAR-BITS-CLEAR?)
  (fix:= 0 (fix:and (char-bits char) bits)))

(define (set-char-bits bits char)
  (guarantee-limited-index-fixnum bits char-bits-limit 'SET-CHAR-BITS)
  (%make-char (char-code char)
	      (fix:or (char-bits char) bits)))

(define (clear-char-bits bits char)
  (guarantee-limited-index-fixnum bits char-bits-limit 'CLEAR-CHAR-BITS)
  (%make-char (char-code char)
	      (fix:andc (char-bits char) bits)))

(define (8-bit-char? object)
  (and (char? object)
       (fix:< (char->integer object) #x100)))

(define (guarantee-8-bit-char object #!optional caller)
  caller
  (if (not (8-bit-char? object))
      (error:not-8-bit-char object)))

(define (char-ascii? char)
  (let ((n (char->integer char)))
    (and (fix:< n #x100)
	 n)))

(define (char->ascii char)
  (guarantee-8-bit-char char 'CHAR->ASCII)
  (char->integer char))

(define (ascii->char code)
  (guarantee-limited-index-fixnum code #x100 'ASCII->CHAR)
  (%make-char code 0))

(define (chars->ascii chars)
  (map char->ascii chars))

(define (char=? x y)
  (fix:= (char->integer x) (char->integer y)))

(define (char<? x y)
  (fix:< (char->integer x) (char->integer y)))

(define (char<=? x y)
  (fix:<= (char->integer x) (char->integer y)))

(define (char>? x y)
  (fix:> (char->integer x) (char->integer y)))

(define (char>=? x y)
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

(define (char=-predicate char)
  (guarantee char? char 'char=-predicate)
  (lambda (char*)
    (char=? char* char)))

(define (char-ci=-predicate char)
  (guarantee char? char 'char-ci=-predicate)
  (lambda (char*)
    (char-ci=? char* char)))

(define (char-downcase char)
  (%case-map-char char downcase-table))

(define (char-upcase char)
  (%case-map-char char upcase-table))

(define-integrable (%case-map-char char table)
  (if (fix:< (char-code char) #x100)
      (%make-char (bytevector-u8-ref table (char-code char))
		  (char-bits char))
      char))

(define downcase-table)
(define upcase-table)

(define (initialize-case-conversions!)
  (set! downcase-table (make-bytevector #x100))
  (set! upcase-table (make-bytevector #x100))
  (do ((i 0 (fix:+ i 1)))
      ((fix:= i #x100))
    (bytevector-u8-set! downcase-table i i)
    (bytevector-u8-set! upcase-table i i))
  (let ((case-range
	 (lambda (uc-low uc-high lc-low)
	   (do ((i uc-low (fix:+ i 1))
		(j lc-low (fix:+ j 1)))
	       ((fix:> i uc-high))
	     (bytevector-u8-set! downcase-table i j)
	     (bytevector-u8-set! upcase-table j i)))))
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
  '((#x00 "null" "nul")
    (#x01 #f "soh")
    (#x02 #f "stx")
    (#x03 #f "etx")
    (#x04 #f "eot")
    (#x05 #f "enq")
    (#x06 #f "ack")
    (#x07 "alarm" "bel")
    (#x08 "backspace" "bs")
    (#x09 "tab" "ht")
    (#x0A "newline" "linefeed" "lfd" "lf")
    (#x0B #f "vt")
    (#x0C "page" "formfeed" "ff" "np")
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

(define char-bit:meta #x01)
(define char-bit:control #x02)
(define char-bit:super #x04)
(define char-bit:hyper #x08)

(define named-bits
  `((,char-bit:meta "M" "meta")
    (,char-bit:control "C" "control" "ctrl")
    (,char-bit:super "S" "super")
    (,char-bit:hyper "H" "hyper")))

;;;; Unicode characters

(define (unicode-char? object)
  (and (char? object)
       (legal-code-32? (char->integer object))))

(define (unicode-scalar-value? object)
  (and (index-fixnum? object)
       (legal-code-32? object)))

(define-guarantee unicode-char "a Unicode character")
(define-guarantee unicode-scalar-value "a Unicode scalar value")

(define (unicode-char->scalar-value char #!optional caller)
  (let ((cp (char->integer char)))
    (if (not (legal-code-32? cp))
	(error:not-a unicode-char? char caller))
    cp))

(define-integrable (legal-code-32? cp)
  (and (fix:< cp char-code-limit)
       (not (utf16-surrogate? cp))
       (not (non-character? cp))))

(define (legal-code-16? pt)
  (and (not (utf16-surrogate? pt))
       (not (non-character? pt))))

(define-integrable (utf16-surrogate? cp)
  (fix:= #xD800 (fix:and #xF800 cp)))

(define-integrable (utf16-high-surrogate? cp)
  (fix:= #xD800 (fix:and #xFC00 cp)))

(define-integrable (utf16-low-surrogate? cp)
  (fix:= #xDC00 (fix:and #xFC00 cp)))

(define-integrable (non-character? cp)
  (or (and (fix:<= #xFDD0 cp) (fix:< cp #xFDF0))
      (fix:= #xFFFE (fix:and #xFFFE cp))))

(define-integrable (guarantee-cp-is-character cp)
  (if (non-character? cp)
      (error "Code point is a non-character:" cp)))

(define-integrable (guarantee-cp-in-range cp)
  (if (not (fix:< cp char-code-limit))
      (error "Value is not a code point:" cp)))

(define-integrable (guarantee-cp-not-utf16-surrogate cp)
  (if (utf16-surrogate? cp)
      (error "Code point is a UTF-16 surrogate:" cp)))

(define-integrable (extract-bits word mask shift)
  (fix:lsh (fix:and word mask) shift))

(define-integrable (insert-bits word mask shift)
  (fix:and (fix:lsh word shift) mask))

;;;; UTF-{8,16,32} encoders

(define (char-utf8-byte-length char)
  (let ((sv (unicode-char->scalar-value char 'char-utf8-byte-length)))
    (cond ((fix:< sv #x80) 1)
	  ((fix:< sv #x800) 2)
	  ((fix:< sv #x10000) 3)
	  (else 4))))

(define (encode-utf8-char! bytes index char)
  (let ((sv (unicode-char->scalar-value char 'encode-utf8-char!)))

    (define-integrable (initial-byte leader offset)
      (fix:or leader (fix:lsh sv offset)))

    (define-integrable (trailing-byte offset)
      (fix:or #x80 (insert-bits sv #x3F offset)))

    (cond ((fix:< sv #x00000080)
	   (bytevector-u8-set! bytes index sv)
	   (fix:+ index 1))
	  ((fix:< sv #x00000800)
	   (bytevector-u8-set! bytes index (initial-byte #xC0 -6))
	   (bytevector-u8-set! bytes (fix:+ index 1) (trailing-byte 0))
	   (fix:+ index 2))
	  ((fix:< sv #x00010000)
	   (bytevector-u8-set! bytes index (initial-byte #xE0 -12))
	   (bytevector-u8-set! bytes (fix:+ index 1) (trailing-byte -6))
	   (bytevector-u8-set! bytes (fix:+ index 2) (trailing-byte 0))
	   (fix:+ index 3))
	  (else
	   (bytevector-u8-set! bytes index (initial-byte #xF0 -18))
	   (bytevector-u8-set! bytes (fix:+ index 1) (trailing-byte -12))
	   (bytevector-u8-set! bytes (fix:+ index 2) (trailing-byte -6))
	   (bytevector-u8-set! bytes (fix:+ index 3) (trailing-byte 0))
	   (fix:+ index 4)))))

(define (char-utf16-byte-length char)
  (if (fix:< (unicode-char->scalar-value char 'char-utf16-byte-length) #x10000)
      2
      4))

(define (utf16-char-encoder setter caller)
  (lambda (bytes index char)
    (let ((sv (unicode-char->scalar-value char caller)))
      (cond ((fix:< sv #x10000)
	     (setter bytes index sv)
	     (fix:+ index 2))
	    (else
	     (let ((n (fix:- sv #x10000)))
	       (setter bytes index
		       (fix:or #xD800 (insert-bits n #x3FF -10)))
	       (setter bytes (fix:+ index 2)
		       (fix:or #xDC00 (insert-bits n #x3FF 0))))
	     (fix:+ index 4))))))

(define encode-utf16be-char!
  (utf16-char-encoder bytevector-u16be-set! 'encode-utf16be-char!))

(define encode-utf16le-char!
  (utf16-char-encoder bytevector-u16le-set! 'encode-utf16le-char!))

(define (char-utf32-byte-length char)
  (unicode-char->scalar-value char 'char-utf32-byte-length)
  4)

(define (utf32-char-encoder setter caller)
  (lambda (bytes index char)
    (setter bytes index (unicode-char->scalar-value char caller))))

(define encode-utf32be-char!
  (utf32-char-encoder bytevector-u32be-set! 'encode-utf32be-char!))

(define encode-utf32le-char!
  (utf32-char-encoder bytevector-u32le-set! 'encode-utf32le-char!))

;;;; UTF-{8,16,32} decoders

(define (initial-byte->utf8-char-length byte)
  (guarantee byte? byte 'initial-byte->utf8-char-length)
  (cond ((utf8-initial-byte-1? byte) 1)
	((utf8-initial-byte-2? byte) 2)
	((utf8-initial-byte-3? byte) 3)
	((utf8-initial-byte-4? byte) 4)
	(else (error "Illegal UTF-8 initial byte:" byte))))

(define (decode-utf8-char bytes index)
  (integer->char
   (let ((b0 (bytevector-u8-ref bytes index)))
     (cond ((utf8-initial-byte-1? b0)
	    b0)
	   ((utf8-initial-byte-2? b0)
	    (decode-utf8-2 b0
			   (bytevector-u8-ref bytes (fix:+ index 1))))
	   ((utf8-initial-byte-3? b0)
	    (decode-utf8-3 b0
			   (bytevector-u8-ref bytes (fix:+ index 1))
			   (bytevector-u8-ref bytes (fix:+ index 2))))
	   ((utf8-initial-byte-4? b0)
	    (decode-utf8-4 b0
			   (bytevector-u8-ref bytes (fix:+ index 1))
			   (bytevector-u8-ref bytes (fix:+ index 2))
			   (bytevector-u8-ref bytes (fix:+ index 3))))
	   (else
	    (error "Illegal UTF-8 initial byte:" b0))))))

(define (decode-utf8-2 b0 b1)
  (if (not (and (fix:> b0 #xC1)
		(utf8-trailing-byte? b1)))
      (error "Ill-formed UTF-8 sequence:" b0 b1))
  (fix:or (extract-bits b0 #x1F 6)
	  (extract-bits b1 #x3F 0)))

(define (decode-utf8-3 b0 b1 b2)
  (if (not (and (or (fix:> b0 #xE0) (fix:> b1 #x9F))
		(utf8-trailing-byte? b1)
		(utf8-trailing-byte? b2)))
      (error "Ill-formed UTF-8 sequence:" b0 b1 b2))
  (let ((cp
	 (fix:or (fix:or (extract-bits b0 #x0F 12)
			 (extract-bits b1 #x3F 6))
		 (extract-bits b2 #x3F 0))))
    (guarantee-cp-not-utf16-surrogate cp)
    (guarantee-cp-is-character cp)
    cp))

(define (decode-utf8-4 b0 b1 b2 b3)
  (if (not (and (or (fix:> b0 #xF0) (fix:> b1 #x8F))
		(utf8-trailing-byte? b1)
		(utf8-trailing-byte? b2)
		(utf8-trailing-byte? b3)))
      (error "Ill-formed UTF-8 sequence:" b0 b1 b2 b3))
  (let ((cp
	 (fix:or (fix:or (extract-bits b0 #x07 18)
			 (extract-bits b1 #x3F 12))
		 (fix:or (extract-bits b2 #x3F 6)
			 (extract-bits b3 #x3F 0)))))
    (guarantee-cp-in-range cp)
    (guarantee-cp-is-character cp)
    cp))

(define-integrable (utf8-initial-byte-1? byte)
  (fix:= #x00 (fix:and #x80 byte)))

(define-integrable (utf8-initial-byte-2? byte)
  (fix:= #xC0 (fix:and #xE0 byte)))

(define-integrable (utf8-initial-byte-3? byte)
  (fix:= #xE0 (fix:and #xF0 byte)))

(define-integrable (utf8-initial-byte-4? byte)
  (fix:= #xF0 (fix:and #xF8 byte)))

(define-integrable (utf8-trailing-byte? byte)
  (fix:= #x80 (fix:and #xC0 byte)))

(define (initial-u16->utf16-char-length u16)
  (guarantee u16? u16 'initial-u16->utf16-char-length)
  (if (utf16-high-surrogate? u16) 4 2))

(define (utf16-char-decoder getter)
  (lambda (bytes index)
    (let ((d0 (getter bytes index)))
      (if (utf16-low-surrogate? d0)
	  (error "Ill-formed UTF-16 sequence:" d0))
      (let ((cp
	     (if (utf16-high-surrogate? d0)
		 (let ((d1 (getter bytes (fix:+ index 2))))
		   (if (not (utf16-low-surrogate? d1))
		       (error "Ill-formed UTF-16 sequence:" d0 d1))
		   (fix:+ (fix:or (extract-bits d0 #x3FF 10)
				  (extract-bits d1 #x3FF 0))
			  #x10000))
		 d0)))
	(guarantee-cp-in-range cp)
	(guarantee-cp-is-character cp)
	(integer->char cp)))))

(define decode-utf16be-char
  (utf16-char-decoder bytevector-u16be-ref))

(define decode-utf16le-char
  (utf16-char-decoder bytevector-u16le-ref))

(define (initial-u32->utf32-char-length u32)
  (guarantee u32? u32 'initial-u32->utf32-char-length)
  4)

(define (utf32-char-decoder getter)
  (lambda (bytes index)
    (let ((u32 (getter bytes index)))
      (if (not (< u32 char-code-limit))
	  (error "Value is not a code point:" u32))
      (guarantee-cp-not-utf16-surrogate u32)
      (guarantee-cp-is-character u32)
      (integer->char u32))))

(define decode-utf32be-char
  (utf32-char-decoder bytevector-u32be-ref))

(define decode-utf32le-char
  (utf32-char-decoder bytevector-u32le-ref))